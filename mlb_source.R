#load packages from library
library(tidyverse);library(rvest);library(magrittr);library(XML);library(janitor);library(stringr);library(rlang);library(readxl);

#player id map
id_map <- read.csv('http://crunchtimebaseball.com/master.csv', stringsAsFactors = F) %>% select(fg_name, fg_pos)
pos_fg <- id_map %>% select(fg_name, fg_pos) %>% set_colnames(c('name','pos')) %>% 
  mutate(pos = str_extract(pos,"[^/]+")) %>% filter(!is.na(pos))

#load subtraction vals for SGPs
hit_subt <- read_xlsx('C:/Users/jimmy/OneDrive/Documents/MLB/subtr_show.xlsx', sheet = 'hit') %>% data.frame()
pit_subt <- read_xlsx('C:/Users/jimmy/OneDrive/Documents/MLB/subtr_show.xlsx', sheet = 'pit')



get_page_hit <- function(page = 1) {
  
url <- paste0('https://www.fangraphs.com/projections.aspx?pos=all&stats=bat&type=steamer&team=',page,'&lg=all&players=0')

dat <- read_html(url)
dat %>% html_node('#ProjectionBoard1_dg1_ctl00') %>% html_table(fill = TRUE) %>% 
  bind_rows()
         
}

get_page_pit <- function(page = 1) {
  
  url <- paste0('https://www.fangraphs.com/projections.aspx?pos=all&stats=pit&type=steamer&team=',page,'&lg=all&players=0')
  
  dat <- read_html(url)
  dat %>% html_node('#ProjectionBoard1_dg1_ctl00') %>% html_table(fill = TRUE) %>% 
    bind_rows()
  
}

teams <- 1:30

proj_hit <- map_df(teams, ~ {
  df <- get_page_hit(.x)
  Sys.sleep(sample(10, 1) * 0.1)
  return(df)
})

proj_pit <- map_df(teams, ~ {
  df <- get_page_pit(.x)
  Sys.sleep(sample(10, 1) * 0.1)
  return(df)
})

#clean hitters
clean_proj <- function(df) {

df %<>% filter(!grepl('12',X1)) %>% unique()

names(df) <- as.matrix(df[1, ])
df <- df[-1, ] %>% clean_names() %>% select(-x) %>% filter(g > 0)
df[,3:ncol(df)] = as.numeric(as.matrix(df[,3:ncol(df)]))
df


}

distinct_player <- function(df, unsel) {
  
  df_names <- names(df)
  sel <- rlang::syms(setdiff(df_names, unsel))
  
  df %>% 
    dplyr::distinct(!!!sel, .keep_all = TRUE)
  
}

proj_hit %<>% clean_proj() %>% left_join(pos_fg, by = 'name') %>% drop_na(pos) %>% filter(pos != 'P') %>% distinct_player('pos') %>% 
  select(1:2, pos, everything())
proj_pit %<>% clean_proj() %>% mutate(pos = ifelse(gs/g < 0.5, 'RP','SP')) %>% unique() %>% 
  select(1:2, pos, everything())


  
sgp <- function(stat, stat2) {
  
 round(stat/hit_subt[which(hit_subt$cat==stat2),2],2)
  
}
sgp_hit <- proj_hit %>% mutate(rsgp = sgp(r,'r'),
                               x2bsgp = sgp(x2b,'x2b'),
                               x3bsgp = sgp(x3b,'x3b'),
                               hrsgp = sgp(hr,'hr'),
                               rbisgp = sgp(rbi,'rbi'),
                               sbsgp = sgp(sb,'sb'),
                               sosgp = sgp(so,'so'),
                               obpsgp = round((((h+bb+hbp)+2802)/(pa+8297)-0.337)/hit_subt[which(hit_subt$cat=='obp'),2],2),
                               totsgp = rsgp+x2bsgp+x3bsgp+hrsgp+rbisgp+sbsgp+sosgp+obpsgp) %>% arrange(desc(totsgp))
