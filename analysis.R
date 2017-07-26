library(rvest)
library(stringr)
library(stringi)
library(tidyverse)
library(tidytext)
library(hrbrthemes)
library(igraph)
library(magrittr)

source("functions.R")
##############################################
season <- c(rep(1,6),rep(2,13),rep(3:7,each=16))
episode <- c(1:6,1:13,rep(1:16,5))
##############################################
# check if data has been scraped yet
if(file.exists("data/episode_lines.csv")){
  text_df <- read_csv("data/episode_lines.csv")
} else {
  links <- get_episode_links()
  get_episodes(links)
}

text_df %>% group_by(name) %>% count() %>% 
  dplyr::filter(n>10) %>% 
  ggplot(aes(x=reorder(name,n),y=n))+geom_col()+coord_flip()+
  theme_ipsum_rc(grid=F)+
  labs(y="number of lines",x="",title="The Talking Dead",subtitle="Number of lines per character in season 1")
ggsave("talking_dead.png")
#####################################
ep_char <- get_imdb_cast()
# char_per_ep <- lapply(ep_char,length) %>% unlist
# 
# 
# char_count <- tibble(character=unlist(ep_char),season=rep(season,char_per_ep))
# char_count %>% group_by(character,season) %>% count() %>% 
#   dplyr::filter(n>5,!grepl("uncredited",character)) %>% 
#   ggplot()+geom_col(aes(x=character,y=n))+
#   coord_flip()+
#   facet_wrap(~season)

#character appearances ----
char_count <- ep_char %>% unlist() %>% table(.) %>% sort(.) %>% c(.)
tibble(name=names(char_count),count=unname(char_count)) %>% 
  dplyr::filter(count>10,!grepl("uncredited",name)) %>% 
  ggplot(aes(x=reorder(name,count),y=count/99))+geom_col()+coord_flip()+
  theme_ipsum_rc(grid = F)+
  scale_y_percent()+
  labs(y="Percentage of episodes appeared in",x="",title="Episodes per Character",subtitle="All episodes from S01E01 to S07E16")
ggsave("episodes_per_character.png",width=6,height=6)

#walkers per episode ----
walker_count <- lapply(1:99,function(x) sum(grepl("Walker",ep_char[[x]]))) %>% unlist()
tibble(count=walker_count,
       season=factor(season),
       episode=factor(episode),
       id=1:99) %>% 
  ggplot()+geom_col(aes(x=id,y=count,fill=season))+
  theme_ipsum_rc(grid=F)+
  labs(x="Episode",y="Count",title="Walking Dead in The Walking Dead",
       subtitle="Numbers of walkers per episode appearing in castlist")+
  theme(legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 1))

ggsave("walker_count.png")

#coappearences ----
core_char <- 
tibble(name=names(char_count),count=unname(char_count)) %>% 
  dplyr::filter(count>6,!grepl("uncredited",name)) %>% .$name

el_df <- 
lapply(1:99,function(x) tibble(name=ep_char[[x]],episode=x)) %>%
  do.call("rbind",.)  %>% 
  dplyr::filter(!grepl("Walker",name))

el <- matrix(0,0,2)
for(i in 1:99){
  tmp <- el_df %>% dplyr::filter(episode==i,name%in%core_char) %>% .$name
  tmp_el <- expand.grid(tmp,tmp)
  el <- rbind(el,tmp_el)
}
g <- graph_from_edgelist(as.matrix(el),directed = FALSE)
E(g)$weight <- 1
g
g <- igraph::simplify(g,edge.attr.comb = "sum")
#edges are counted twice
E(g)$weight <- E(g)$weight/2
plot(g)
write.graph(g,"data/cooccurrence.graphml",format="graphml")
#####################################
