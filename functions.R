get_episode_links <- function(){
  base.url <- "http://transcripts.foreverdreaming.org/"
  all_links <- c()
  for(page in c(0,25,50,75)){
    links <- 
      paste0("http://transcripts.foreverdreaming.org/viewforum.php?f=15&start=",page) %>% 
      read_html() %>%
      html_nodes(".topic-titles") %>% 
      html_nodes("a") %>% html_attr("href")
    
    #get full links
    links <- stri_replace_first(links,fixed="./",replacement = base.url)
    #remove the info
    links <- links[-1]

    all_links <- c(all_links,links)
  }
  return(all_links)
}

################################################################################
#NOTE: only works for the first season. The others dont have character names
################################################################################
get_episodes <- function(links){
  season <- c(rep(1,6),rep(2,13),rep(3:6,each=16),rep(7,10)) #there are some episodes missing
  episode <- c(1:6,1:13,rep(1:16,4),1:10)
  k <- 1
  for(l in links){
    tmp <- read_html(l) %>% 
      html_node(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "postbody", " " ))]') %>% 
      html_text() %>% 
      str_replace_all("\t","") %>% 
      str_split("\n") %>% 
      .[[1]]
    
    tmp <- tmp[stri_detect(tmp,regex = "^[a-zA-Z]+:")]
    tmp <- stri_split(tmp,regex = ":")
    if(any(unlist(lapply(tmp,length))>2)){
      ids <- which(unlist(lapply(tmp,length))>2)
      for(i in ids){
        len <- length(tmp[[i]])
        tmp[[i]] <- c(tmp[[i]][1],paste(tmp[[i]][2:len],collapse = " "))
      }
    }
    person <- lapply(tmp,function(x)x[1]) %>% unlist
    text <- lapply(tmp,function(x)x[2]) %>% unlist
    df <- tibble(name=person,text=text,season=season[k],episode=episode[k])
    if(k==1){
      write_csv(df,"data/episode_lines.csv")
    } else{
      write_csv(df,"data/episode_lines.csv",append = T,col_names = F)
    }
    print(k)
    k <- k+1  
  }
  
}

################################################################################
# characters per episode
get_imdb_cast <- function(){
  url <- "http://www.imdb.com/title/tt1520211/epcast"
  tab <- 
  url %>% 
    read_html() %>% 
    # html_node('#tn15main')
    html_table()
  tab <- tab[1:99]
  tab <- lapply(1:99,function(x) tab[[x]]$X4)
  tab <- lapply(1:99,function(x) tab[[x]][!grepl("(credit only)",tab[[x]])])
  tab
}
################################################################################