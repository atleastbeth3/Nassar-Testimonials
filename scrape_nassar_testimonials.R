
################################
########### Setup ##############
################################
setwd("C:/Users/taman/Documents/Nassar Survivors")

library(tibble)
library(dplyr)
library(magrittr)
library(rvest) #web parsing
library (XML)
library (qdapRegex)
library (sqldf)

########################################
############## SCRAPING ################
########################################
##Scrape
url="https://www.glamour.com/story/the-survivors-of-larry-nassar-in-their-own-words"
download.file(url,destfile="nassar_testimonials.html",quiet=TRUE)

html=read_html("nassar_testimonials.html")

########Parse Names ##########
names = html %>% 
     html_nodes("h2") %>% 
     rm_between(., "-->", "<!--", extract=TRUE) %>% unlist %>% as.character

names_id = html %>%
          html_nodes("h2") %>%
          html_attr("data-reactid") %>% as.character %>% as.numeric

## create bound for join with text
names_id_bound = c ( names_id, "9000") %>% .[2:length(.)] %>% as.numeric

names_df = data.frame (names = names
                      , names_id = names_id
                      , names_id_bound = names_id_bound)


####### Parse Text ############
text = html %>% 
        html_nodes ("p") %>%
        rm_between(., "-->", "<!--", extract=TRUE) 


for ( i in 1:length(text) ) {
  
  text[[i]] = paste(text[[i]], sep="", collapse="") 
  
}


text %<>% unlist %>% as.character  


text_id =  html %>%
            html_nodes("p") %>%
            html_attr("data-reactid") %>% 
            as.character %>% as.numeric

text_df = data.frame (text = text
                      , text_id = text_id )

########## Testimonials Data Frame ###########
#############################################
testimonials = sqldf ( " select *
                          from text_df a 
                          left join names_df b 
                              on a.text_id between b.names_id and b.names_id_bound "  )

testimonials$text %<>% as.character


## Extract Direct quotes from Survivors 
testimonials$text2 = ifelse ( grepl("\U201C", testimonials$text) & !grepl( "\U201D", testimonials$text), paste0(testimonials$text,"\U201D" ), testimonials$text ) 

testimonials$text2 %<>% gsub ("\U201C" , "\"" , .) %>%
                         gsub ("\U201D", "\"" , .)

#quotes = rm_between(testimonials$text2, "\U201C", "\U201D", extract=TRUE) 

quotes = rm_between(testimonials$text2, "\"", "\"", extract=TRUE) 


for ( i in 1:length(quotes) ) {
  
  quotes[[i]] = paste(quotes[[i]], sep="", collapse="") 
  
}

testimonials$quote = quotes %>% unlist %>%as.character()

rm(quotes)

save (testimonials, file="nassar_testimonials.rda")


