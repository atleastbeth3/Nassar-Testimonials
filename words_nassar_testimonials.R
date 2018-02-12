################################
########### Setup ##############
################################
setwd("C:/Users/taman/Documents/Nassar Survivors")

library (tibble)
library (dplyr)
library (magrittr)

library (qdapRegex)
library (sqldf)

library (tidytext) # sentiments
library (tm) # has stopwords
library (reshape2)
library (stringr)
library (tidyr)
library (wordcloud)

load ("nassar_testimonials.rda")
####################################
########## CLEAN UP ################
####################################
conv_fun <- function (x) iconv (x, "latin1", "ASCII", "")

testimonials$clean_quote <-  testimonials$quote %>% 
     conv_fun %>%
     gsub ( "<a href=.*>", "", .) %>%
     gsub("[[:digit:]]", " ", .) %>%
     gsub ("\'t", "t" ,.) %>%
     str_replace_all( "https?[[:punct:][:alnum:]]*", " ") %>%
     gsub("[[:punct:]]", " ", .) %>%
     gsub ( "\\s{2,}" , " ", . ) %>% #reduce repeating spaces to single space
     gsub ("^\\s|\\s$" , "", .)

testimonials%<>%   unnest_tokens(word, clean_quote)

############################
##### Word Properties ######
############################
stopwords = stopwords("en") %>% as.data.frame
colnames (stopwords) = "word"

testimonials %<>%  anti_join (stopwords) 

#################################################
############ Wordcloud Preparation ##############
#################################################
## Get in format for input in wordcloud website
afinn = get_sentiments ("afinn")

####################
## Neutral Words ###
####################
words_df = testimonials %>% 
     left_join (afinn, by = "word") %>%
     group_by (word, score) %>% 
     summarize (freq = n() )     %>%
     filter (is.na(score)) %>%
     mutate (score = 0) %>%
     anti_join (stopwords,  by = "word") %>%
     filter (word != "na")

context_words = c("larry" , "nassar")

# Assign COlors
words_df$hex = ifelse (words_df$word %in% context_words, "#7f0000", "#7d7d7d" )


# Write text input for wordcloud.com
input =   paste0 (words_df$freq," ", words_df$word, " ", words_df$hex) %>% 
     toString %>% 
     gsub ("," ,"\n" ,.) %>% 
     gsub ("\n ", "\n",.)

connection = file("wordcloud_input_nassar_test.txt")
writeLines( input, connection)
close(connection)

##########################
##### EMOTIVE WORDS ######
##########################
hex_df = data.frame ( score = c(-5:5)
                      , hex = c("#a70000" , "#ff0000" , "#ff5252" , "#ff7b7b",
                                "#ffbaba", "#8c4954" , "#8bc34a" , "#7cb342",
                                 "#689f38" ,"#558b2f", "#33691e") )

words_df = testimonials %>% 
     inner_join (afinn, by = "word") %>%
     group_by (word, score) %>% 
     summarize (freq = n() )  %>%
     filter (word != "like")


#Assign COlors to words
words_df %<>% left_join (hex_df, by = "score")

input =   paste0 (words_df$freq," ", words_df$word, " ", words_df$hex) %>% 
     toString %>% 
     gsub ("," ,"\n" ,.) %>% 
     gsub ("\n ", "\n",.)

connection = file("wordcloud_input_nassar_test.txt")
writeLines( input, connection)
close(connection)

