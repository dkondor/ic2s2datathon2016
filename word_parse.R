

# packages ----------------------------------------------------------------
library(RCulr)
library(XML)
library(dplyr)
library(stringr)
library(tm)

# load datasets -----------------------------------------------------------

setwd("C:\\Users\\Davidjohn\\Desktop\\Dropbox\\ICCSS - Datathon Team 5\\RemarksAndStatement\\text_files")

test =
  scan("speech_example_2.txt", what = "character") %>%
  paste(.,collapse=  " ")#load in documents

# find all the match a criteric
people = str_match_all(test, paste("[a-zA-Z ]*:",sep = '' ))[[1]] %>% str_trim %>% unique
# test code to parse names of people --------------------------------------
# left_border = "clinton :"
# right_border = "Unidentified Female :"




# find all the file names in folder
file_names = list.files()
j = 1 
for( i in file_names){
  # i = file_names[2]
  print(paste0("currently working on file ", i))
  print(paste0("We are ", j/length(i), " through the list."))
  cat("\n\n______________________________________________________\n\n")
  
  # i = file_names[2]
  data = # data %>% class
    scan(i, what = "character") %>%
    paste(.,collapse=  " ")# load in documents
  data = data %>% tolower() # text to lower
  
  # now we want to find the text between the proper speaker
#   people = str_match_all(data, paste("[a-zA-Z ]*:",sep = '' ))[[1]] %>% str_trim %>% unique
#   left_border = c(paste0("clinton",c(":", " :"))) # need the canididate
#   right_border = people[-which(people %in% c(left_border,":"))]
#   
#   left_border = paste0(left_border, collapse = "|")
#   right_border = paste0(right_border, collapse = "|")
# 
#   str_match(test, paste0(left_border, '(.+)',right_border,sep = '' ))
#   
  # now finish cleaning the text
  data = Corpus(VectorSource(data)) # create a corpus for my sanity
  data = tm_map(data, removeWords, stopwords('english')) # remove stop words
  data = data.frame(text = sapply(data, as.character), stringsAsFactors = FALSE)
  data = str_replace_all(data,"[[:punct:]]","")
    
  write(x = data, file = paste0(
    strsplit(i,"\\.")[[1]][1],
    "_cleanted",".txt")
                                )
  j = j + 1
}


# for the first match
# test2 = str_match(test, paste(left_border, '(.+)',sep = ''))[,2]
left_border = "Clinton :"
right_border = "Unidentified Female :"
str_match(test, paste(left_border, '(.+)',right_border,sep = '' ))[,2]

test2 = test2 %>% tolower()
test2 = Corpus(VectorSource(test2))
test2 <- tm_map(test2, removeWords, stopwords('english'))
test2[[1]]$content
str_replace_all(test2,"[[:punct:]]"," ")

readHTMLTable(candidate_pages)







