
# load packages -----------------------------------------------------------

library(RCurl)
library(XML)
library(dplyr)
library(stringr)
library(tm)





# read in the data --------------------------------------------------------
setwd("C:\\Users\\Davidjohn\\Desktop\\Dropbox\\ICCSS - Datathon Team 5\\RemarksAndStatement\\text_files_with_punctuation")

file_names = list.files()
j = 1
for( i in file_names){
  
  # i = file_names[2]
  print(paste0("currently working on file ", i))
  print(paste0("We are ", j/length(file_names), " through the list."))
  cat("\n\n______________________________________________________\n\n")
  
  #i = file_names[2]
  data = # data %>% class
    scan(i, what = "character") %>%
    paste(.,collapse=  " ")# load in documents
  data = data %>% tolower() # text to lower
  # data = strsplit(x = data, "\\.")[[1]]
  write.table(x = data, file = paste0(
    strsplit(i,"\\.")[[1]][1],
    "_cleaned",".txt"
    ),row.names = FALSE, col.names = FALSE
  )
  j = j + 1
  
}


# actually easier if it is all in one fil ---------------------------------
setwd("C:\\Users\\Davidjohn\\Desktop\\Dropbox\\ICCSS - Datathon Team 5\\RemarksAndStatement\\text_files_with_punctuation_cleaned")

data_list = list()
file_names = list.files()
j = 1
for( i in file_names){
  # i = file_names[2]
  print(paste0("currently working on file ", i))
  print(paste0("We are ", j/length(file_names), " through the list."))
  cat("\n\n______________________________________________________\n\n")
  
  #i = file_names[2]
  data= # data %>% class
    scan(i, what = "character") %>%
    paste(.,collapse=  " ")# load in documents
  data = data %>% tolower() # text to lower
  data = strsplit(x = data, "\\.")[[1]]
#   write.table(x = data, file = paste0(
#     strsplit(i,"\\.")[[1]][1],
#     "_cleaned",".txt"
#   ),row.names = FALSE, col.names = FALSE
#   )
  
  data_list[[j]] = data
  j = j + 1
}

length(data_list)
data_full = c("FILE_SEP",data_list[[1]])
for(i in 2:length(data_list)){

  data_full = c(data_full,"FILE_SEP", data_list[[i]])
  print(i/length(data_list))
}
data_full = c(data_full,"FILE_SEP")
data_full %>% length
data_full %>% head(.,40)

head(data_full)

sum(data_full == "FILE_SEP")

write.table(x = data_full, file = "speech_example_cleaned_full.txt",row.names = FALSE, col.names = FALSE)
data_full %>%length

# pulling the dataset backtogether ----------------------------------------

sentiment = read.table("speech_example_cleaned_full+results_fix.txt", sep = "\t", fill = TRUE, allowEscapes = TRUE)
sentiment = rbind(c(1,-1),sentiment)
sentiment[which(is.na(sentiment),arr.ind = TRUE)] = 0
sentiment = sentiment[,1] + sentiment[,2]
sentiment %>% mean(.,na.rm = TRUE)
ind = which(data_full == "FILE_SEP")

setwd("C:\\Users\\Davidjohn\\Desktop\\Dropbox\\ICCSS - Datathon Team 5\\RemarksAndStatement\\text_sentiment")

file_names[[i]]

for( i in 1:(length(ind)-1) ){
  data = cbind( data_list[[i]],
  sentiment[(ind[i]+1):(ind[i+1]-1)])
  
  colnames(data) = c("text","sentiment")
  
  write.table(x = data, 
              file = paste0(strsplit(file_names[i],"\\.")[[1]][1],
                            "_sentiment.txt"),
              row.names = FALSE, col.names = FALSE)
  
}
