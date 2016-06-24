
# load packages -----------------------------------------------------------

library(RCurl)
library(XML)
library(dplyr)
library(stringr)
library(tm)

speech_return("Walker")
# read in data ------------------------------------------------------------
setwd("C:\\Users\\Davidjohn\\Desktop\\Dropbox\\ICCSS - Datathon Team 5\\RemarksAndStatement")

file_id = read.table("id_candidates_speech.txt"); colnames(can_id) = "id"
can_id = cbind(read.table("name_candidates_speech.txt"),
               file_id %>% unique); colnames(can_id) = c("first","last","id")

setwd("C:\\Users\\Davidjohn\\Desktop\\Dropbox\\ICCSS - Datathon Team 5\\RemarksAndStatement\\text_sentiment")

# find and order all the files
file_names = list.files()
split <- strsplit(file_names, "speech_example_") 
split <- as.numeric(sapply(split, function(x) x <- sub("_cleaned_sentiment.txt", "", x[2])))
file_names <- file_names[order(split)]



# remapping of names ------------------------------------------------------



# how many files for each dude --------------------------------------------

rownames(table(file_id))[table(file_id)>15]

can_id = can_id[which(can_id$id %in% as.numeric(rownames(table(file_id))[table(file_id)>15])),]
# create the datasets -----------------------------------------------------


# data_list = list()
# # let look at sanders and clinton
# for( i in 1:length(file_names)){
#   data_list[[i]] = read.table(file_names[i])
#   print(i)
# }

topics = 
  read.table("C:\\Users\\Davidjohn\\Desktop\\Dropbox\\ICCSS - Datathon Team 5\\mallet_lda_testrun\\speeches_10_keys.out",
             stringsAsFactors = FALSE)
topics = topics[,3:ncol(topics)]

# topics, 3 = iran/isis, 4 = security, 5 immigraion, 6 freedom,  9 education/care
topics = topics[c(3,4,5,6,9),]
# topic_sen_matrix = can_sen_topic()

topic_sen_matrix = matrix(0, ncol = nrow(can_id), nrow = nrow(topics))
colnames(topic_sen_matrix) = can_id$last
rownames(topic_sen_matrix) = paste0("topic_",1:nrow(topic_sen_matrix))

for(i in 1:ncol(topic_sen_matrix)){
  data_list = speech_return(colnames(topic_sen_matrix)[i])
  print(paste("Working on candidate",colnames(topic_sen_matrix)[i]))
  for(j in 1:nrow(topic_sen_matrix)){
    topic_sen_matrix[j,i] = 
      topic_sen(data_list,topics[j,]) %>% mean
  }
}
topic_sen_matrix[is.nan(topic_sen_matrix)] = 0
topic_sen_matrix = topic_sen_matrix %>% t
colnames(topic_sen_matrix) = c("middle_east","homeland_security","immigration","freedom","education_care")

topic_sen_matrix = topic_sen_matrix[order(topic_sen_matrix[,1]),]

write.table(x = topic_sen_matrix %>% round(.,2), file = "topic_sen_matrix.txt", sep = "\t")

# clustering of users -----------------------------------------------------
wss <- (nrow(topic_sen_matrix)-1)*sum(apply(topic_sen_matrix,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(topic_sen_matrix,
                                     centers=i)$withinss)
plot(wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

cl = kmeans(topic_sen_matrix,4)
cl$cluster %>% sort %>% as.matrix

write.table(cl$cluster %>% sort %>% as.matrix,file = "cluster_groups_sentiment.txt", sep = "\t")

# functions ---------------------------------------------------------------
# functions finds all the speeches of a canididate
speech_return = function(can_last_name){
  id = can_id[which(can_id$last == can_last_name),]$id
  id = which(file_id == id)
  
  res1 = res2 = list()
  for(i in 1:length(id)){
    res1[[i]] = read.table(file_names[id[i]],stringsAsFactors = FALSE)[,1]
    res2[[i]] = read.table(file_names[id[i]],stringsAsFactors = FALSE)[,2]
    }
  return(results = list(text = res1, sen = res2))
}



# function find returns average senitment of a 
topic_sen = function(data_list,topic_words){
  pattern = paste0(topic_words,collapse = "|"); pattern = paste0("(",pattern,")", collapse = "")

  pos = 
    lapply(X = data_list$text,FUN = function(X){
      return(grep(pattern,X))
    })
  
  results = numeric(length(pos))
  for(i in 1:length(results)){
    results[i] = data_list$sen[[i]][pos[[i]]] %>% mean(.,na.rm = TRUE)
  }
  
  return(results[!is.nan(results)])
}

can_sen_topic = function(can_id = can_id,topics = topics){
  
  topic_sen_matrix = matrix(0, ncol = nrow(can_id), nrow = nrow(topics))
  colnames(topic_sen_matrix) = can_id$last
  rownames(topic_sen_matrix) = paste0("topic_",1:nrow(topic_sen_matrix))
  
  for(i in 1:ncol(topic_sen_matrix)){
    data_list = speech_return(colnames(topic_sen_matrix)[i])
    print(paste("Working on candidate",colnames(topic_sen_matrix)[i]))
    for(j in 1:nrow(topic_sen_matrix)){
      topic_sen_matrix[j,i] = 
        topic_sen(data_list,topics[j,]) %>% mean
    }
  }
  topic_sen_matrix[is.nan(topic_sen_matrix)] = 0
  topic_sen_matrix = topic_sen_matrix %>% t
  return(topic_sen_matrix)
}