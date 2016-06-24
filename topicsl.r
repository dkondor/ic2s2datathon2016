# topicsl.r -- run topic detection with several parameters on the texts, plot topic compositions

stop("do not run the whole file!")

library(ggplot2)
library(reshape2)

setwd("/home/dkondor/program/ic2s2datathon2016/mallet-2.0.8RC3/")
dir.create("crun1")
system("bin/mallet import-dir --input ../RemarksAndStatement/text_files_with_punctuation/ --output crun1/speeches.mallet --keep-sequence --remove-stopwords --extra-stopwords ../stoplist2.txt")

for(t in 10:20) {
  cmd = paste0("bin/mallet train-topics --input crun1/speeches.mallet --num-topics ",t," --output-state crun1/speeches_",t,".out.gz --output-doc-topics crun1/speeches_",t,"_topics.out --output-topic-keys crun1/speeches_",t,"_keys.out --optimize-interval 20 --topic-word-weights-file crun1/speeches_",t,"_wordsw.out --word-topic-counts-file crun1/speeches_",t,"_wordsc.out --output-model crun1/speeches_",t,"_model --inferencer-filename crun1/speeches_",t,"_inferencer --evaluator-filename crun1/speeches_",t,"_evaluator --num-threads 4")
  system(cmd,ignore.stderr = TRUE)
  print(t)
}
for(t in 10:20) {
  cmd = paste0("awk '{split($2,a,\"_\"); split(a[6],b,\".\"); $1=$2=\"\"; print b[1],$0}' crun1/speeches_",t,"_topics.out | sort -n > crun1/speeches_",t,"_topics.dat")
  system(cmd)
}
for(t in 10:20) {
  cmd = paste0("sort -k 1n,1 -k 3gr,3 crun1/speeches_",t,"_wordsw.out | awk 'BEGIN{t=0;c=0;}{if($1 != t) { t = $1; c = 0; } if(c<50) print $0; c++;}' > crun1/speeches_",t,"_wordsw_top50.dat")
  system(cmd)
}  



# create plots with avg. topic scores per candidate
# + also try to look at distribution of speeches in topic space
# load which document corresponds to which candidate
# awk '{print NR-1,$0}' id_candidates_speech.txt > id_candidates_speech.dat
# awk '{printf "%d\t%s\n",NR-1,$0}' name_candidates_speech.txt > name_candidates_speech.dat
candidates = read.table("../RemarksAndStatement/id_candidates_speech.dat")
names(candidates) = c("docid","cid")
cnames = read.table("../RemarksAndStatement/name_candidates_speech.dat")
names(cnames) = c("cid","firstname","lastname")
cnames$name = paste0(cnames$firstname," ",cnames$lastname)

for(t in 10:20) {
  # load the topic values per speech
  topics = read.table(paste0("crun1/speeches_",t,"_topics.dat"))
  names(topics)[1] = "docid"
  topics = merge(topics,candidates,by="docid")
  topics = merge(topics,cnames,by="cid")
  tavg = aggregate(topics[3:(t+2)],by=topics["lastname"],FUN=mean)
  tavg = melt(tavg,"lastname")
  tavg$variable = as.integer(substr(tavg$variable,2,4)) - 1
  p1 = ggplot(tavg) + geom_bar(aes(x=variable,y=value,
    fill=factor(variable)),stat="identity") + facet_wrap(~lastname) +
    guides(fill=guide_legend(title="topic")) + xlab("topic") + 
    scale_x_continuous(breaks=1:t)
  
  topicsm = melt(topics[c(3:(t+2),t+4)],"lastname")
  topicsm$variable = as.integer(substr(topicsm$variable,2,4)) - 1
  p2 = ggplot(topicsm) + geom_point(aes(x=variable,y=value,color=factor(variable)),
    size=2) + facet_wrap(~lastname) + guides(color=guide_legend(title="topic")) +
    xlab("topic") + scale_x_continuous(breaks=1:t)
 
  p3 = ggplot(topicsm) + geom_boxplot(aes(x=factor(variable),y=value,
    fill=factor(variable))) + facet_wrap(~lastname) +
    guides(fill=guide_legend(title="topic")) + xlab("topic")
  
  ggsave(paste0("crun1/topics_",t,"_avg.pdf"),p1,width=7,height=7)
  ggsave(paste0("crun1/topics_",t,"_scatter.pdf"),p2,width=7,height=7)
  ggsave(paste0("crun1/topics_",t,"_boxplot.pdf"),p3,width=7,height=7)
  ggsave(paste0("crun1/topics_",t,"_avg.png"),p1,width=7,height=7,dpi=150)
  ggsave(paste0("crun1/topics_",t,"_scatter.png"),p2,width=7,height=7,dpi=150)
  ggsave(paste0("crun1/topics_",t,"_boxplot.png"),p3,width=7,height=7,dpi=150)
}



