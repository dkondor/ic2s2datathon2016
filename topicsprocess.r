# topicsprocess.r
# process the output of the Mallett topic detector

setwd('~/program/ic2s2datathon2016/mallet-2.0.8RC3/testrun/')
# 1. format the output to get:
# awk '{split($2,a,"_"); split(a[4],b,"."); $1=$2=""; print b[1],$0}' speeches_10_topics.out | sort -n > speeches_10_topics.dat
# awk '{split($2,a,"_"); split(a[4],b,"."); $1=$2=""; print b[1],$0}' speeches_10_2_topics.out | sort -n > speeches_10_2_topics.dat
# awk '{split($2,a,"_"); split(a[4],b,"."); $1=$2=""; print b[1],$0}' speeches_20_3_topics.out | sort -n > speeches_20_3_topics.dat

topics = read.table("speeches_10_topics.dat")
names(topics) = c("docid","t0","t1","t2","t3","t4","t5","t6","t7","t8","t9")

setwd('../../RemarksAndStatement/')
candidates = read.table("id_candidates_speech.txt")
names(candidates) = c("cid")

topics$cid = candidates$cid

topicsavg = aggregate(topics[,2:11],by=topics["cid"],FUN=mean)

library(ggplot2)
library(reshape2)

topicsavg2 = melt(topicsavg,"cid")
names(topicsavg2)[2] = "topic"

cnames = read.table("name_candidates_speech.dat",sep="\t",colClasses = c("integer","character"),quote="")
names(cnames) = c("cid","cname")
topicsavg2 = merge(topicsavg2,cnames,by="cid")
topicsavg2$topic = factor(topicsavg2$topic)

p1 = ggplot(topicsavg2) + geom_bar(aes(x=topic,y=value,fill=topic),stat="identity") +
  facet_wrap(~cname)
ggsave("topics_testrun.pdf",p1,width=7,height=7)
ggsave("topics_testrun.png",p1,width=7,height=7,dpi=120)

topics = merge(topics,cnames,by="cid")
ggplot(topics) + geom_point(aes(x=t1,y=t2,color=cname))
ggplot(topics) + geom_point(aes(x=t1,y=t3,color=cname))
ggplot(topics) + geom_point(aes(x=t2,y=t3,color=cname))
ggplot(topics) + geom_point(aes(x=t1,y=t4,color=cname))
ggplot(topics) + geom_point(aes(x=t2,y=t4,color=cname))
ggplot(topics) + geom_point(aes(x=t3,y=t4,color=cname))



library(Matrix)
# SVD results
setwd("~/program/ic2s2datathon2016/MatrixCreation/")
u = read.table("svd_U.txt")
# 448 X 448 matrix
um = as.matrix(u)

candidates = read.table("../RemarksAndStatement/id_candidates_speech.txt")
names(candidates) = c("cid")
cnames = read.table("../RemarksAndStatement/name_candidates_speech.dat",sep="\t",colClasses = c("integer","character"),quote="")
names(cnames) = c("cid","cname")
cnames$cname = factor(cnames$cname)

u$cid = candidates$cid
u = merge(u,cnames,by="cid")

p1 = ggplot(u) + geom_point(aes(x=V2,y=V3,color=cname))

# avg. "topic values" per candidate

uavg = aggregate(u[2:21],by=u["cname"],FUN=mean)
usd = aggregate(u[2:21],by=u["cname"],FUN=sd)

uavg10 = melt(uavg[c(1,3:12)],"cname")
usd10 = melt(usd[c(1,3:12)],"cname")

p1 = ggplot(uavg10) + geom_bar(aes(x=variable,y=value,fill=variable),stat="identity") +
  facet_wrap(~cname)
p2 = ggplot(usd10) + geom_bar(aes(x=variable,y=value,fill=variable),stat="identity") +
  facet_wrap(~cname)
p3 = ggplot(uavg) + geom_point(aes(x=V2,y=V3,color=cname),size=5)


#############################################################################
# SVD with fewer words
u2 = read.table("svd2_U.txt")

u2$cid = candidates$cid
u2 = merge(u2,cnames,by="cid")

p1 = ggplot(u2) + geom_point(aes(x=V2,y=V3,color=cname))

# avg. "topic values" per candidate

u2avg = aggregate(u2[2:21],by=u["cname"],FUN=mean)
u2sd = aggregate(u2[2:21],by=u["cname"],FUN=sd)

u2avg10 = melt(u2avg[c(1,3:12)],"cname")
u2sd10 = melt(u2sd[c(1,3:12)],"cname")

p1 = ggplot(u2avg10) + geom_bar(aes(x=variable,y=value,fill=variable),stat="identity") +
  facet_wrap(~cname)
p2 = ggplot(u2sd10) + geom_bar(aes(x=variable,y=value,fill=variable),stat="identity") +
  facet_wrap(~cname)
p3 = ggplot(u2avg) + geom_point(aes(x=V2,y=V3,color=cname),size=5)



##############################################################################
# NNMF
nmfw = read.table("nmf_W.txt")
# 448 X 10
nmfh = read.table("nmf_H.txt")
# 10 X 1785

head(nmfw)

nmfw$cid = candidates$cid
nmfw = merge(nmfw,cnames,by="cid")

p1 = ggplot(nmfw) + geom_point(aes(x=V1,y=V2,color=cname),size=5)

nmfwavg = aggregate(nmfw[2:11],by=nmfw["cname"],FUN=mean)
nmfwsd = aggregate(nmfw[2:11],by=nmfw["cname"],FUN=sd)

nmfwavg10 = melt(nmfwavg,"cname")
nmfwsd10 = melt(nmfwsd,"cname")

p1 = ggplot(nmfwavg10) + geom_bar(aes(x=variable,y=value,fill=variable),stat="identity") +
  facet_wrap(~cname)
p2 = ggplot(nmfwsd10) + geom_bar(aes(x=variable,y=value,fill=variable),stat="identity") +
  facet_wrap(~cname)
p3 = ggplot(nmfwavg) + geom_point(aes(x=V2,y=V3,color=cname),size=5)



##############################################################################
# LDA
ldat = read.table("lda_topics.txt")
# 448 x 10


ldat$cid = candidates$cid
ldat = merge(ldat,cnames,by="cid")

p1 = ggplot(ldat) + geom_point(aes(x=V1,y=V2,color=cname))

# avg. "topic values" per candidate

ldatavg = aggregate(ldat[2:11],by=u["cname"],FUN=mean)
ldatsd = aggregate(ldat[2:11],by=u["cname"],FUN=sd)

ldatavg10 = melt(ldatavg,"cname")
ldatsd10 = melt(ldatsd,"cname")

p1 = ggplot(ldatavg10) + geom_bar(aes(x=variable,y=value,fill=variable),stat="identity") +
  facet_wrap(~cname)
p2 = ggplot(ldatsd10) + geom_bar(aes(x=variable,y=value,fill=variable),stat="identity") +
  facet_wrap(~cname)
p3 = ggplot(ldatavg) + geom_point(aes(x=V2,y=V3,color=cname),size=5)




ldat2 = read.table("lda_gensim_documents.txt")
# 448 x 10


ldat2$cid = candidates$cid
ldat2 = merge(ldat2,cnames,by="cid")

p1 = ggplot(ldat2) + geom_point(aes(x=V1,y=V2,color=cname))
ggplot(ldat2) + geom_point(aes(x=V1,y=V3,color=cname))
ggplot(ldat2) + geom_point(aes(x=V2,y=V3,color=cname))
ggplot(ldat2) + geom_point(aes(x=V1,y=V4,color=cname))
ggplot(ldat2) + geom_point(aes(x=V2,y=V4,color=cname))
ggplot(ldat2) + geom_point(aes(x=V3,y=V4,color=cname))

# avg. "topic values" per candidate

ldat2avg = aggregate(ldat2[2:11],by=u["cname"],FUN=mean)
ldat2sd = aggregate(ldat2[2:11],by=u["cname"],FUN=sd)

ldat2avg10 = melt(ldat2avg,"cname")
ldat2sd10 = melt(ldat2sd,"cname")

ldat2avg10 = ldat2avg10[!(ldat2avg10$cname %in% c("Chris Christie","Marco Rubio")),]

p1 = ggplot(ldat2avg10) + geom_bar(aes(x=variable,y=value,fill=variable),stat="identity") +
  facet_wrap(~cname)
p2 = ggplot(ldat2sd10) + geom_bar(aes(x=variable,y=value,fill=variable),stat="identity") +
  facet_wrap(~cname)
p3 = ggplot(ldatavg) + geom_point(aes(x=V2,y=V3,color=cname),size=5)


