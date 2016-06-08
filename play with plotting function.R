setwd("//andrew.ad.cmu.edu/users/users6/xiaochez/Desktop")
require(data.table)
require(ggplot2)
dt.pi=data.table(read.csv("profit.csv"))
k<-ggplot(dt.pi,x=criterion,y=mean,ymin=X95.LB,ymax=X95.UB,color="grey")
k+geom_bar(stat="identity",position="dodge")+geom_errorbar(stat="identity",aes(ymin=X95.LB,ymax=X95.UB),position="dodge", width=0.25)


#plot unsuccessfully, begin attending the workshop
#http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html
#aesthetic mapping and geometic objects
setwd("Rgraphics")
housing <- read.csv("dataSets/landdata-states.csv")
head(housing[1:5])
hist(housing$Home.Value)
library(ggplot2)
ggplot(housing,aes(x=Home.Value))+geom_histogram()

plot(Home.Value~Date,
     data=subset(housing,State =="MA"))
points(Home.Value~Date,col="red",
       data=subset(housing,State=="TX"))
legend(19750,400000,
       c("MA","TX"),title="State",
       col=c("black","red"),
       pch=c(1,1))
ggplot(subset(housing,State %in% c("MA","TX")),
       aes(x=Date,y=Home.Value,
           color=State))+
  geom_point()
hp2001Q1<-subset(housing,Date==20011)
ggplot(data=hp2001Q1,
       aes(y=Structure.Cost,x=Land.Value))+
  geom_point()

hp2001Q1$pred.SC<-predict(lm(Structure.Cost~Land.Value,data=hp2001Q1))
g1<-ggplot(data=hp2001Q1,aes(x=Land.Value,y=Structure.Cost))
g1+geom_point(aes(col=Home.Value))+
  geom_line(aes(y=pred.SC))
g1+geom_point(aes(col=Home.Value))+
  geom_smooth()
g1+geom_text(aes(label=State),size=3)
install.packages("ggrepel")
library("ggrepel")
ge1+geom_point()+
  geom_text_ggrepel(aes(label=State),size=3)

hp2001Q1<-subset(housing,Date==20011)
p1=ggplot(data=hp2001Q1,aes(x=Land.Value,y=Structure.Cost))
p1+geom_point(aes(col=Home.Value,shape=region))
#exercise
dat<-read.csv("dataSets/EconomistData.csv")
head(dat)
p1.ex<-ggplot(data=dat,aes(x=CPI,y=HDI))
p1.ex + geom_point(col="blue")
ggsave("ex1-Q1-2.png",width=8,height=5)
p1.ex + geom_point(aes(col=Region))
ggsave("ex1-Q1-3.png",width=6,height=5)
p1.ex4<- ggplot(data=dat,aes(x=Region,y=CPI))+ geom_boxplot()
ggsave("ex1-Q1-4.png",width=6,height=5)
p1.ex4 +geom_point()
ggsave("ex1-Q1-5.png",width=6,height=5)
#statistical transformation
p2<-ggplot(housing, aes(x=Home.Value))
p2 + geom_histogram(stat = "bin",binwidth=4000)
