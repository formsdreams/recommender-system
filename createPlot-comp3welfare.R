library(ggplot2)
library(data.table)
dt = data.table(L=seq(from=0,to=500,by=1),dPi=0,dCS=0,dTW=0)
r0=0.2;t=1
dt[L<=1/r0,`:=`(dPi = 0.5*t*(r0^2)*L,
               dCS = 0.25*t*(r0^2)*L,
               dTW = 0.75*t*(r0^2)*L)]
dt[L>=1/r0,`:=`(dPi = t*(r0 - 1/2/L),
                dCS = t/4/L,
                dTW = t*(r0 - 1/4/L))]
#simple version
dt[,plot(L,dPi,type = 'l',col="blue")]
dt[,points(L,dCS,type = 'l',col = "red")]
dt[,points(L,dTW,type = 'l',col = "black")]
#using ggplot
dt.g = data.table(L=rep(0:500,each=3),type = rep(c("profit","CS","total welfare"),times=501),welfare = 0)
dt.g[type =="profit" & (L !=0),welfare := 0.5*t*(r0^2)*L*(L<=1/r0)+t*(r0 - 1/2/L)*(L>1/r0)]
dt.g[type =="CS" &(L !=0),welfare := 0.25*t*(r0^2)*L*(L<=1/r0)+t/4/L*(L>1/r0)]
dt.g[type =="total welfare" & (L !=0),welfare := 0.75*t*(r0^2)*L*(L<=1/r0)+t*(r0 - 1/4/L)*(L>1/r0)]
setwd("//andrew.ad.cmu.edu/users/users6/xiaochez/Desktop")

png("compare3welfare-logscale.png",width=3000,height=2000,res=300)
theme_set(theme_gray(base_size = 18))
g = ggplot(data = dt.g,aes(x=L,y=welfare,group=type,color=factor(type)))+ geom_line() + labs(x="L in log10 scale")
g2 = g  + theme(legend.position = "top") + scale_color_discrete(name="Welfare measure types")+ scale_x_log10()
g2
dev.off()

png("compare3welfare.png",width=3000,height=2000,res=300)
theme_set(theme_gray(base_size = 18))
g = ggplot(data = dt.g,aes(x=L,y=welfare,group=type,color=factor(type)))+geom_line()
g2 = g  + theme(legend.position = "top") + scale_color_discrete(name="Welfare measure types")
g2
dev.off()
g2 = g  + theme(axis.title.x = element_text(size = 20,face="bold")
               ,axis.title.y = element_text(size = 20,face="bold")
               ,legend.position = "top"
               ,legend.title = element_text(size = 16)) + scale_color_discrete(name="Welfare measure types")

g2 + scale_x_log10()


library(ggplot2)
library(data.table)

dt2.g = data.table(L=rep(0:500,each=3),type = rep(c("profit","CS","total welfare"),times=501),welfare = 0)
dt2.g[type =="profit" & (L !=0),welfare := t*(r0^2)*L*(L<=1/2/r0)+t*(r0 - 1/4/L)*(L>1/2/r0)]
dt2.g[type =="CS" &(L !=0),welfare := 0]
dt2.g[type =="total welfare" & (L !=0),welfare := t*(r0^2)*L*(L<=1/2/r0)+t*(r0 - 1/4/L)*(L>1/2/r0)]
setwd("//andrew.ad.cmu.edu/users/users6/xiaochez/Desktop")
png("compare3welfare-v2-combine.png",width=3000,height=2000,res=300)
theme_set(theme_gray(base_size = 18))
g = ggplot(data = dt2.g,aes(x=L,y=welfare,group=type,color=factor(type)))+geom_line()
g2 = g  + theme(legend.position = "top") + scale_color_discrete(name="Welfare measure types")
g2
dev.off()


dt2.g[,case := "persi-pdiscrim" ]
dt.g[,case:="persi-only"]
dt3.g = rbind(dt.g,dt2.g)


png("compareTW-persi-pdiscrimvspersi-only.png",width=3000,height=2000,res=300)
theme_set(theme_gray(base_size = 18))
g = ggplot(data = dt3.g[type=="total welfare"],aes(x=L,y=welfare,group=case,color=factor(case)))+geom_line()
g2 = g  + theme(legend.position = "top") + scale_color_discrete(name="Welfare measure types")
g2
dev.off()

png("compareTW-persi-pdiscrimvspersi-only-logscale.png",width=3000,height=2000,res=300)
theme_set(theme_gray(base_size = 18))
g = ggplot(data = dt3.g[type=="total welfare"],aes(x=L,y=welfare,group=case,color=factor(case)))+geom_line()+ labs(x="L in log10 scale")
g2 = g  + theme(legend.position = "top") + scale_color_discrete(name="Welfare measure types")
g2+scale_x_log10()
dev.off()

dt3.g[,cases:=paste(case,":",type)]
png("compareTprofitANDCS-persi-pdiscrimvspersi-only.png",width=3000,height=2000,res=300)
theme_set(theme_gray(base_size = 18))
g = ggplot(data = dt3.g[type %in%c("profit","CS")],aes(x=L,y=welfare,group=cases,color=factor(cases)))+geom_line()
g2 = g  + theme(legend.position = "top") + scale_color_discrete(name="Welfare measure types")
g2
dev.off()


