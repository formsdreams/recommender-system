#****************input the ranks of four types of RS************************************
#low rank implies larger value
setwd("~/Documents/Datasets/simulation_research")
library(data.table)
x.259movies = data.table(read.csv("259movies.csv"))
x.259movies[,rank.mostsold:=rank(-augsales,ties.method= "random")]
x.259movies[,rank.highestrating:=rank(-imdb_rating,ties.method= "random")]
x.259movies[,rank.mostrated:=rank(-imdb_votes,ties.method= "random")]
x.259movies = data.table(x.259movies, random_thing = sample(1:259,size=259,replace=F))
x.259movies[,rank.random:=rank(random_thing,ties.method= "random")]
#rank returns a vector with the "rank" of each value. 
#order returns the indices that would put the initial vector x in order
#************************************load Miguel's dataset************************************
load('./20140301-dt.recsys_dataset_full.RData', verbose = T)
dt.recsys_dataset_trimed  <- dt.recsys_dataset_full
df.movie_order            <- read.csv('./20140301-movie_line_order.csv')
dt.movie_order            <- data.table(df.movie_order); rm(df.movie_order)
setnames( dt.movie_order, colnames(dt.movie_order), tolower(colnames(dt.movie_order)))
#M:Join the main dataset with the movie order dataset
setattr(dt.recsys_dataset_full,"index",NULL)
dt.recsys_dataset_full  <- merge(dt.recsys_dataset_full, dt.movie_order, by = c('month_cycle_id','biweek_cycle_id', 'packagename'))
dt.recsys_dataset_full[, order:= avg_order]
dt.recsys_dataset_full[, movie_group:= paste(sep='', 10 * round(n_macs/10000,0), 'K')]
dt.recsys_dataset_trimed<-dt.recsys_dataset_full
dt.tmp <- dt.recsys_dataset_trimed[ line_visible=='VISIBLE' & movie_position == 'LINE' ]
dt.tmp[,logprice:=log(price)]
dt.tmp[,logprice_isOrderLowerThanR:=log(price)*(order<=2)]
dt.tmp[mac_type=="IRIS",consumer_type:= "premium"]
dt.tmp[mac_type=="NAGRA",consumer_type:= "standard"]
#************************************regression models************************************
#general model with R = 3-----(PROPOSALDRAFT_5-11-2016: TABLE 2.1)
model.3<-n_lease ~ logprice + logprice_isOrderLowerThanR + as.factor(packagename_orig)+factor( movie_group ) + factor(start_date) #no mac type!!!
model.3.result<-glm(model.3,data = dt.tmp, family = poisson())
coefficients(summary(model.3.result))[1:3,]#show regression coefficients' estimates
#model by consumer types (used for ct and ct-noPersi) (PROPOSALDRAFT_5-11-2016: TABLE 2.2)
model.3.ct<-n_lease   ~  logprice:consumer_type+logprice_isOrderLowerThanR:consumer_type+consumer_type+factor( movie_group )  + as.factor(packagename_orig) + factor(start_date)
model.3.ct.result<-glm(model.3.ct,data = dt.tmp, family = poisson())
coefficients(summary(model.3.ct.result))[c(1:2,295:298),] #show regression coefficients' estimates
#test if using a model without collinearity, the rank deficiency problem when doing prediction will still exist?
model.nT<-n_lease ~ logprice + logprice_isOrderLowerThanR + as.factor(packagename_orig) + factor(movie_group)
model.nT.result<-glm(model.nT,data=dt.tmp,family=poisson())
dt.tmp[,start_date := "2013-09-06 00:00:00"];dt.tmp[,movie_group :="270K"];
lowsales = predict(model.nT.result, dt.tmp[,logprice_isOrderLowerThanR:=logprice],type = "link",se.fit=FALSE)
highsales = predict(model.3.result, dt.tmp[,logprice_isOrderLowerThanR:=0],type = "link",se.fit=FALSE)
#result:no warning
#creat dummies manually
for (t in unique(dt.tmp[,start_date])){
  dt.tmp[,paste("time",gsub("[- :]","",t),sep=""):= ifelse(dt.tmp$start_date == t, 1, 0)]
}
model.manualDummy<-n_lease ~logprice + logprice_isOrderLowerThanR + as.factor(packagename_orig) + factor(movie_group) +time20130906000000+time20130913000000+time20130920000000+time20130927000000+time20131004000000+time20131011000000+time20131018000000+time20131025000000+time20131101000000+time20131108000000+time20131115000000+time20131122000000+time20131129000000+time20131206000000+time20131213000000+time20131220000000+time20131227000000+time20140103000000+time20140110000000+time20140117000000+time20140124000000+time20140207000000+time20140214000000+time20140221000000
model.manualDummy.result<- glm(model.manualDummy,data=dt.tmp,family=poisson())
model.manualDummy.ct<-n_lease ~ logprice:consumer_type+logprice_isOrderLowerThanR:consumer_type+consumer_type+factor( movie_group )+as.factor(packagename_orig) +time20130906000000+time20130913000000+time20130920000000+time20130927000000+time20131004000000+time20131011000000+time20131018000000+time20131025000000+time20131101000000+time20131108000000+time20131115000000+time20131122000000+time20131129000000+time20131206000000+time20131213000000+time20131220000000+time20131227000000+time20140103000000+time20140110000000+time20140117000000+time20140124000000+time20140207000000+time20140214000000+time20140221000000
model.manualDummy.ct.result<-glm(model.manualDummy.ct,data = dt.tmp, family = poisson())

#************************************predict consumer sales when lower/higher order************************************
#profit is a product of margin and demand, so we could use predict function to predict demand first
#--------------predict general model demands------------------------------------------
dt.tmp[,start_date := "2013-09-06 00:00:00"]
for (t in unique(dt.tmp[,start_date])){
  dt.tmp[,paste("time",gsub("[- :]","",t),sep=""):= 0]
}
dt.tmp[,time20130906000000:=1]
dt.tmp[,movie_group :="270K"]
lowsales = predict(model.manualDummy.result, dt.tmp[,logprice_isOrderLowerThanR:=logprice],type = "link",se.fit=FALSE)#predict the low and high order demands from the general model
highsales = predict(model.manualDummy.result, dt.tmp[,logprice_isOrderLowerThanR:=0],type = "link",se.fit=FALSE)#predict the low and high order demands from the general model
#--------------predict ct model demands-----------------------------------------------
#l stands for low order; h stands for high order
#p stands for premium consumers
dt.tmp.p = dt.tmp[,consumer_type:="premium"] 
dt.tmp.p.l = dt.tmp.p[,logprice_isOrderLowerThanR:=logprice]
premium.lowsales = predict(model.manualDummy.ct.result,dt.tmp.p.l, type="link",se.fit=FALSE)
dt.tmp.p.h = dt.tmp.p[,logprice_isOrderLowerThanR:=0]
premium.highsales = predict(model.manualDummy.ct.result,dt.tmp.p.h, type="link",se.fit=FALSE)
#s stands for standard consumers
dt.tmp.s = dt.tmp[,consumer_type:="standard"] 
dt.tmp.s.l = dt.tmp.s[,logprice_isOrderLowerThanR:=logprice]
standard.lowsales = predict(model.manualDummy.ct.result,dt.tmp.s.l, type="link",se.fit=FALSE)
dt.tmp.s.h = dt.tmp.s[,logprice_isOrderLowerThanR:=0]
standard.highsales = predict(model.manualDummy.ct.result,dt.tmp.s.h, type="link",se.fit=FALSE)
#--------------save each movies' predicted sales (general/premium/standard consumer & low/high order) into the 259 movies attribute table "x.259movies"
dt.tmp.predict = data.table(dt.tmp
								,demand_loworder = exp(lowsales)
								,demand_highorder = exp(highsales)
								,demand_premium_loworder = exp(premium.lowsales)
								,demand_premium_highorder = exp(premium.highsales)
								,demand_standard_loworder = exp(standard.lowsales)
								,demand_standard_highorder = exp(standard.highsales))
setkey(x.259movies,packagename)
setkey(dt.tmp.predict,packagename)
x.259movies = unique(dt.tmp.predict[x.259movies])
#----------------------------rank movies by their predicted profits(at low/high order and from general/premium/standard consumers)--------------------------------------------------------
x.259movies[,rank.pi.p:=rank(-(demand_premium_loworder-demand_premium_highorder)*(price_orig/1.23-cost_no_vat),ties.method= "random")]
x.259movies[,rank.pi.s:=rank(-(demand_standard_loworder-demand_standard_highorder)*(price_orig/1.23-cost_no_vat),ties.method= "random")]
#rank.pi.ps is when firm doesn't personalize.
x.259movies[,rank.pi.ps:=rank(-(0.5*demand_standard_loworder+0.5*demand_premium_loworder-0.5*demand_standard_highorder-0.5*demand_premium_highorder)*(price_orig/1.23-cost_no_vat)
								,ties.method= "random")]
#************************************predict consumer surplus when lower/higher order************************************
#Since we couldn't use predict function to get consumer surplus, we first decompose the predicted profit into fixed effects A and exponential form p^(b1+b2*loworder)
#Method:
#A's are calculated for general/premium/standard consumers: A(if loworder) = predicted sales/p^(b1+b2*(if loworder))
#find consumer surplus without constant: integrate demand again price ranging from real price to maxprice
#the resulted predicted consumer surplus is the product of the two


#find the A's for all 259 movies in the 2-consumer-type model
b1.p =-0.30592855 #-3.059e-01 
b1.s =-0.76081471 #-7.608e-01  
b2.p =0.08448860 #8.449e-02  
b2.s =0.06650716 #6.651e-02
x.259movies[,A.p.l:=demand_premium_loworder/exp((b1.p+b2.p)*logprice)]
x.259movies[,A.p.h:=demand_premium_highorder/exp(b1.p*logprice)]
x.259movies[,A.s.l:=demand_standard_loworder/exp((b1.s+b2.s)*logprice)]
x.259movies[,A.s.h:=demand_standard_highorder/exp(b1.s*logprice)]
#find the CS of 2X2 cases
cs.p.l<-function(x) x^(b1.p+b2.p)
cs.p.h<-function(x) x^(b1.p)
cs.s.l<-function(x) x^(b1.s+b2.s)
cs.s.h<-function(x) x^(b1.s)
x.259movies[,cs_premium_loworder:=A.p.l*integrate(cs.p.l,lower = exp(logprice),upper = 400)$value,by=packagename]
x.259movies[,cs_premium_highorder:=A.p.h*integrate(cs.p.h,lower=exp(logprice),upper=400)$value,by=packagename]
x.259movies[,cs_standard_loworder:=A.s.l*integrate(cs.s.l,lower = exp(logprice),upper = 400)$value,by=packagename]
x.259movies[,cs_standard_highorder:=A.s.h*integrate(cs.s.h,lower=exp(logprice),upper=400)$value,by=packagename]
#-------calculation of total welfare 
x.259movies[,tw_premium_loworder:=cs_premium_loworder+demand_premium_loworder*(price_orig/1.23-cost_no_vat)]
x.259movies[,tw_premium_highorder:=cs_premium_highorder+demand_premium_highorder*(price_orig/1.23-cost_no_vat)]
x.259movies[,tw_standard_loworder:=cs_standard_loworder+demand_standard_loworder*(price_orig/1.23-cost_no_vat)]
x.259movies[,tw_standard_highorder:=cs_standard_highorder+demand_standard_highorder*(price_orig/1.23-cost_no_vat)]
##----------------------------rank movies by their predicted profit
x.259movies[,rank.tw.p:=rank(-(tw_premium_loworder-tw_premium_highorder),ties.method = "random")]
x.259movies[,rank.tw.s:=rank(-(tw_standard_loworder-tw_standard_highorder),ties.method = "random")]
x.259movies[,rank.tw.ps:=rank(-0.5*(tw_standard_loworder-tw_standard_highorder)-0.5*(tw_premium_loworder-tw_premium_highorder),ties.method = "random")]
##----------------------------rank movies by their predicted consumer surplus(at low/high order and from general/premium/standard consumers)--------------------------------------------------------
x.259movies[,rank.cs.p:=rank(-(cs_premium_loworder-cs_premium_highorder),ties.method = "random")]
x.259movies[,rank.cs.s:=rank(-(cs_standard_loworder-cs_standard_highorder),ties.method = "random")]
#rank.cs.ps is when firm doesn't personalize.
x.259movies[,rank.cs.ps:=rank(-0.5*(cs_standard_loworder-cs_standard_highorder)-0.5*(cs_premium_loworder-cs_premium_highorder),ties.method = "random")]
##************************************Optimize the configuration for all 1000 simulated 15movie RS************************************
#get the 1000 simulations of 15 movies
x.1000sim = data.table(read.csv("1000simulations.csv"))
profit.15.1000 = data.table(id=1:1000)
cs.15.1000 = data.table(id=1:1000)
#get the maximized CS for the 1000 simulations of 15 movies.
for(sim_id in 1:1000){
	names = unlist(strsplit(as.character(x.1000sim[sim_id]$movienames),split='|',fixed=T)) #get the names of 15 movies in each simulation
	x.15movies = x.259movies[packagename%in%names]
	#----------------------------configuration: ranking by maximizing profit----------------------------
	#method1:personalized, ct model
	x.15movies[,reorder:=rank(rank.pi.p)]
	cs.15.maxprofit.p = x.15movies[,sum(cs_premium_loworder*(reorder<=2)+ cs_premium_highorder*(reorder>2))]
	maxprofit.15.p = x.15movies[,sum((demand_premium_loworder*(reorder<=2)+ demand_premium_highorder*(reorder>2))*(price_orig/1.23-cost_no_vat))]
	x.15movies[,reorder:=rank(rank.pi.s)]
	cs.15.maxprofit.s = x.15movies[,sum(cs_standard_loworder*(reorder<=2)+cs_standard_highorder*(reorder>2))]
	maxprofit.15.s = x.15movies[,sum((demand_standard_loworder*(reorder<=2)+ demand_standard_highorder*(reorder>2))*(price_orig/1.23-cost_no_vat))]	
	#resulting cs
	cs.15.maxprofit.ct = 0.5*cs.15.maxprofit.p+0.5*cs.15.maxprofit.s 
	#resulting profit
	maxprofit.15.ct = 0.5*maxprofit.15.p + 0.5*maxprofit.15.s

	#-----method2: no personalization ct-noPersi model
	x.15movies[,reorder:=rank(rank.pi.ps)]
	#resulting cs
	cs.15.maxprofit.ct.noPersi = x.15movies[,sum((0.5*cs_standard_loworder+0.5*cs_premium_loworder)*(reorder<=2)+ (0.5*cs_standard_highorder+0.5*cs_premium_highorder)*(reorder>2))]
	#resulting profit
	maxprofit.15.ct.noPersi = x.15movies[,sum(((0.5*demand_standard_loworder+0.5*demand_premium_loworder)*(reorder<=2)+ (0.5*demand_standard_highorder+0.5*demand_premium_highorder)*(reorder>2))*(price_orig/1.23-cost_no_vat))]	
	#----------------------------configuration: ranking by maximizing CS---------------------------------
	#-----method1:personalized, ct model
	x.15movies[,reorder:=rank(rank.cs.p)]
	maxcs.15.p = x.15movies[,sum(cs_premium_loworder*(reorder<=2)+ cs_premium_highorder*(reorder>2))]
	profit.15.maxcs.p = x.15movies[,sum(demand_premium_loworder*(reorder<=2)*(price_orig/1.23-cost_no_vat)+ demand_premium_highorder*(reorder>2)*(price_orig/1.23-cost_no_vat))]
	x.15movies[,reorder:=rank(rank.cs.s)]
	maxcs.15.s = x.15movies[,sum(cs_standard_loworder*(reorder<=2)+ cs_standard_highorder*(reorder>2))]
	profit.15.maxcs.s = x.15movies[,sum(demand_standard_loworder*(reorder<=2)*(price_orig/1.23-cost_no_vat)+ demand_standard_highorder*(reorder>2)*(price_orig/1.23-cost_no_vat))]
	#resulting cs 
	maxcs.15.ct = 0.5*maxcs.15.p + 0.5*maxcs.15.s
	#resulting profit
	profit.15.maxcs.ct = 0.5*profit.15.maxcs.p+0.5*profit.15.maxcs.s 
	
	#-----method2: no personalization, ct-noPersi model
	x.15movies[,reorder:=rank(rank.cs.ps)]
	maxcs.15.ct.noPersi = x.15movies[,sum((0.5*cs_standard_loworder+0.5*cs_premium_loworder)*(reorder<=2)+ (0.5*cs_standard_highorder+0.5*cs_premium_highorder)*(reorder>2))]
	profit.15.maxcs.ct.noPersi = x.15movies[,sum((0.5*demand_standard_loworder+0.5*demand_premium_loworder)*(reorder<=2)*(price_orig/1.23-cost_no_vat)+ (0.5*demand_standard_highorder+0.5*demand_premium_highorder)*(reorder>2)*(price_orig/1.23-cost_no_vat))]
	
	#----------------------------configuration: ranking by maximizing total welfare---------------------------------
	#-----method1:personalized, ct model
	x.15movies[,reorder:=rank(rank.tw.p)]
	cs.15.maxtw.p = x.15movies[,sum(cs_premium_loworder*(reorder<=2)+ cs_premium_highorder*(reorder>2))]
	profit.15.maxtw.p = x.15movies[,sum(demand_premium_loworder*(reorder<=2)*(price_orig/1.23-cost_no_vat)+ demand_premium_highorder*(reorder>2)*(price_orig/1.23-cost_no_vat))]
	x.15movies[,reorder:=rank(rank.tw.s)]
	cs.15.maxtw.s = x.15movies[,sum(cs_standard_loworder*(reorder<=2)+ cs_standard_highorder*(reorder>2))]
	profit.15.maxtw.s = x.15movies[,sum(demand_standard_loworder*(reorder<=2)*(price_orig/1.23-cost_no_vat)+ demand_standard_highorder*(reorder>2)*(price_orig/1.23-cost_no_vat))]
	#resulting cs 
	cs.15.maxtw.ct = 0.5*cs.15.maxtw.p + 0.5*cs.15.maxtw.s
	#resulting profit
	profit.15.maxtw.ct = 0.5*profit.15.maxtw.p+0.5*profit.15.maxtw.s 
	
	#-----method2: no personalization, ct-noPersi model
	x.15movies[,reorder:=rank(rank.tw.ps)]
	cs.15.maxtw.ct.noPersi = x.15movies[,sum((0.5*cs_standard_loworder+0.5*cs_premium_loworder)*(reorder<=2)+ (0.5*cs_standard_highorder+0.5*cs_premium_highorder)*(reorder>2))]
	profit.15.maxtw.ct.noPersi = x.15movies[,sum((0.5*demand_standard_loworder+0.5*demand_premium_loworder)*(reorder<=2)*(price_orig/1.23-cost_no_vat)+ (0.5*demand_standard_highorder+0.5*demand_premium_highorder)*(reorder>2)*(price_orig/1.23-cost_no_vat))]
	
	#----------------------------configuration: ranking by others---------------------------------
	#most sold;
	x.15movies[,reorder:=rank(rank.mostsold)]
	cs.15.mostsold.ct = x.15movies[,sum((0.5*cs_standard_loworder+0.5*cs_premium_loworder)*(reorder<=2)+ (0.5*cs_standard_highorder+0.5*cs_premium_highorder)*(reorder>2))]
	profit.15.mostsold.ct = x.15movies[,sum(((0.5*demand_standard_loworder+0.5*demand_premium_loworder)*(reorder<=2)+ (0.5*demand_standard_highorder+0.5*demand_premium_highorder)*(reorder>2))*(price_orig/1.23-cost_no_vat))]
	#highest rating;
	x.15movies[,reorder:=rank(rank.highestrating)]
	cs.15.highestrating.ct = x.15movies[,sum((0.5*cs_standard_loworder+0.5*cs_premium_loworder)*(reorder<=2)+ (0.5*cs_standard_highorder+0.5*cs_premium_highorder)*(reorder>2))]
	profit.15.highestrating.ct = x.15movies[,sum(((0.5*demand_standard_loworder+0.5*demand_premium_loworder)*(reorder<=2)+ (0.5*demand_standard_highorder+0.5*demand_premium_highorder)*(reorder>2))*(price_orig/1.23-cost_no_vat))]
	#most rated;
	x.15movies[,reorder:=rank(rank.mostrated)]
	cs.15.mostrated.ct = x.15movies[,sum((0.5*cs_standard_loworder+0.5*cs_premium_loworder)*(reorder<=2)+ (0.5*cs_standard_highorder+0.5*cs_premium_highorder)*(reorder>2))]
	profit.15.mostrated.ct = x.15movies[,sum(((0.5*demand_standard_loworder+0.5*demand_premium_loworder)*(reorder<=2)+ (0.5*demand_standard_highorder+0.5*demand_premium_highorder)*(reorder>2))*(price_orig/1.23-cost_no_vat))]
	#random;
	x.15movies[,reorder:=rank(rank.random)]
	cs.15.random.ct = x.15movies[,sum((0.5*cs_standard_loworder+0.5*cs_premium_loworder)*(reorder<=2)+ (0.5*cs_standard_highorder+0.5*cs_premium_highorder)*(reorder>2))]
	profit.15.random.ct = x.15movies[,sum(((0.5*demand_standard_loworder+0.5*demand_premium_loworder)*(reorder<=2)+ (0.5*demand_standard_highorder+0.5*demand_premium_highorder)*(reorder>2))*(price_orig/1.23-cost_no_vat))]
	#save the resulting profit/cs of 6 types of configurations into the cs & profit datatable "profit.15.1000","cs.15.1000"
	#cs
	cs.15.1000[id==sim_id,cs.maxcs.ct:=maxcs.15.ct]
	cs.15.1000[id==sim_id,cs.maxcs.ct.noPersi:=maxcs.15.ct.noPersi]
	cs.15.1000[id==sim_id,cs.maxprofit.ct:=cs.15.maxprofit.ct]
	cs.15.1000[id==sim_id,cs.maxprofit.ct.noPersi:=cs.15.maxprofit.ct.noPersi]
	cs.15.1000[id==sim_id,cs.maxtw.ct:=cs.15.maxtw.ct]
  cs.15.1000[id==sim_id,cs.maxtw.ct.noPersi:=cs.15.maxtw.ct.noPersi]
  
	cs.15.1000[id==sim_id,cs.mostsold.ct:=cs.15.mostsold.ct]
	cs.15.1000[id==sim_id,cs.highestrating.ct:=cs.15.highestrating.ct]
	cs.15.1000[id==sim_id,cs.mostrated.ct:=cs.15.mostrated.ct]
	cs.15.1000[id==sim_id,cs.random.ct:=cs.15.random.ct]

	#profits
	profit.15.1000[id==sim_id,profit.maxcs.ct:=profit.15.maxcs.ct]
	profit.15.1000[id==sim_id,profit.maxcs.ct.noPersi:=profit.15.maxcs.ct.noPersi]
	profit.15.1000[id==sim_id,profit.maxprofit.ct:=maxprofit.15.ct]
	profit.15.1000[id==sim_id,profit.maxprofit.ct.noPersi:=maxprofit.15.ct.noPersi]
	profit.15.1000[id==sim_id,profit.maxtw.ct:=profit.15.maxtw.ct]
  profit.15.1000[id==sim_id,profit.maxtw.ct.noPersi:=profit.15.maxtw.ct.noPersi]
  
	profit.15.1000[id==sim_id,profit.mostsold.ct:=profit.15.mostsold.ct]
	profit.15.1000[id==sim_id,profit.highestrating.ct:=profit.15.highestrating.ct]
	profit.15.1000[id==sim_id,profit.mostrated.ct:=profit.15.mostrated.ct]
	profit.15.1000[id==sim_id,profit.random.ct:=profit.15.random.ct]

}

#---------welfare results using the general model (without adding consumer types)
b1=-0.4430
b2=0.08
x.259movies[,A:=demand_highorder/exp(b1*logprice)] 
cs.l<-function(x) x^(b1+b2) 
cs.h<-function(x) x^b1 
x.259movies[,cs_loworder:=A*integrate(cs.l,lower=exp(logprice),upper=400)$value,by=packagename]#----------
x.259movies[,cs_highorder:=A*integrate(cs.h,lower=exp(logprice),upper=400)$value,by=packagename]#----------
x.259movies[,rank.cs:=rank(-(cs_loworder-cs_highorder),ties.method = "random")]#----------
x.259movies[,rank.pi:=rank(-(demand_loworder-demand_highorder)*(price_orig/1.23-cost_no_vat),ties.method= "random")]
for(sim_id in 1:1000){
    names = unlist(strsplit(as.character(x.1000sim[sim_id]$movienames),split='|',fixed=T)) #get the names of 15 movies in each simulation
    x.15movies = x.259movies[packagename%in%names]
    x.15movies[,reorder:=rank(rank.cs)]
    cs.15.maxcs = x.15movies[,sum(cs_loworder*(reorder<=2)+ cs_highorder*(reorder>2))]
    profit.15.maxcs = x.15movies[,sum(demand_loworder*(price_orig/1.23-cost_no_vat)*(reorder<=2)+ demand_highorder*(price_orig/1.23-cost_no_vat)*(reorder>2))]
    x.15movies[,reorder:=rank(rank.pi)]
    cs.15.maxprofit = x.15movies[,sum(cs_loworder*(reorder<=2)+ cs_highorder*(reorder>2))]    
    profit.15.maxprofit = x.15movies[,sum(demand_loworder*(price_orig/1.23-cost_no_vat)*(reorder<=2)+ demand_highorder*(price_orig/1.23-cost_no_vat)*(reorder>2))]    
   	cs.15.1000[id==sim_id,cs.maxcs := cs.15.maxcs]
    cs.15.1000[id==sim_id,cs.maxprofit := cs.15.maxprofit]
    profit.15.1000[id==sim_id,profit.maxcs := profit.15.maxcs]
    profit.15.1000[id==sim_id,profit.maxprofit := profit.15.maxprofit]
}
#*****************************calculation of total welfare from the profit and cs table*******************
setkey(profit.15.1000,id)
tw.15.1000 = profit.15.1000[cs.15.1000]
tw.15.1000[,tw.maxprofit := profit.maxprofit +cs.maxprofit]
tw.15.1000[,tw.maxprofit.ct := profit.maxprofit.ct+cs.maxprofit.ct]
tw.15.1000[,tw.maxprofit.ct.noPersi := profit.maxprofit.ct.noPersi+cs.maxprofit.ct.noPersi]
tw.15.1000[,tw.maxcs:=profit.maxcs+cs.maxcs]
tw.15.1000[,tw.maxcs.ct:=profit.maxcs.ct+cs.maxcs.ct]
tw.15.1000[,tw.maxcs.ct.noPersi:=profit.maxcs.ct+cs.maxcs.ct.noPersi]
tw.15.1000[,tw.mostsold.ct:=profit.mostsold.ct+cs.mostsold.ct]
tw.15.1000[,tw.highestrating.ct:=profit.highestrating.ct+cs.highestrating.ct]
tw.15.1000[,tw.mostrated.ct:=profit.mostrated.ct+cs.mostrated.ct]
tw.15.1000[,tw.random.ct:=profit.random.ct+cs.random.ct]
for(sim_id in 1:1000){
  names = unlist(strsplit(as.character(x.1000sim[sim_id]$movienames),split='|',fixed=T))
  x.15movies = x.259movies[packagename%in%names]
  x.15movies[,reorder:=rank(rank.tw.p)]
  tw.15.maxtw.p = x.15movies[,sum((reorder<=2)*tw_premium_loworder+(reorder>2)*tw_premium_highorder)]
  x.15movies[,reorder:=rank(rank.tw.s)]
  tw.15.maxtw.s = x.15movies[,sum((reorder<=2)*tw_standard_loworder+(reorder>2)*tw_standard_highorder)]
  x.15movies[,reorder:=rank(rank.tw.ps)]
  tw.15.maxtw.ps = x.15movies[,sum((reorder<=2)*(0.5*tw_premium_loworder+0.5*tw_standard_loworder)+(reorder>2)*(0.5*tw_premium_highorder+0.5*tw_standard_highorder))]
  tw.15.maxtw.ct = 0.5*tw.15.maxtw.p + 0.5*tw.15.maxtw.s
  tw.15.1000[id==sim_id,tw.maxtw.ct:=tw.15.maxtw.ct]
  tw.15.1000[id==sim_id,tw.maxtw.ct.noPersi := tw.15.maxtw.ps]
}
#*******************************calculation of welfare loss from profit, cs and total welfare data table*****************************************
#cs loss: average 100(1 - Cs when maximizing profit/maxcs)%
#profit loss: average 100(1-profit when maximizing CS/maxprofit)%
#welfare loss: average 100(1-welfare when maximizing profit/welfare when maximizing CS)%
#---------------general model-----------------
cs.15.1000[,1-mean(cs.maxprofit/cs.maxcs)] #0.1077831
profit.15.1000[,1-mean(profit.maxcs/profit.maxprofit)] #0.08318013
tw.15.1000[,1-mean(tw.maxprofit/tw.maxcs)] #0.00601709

#---------------ct model----------------------
cs.15.1000[,1-mean(cs.maxprofit.ct/cs.maxcs.ct)] #0.1081632
profit.15.1000[,1-mean(profit.maxcs.ct/profit.maxprofit.ct)] # 0.08423205
tw.15.1000[,1-mean(tw.maxprofit.ct/tw.maxcs.ct)] #0.005518016

tw.15.1000[,1-mean(tw.maxprofit.ct/tw.maxtw.ct)]#  0.02269546
tw.15.1000[,1-mean(tw.maxcs.ct/tw.maxtw.ct)] #0.01673022
#---------------ct-noPersi model--------------
cs.15.1000[,1-mean(cs.maxprofit.ct.noPersi/cs.maxcs.ct.noPersi)] #0.1075902
profit.15.1000[,1-mean(profit.maxcs.ct.noPersi/profit.maxprofit.ct.noPersi)] # 0.08429601
tw.15.1000[,1-mean(tw.maxprofit.ct.noPersi/tw.maxcs.ct.noPersi)] #0.005301661

tw.15.1000[,1-mean(tw.maxprofit.ct.noPersi/tw.maxtw.ct.noPersi)] # 0.02238126
tw.15.1000[,1-mean(tw.maxcs.ct.noPersi/tw.maxtw.ct.noPersi)] #0.01663161
#------------------comparing personalization with no personalization
tw.15.1000[,1-mean(tw.maxtw.ct.noPersi/tw.maxtw.ct)] #0.0001288731
tw.15.1000[,1-mean(cs.maxcs.ct.noPersi/cs.maxcs.ct)] #5.862816e-05
tw.15.1000[,1-mean(profit.maxprofit.ct.noPersi/profit.maxprofit.ct)] #0.0001174825
#**************** 6 RS comparison of three welfare measures*******************************
#--------save in three data files: cs.csv, proft.csv, totalwelfare.csv--------------------
#--------profit
x = rbind(as.numeric(unlist(profit.15.1000[,t.test(profit.maxprofit.ct,conf.level = 0.95)][c("conf.int","estimate")]))
	,as.numeric(unlist(profit.15.1000[,t.test(profit.maxcs.ct,conf.level = 0.95)][c("conf.int","estimate")]))
	,as.numeric(unlist(profit.15.1000[,t.test(profit.maxtw.ct,conf.level = 0.95)][c("conf.int","estimate")]))
	,as.numeric(unlist(profit.15.1000[,t.test(profit.mostsold.ct,conf.level = 0.95)][c("conf.int","estimate")]))
	,as.numeric(unlist(profit.15.1000[,t.test(profit.mostrated.ct,conf.level = 0.95)][c("conf.int","estimate")]))
	,as.numeric(unlist(profit.15.1000[,t.test(profit.highestrating.ct,conf.level = 0.95)][c("conf.int","estimate")]))
	,as.numeric(unlist(profit.15.1000[,t.test(profit.random.ct,conf.level = 0.95)][c("conf.int","estimate")]))
	)
x.dt = data.table(criterion=c("maxprofit","maxcs","maxtotalwelfare","mostsold","mostrated","highestrating","random"),x)
setnames(x.dt,c("V1","V2","V3"),c("95%LB","95%UB","mean"))
write.csv(x.dt,file="profit.csv")
#--------cs
x.2 = rbind(as.numeric(unlist(cs.15.1000[,t.test(cs.maxprofit.ct,conf.level = 0.95)][c("conf.int","estimate")]))
	,as.numeric(unlist(cs.15.1000[,t.test(cs.maxcs.ct,conf.level = 0.95)][c("conf.int","estimate")]))
	,as.numeric(unlist(cs.15.1000[,t.test(cs.maxtw.ct,conf.level = 0.95)][c("conf.int","estimate")]))
	,as.numeric(unlist(cs.15.1000[,t.test(cs.mostsold.ct,conf.level = 0.95)][c("conf.int","estimate")]))
	,as.numeric(unlist(cs.15.1000[,t.test(cs.mostrated.ct,conf.level = 0.95)][c("conf.int","estimate")]))
	,as.numeric(unlist(cs.15.1000[,t.test(cs.highestrating.ct,conf.level = 0.95)][c("conf.int","estimate")]))
	,as.numeric(unlist(cs.15.1000[,t.test(cs.random.ct,conf.level = 0.95)][c("conf.int","estimate")]))
	)
x.2.dt = data.table(criterion=c("maxprofit","maxcs","maxtotalwelfare","mostsold","mostrated","highestrating","random"),x.2)
setnames(x.2.dt,c("V1","V2","V3"),c("95%LB","95%UB","mean"))
write.csv(x.2.dt,file="cs.csv")
#--------total welfare
x.3 = rbind(as.numeric(unlist(tw.15.1000[,t.test(tw.maxprofit.ct,conf.level = 0.95)][c("conf.int","estimate")]))
	,as.numeric(unlist(tw.15.1000[,t.test(tw.maxcs.ct,conf.level = 0.95)][c("conf.int","estimate")]))
	,as.numeric(unlist(tw.15.1000[,t.test(tw.maxtw.ct,conf.level = 0.95)][c("conf.int","estimate")]))
	,as.numeric(unlist(tw.15.1000[,t.test(tw.mostsold.ct,conf.level = 0.95)][c("conf.int","estimate")]))
	,as.numeric(unlist(tw.15.1000[,t.test(tw.mostrated.ct,conf.level = 0.95)][c("conf.int","estimate")]))
	,as.numeric(unlist(tw.15.1000[,t.test(tw.highestrating.ct,conf.level = 0.95)][c("conf.int","estimate")]))
	,as.numeric(unlist(tw.15.1000[,t.test(tw.random.ct,conf.level = 0.95)][c("conf.int","estimate")]))
	)
x.3.dt = data.table(criterion=c("maxprofit","maxcs","maxtotalwelfare","mostsold","mostrated","highestrating","random"),x.3)
setnames(x.3.dt,c("V1","V2","V3"),c("95%LB","95%UB","mean"))
write.csv(x.3.dt,file="totalwelfare.csv")
#***************************************two way scatterplots comparing maxProfit and maxCS in three models**********************************************
#-------------plotting of cs 
#------general model------- (PROPOSALDRAFT_5-11-2016: FIGURE 2.3(middle))
png(file="cs_noCTmodel_scatter_6-8_R=3.png",height=2000,width=2000,res=300)
cs.15.1000[,plot(cs.maxprofit*0.0115,cs.maxcs*0.0115,xlim=c(min(cs.maxprofit*0.0115),max(cs.maxcs*0.0115)),ylim=c(min(cs.maxprofit*0.0115),max(cs.maxcs*0.0115)),xlab = "CS (in dollar) when profit is maximized",ylab = "Maximized CS in Dollar",cex=0.3,pch=19)]
abline(0,1,col="red")
dev.off()
#------ct model------- (PROPOSALDRAFT_5-11-2016: FIGURE 2.4(middle))
png(file="cs_scatter_CTmodel_personalized_scatter_6-8_R=3.png",height=2000,width=2000,res=300)
cs.15.1000[,plot(cs.maxprofit.ct*0.0115,cs.maxcs.ct*0.0115
	,xlim=c(min(cs.maxprofit.ct*0.0115),max(cs.maxcs.ct*0.0115)),ylim=c(min(cs.maxprofit.ct*0.0115),max(cs.maxcs.ct*0.0115)),xlab = "CS (in dollar) when profit is maximized",ylab = "Maximized CS in Dollar",cex=0.3,pch=19)]
abline(0,1,col="red")
dev.off()
#------ct-noPersi model------- (PROPOSALDRAFT_5-11-2016: FIGURE 2.5(middle))
png(file="cs_scatter_CTmodel_notpersonalized_scatter_6-8_R=3.png",height=2000,width=2000,res=300)
cs.15.1000[,plot(cs.maxprofit.ct.noPersi*0.0115,cs.maxcs.ct.noPersi*0.0115
	,xlim=c(min(cs.maxprofit.ct.noPersi*0.0115),max(cs.maxcs.ct.noPersi*0.0115)),ylim=c(min(cs.maxprofit.ct.noPersi*0.0115),max(cs.maxcs.ct.noPersi*0.0115)),xlab = "CS (in dollar) when profit is maximized",ylab = "Maximized CS in Dollar",cex=0.3,pch=19)]
abline(0,1,col="red")
dev.off()
#--------------plotting of profit 
#------general model--------(PROPOSALDRAFT_5-11-2016: FIGURE 2.3(left))
png(file="profit_noCTmodel_scatter_6-8_R=3.png",height=2000,width=2000,res=300)
profit.15.1000[,plot(profit.maxprofit*0.0115,profit.maxcs*0.0115,
	xlim=c(min(profit.maxcs*0.0115),max(profit.maxprofit*0.0115)),ylim=c(min(profit.maxcs*0.0115),max(profit.maxprofit*0.0115)),xlab = "Profit (in dollar) when profit is maximized",ylab = "profit (in dollar) when CS is maximized",cex=0.3,pch=19)]
abline(0,1,col="red")
dev.off()
#------ct model-------(PROPOSALDRAFT_5-11-2016: FIGURE 2.4(left))
png(file="profit_scatter_CTmodel_personalized_scatter_6-8_R=3.png",height=2000,width=2000,res=300)
profit.15.1000[,plot(pch=19,profit.maxprofit.ct*0.0115,profit.maxcs.ct*0.0115
	,xlim=c(min(profit.maxcs.ct*0.0115),max(profit.maxprofit.ct*0.0115)),ylim=c(min(profit.maxcs.ct*0.0115),max(profit.maxprofit.ct*0.0115)),xlab = "Profit (in dollar) when profit is maximized",ylab = "profit (in dollar) when CS is maximized",cex=0.3)]
abline(0,1,col="red")
dev.off()
#------ct-noPersi model--------(PROPOSALDRAFT_5-11-2016: FIGURE 2.5(left))
png(file="profit_scatter_CTmodel_notpersonalized_scatter_6-8_R=3.png",height=2000,width=2000,res=300)
profit.15.1000[,plot(profit.maxprofit.ct.noPersi*0.0115,profit.maxcs.ct.noPersi*0.0115
	,xlim=c(min(profit.maxcs.ct.noPersi*0.0115),max(profit.maxprofit.ct.noPersi*0.0115)),ylim=c(min(profit.maxcs.ct.noPersi*0.0115),max(profit.maxprofit.ct.noPersi*0.0115)),xlab = "profit (in dollar) when profit is maximized",ylab = "profit (in dollar) when CS is maximized",cex=0.3,pch=19)]
abline(0,1,col="red")
dev.off()
#--------------plotting of total welfare
#------general model----------(PROPOSALDRAFT_5-11-2016: FIGURE 2.3(right))
png(file="tw_noCTmodel_scatter_6-8_R=3.png",height=2000,width=2000,res=300)
tw.15.1000[,plot(tw.maxprofit*0.0115,tw.maxcs*0.0115,xlim=c(min(tw.maxprofit*0.0115),max(tw.maxcs*0.0115)),ylim=c(min(tw.maxprofit*0.0115),max(tw.maxcs*0.0115)),xlab = "Total welfare (in dollar) when profit is maximized",ylab = "Total welfare (in dollar) when CS is maximized",cex=0.3,pch=19)]
abline(0,1,col="red")
dev.off()
#------ct model---------------(PROPOSALDRAFT_5-11-2016: FIGURE 2.4(right))
png(file="tw_scatter_CTmodel_personalized_6-8_R=3.png",height=2000,width=2000,res=300)
tw.15.1000[,plot(tw.maxprofit.ct*0.0115,tw.maxcs.ct*0.0115,	xlim=c(min(tw.maxprofit.ct*0.0115),max(tw.maxcs.ct*0.0115)),ylim=c(min(tw.maxprofit.ct*0.0115),max(tw.maxcs.ct*0.0115)),xlab = "Total welfare (in dollar) when profit is maximized",ylab = "Total welfare (in dollar) when CS is maximized",cex=0.3,pch=19)]
abline(0,1,col="red")
dev.off()
#------ct-noPersi model-------(PROPOSALDRAFT_5-11-2016: FIGURE 2.5(right))
png(file="tw_scatter_CTmodel_notpersonalized_6-8_R=3.png",height=2000,width=2000,res=300)
tw.15.1000[,plot(tw.maxprofit.ct.noPersi*0.0115,tw.maxcs.ct.noPersi*0.0115,xlim=c(min(tw.maxprofit.ct.noPersi*0.0115),max(tw.maxcs.ct.noPersi*0.0115)),ylim=c(min(tw.maxprofit.ct.noPersi*0.0115),max(tw.maxcs.ct.noPersi*0.0115)),xlab = "Total welfare (in dollar) when profit is maximized",ylab = "Total welfare (in dollar) when CS is maximized",cex=0.3,pch=19)]
abline(0,1,col="red")
dev.off()
#-----ct model---------------rank by maxmizing profit VS rank by maximing total welfare
png(file="tw_scatter_CTmodel_maxtwVSmaxprofit_6-8_R=3.png",height=2000,width=2000,res=300)
tw.15.1000[,plot(tw.maxprofit.ct*0.0115,tw.maxtw.ct*0.0115,xlim=c(min(tw.maxprofit.ct*0.0115),max(tw.maxtw.ct*0.0115)),ylim=c(min(tw.maxprofit.ct*0.0115),max(tw.maxtw.ct*0.0115)),xlab = "Total welfare (in dollar) when profit is maximized",ylab = "Maximized Total welfare (in dollar)",cex=0.3,pch=19)]
abline(0,1,col="red")
dev.off()


