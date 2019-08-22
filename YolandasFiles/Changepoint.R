# Automated analysis of activity changepoints
# Based on 2014 & 2015 Motus data
# Last revised: 5 May 2019

# Note to Patrick: change pathnames to where files are saved.

# The data going into the changepoint analysis are the sqrt(abs(siglag-sig)) values 
# (i.e., sqrt(diff.abs), where diff.abs is the absoluate change in sig between consecutive 
# measurements. Taking the sqrt of diff.abs values produces more stable results than just using 
# diff.abs, likely because the influence of large diff.abs values are moderated.
# All data >= 11:00 am is used, although post-tagging periods are peri-departure activity are
# cropped out.
# The changepoint is based on a single change in mean and variance.
# Various constraints are specified to ensure retention of true changepoints from activity to 
# night-time inactivity: 
# A minimum number of daily observations
# Minimum time intervals between consecutive observations
# Mean and variance values before and after the changepoint.

Sys.setenv(TZ='GMT')  

.libPaths("C:/Users/ymorbey/Documents/R/win-library/3.4") 

setwd("C:/Users/ymorbey/Documents/Manuscripts - current/Protandry 2014")

require(dplyr)
require(lattice)
library(lubridate)
library(changepoint)
library(ggplot2)
library(maptools)

rm(list=ls())

### import bird data, adjust some departure dates from Old Cut, and select those with mlos at Old Cut >= 2
### This gives me my bird lists for 2014 and 2015.
### Make sure to keep depart_night2, which is the last full day a bird was present at Old Cut.

bird <- read.csv("dryad/morbey_radio.csv",header=T)
bird$depart_night2 <- as.POSIXct(strptime(as.character(bird$last_day),"%m/%d/%Y"))
attr(bird$depart_night2,"tzone") <- "UTC"

# previous lists of birds used in changepoint analysis:
# filter(id %in% c(417,421,424,425,431,449,466,474,482,487,488,490,491,436,450,457,461,464,465,486,494,438,462,483)) %>%    # birds present at Old Cut for 1 day or gappy
# filter(id %in% c(12,15,20,22,25,27,33,277,289,291,294,295,297,303,304,398,401,405,409,418,420,421)) # gappy birds in 2015

bird.2014 <- bird %>%
   filter(year==2014 & !is.na(mlos == T)) %>%
   select(year,id,mlos,depart_night2) 
( bird.2014.list <- as.factor(bird.2014$id) )  # duplicates in 2015 = 417,418,421. 

bird.2015 <- bird %>%
   filter(year==2015 & !is.na(mlos == T)) %>%
   select(year,id,mlos,depart_night2)
( bird.2015.list <- as.factor(bird.2015$id) )  

( bird2 <- rbind(bird.2014,bird.2015) )
bird2 <- rename(bird2,mfgID=id)

#################################################################################################
### read in motus data as *.rds files 
### This section does not need to be run, because the output was already saved as quiescence.rds

gc()     # sometimes I run this to clear up space
ls()

test2014 <- readRDS("protandry2014.rds")
test2014.2 <- test2014 %>%
  filter(mfgID %in% bird.2014.list) %>%
  filter(recvDeployName %in% c("Old Cut","Western_University_Trailer_OldCut")) %>%
  select(mfgID,recvDeployName,motusTagID,tagProjID,port,ts,sig,sigsd,runLen,freqsd)
rm(test2014)

test2015 <- readRDS("protandry2015.rds")
test2015.2 <- test2015 %>%
  filter(mfgID %in% bird.2015.list & year(ts) == 2015) %>%
  filter(recvDeployName %in% c("Old Cut","Western_University_Trailer_OldCut")) %>%
  select(mfgID,recvDeployName,motusTagID,tagProjID,port,ts,sig,sigsd,runLen,freqsd)
rm(test2015)

test <- rbind(test2014.2,test2015.2)

rm(test2014.2,test2015.2)

dim(test)

### 

test <- test %>% mutate(year=year(ts))
names(test)
names(bird2)

# merge data & motus data
test <- merge(test,bird2,by.x = c("year","mfgID"), by.y = c("year","mfgID"))
glimpse(test)   # n = 4,300,676

saveRDS(test, "quiescence.rds")

#################################################################################################



### Start here.

test <- readRDS("quiescence.rds")    

test2 <- test %>% mutate(year = as.integer(year(ts))) %>%
	group_by(year,mfgID) %>% filter(yday(ts) <= yday(depart_night2)) %>%
	filter(runLen > 2 & freqsd < 0.1)

# Exclude departure activity by subtracting off last 12 minutes of activity
# Exclude first 4 hours after tagging because birds sometimes show inactivtiy post-tagging.

test2 %>% group_by(year,mfgID) %>% summarize(max = max(ts)) %>% View()

test3 <- test2 %>% group_by(year,mfgID) %>% filter(ts < max(ts)-720) %>%
	filter(ts > min(ts) + 4*60*60) 

# show data for one individual

( id <- bird.2015.list[2] )
ind <- test2 %>% filter(mfgID==id & year == 2015) %>%
	mutate(time=hour(ts)+minute(ts)/60)
xyplot(sig~time|date(ts),group=port,data=ind,pch=20,cex=1.5,type=c("r","p"),auto.key=TRUE)

names(test2)

### Automate the changepoint detection method ###

# Prepare data frame for analysis.
# For each recvDeployName (Old Cut and Western University Trailer) & port, calculate lagged differences in sig.
# Keep diffs only if consecutive observations occur with a short time lag.

crit.diff.ts <- 30   # consecutive observations must be less than this number of seconds
crit.start.h <- 10   # observations must be later than 11:00.

# generate the variable to assess changepoints.
# I found sqrt(abs(siglag-sig)) to show good behaviour.

test4 <- test3 %>% mutate(id=paste(tagProjID,mfgID,sep="."),
  day = (yday(ts))-(min(yday(ts)))+1) %>% group_by(id,recvDeployName,port) %>% arrange(recvDeployName,port,ts) %>% 
  mutate(siglag=lag(sig), tslag=lag(ts), hour=hour(ts)+minute(ts)/60, yday=yday(ts)) %>% 
  mutate(diff.abs=sqrt(abs(siglag-sig)),diff.ts=ts-tslag) %>% 
  filter(diff.ts < crit.diff.ts & hour > crit.start.h) %>%
  as.data.frame()

# plot individual data

ind <- test4 %>% 
   filter(mfgID==424 & tagProjID == "20") %>%
   arrange(ts)
xyplot(diff.abs~ts,data=ind,group=port,pch=20,cex=1.5,type=c("r","p"),auto.key=TRUE)
xyplot(sig~ts,data=ind,group=recvDeployName,pch=20,cex=1.5,type=c("r","p"),auto.key=TRUE)

( bird.list <- sort(unique(test4$id)) )

# Loop by bird & day. 
# Changepoint analysis is done for each bird and day
# Changepoint is based on "at most one change" (AMOC) with pen.value = 0.001 (this is similar to an alpha value) 
# Results of changepoint analyses are put into arrays.
# If the changepoint analysis returns 0 changepoints, then changepoint is assumed to be final observation.
# Cases of 0 changepoints can be investigated. Scroll back through console output to see the changepoint message.

test4 <- test4 %>% arrange(id,ts)   # Inter-digitate diff.abs values from all ports for one sequence.

crit.nobs <- 500                    # number of observations required in a day to do the changepoint analysis.

dim.i <- length(bird.list)          # number of unique bird ids
dim.j <- max(test4$day)             # number of days

# initialize arrays

cpt.ts1.array <- array(NA,dim=c(dim.i,dim.j))	# hold changepoints
cpt.tsmax <- array(NA,dim=c(dim.i,dim.j))	      # hold maximum ts
cpt.tspre <- array(NA,dim=c(dim.i,dim.j))       # hold ts of changepoint - 1 observation
cpt.tspost <- array(NA,dim=c(dim.i,dim.j))      # hold ts of changepoint + 1 observation
cpt.meanpre <- array(NA,dim=c(dim.i,dim.j))     # hold mean of diff.abs before the changepoint
cpt.meanpost <- array(NA,dim=c(dim.i,dim.j))    # hold mean of diff.abs after the changepoint
cpt.varpre <- array(NA,dim=c(dim.i,dim.j))      # hold var of diff.abs before the changepoint
cpt.varpost <- array(NA,dim=c(dim.i,dim.j))     # hold var of diff.abs after the changepoint

# assign names to arrays

dimnames(cpt.ts1.array) <- list(bird.num=1:dim.i, day=1:dim.j )
dimnames(cpt.tsmax) <- list(bird.num=1:dim.i, day=1:dim.j )
dimnames(cpt.tspre) <- list(bird.num=1:dim.i, day=1:dim.j )
dimnames(cpt.tspost) <- list(bird.num=1:dim.i, day=1:dim.j )
dimnames(cpt.meanpre) <- list(bird.num=1:dim.i, day=1:dim.j )
dimnames(cpt.meanpost) <- list(bird.num=1:dim.i, day=1:dim.j )
dimnames(cpt.varpre) <- list(bird.num=1:dim.i, day=1:dim.j )
dimnames(cpt.varpost) <- list(bird.num=1:dim.i, day=1:dim.j )


for (i in 1:dim.i) {                           
	id <- bird.list[i]
      for (j in 1:dim.j) {
        print(paste("id=",bird.list[i],"i=",i,"day=",j))
        temp <- test4 %>% filter(id==bird.list[i] & day==j)
	  n <- length(temp$diff.abs)
        print(paste("n=",n))
        if (n > crit.nobs) {
          out1.pmv <- cpt.meanvar(temp$diff.abs, penalty="Asymptotic", pen.value=0.001, minseglen=20,param.estimates=T,class=T)
          if (ncpts(out1.pmv) > 0) {         
	      cpt1 <- cpts(out1.pmv)
	      cpt.ts1 <- temp[cpt1,"ts"]
   	      cpt.ts1.array[i,j] <- cpt.ts1
		cpt.tsmax[i,j] <- max(temp$ts)
	      cpt.tspre[i,j]  <- round(cpt.ts1-temp[cpt1-1,"ts"],2)
	      cpt.tspost[i,j] <- round(temp[cpt1+1,"ts"]-cpt.ts1,2)
            cpt.meanpre[i,j] <- param.est(out1.pmv)$mean[1]
            cpt.meanpost[i,j] <- param.est(out1.pmv)$mean[2]
		cpt.varpre[i,j] <- param.est(out1.pmv)$variance[1]
            cpt.varpost[i,j] <- param.est(out1.pmv)$variance[2]
		print(paste(cpt.ts1))
            } else {
              if (ncpts(out1.pmv) == 0) {
		     cpt.ts1 <- max(temp$ts)
                 cpt.ts1.array[i,j] <- cpt.ts1
 		     print(paste(cpt.ts1,"***************************No change points***************************"))
              }}
          }
	}    # end j
}          # end i

# create data frames from the arrays

ts1 <- as.data.frame.table(cpt.ts1.array, responseName = "ts1") 
tsmax <- as.data.frame.table(cpt.tsmax, responseName = "tsmax") 
tspre <- as.data.frame.table(cpt.tspre, responseName = "tspre") 
tspost <- as.data.frame.table(cpt.tspost, responseName = "tspost") 
meanpre <- as.data.frame.table(cpt.meanpre, responseName = "meanpre")
meanpost <- as.data.frame.table(cpt.meanpost, responseName = "meanpost")
varpre <- as.data.frame.table(cpt.varpre, responseName = "varpre")
varpost <- as.data.frame.table(cpt.varpost, responseName = "varpost")

# merge the data frames

cpts <- merge(ts1,tspre,by=c("bird.num","day"))
cpts <- merge(cpts,tsmax,by=c("bird.num","day"))
cpts <- merge(cpts,tspost,by=c("bird.num","day"))
cpts <- merge(cpts,meanpre,by=c("bird.num","day"))
cpts <- merge(cpts,meanpost,by=c("bird.num","day"))
cpts <- merge(cpts,varpre,by=c("bird.num","day"))
cpts <- merge(cpts,varpost,by=c("bird.num","day"))

# data frame manipulation

cpts <- cpts %>% 
   mutate(cpt = as_datetime(ts1), yday = yday(cpt)) %>%
   mutate(tsmax.dt = as_datetime(tsmax)) %>% 
   filter(ts1 > 0) %>%   # keep changepoints
   select(bird.num,day,yday,tspre,tspost,tsmax.dt,meanpre,meanpost,varpre,varpost,cpt)

names(cpts)
dim(cpts)                          # note the number of changepoints (428)
head(cpts)

# View the changepoint metrics.

qplot(na.exclude(cpts$meanpre),bins=50)  # View distribution of meanpre values (most > 1)
qplot(na.exclude(cpts$meanpost),bins=50) # View distribution of meanpost values (most < 1)
qplot(na.exclude(cpts$meanpre-cpts$meanpost),bins=50)  # View distribution of diff in mean (most > 0)
qplot(na.exclude(cpts$varpre),bins=50)  # View distribution of varpre values (most > 0.3)
qplot(na.exclude(cpts$varpost),bins=50) # View distribution of varpost values (most < 0.3)
qplot(na.exclude(cpts$varpre-cpts$varpost),bins=50)  # View distribution of diff in var (most > 0)


# filter cpts based on criteria

crit.neigh <- 60  # critical # seconds +/- a changepoint with sig data
crit.mean1 <- 1   # critical meanpre value
crit.mean2 <- 1   # critical meanpost value

   
### View questionable changepoints for investigation (n = 26)

delete <- cpts[which(cpts$meanpre < crit.mean1 |
            cpts$meanpost > crit.mean2 |
		cpts$meanpre < cpts$meanpost | 
            cpts$varpre < cpts$varpost),]
delete %>% arrange(bird.num,day) %>% select(bird.num,cpt,tsmax.dt,meanpre,meanpost,varpre,varpost)

delete <- which(cpts$meanpre < crit.mean1 |
            cpts$meanpost > crit.mean2 |
		cpts$meanpre < cpts$meanpost | 
            cpts$varpre < cpts$varpost)

# Questionable changepoints can be deleted or they can be individually assessed and corrected.
# To eliminate subjective re-assignment, I just deleted these questional changepoints.

cpts2 <- cpts[-delete,]         # delete 26 questionable changepoints
dim(cpts2)                      # 402 changepoints remaining.

cpts3 <- cpts2 %>% 
   mutate(time = hour(cpt)+minute(cpt+(second(cpt)/60))/60) %>% arrange(time)

# delete "outliers" for changepoint times.

number_ticks <- function(n) {function(limits) pretty(limits, n)}

cpts3 %>% arrange(time) %>% View()

ggplot(data=cpts3,aes(x=time))+geom_histogram()+
	scale_x_continuous(breaks=number_ticks(40))
delete <- which(cpts3$time < 19 | cpts3$time >= 21.2)
cpts4 <- cpts3[-delete,]
ggplot(data=cpts4,aes(x=time))+geom_histogram()+
	scale_x_continuous(breaks=number_ticks(40))

?qplot

# convert changepoints to minutes since sunset.

coords <- matrix(c(-80.3965,42.5835), nrow=1)
cpts5 <- cpts4 %>% mutate(cpt=as.POSIXct(cpt)) %>%
   mutate(id = bird.list[bird.num]) %>%
   arrange(bird.num,day) 
cpts5$cpt <- force_tz(cpts5$cpt,tzone="America/New_York",roll=F)
cpts5$cpt[1]
cpts5$sunset <- sunriset(coords,cpts5$cpt,direction="sunset",POSIXct.out=TRUE)$time
cpts5$cpt_min <- (cpts5$cpt-cpts5$sunset)/60
cpts5$cpt_min


qplot(cpts5$cpt_min)

last_day <- test4 %>% group_by(id) %>%
	summarize(last = mean(yday(depart_night2))) 
cpts6 <- merge(cpts5,last_day,by=c("id"))
cpts6 <- cpts6 %>%
	mutate(day2 = last-yday+1,ComboID = id) %>%
      select(bird.num,ComboID,yday,day,day2,last,cpt_min) %>% as.data.frame()



glimpse(cpts6)
head(cpts6,20)
View(cpts6)



# write.csv(cpts6,"BTBW_MAWA_changepoints.csv",row.names=F)

cpts7 <- cpts6 %>% filter(day2 <= 3)
ggplot(data=cpts7,aes(x=as.factor(day2),y=cpt_min)) +
  geom_boxplot()





#  Prelim analysis....

names(cpts4)

temp <- cpts4 %>%
	group_by(bird.num) %>% 
 	mutate(day = as.numeric(day)) %>% 
 	summarise(last = max(day), first=min(day)) %>% 
 	inner_join(cpts4,by="bird.num") %>% 
	mutate(day2 = last-as.numeric(day)+1) %>% filter(as.numeric(day) > first & day2 <= 3) %>%
 	select(bird.num,id, day, first, last, day2,time)








dim(temp)
head(temp)

ggplot(data=temp, aes(x=day2,y=time, col=as.factor(bird.num)))+
  geom_point() +
  geom_line(aes(group=bird.num)) 


library(nlme)

temp1 <- temp %>% filter(day2 %in% c(1,2))
temp2 <- temp %>% filter(day2 %in% c(1,3))
temp3 <- temp %>% filter(day2 %in% c(2,3))

mod1 <- lme(data=temp1,time~as.factor(day2),random=~1|bird.num)
plot(mod1)
summary(mod1)
mod1 <- lme(data=temp2,time~as.factor(day2),random=~1|bird.num)
plot(mod1)
summary(mod1)
mod1 <- lme(data=temp3,time~as.factor(day2),random=~1|bird.num)
plot(mod1)
summary(mod1)



### Conduct the changepoint analysis on a single bird

( bird <- bird.list[4] )
temp <- test4 %>% arrange(ts) %>% filter(id==bird & day==15)
( n <- length(temp$diff.abs) )
out1.pmv <- cpt.meanvar(temp$diff.abs, penalty="Asymptotic", pen.value=0.01,minseglen=20,param.estimates=T,class=T)
p1 <- plot(out1.pmv,type="l",cpt.col="black",cpt.width=4,xlab="",ylab="")
  title(xlab="Index",ylab="Signal strength (dB)",cex.lab=1.2)
  title("(b)", cex.main = 1.2, adj=0,line=0.6,font.main=1)
cpt1 <- cpts(out1.pmv)
cpt.ts1 <- temp[cpt1,"ts"]
cpt.ts1
ncpts(out1.pmv)
param.est(out1.pmv)$mean
param.est(out1.pmv)$variance
max(temp$ts)

out1.pm <- cpt.mean(temp$diff.abs,penalty="Asymptotic",pen.value=0.001,minseglen=20)
ncpts(out1.pm)
out1.pv <- cpt.var(temp$diff.abs,penalty="Asymptotic",pen.value=0.001,minseglen=20)
ncpts(out1.pv)


