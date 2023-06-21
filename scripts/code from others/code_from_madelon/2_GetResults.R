library(ggplot2)
library(plyr)
theme_set(theme_bw())
library(cowplot)



# classify migrants based on number of ranges etc:

# load data:
load("RangeDat2.RData")

### how many deer had multiple years? ###
multip=RangeDat[!duplicated(RangeDat$burst),]
mult=as.data.frame(table(multip$ID))
mult=mult[mult$Freq>1,]

# deal with the 14 day buffer
# maybe change range in NA or something like that
RangeDat$Range[RangeDat$Day > 365 | RangeDat$Day < 0]=NA

# classify into 4 groups: 1) no breakpoints, 2) one trip, 3) multiple trips, 4) noreturn

# add number of ranges:
agg <- aggregate(data=RangeDat, Range ~ burst, function(x) length(unique(x)))
colnames(agg)=c("burst", "RangeSum")
RangeDat=merge(RangeDat, agg, all=TRUE)
RangeDat= RangeDat[with(RangeDat, order(RangeDat$burst, RangeDat$Day)),] #ORDER!!

oner=RangeDat[RangeDat$RangeSum==1,] #group 1 = no breakpoints
morer=RangeDat[RangeDat$RangeSum>1,] #groups 2-4

# renumber ranges (first = 1)
rd_by_ID <- split(morer, morer$burst)
IDs=as.character(unique(morer$burst))
reslist=list()
for (b in IDs){
  idat=rd_by_ID[[b]]
  ranges=(idat$Range[!duplicated(idat$Range)])
  merger=data.frame(Range=ranges, Range2=c(NA, seq(1, length(ranges)-1, 1)))
  idat2=merge(idat, merger)
  idat2= idat2[with(idat2, order(idat2$burst, idat2$Day)),] #ORDER!!
  reslist[[b]]=idat2
}
morer=do.call(rbind, reslist)


## MORE THAN 1 RANGE: 
# split up into 'never returns', 'one trip', 'multiple trips'
# home range is range 1, count how many times home range occurs
# never returns: range 1 only occurs once
# one trip: range one occurs twice, at beginning plus end
# rest: multiple trips

# split up by burst:
rd_by_ID <- split(morer, morer$burst)
IDs=as.character(unique(morer$burst))
resdf=data.frame(burst=unique(morer$burst), trips="Multiple", RangeSum=0, Class="TBD", stringsAsFactors=FALSE)

for (b in IDs){
  #  b=IDs[1]
  idat=rd_by_ID[[b]]
  #  idat=rd_by_ID[["1026_4"]]
  resdf$RangeSum[resdf$burst==b]=idat$RangeSum[1]
  ranges=idat$Range2[!is.na(idat$Range2)]
  ranges[ranges!=1]=0
  ranges=ranges[!c(FALSE, diff(ranges) == 0)]
  if (identical(ranges,c(1,0,1))){
    resdf$trips[resdf$burst==b]="One"
  }
  if (identical(ranges,c(1,0,1,0))){
    resdf$trips[resdf$burst==b]="Two"
  }
  if (identical(ranges,c(1,0,1,0,1))){
    resdf$trips[resdf$burst==b]="Two"
  }
  if (identical(ranges,c(1,0,1,0,1,0))){
    resdf$trips[resdf$burst==b]="Three"
  }
  if (identical(ranges,c(1,0,1,0,1,0,1))){
    resdf$trips[resdf$burst==b]="Three"
  }
  if (sum(ranges, na.rm=TRUE)==1){
    resdf$trips[resdf$burst==b]="NoReturn"
  }
}

nrow(resdf[resdf$trips=="Multiple",])

# assign all classes except for one trip / 2 range migrants, for which the timing matters:
resdf$Group=2
resdf$Group[resdf$trips=="NoReturn"]=4
resdf$Group[resdf$trips=="One"]=2
resdf$Group[resdf$trips=="Two"]=3
resdf$Group[resdf$trips=="Three"]=3

morer234=merge(morer, resdf)




## GROUP 1: stayed at one range or showed some gradual movements

## ONE RANGE:
# 1 range: split by range size : nomad or resident
means <- aggregate(data=oner, R2norig ~ burst, function(x) mean(x))
hist(sqrt(means$R2norig)/1000, breaks=30)
# use threshold of 5km2, where smaller is resident and larger is nomad:
means$Class="Resident"
means$Class[sqrt(means$R2norig)/1000>5]="Nomad"
#means$Class=as.factor(means$Class)
oner_classes=means[,c("burst", "Class")]
fems=merge(oner, oner_classes)
ddat=fems[fems$Class %in% c("Resident", "Nomad"),]
group1=ddat[,c("ID", "burst", "Class")]
group1=group1[!duplicated(group1),]
summ <- ddply(ddat, c("burst", "Class"), summarise,
              # N    = length(R2n),
              Dist = mean(sqrt(R2n)/1000, na.rm=T)
              # sd   = sd(R2n),
              # se   = sd / sqrt(N)
)
head(summ)

parms <- ddply(summ, c("Class"), summarise,
               N=length(unique(burst)),
               mean = mean(Dist),
               se   = sd(Dist) / sqrt(length(Dist))
)
head(parms)


# GROUP 2: ONE TRIP, EITHER SINGLE RANGE OR MULTI RANGE
ddat=morer234[morer234$Group==2,]
ddat$Strategy="Typical migrant"
ddat$Strategy[ddat$RangeSum>2]="Multiphasic migrant"

ddat=ddat[ddat$Day>0 & ddat$Day<366,]
gr2dat=ddat
ddat$Departure=0
ddat$Arrival=0
ddat$Distance=0
rd_by_ID <- split(ddat, ddat$burst)
IDs=as.character(unique(ddat$burst))
for (b in IDs){
  idat=rd_by_ID[[b]]
  days=idat$Day[!c(FALSE, diff(idat$Range) == 0)]
  ddat$Departure[ddat$burst==b]=days[2]
  ddat$Arrival[ddat$burst==b]=days[3]
  distance=mean(idat$R2n[idat$Range==2], na.rm=T)-mean(idat$R2n[idat$Range==1], na.rm=T)
  ddat$Distance[ddat$burst==b]=sqrt(distance)/1000
}
ddat$TripLength=ddat$Arrival - ddat$Departure
group2=ddat[,c("ID", "burst", "Strategy")]
group2=group2[!duplicated(group2),]

allsum=ddat[!duplicated(ddat$burst),]

parms <- ddply(allsum, c("Strategy"), summarise,
               N=length(unique(burst)),
               meanOD = format(as.Date(paste0(2018,"-06-30"))+(mean(Departure)),"%m-%d"),
               minOD = format(as.Date(paste0(2018,"-06-30"))+(min(Departure)),"%m-%d"),
               maxOD = format(as.Date(paste0(2018,"-06-30"))+(max(Departure)),"%m-%d"),
               seOD   = sd(Departure) / sqrt(length(Departure)),
               meanDist = mean(Distance, na.rm=TRUE),
               seDist   = sd(Distance, na.rm=TRUE) / sqrt(length(Distance)),
               meanAway = mean(TripLength),
               seAway   = sd(TripLength) / sqrt(length(TripLength)),
               meanIA = format(as.Date(paste0(2018,"-06-30"))+(mean(Arrival)),"%m-%d"),
               minIA = format(as.Date(paste0(2018,"-06-30"))+(min(Arrival)),"%m-%d"),
               maxIA = format(as.Date(paste0(2018,"-06-30"))+(max(Arrival)),"%m-%d"),
               seIA   = sd(Arrival) / sqrt(length(Arrival)),
               meanRanges = mean(RangeSum),
               seRanges = sd(RangeSum) / sqrt(length(RangeSum))
)
parms

# number of typical timers in both groups:
allsum$TypTime="Typ"
allsum$TypTime[allsum$Departure<(244-181)]="Atyp"
allsum$TypTime[allsum$Departure>(365-181)]="Atyp"
allsum$TypTime[allsum$Arrival<(91+(365-181))]="Atyp"
allsum$TypTime[allsum$Arrival>(181+(365-181))]="Atyp"

typs <- ddply(allsum, c("Strategy", "TypTime"), summarise,
               N=length(unique(burst))
)
typs

# make figures of arrival and departure for single range and multi range
dep=ddat
dep$Par="Inbound arrival"
dep$MovingDay=dep$Arrival
arr=ddat
arr$Par="Outbound departure"
arr$MovingDay=arr$Departure
depar=rbind(dep,arr)
depar$strat="Single range migrants"
depar$strat[depar$Strategy=="Multiphasic migrant"]="Multi range migrants"
depar$strat=factor(depar$strat, levels=c("Single range migrants", "Multi range migrants"))
depar$Par=factor(depar$Par, levels=c("Outbound departure", "Inbound arrival"))


deparr=ggplot(depar, aes(x=MovingDay, fill=Par, linetype=Par))+
  geom_density(alpha=.7, size=1)+
  labs(y="Frequency", x="Day (starting July 1st)", fill="", linetype="")+
  theme(panel.grid.minor = element_blank(),panel.border = element_blank(),
        axis.line.x=element_line(size=1, linetype="solid", colour="black"),
        axis.line.y=element_line(size=1, linetype="solid", colour="black"))+
  scale_fill_manual(values=c("purple4", "green4"))+
  annotate("rect", xmin = 63, xmax = 184, ymin = 0, ymax = Inf, fill="grey", color="black", alpha=.5)+
  annotate("rect", xmin = 275, xmax = 365, ymin = 0, ymax = Inf, fill="grey", color="black", alpha=.5)+
  facet_wrap(~strat, nrow=2)#+
deparr
#ggsave("ArrDep.png", deparr,   width = 6, height = 4, units = "in")




# GROUP 3: MULTIPLE TRIPS, EITHER TO SAME OR TO DIFFERENT RANGE
ddat=morer234[morer234$Group==3,]
ddat$Strategy="Commuter"
ddat$Strategy[ddat$RangeSum>2]="Poly"
ddat=ddat[ddat$Day>0 & ddat$Day<366,]
gr3dat=ddat
# get ranges, trips and time away:
ddat$Ranges=0
ddat$Trips=0
ddat$TimeAway=0
rd_by_ID <- split(ddat, ddat$burst)
IDs=as.character(unique(ddat$burst))
for (b in IDs){
  idat=rd_by_ID[[b]]
  ddat$Ranges[ddat$burst==b]=length(unique(idat$Range))
  ranges=idat$Range
  ranges[ranges!=1]=0
  ranges=ranges[!c(FALSE, diff(ranges) == 0)]
  ddat$Trips[ddat$burst==b]=length(ranges[ranges==0])
  ddat$TimeAway[ddat$burst==b]=365-sum(idat$Range[idat$Range==1])
}

group3=ddat[,c("ID", "burst", "Strategy")]
group3=group3[!duplicated(group3),]

allsum=ddat[!duplicated(ddat$burst),]

parms <- ddply(allsum, c("Strategy"), summarise,
               N=length(unique(burst)),
               meanTrips = mean(Trips),
               seTrips  = sd(Trips) / sqrt(length(Trips)),
               meanRanges = mean(Ranges),
               seRanges  = sd(Ranges) / sqrt(length(Ranges)),
               meanAway = mean(TimeAway),
               seAway   = sd(TimeAway) / sqrt(length(TimeAway))
)
parms




# GROUP 4 = NO RETURNERS AKA DISPERERS
ddat=morer234[morer234$Group==4,]
ddat$Strategy="Disperser"
ddat=ddat[ddat$Day>0 & ddat$Day<366,]

ddat$Departure=0
ddat$Arrival=0
ddat$Distance=0
rd_by_ID <- split(ddat, ddat$burst)
IDs=as.character(unique(ddat$burst))
for (b in IDs){
  idat=rd_by_ID[[b]]
  days=idat$Day[!c(FALSE, diff(idat$Range) == 0)]
  ddat$Departure[ddat$burst==b]=days[2]
  ddat$Arrival[ddat$burst==b]=days[3]
  distance=mean(idat$R2n[idat$Range==2], na.rm=T)-mean(idat$R2n[idat$Range==1], na.rm=T)
  ddat$Distance[ddat$burst==b]=sqrt(distance)/1000
}
ddat$TripLength=ddat$Arrival - ddat$Departure

group4=ddat[,c("ID", "burst", "Strategy")]
group4=group4[!duplicated(group4),]

allsum=ddat[!duplicated(ddat$burst),]

parms <- ddply(allsum, c("Strategy"), summarise,
               N=length(unique(burst)),
               meanOD = format(as.Date(paste0(2018,"-06-30"))+(mean(Departure)),"%m-%d"),
               minOD = format(as.Date(paste0(2018,"-06-30"))+(min(Departure)),"%m-%d"),
               maxOD = format(as.Date(paste0(2018,"-06-30"))+(max(Departure)),"%m-%d"),
               seOD   = sd(Departure) / sqrt(length(Departure)),
               meanDist = mean(Distance, na.rm=TRUE),
               seDist   = sd(Distance, na.rm=TRUE) / sqrt(length(Distance)),
               meanAway = mean(TripLength),
               seAway   = sd(TripLength) / sqrt(length(TripLength)),
               meanIA = format(as.Date(paste0(2018,"-06-30"))+(mean(Arrival)),"%m-%d"),
               minIA = format(as.Date(paste0(2018,"-06-30"))+(min(Arrival)),"%m-%d"),
               maxIA = format(as.Date(paste0(2018,"-06-30"))+(max(Arrival)),"%m-%d"),
               seIA   = sd(Arrival) / sqrt(length(Arrival)),
               meanRanges = mean(RangeSum),
               seRanges = sd(RangeSum) / sqrt(length(RangeSum))
)
parms

colnames(group1)=colnames(group2)
allstrats=rbind(group1, group2, group3, group4)
allstrats$Strategy[allstrats$Strategy=="Nomad"]="Gradual migrant"
allstrats$Strategy[allstrats$Strategy=="Typical migrant"]="Single-range migrant"
allstrats$Strategy[allstrats$Strategy=="Multiphasic migrant"]="Multi-range migrant"
allstrats$Strategy[allstrats$Strategy=="Poly"]="Poly migrant"

#save(allstrats, file="AllStrats.RData")



# for groups 2 and 3 (everyone who takes trips), do distance and time:
tripdat=rbind(gr2dat, gr3dat)
tripdat$Strategy[tripdat$Strategy=="Typical migrant"]="Single-range migrant"
tripdat$Strategy[tripdat$Strategy=="Multiphasic migrant"]="Multi-range migrant"
tripdat$Strategy[tripdat$Strategy=="Poly"]="Poly migrant"
ddat=tripdat

# calculate mean trip length and total combined trip length:

# get ranges, trips and time away:
ddat$Ranges=0
ddat$Trips=0
ddat$MeanDist=0
ddat$MaxDist=0
ddat$TotalTimeAway=0
ddat$MeanTimeAway=0
rd_by_ID <- split(ddat, ddat$burst)
IDs=as.character(unique(ddat$burst))
for (b in IDs){
  idat=rd_by_ID[[b]]
  ddat$Ranges[ddat$burst==b]=length(unique(idat$Range))
  ranges=idat$Range
  ranges[ranges!=1]=0
  ranges=ranges[!c(FALSE, diff(ranges) == 0)]
  distance=mean(idat$R2n[idat$Range!=1], na.rm=T)-mean(idat$R2n[idat$Range==1], na.rm=T)
  ddat$MeanDist[ddat$burst==b]=sqrt(distance)/1000
  ddat$Trips[ddat$burst==b]=length(ranges[ranges==0])
  ddat$TotalTimeAway[ddat$burst==b]=365-sum(idat$Range[idat$Range==1])
  ddat$MeanTimeAway[ddat$burst==b]=ddat$TotalTimeAway[ddat$burst==b] / ddat$Trips[ddat$burst==b]
  
  rangedists=ddply(idat, c("Range"), summarise,
                   meandist=mean(R2n, na.rm=T))
  maxdist=max(rangedists$meandist - mean(idat$R2n[idat$Range==1], na.rm=T))
  ddat$MaxDist[ddat$burst==b]=sqrt(maxdist)/1000
}

allsum=ddat[!duplicated(ddat$burst),]
allsum$Strategy[allsum$Strategy=="Single-range migrant"]="Dual-range \nmigrant"
allsum$Strategy[allsum$Strategy=="Multi-range migrant"]="Multi-range \nmigrant"
allsum$Strategy=factor(allsum$Strategy, levels=c("Dual-range \nmigrant", "Multi-range \nmigrant", "Commuter", "Poly migrant"))

meandistance <- ddply(allsum, c("Strategy"), summarise,
               mean = mean(MeanDist, na.rm=TRUE),
               se  = sd(MeanDist, na.rm=TRUE) / sqrt(length(MeanDist)),
               parm="Distance to other range(s)",
               var="Distance"
)
maxdistance <- ddply(allsum, c("Strategy"), summarise,
                      mean = mean(MaxDist, na.rm=TRUE),
                      se  = sd(MaxDist, na.rm=TRUE) / sqrt(length(MaxDist)),
                      parm="Distance to farthest range",
                     var="Distance"
)
maxdistance=maxdistance[maxdistance$Strategy %in% c("Multi-range \nmigrant", "Poly migrant"),]

meantrip <- ddply(allsum, c("Strategy"), summarise,
                      mean = mean(MeanTimeAway, na.rm=TRUE),
                      se  = sd(MeanTimeAway, na.rm=TRUE) / sqrt(length(MeanTimeAway)),
                      parm="Single trip length",
                  var="Time"
)
totaltrip <- ddply(allsum, c("Strategy"), summarise,
                  mean = mean(TotalTimeAway, na.rm=TRUE),
                  se  = sd(TotalTimeAway, na.rm=TRUE) / sqrt(length(TotalTimeAway)),
                  parm="Combined trip length",
                  var="Time"
)
totaltrip=totaltrip[totaltrip$Strategy %in% c("Commuter", "Poly migrant"),]

distparms=rbind(meandistance, maxdistance)
distparms$parm=factor(distparms$parm, levels=c("Distance to other range(s)", "Distance to farthest range"))

tripparms=rbind(meantrip, totaltrip)
tripparms$parm=factor(tripparms$parm, levels=c("Single trip length", "Combined trip length"))

distp=ggplot(distparms)+
  geom_point(aes(x=Strategy, y=mean, shape=parm), size=2)+
  geom_errorbar(aes(x=Strategy, ymin=mean-se, ymax=mean+se), width = 0.25)+
  labs(x="Strategy", y="Distance (km)", shape="")+
  theme(panel.grid.minor = element_blank(),panel.border = element_blank(),
        legend.position = "top",
        axis.line.x=element_line(size=1, linetype="solid", colour="black"),
        axis.line.y=element_line(size=1, linetype="solid", colour="black"))
timep=ggplot(tripparms)+
  geom_point(aes(x=Strategy, y=mean, shape=parm), size=2)+
  geom_errorbar(aes(x=Strategy, ymin=mean-se, ymax=mean+se), width = 0.25)+
  labs(x="Strategy", y="Time (days)", shape="")+
  theme(panel.grid.minor = element_blank(),panel.border = element_blank(),
        legend.position = "top",
        axis.line.x=element_line(size=1, linetype="solid", colour="black"),
        axis.line.y=element_line(size=1, linetype="solid", colour="black"))

plot_grid(distp, timep, label_size = 12, nrow=1)
#ggsave("disttime.emf",   width = 8.5, height = 4, units = "in")



