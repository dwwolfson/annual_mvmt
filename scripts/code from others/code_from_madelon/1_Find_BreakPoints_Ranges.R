
#####################
# breakpoint models:
#####################

library(strucchange)
library(imputeTS)
library(overlapping)
library(sp)
library(adehabitatHR)
library(ggplot2)
theme_set(theme_bw())

#load data:
load("mddf2.RData")

#remove males:
mddfcovar2=mddfcovar2[mddfcovar2$Sex=="Female",]

# attach day (july 1st = 0; technically days since latest july 1)
mddfcovar2$Day=mddfcovar2$Date-as.Date("2015-7-1")
mddfcovar2$Day[mddfcovar2$MigrYear==2]=mddfcovar2$Date[mddfcovar2$MigrYear==2]-as.Date("2016-7-1")
mddfcovar2$Day[mddfcovar2$MigrYear==3]=mddfcovar2$Date[mddfcovar2$MigrYear==3]-as.Date("2017-7-1")
mddfcovar2$Day[mddfcovar2$MigrYear==4]=mddfcovar2$Date[mddfcovar2$MigrYear==4]-as.Date("2018-7-1")
mddfcovar2$Day[mddfcovar2$MigrYear==5]=mddfcovar2$Date[mddfcovar2$MigrYear==5]-as.Date("2019-7-1")
mddfcovar2$Day=as.numeric(mddfcovar2$Day)

# split up by burst:
md_by_ID <- split(mddfcovar2, mddfcovar2$burst)
IDs=as.character(unique(mddfcovar2$burst))
#IDs=as.character(c("1032_4", "10_1", "1008_4"))

reslist=list()

# RUN MODELS FOR EACH DEER / YEAR :
start=Sys.time()
k=0
for (b in IDs){
  k=k+1
  print(k)
  idat=md_by_ID[[b]]
  
  # idat=md_by_ID[["1146_2"]]
  dats=data.frame(Day=seq(-14,379,1))
  nogaps=merge(dats, idat[,c("Day", "R2n", "x", "y")], all.x=TRUE)
  myts=ts(nogaps$R2n, start=1, frequency = 1)
  myts=na_interpolation(myts)
  bp.myts <- breakpoints(myts ~ 1, h=0.03)
  
  nogaps$R2n=as.numeric(myts)
  nogaps$Segment=as.numeric(breakfactor(bp.myts))
  
  
  # check for overlap:
  NSD_by_Seg <- split(nogaps$R2n, nogaps$Segment)
  
  out <- overlap(NSD_by_Seg)
  #print(out$OV)
  overl=data.frame(overlap=out$OV)
  overl$Segments=rownames(overl)
  overl$range1=sapply(strsplit(overl$Segments, "-"), "[[", 1)
  overl$range2=sapply(strsplit(overl$Segments, "-"), "[[", 2)
  
  overl=overl[overl$overlap>0.05,]
  
  
  
  dat=overl
  
  # first combine all rows into groups:
  rangesA=list()
  rangesA[[1]]=c(dat$range1[1], dat$range2[1])
  
  if (nrow(dat)>1){
    for (i in 2:nrow(dat)){
      curlist=rangesA
      for (j in 1:length(rangesA)){
        if(dat$range1[i] %in% rangesA[[j]] | dat$range2[i] %in% rangesA[[j]]){
          rangesA[[j]]=c(rangesA[[j]], dat$range1[i], dat$range2[i])
        }
      }
      if (identical(curlist, rangesA)){
        rangesA[[(length(rangesA)+1)]]=c(dat$range1[i], dat$range2[i])
      }
    }
  }
  
  # now in order, if one number matches, combine them:
  rangesB=list()
  rangesB[[1]]=rangesA[[1]]
  if (length(rangesA)>1){
    for (i in 2:length(rangesA)){
      curlist=rangesB
      for (j in 1:length(rangesB)){
        if (sum(rangesA[[i]] %in% rangesB[[j]])>1){
          rangesB[[j]]=c(rangesB[[j]], rangesA[[i]])
        }
      }
      if (identical(curlist, rangesB)){
        rangesB[[(length(rangesB)+1)]]=rangesA[[i]]
      }
    }
  }
  
  
  
  
  
  nogaps$newseg=0
  
  for (i in 1:length(rangesB)){
    nogaps$newseg[nogaps$Segment %in% rangesB[[i]]]=i
  }
  
  # number the ranges that don't overlap with anything else:
  for (d in unique(nogaps$Segment)){
    if (sum(sapply(rangesB, `%in%`, x = d))==0){
    nogaps$newseg[nogaps$Segment==d]=max(nogaps$newseg) + 1
    }
  }
  
  
  # HERE, GET RID OF SEGMENTS <2 WEEKS:
  # remove 'sallies' less than two weeks:
  inds=c(1, which(c(FALSE, tail(nogaps$newseg,-1) != head(nogaps$newseg,-1))))
  nogaps$unirange=NA
  nogaps$unirange[inds]=1:length(inds)
  nogaps$unirange=na_locf(nogaps$unirange)
  
  nogaps$Length=sequence(rle(as.character(nogaps$unirange))$length)
  maxes=by(nogaps$Length, nogaps$unirange, max)
  shorties=as.numeric(which(maxes<14))
  nogaps$R2norig=nogaps$R2n
  
  # remove short visits:
  nogaps$R2n[nogaps$unirange %in% shorties]=NA
  
  # if now one level only has NAs, set the newseg also to NAs:
  usegs=unique(nogaps$newseg)
  nonasegs=unique(nogaps$newseg[!is.na(nogaps$R2n)])
  nasegs=setdiff(usegs,nonasegs)
  nogaps$newseg[nogaps$newseg %in% nasegs]=NA
  
  nogaps$newseg=as.factor(nogaps$newseg)
  
  if(length(levels(nogaps$newseg))>1){ # if more than one segment left 
    fm <- lm(nogaps$R2n ~ nogaps$newseg, na.action=na.exclude)}else{ # fit lm by segment
      fm <- lm(nogaps$R2n ~ 1, na.action=na.exclude) # otherwise just fit one lm
    }
  
  #nogaps$fit=predict(fm, nogaps$newseg)
  nogaps$fit=fitted(fm)
  
  # fill in NA values with closest value
  if (any(is.na(nogaps$R2n))){
     na.pos <- which(is.na(nogaps$fit))
     if (length(na.pos) == length(nogaps$fit)) {
       return(nogaps$fit)
     }
    non.na.pos <- setdiff(seq_along(nogaps$fit), na.pos)
    nearest.non.na.pos <- sapply(na.pos, function(x) {
      return(which.min(abs(non.na.pos - x)))
    })
    nogaps$fit[na.pos] <- nogaps$fit[non.na.pos[nearest.non.na.pos]]
  }
  
  # renumber segments based on unique fitted values:
  nogaps$Range=as.numeric(as.factor(round(nogaps$fit)))
  
  nogaps$ID=idat$ID[1]
  nogaps$burst=idat$burst[1]
  nogaps$Year=idat$Year[1]
  nogaps$Unit=idat$Unit[1]
  nogaps$Sex=idat$Sex[1]
  nogaps$Age=idat$Age[1]
  
  resdat=nogaps[c("ID", "burst", "Year", "Unit", "Sex", "Age", "Day", 
                  "R2norig", "R2n", "x", "y", "fit", "Segment", "Range")]
  
  reslist[[b]]=resdat
   
  # plot(myts)
  # lines(ts(fitted(fm), start=1), col = 4)
  # lines(bp.myts)
  # title(b)
  # 
  #   print(ggplot(nogaps)+
  #           geom_line(aes(x=Day, y=sqrt(R2norig)/1000), col="grey")+
  #           geom_point(aes(x=Day, y=sqrt(fit)/1000, color=as.factor(Range)), size=2)+
  #           geom_line(aes(x=Day, y=sqrt(fit)/1000), col="black")+
  #           labs(y="Net Disp (km)", color="Range", title=paste(k,b, sep=": ")))
  
  
#  readline(prompt="Press [enter] to continue")
}

RangeDat=do.call(rbind, reslist)
save(RangeDat, file="RangeDat2.RData")
end=Sys.time()
elapsed=end-start
elapsed




