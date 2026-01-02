library(survival)
library(biostat3)
library(tidyverse)

sumdata <- read.csv("sumbackblaze.csv")

theyears <- as.data.frame(table(sumdata$year))

sumdata <- sumdata %>%
  dplyr::mutate(model = if_else(model=="WDC  WUH721414ALE6L4","WDC WUH721414ALE6L4", model)) %>%
  dplyr::mutate(manuf=str_sub(model,1,2),
        manuf = if_else(manuf %in% c("HG","Hi", "ST","TO","WD"), manuf, "OT"),
manuf = factor(manuf, levels=c("HG","Hi", "ST","TO","WD","OT"), labels=c("HGST","Hitachi","Seagate","Toshiba","Western Digital","Other")),
manuf = as.character(manuf)) %>%
  dplyr::mutate(groupyear = factor(year))

disks.fityear <- survfit(Surv(time,status)~groupyear, data=sumdata)
plot(disks.fityear, xlab="Time (years)", ylab="Cumulative Proportion with Failure",
     conf.int=FALSE,
     xmax=10, col=rainbow(nlevels(sumdata$groupyear)), lty=1, lwd=2, fun="event",
     main=paste("Cumulative Failures for Drives by Starting Year"))

legend("topleft", levels(sumdata$groupyear), lty=1, lwd=2, col = rainbow(nlevels(sumdata$groupyear)))

fit2 <- muhaz2(Surv(time, status)~groupyear, max.time=min(4,max(sumdata$time)), data=sumdata[sumdata$year <=2023,])

plot(fit2, col=rainbow(nlevels(sumdata$groupyear)), lwd=2, lty=1, 
     xlim=c(0,4),
     legend.args=list(legend=levels(sumdata$groupyear),x="topleft",lwd=2,
                      col=rainbow(nlevels(sumdata$groupyear))),
     main=paste("Hazard for Drives Installed by Year \nMore than 2000 drives installed"))


for (startyear in min(sumdata$year):(max(sumdata$year)-2)) {
  
  nodrives <- as.data.frame(table(sumdata$model[sumdata$year==startyear]))
  names(nodrives) <- c("model","nodrives")
  
  sumdata2 <- merge(sumdata,nodrives)
  sumdata2 <- sumdata2[(sumdata2$year==startyear) & (sumdata2$nodrives>2000),]
  

  sumdata2$model <- factor(sumdata2$model)
  #browser()
  disks.fitall <- survfit(Surv(time,status)~model, data=sumdata2)
  plot(disks.fitall, xlab="Time (years)", ylab="Cumulative Proportion with Failure",
       conf.int=FALSE,
       xmax=floor(min(4,max(sumdata2$time))), col=rainbow(nlevels(sumdata2$model)), lty=1, lwd=2, fun="event",
       main=paste("Cumulative Failures for Drives Installed in ", startyear,"\nMore than 2000 drives installed"))
       
  legend("topleft", levels(sumdata2$model), lty=1, lwd=2, col = rainbow(nlevels(sumdata2$model)))
}

for (startyear in min(sumdata$year):(max(sumdata$year)-3)) {
#for (startyear in 2021:(max(sumdata$year)-4)) {
    
  nodrives <- as.data.frame(table(sumdata$model[sumdata$year==startyear]))
  names(nodrives) <- c("model","nodrives")
  
  sumdata2 <- merge(sumdata,nodrives)
  sumdata2 <- sumdata2[(sumdata2$year==startyear) & (sumdata2$nodrives>2000),]
  sumdata2$model <- factor(sumdata2$model)
  #browser()
#  disks.fitall <- survfit(Surv(time,status)~model, data=sumdata2)
#  if (startyear==2021) browser()
  fit2 <- muhaz2(Surv(time, status)~model, max.time=min(4,max(sumdata2$time)), data=sumdata2)
#  fit2 <- muhaz2(Surv(time, status)~model, data=sumdata2)
#  browser()
  plot(fit2, col=rainbow(nlevels(sumdata2$model)), lwd=2, lty=1, 
       xlim=c(0,4),
       legend.args=list(legend=levels(sumdata2$model),x="topleft",lwd=2,
                        col=rainbow(nlevels(sumdata2$model))),
       main=paste("Hazard for Drives Installed in ", startyear,"\nMore than 2000 drives installed"))
}
