PMT

dat<-read.csv(file = "trustworthiness_pheo_test.csv", sep = ";", header = TRUE, dec = ",") ## my data
## plot 
library(RColorBrewer)
display.brewer.all()
## let's choose the blue from "set1" for those who did not participate and red to those who didnt
somecols<-brewer.pal(5, "Set1")
red<-col2rgb(somecols[1])
blue<-col2rgb(somecols[2])

transred<-rgb(red=red["red",], green=red["green",], blue=red["blue",], maxColorValue=255, alpha=60) ## let's make them a little transparent so that the dots plotted on top of each other will show
transblue<-rgb(red=blue["red",], green=blue["green",], blue=blue["blue",], maxColorValue=255, alpha=60)
dat$colors<-ifelse(dat$pmt==1, transred, transblue)
dat$cex<-dat$records/max(dat$records) ## relative size of the plotting symbol
oldpar<-par()

## the actual plot:

layout(matrix(c(2,2,2,2,1,1,1,1,1,1,1, 1,1,1,1, 1,1,1,1,1, 1,1,1,1), nrow=6, byrow=T))
library(car)
par(mar=c(5,5,1,0))
plot(0,0,type="n", ylim=c(-10,40), xlim=c(60,105), ylab="completeness in %", xlab="trustworthiness in %", 
     main="",cex.axis=2, las=1, cex.lab=2, cex.main=2, bty="n", yaxs="i", xaxs="i")
points( dat$trustworthiness,dat$completeness, cex=10*dat$cex, col=dat$colors, pch=16) ## i want the maximum size of a plotting symbol to be 8
with(subset(dat, pmt==1), dataEllipse(trustworthiness,completeness, weights=records2, levels=0.95, add=T, center.pch=NULL, plot.points = F, col=red))
with(subset(dat, pmt==0), dataEllipse(trustworthiness,completeness, weights=records2, levels=0.95, add=T, center.pch=NULL, plot.points = F, col='blue'))

with(subset(dat, cex>=0.8 ), text(x=trustworthiness,y=completeness, 
                                  centre))
head(dat)
table(dat$cex)
par(mar=c(0,0,0,0))
plot(0,0,type="n",  ylab="", xlab="", main="", bty="n", xaxt="n", yaxt="n")
legend("center", pch=16,  col=c(transred, transblue), legend=c("PMT", "no PMT"),
       bty="n", pt.cex=5, cex=2, title="PMT impact on ENSAT Pheo", ncol=2)





EURINE-ACT

dat<-read.csv(file = "trustworthiness_ACC_test.csv", sep = ";", header = TRUE, dec = ",") ## my data
## plot 
library(RColorBrewer)
display.brewer.all()
## let's choose the blue from "set1" for those who did not participate and red to those who didnt
somecols<-brewer.pal(5, "Set1")
red<-col2rgb(somecols[1])
blue<-col2rgb(somecols[2])

transred<-rgb(red=red["red",], green=red["green",], blue=red["blue",], maxColorValue=255, alpha=60) ## let's make them a little transparent so that the dots plotted on top of each other will show
transblue<-rgb(red=blue["red",], green=blue["green",], blue=blue["blue",], maxColorValue=255, alpha=60)
dat$colors<-ifelse(dat$EURINE==1, transred, transblue)
dat$cex<-dat$records/max(dat$records) ## relative size of the plotting symbol
oldpar<-par()

## the actual plot:

layout(matrix(c(2,2,2,2,1,1,1,1,1,1,1, 1,1,1,1, 1,1,1,1,1, 1,1,1,1), nrow=6, byrow=T))
library(car)
par(mar=c(5,5,1,0))
plot(0,0,type="n", ylim=c(0,90), xlim=c(65,105), ylab="completeness in %", xlab="trustworthiness in %", 
     main="",cex.axis=2, las=1, cex.lab=2, cex.main=2, bty="n", yaxs="i", xaxs="i")
points( dat$trustworthiness,dat$completeness, cex=10*dat$cex, col=dat$colors, pch=16) ## i want the maximum size of a plotting symbol to be 8
with(subset(dat, EURINE==1), dataEllipse(trustworthiness,completeness, weights=records2, levels=0.95, add=T, center.pch=NULL, plot.points = F, col=red))
with(subset(dat, EURINE==0), dataEllipse(trustworthiness,completeness, weights=records2, levels=0.95, add=T, center.pch=NULL, plot.points = F, col='blue'))

with(subset(dat, cex>=0.8 ), text(x=trustworthiness,y=completeness, 
                                  centre))
head(dat)
table(dat$cex)
par(mar=c(0,0,0,0))
plot(0,0,type="n",  ylab="", xlab="", main="", bty="n", xaxt="n", yaxt="n")
legend("center", pch=16,  col=c(transred, transblue), legend=c("EURINE-ACT", "no EURINE-ACT"),
       bty="n", pt.cex=5, cex=2, title="EURINE-ACT impact on ENSAT ACC", ncol=2)
