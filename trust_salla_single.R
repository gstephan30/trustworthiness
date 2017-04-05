#single centre comparison
dat2<-read.csv(sep = ";", header = TRUE, file = "ACC_single_test.csv", dec = ",")
head(dat2)

somecols<-brewer.pal(5, "Set1")
red<-col2rgb(somecols[1])
blue<-col2rgb(somecols[2])

transred<-rgb(red=red["red",], green=red["green",], blue=red["blue",], maxColorValue=255, alpha=60) ## let's make them a little transparent so that the dots plotted on top of each other will show
transblue<-rgb(red=blue["red",], green=blue["green",], blue=blue["blue",], maxColorValue=255, alpha=60)
dat2$colors<-ifelse(dat2$EURINE==1, transred, transblue)


# legend wrong
# par(mar=c(5,5,1,0))
# plot(0,0,type="n", ylim=c(0,0.7), xlim=c(0,1), ylab="completeness in %", xlab="trustworthiness in %", 
#     main="", las=1, bty="n", yaxs="i", xaxs="i")
# points(dat2$trustworthiness,dat2$completeness, col=dat2$colors, pch=16) ## i want the maximum size of a plotting symbol to be 8

test_pmt_1<-lm(completeness~ trustworthiness, data=subset(dat2, EURINE==0))
test_pmt_2<-lm(completeness~ trustworthiness, data=subset(dat2, EURINE==1))
summary(test_pmt_1)
summary(test_pmt_2)

# par(mar=c(0,0,0,0))
# plot(0,0,type="n",  ylab="", xlab="", main="", bty="n", xaxt="n", yaxt="n")
# legend("center", pch=16,  col=c(transred, transblue), legend=c("EURINE-ACT", "no EURINE-ACT"),
#        bty="n", pt.cex=5, cex=2, title="EURINE-ACT impact on ENSAT ACC", ncol=2)



