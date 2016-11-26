###### Settings
setwd("C:/Users/Jon/Desktop/Investment/CashFlowMonthly")
options(scipen=10)
library(plyr)
library(RColorBrewer)
blue.4<-brewer.pal(4,"Blues")
today<-gsub("-","",as.Date(Sys.time()))

###### Loading data
data<-read.csv("C:/Users/Jon/Desktop/CashFlow.csv")
data$Amount2<-data$Amount
data$Amount2[data$Category=="Costs"]<-data$Amount2[data$Category=="Costs"]*-1

###### Grouping by month
data2<-ddply(data,.(Month,Category),summarise,SUM=sum(Amount2))
data3<-ddply(data,.(Month),summarise,SUM=sum(Amount2))

###### Plotting basic cash flow
maxmax<-ceiling(max(data2$SUM)/2000)*2000
minmin<-floor(min(data2$SUM)/2000)*2000

lbls<-gsub(" ","",prettyNum(seq(minmin,maxmax,by=2000),big.mark=","))
lbls[grepl("-",lbls)]<-gsub("-","-$",lbls[grepl("-",lbls)])
lbls[!grepl("-",lbls)]<-paste0("$",lbls[!grepl("-",lbls)])

fileName<-paste0("MonthlyCashFlow_",today,".jpeg")

jpeg(file=fileName,quality=100,width=1200,height=800)
par(mar=c(6,5,5,5))
barx<-barplot(data2$SUM[data2$Category=="Income"],ylim=c(minmin,maxmax),yaxt="n",las=2,
	names.arg=data2$Month[data2$Category=="Income"],col="blue",
	main="Monthly Cash Flow Statement (including 401K)")
axis(2,at=seq(minmin,maxmax,by=2000),labels=lbls,las=2)
axis(4,at=seq(minmin,maxmax,by=2000),labels=lbls,las=2)
for (i in 1:length(barx))
	{rect(barx[i]-.5,data2$SUM[data2$Category=="Costs"][i],barx[i]+.5,0,col="red")}
for (i in 1:length(barx))
	{rect(barx[i]-.5,data3$SUM[i],barx[i]+.5,0,col="green")}
legend("topleft",c("Income","Costs","Cash Flow"),col=c("blue","red","green"),pch=15)
for (i in seq(minmin,maxmax,by=2000))
	{abline(h=i,lty=3,col="lightgray")}
dev.off()

###### Plotting income and outflow by source
income2<-data[data$Category=="Income",c("Month","Detail","Amount2")]
costs2<-data[data$Category=="Costs",]

income3<-reshape(income2,idvar="Month",timevar="Detail",direction="wide")
colnames(income3)<-gsub("Amount2.","",colnames(income3))
rownames(income3)<-income3$Month
income3<-as.matrix(t(income3[,-1]))

fileName2<-paste0("MonthlyCashFlowSources_",today,".jpeg")

jpeg(file=fileName2,quality=100,width=1200,height=800)
par(mar=c(6,5,5,5))
barx2<-barplot(income3,las=2,col=rev(blue.4),ylim=c(minmin,maxmax),yaxt="n",
	main="Monthly Cash Flow Statement by Source")
axis(2,at=seq(minmin,maxmax,by=2000),labels=lbls,las=2)
axis(4,at=seq(minmin,maxmax,by=2000),labels=lbls,las=2)
for (i in 1:length(barx2))
	{rect(barx[i]-.5,data2$SUM[data2$Category=="Costs"][i],barx[i]+.5,0,col="red")}
for (i in seq(minmin,maxmax,by=2000))
	{abline(h=i,lty=3,col="lightgray")}
legend("topleft",c(rownames(income3),"Costs"),col=c(rev(blue.4),"red"),pch=15)
dev.off()
















