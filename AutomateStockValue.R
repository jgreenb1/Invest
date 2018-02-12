###### Settings
library(XLConnect)
library(RColorBrewer)
options(scipen=10)
col.9<-brewer.pal(9,"Blues")

###### Date
today<-as.Date(substr(Sys.time(),0,10))
today1<-gsub("-","",today)

###### Loading data
data<-readWorksheet(loadWorkbook("C:/Users/Jon/Desktop/StockList2.xlsx"),sheet=1)

###### Create subdirectory
newdir<-paste0("C:/Users/Jon/Desktop/Investment/StockTracking/Images_",today1)
dir.create(newdir)
setwd(newdir)

###### Formatting data
data$Date<-as.Date(data$Date)
data<-data[!is.na(data$Date),]
last<-length(data$Date)
yearago<-which.min(abs(as.numeric(difftime(data$Date,today,units="days"))+365))
yearago1<-today-365

###### CAR
fileName<-paste0("CAR_",gsub("-","",today),".jpeg")

maxmax<-ceiling(max(data$CAR_Price*data$CAR_Shares)/2000)*2000
pur11<-2411
pur12<-2482
pur13<-3875
totpur<-prettyNum(2411+2482+3875,big.mark=",")
sale11<-6937
sale12<-4938
divi1<-paste0(round(100*(data$CAR_Dividends[max(which(data$CAR_Dividends>0),1)]*4/data$CAR_Shares[length(data$CAR_Shares)])/data$CAR_Price[length(data$CAR_Price)],2),"%")
return1<-paste0(round(100*((data$CAR_Price[last]*data$CAR_Shares[last]-pur11-pur12-pur13+sale11+sale12)/(pur11+pur12+pur13))/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[159],units="days"))/365),2),"%")
return1year1<-paste0(round(100*(data$CAR_Price[last]-data$CAR_Price[yearago])/data$CAR_Price[yearago],2),"%")
return1a<-return1
return1year1a<-return1year1

leg1<-c(paste0("Purchase 1 = $",prettyNum(pur11,big.mark=",")," (2/19/13)"),
	paste0("Purchase 2 = $",prettyNum(pur12,big.mark=",")," (10/10/13)"),
	paste0("Purchase 3 = $",prettyNum(pur13,big.mark=",")," (10/24/13)"),
	paste0("Total Purchases = $",totpur)," ",
	paste0("Sale 1 = $",prettyNum(sale11,big.mark=",")," (12/23/14)"),
	paste0("Sale 2 = $",prettyNum(sale12,big.mark=",")," (11/28/16)")," ",
	paste0("Appreciation = $",prettyNum(round(data$CAR_Price[last]*data$CAR_Shares[last]-pur11-pur12-pur13+sale11,0),big.mark=",")),
	"Dividends = $0"," ",
	paste0("Annual Return (Total) = ",return1a),
	paste0("Annual Return (Appreciation Only) = ",return1),
	paste0("Return (Total - 1 year) = ",return1year1a),
	paste0("Return (Appreciation Only - 1 year) = ",return1year1),"",
	paste0("Dividend Yield = ",divi1))

## Accounting for the sale
data$CAR_Value<-data$CAR_Shares*data$CAR_Price
data$CAR_Value1<-data$CAR_Value
data$CAR_Value1[data$Date>=as.Date("2014-12-23")]<-data$CAR_Value1[data$Date>=as.Date("2014-12-23")]+sale11
data$CAR_Value1[data$Date>=as.Date("2016-11-28")]<-data$CAR_Value1[data$Date>=as.Date("2016-11-28")]+sale12

jpeg(fileName,width=1200,height=800,quality=100)
par(mar=c(6,5,5,5))
plot(data$CAR_Value,type="l",lwd=3,col=col.9[4],ylim=c(0,maxmax),
	main="CAR Value",xaxt="n",xlab="",yaxt="n",ylab="")
lines(data$CAR_Value1,col="red")
axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
axis(2,at=seq(0,maxmax,by=2000),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax,by=2000),big.mark=","))),las=2)
axis(4,at=seq(0,maxmax,by=2000),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax,by=2000),big.mark=","))),las=2)
for (i in seq(0,maxmax,by=1000))
	{abline(h=i,lty=3,col="lightgray")}
legend("topleft",leg1,pch=15,col="white")
legend("topright",c("Current Stock Value","Stock Value + Sale Cash"),col=c(col.9[7],"red"),pch=15)
dev.off()

###### CVX
fileName2<-paste0("CVX_",gsub("-","",today),".jpeg")

maxmax2<-ceiling(max(data$CVX_Price*data$CVX_Shares)/2000)*2000
orig2<-13644.17
current2<-paste0("$",prettyNum(round(data$CVX_Price[last]*data$CVX_Shares[last],0),big.mark=","))
divi2<-paste0(round(100*data$CVX_Dividends[max(which(data$CVX_Dividends>0))]*4/(data$CVX_Shares[last]*data$CVX_Price[last]),2),"%")
return2<-paste0(round(100*((data$CVX_Price[length(data$CVX_Price)]-data$CVX_Price[1])/data$CVX_Price[1])/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year2<-paste0(round(100*(data$CVX_Price[last]-data$CVX_Price[yearago])/data$CVX_Price[yearago],2),"%")
return2a<-paste0(round((100*(data$CVX_Price[last]*data$CVX_Shares[last]-orig2)/orig2)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year2a<-paste0(round(100*(data$CVX_Price[last]*data$CVX_Shares[last]-data$CVX_Price[yearago]*data$CVX_Shares[yearago])/(data$CVX_Price[yearago]*data$CVX_Shares[yearago]),2),"%")

leg2<-c(paste0("Original Value = $",prettyNum(round(orig2,0),big.mark=",")),
	paste0("Current Value = ",current2),"",
	paste0("Appreciation = $",prettyNum(round(data$CVX_Price[last]*data$CVX_Shares[last]-orig2-sum(data$CVX_Dividends),0),big.mark=",")),
	paste0("Dividends = $",prettyNum(round(sum(data$CVX_Dividends),0),big.mark=","))," ",
	paste0("Annual Return (Total) = ",return2a),
	paste0("Annual Return (Appreciation Only) = ",return2),
	paste0("Return (Total - 1 year) = ",return1year2a),
	paste0("Return (Appreciation Only - 1 year) = ",return1year2),"",
	paste0("Current Dividend Yield = ",divi2))

jpeg(fileName2,width=1200,height=800,quality=100)
par(mar=c(6,5,5,5))
plot(data$CVX_Price*data$CVX_Shares,type="l",lwd=3,col=col.9[5],ylim=c(0,maxmax2),
	main="CVX Value",xaxt="n",xlab="",yaxt="n",ylab="")
axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
axis(2,at=seq(0,maxmax2,by=2000),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax2,by=2000),big.mark=","))),las=2)
axis(4,at=seq(0,maxmax2,by=2000),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax2,by=2000),big.mark=","))),las=2)
for (i in seq(0,maxmax2,by=1000))
	{abline(h=i,lty=3,col="lightgray")}
legend("topleft",leg2,pch=15,col="white")
dev.off()

###### PEP
fileName3<-paste0("PEP_",gsub("-","",today),".jpeg")

maxmax3<-ceiling(max(data$PEP_Price*data$PEP_Shares)/2000)*2000
orig3<-12073.06
current3<-paste0("$",prettyNum(round(data$PEP_Price[last]*data$PEP_Shares[last],0),big.mark=","))
divi3<-paste0(round(100*(data$PEP_Dividends[max(which(data$PEP_Dividends>0))]*4/data$PEP_Shares[length(data$PEP_Shares)])/data$PEP_Price[length(data$PEP_Price)],2),"%")
return3<-paste0(round(100*((data$PEP_Price[length(data$PEP_Price)]-data$PEP_Price[1])/data$PEP_Price[1])/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year3<-paste0(round(100*(data$PEP_Price[last]-data$PEP_Price[yearago])/data$PEP_Price[yearago],2),"%")
return3a<-paste0(round((100*(data$PEP_Price[last]*data$PEP_Shares[last]-orig3)/orig3)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year3a<-paste0(round(100*(data$PEP_Price[last]*data$PEP_Shares[last]-data$PEP_Price[yearago]*data$PEP_Shares[yearago])/(data$PEP_Price[yearago]*data$PEP_Shares[yearago]),2),"%")

leg3<-c(paste0("Original Value = $",prettyNum(round(orig3,0),big.mark=",")),
	paste0("Current Value = ",current3),"",
	paste0("Appreciation = $",prettyNum(round(data$PEP_Price[last]*data$PEP_Shares[last]-orig3-sum(data$PEP_Dividends),0),big.mark=",")),
	paste0("Dividends = $",prettyNum(round(sum(data$PEP_Dividends),0),big.mark=","))," ",
	paste0("Annual Return (Total) = ",return3a),
	paste0("Annual Return (Appreciation Only) = ",return3),
	paste0("Return (Total - 1 year) = ",return1year3a),
	paste0("Return (Appreciation Only - 1 year) = ",return1year3),"",
	paste0("Current Dividend Yield = ",divi3))

jpeg(fileName3,width=1200,height=800,quality=100)
par(mar=c(6,5,5,5))
plot(data$PEP_Price*data$PEP_Shares,type="l",lwd=3,col=col.9[6],ylim=c(0,maxmax3),
	main="PEP Value",xaxt="n",xlab="",yaxt="n",ylab="")
axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
axis(2,at=seq(0,maxmax3,by=2000),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax3,by=2000),big.mark=","))),las=2)
axis(4,at=seq(0,maxmax3,by=2000),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax3,by=2000),big.mark=","))),las=2)
for (i in seq(0,maxmax3,by=1000))
	{abline(h=i,lty=3,col="lightgray")}
legend("topleft",leg3,pch=15,col="white")
dev.off()

###### MCD
fileName4<-paste0("MCD_",gsub("-","",today),".jpeg")

maxmax4<-ceiling(max(data$MCD_Price*data$MCD_Shares)/2000)*2000
orig4<-10378.77
current4<-paste0("$",prettyNum(round(data$MCD_Price[last]*data$MCD_Shares[last],0),big.mark=","))
divi4<-paste0(round(100*(data$MCD_Dividends[max(which(data$MCD_Dividends>0))]*4/data$MCD_Shares[length(data$MCD_Shares)])/data$MCD_Price[length(data$MCD_Price)],2),"%")
return4<-paste0(round(100*((data$MCD_Price[length(data$MCD_Price)]-data$MCD_Price[1])/data$MCD_Price[1])/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year4<-paste0(round(100*(data$MCD_Price[last]-data$MCD_Price[yearago])/data$MCD_Price[yearago],2),"%")
return4a<-paste0(round((100*(data$MCD_Price[last]*data$MCD_Shares[last]-orig4)/orig4)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year4a<-paste0(round(100*(data$MCD_Price[last]*data$MCD_Shares[last]-data$MCD_Price[yearago]*data$MCD_Shares[yearago])/(data$MCD_Price[yearago]*data$MCD_Shares[yearago]),2),"%")

leg4<-c(paste0("Original Value = $",prettyNum(round(orig4,0),big.mark=",")),
	paste0("Current Value = ",current4),"",
	paste0("Appreciation = $",prettyNum(round(data$MCD_Price[last]*data$MCD_Shares[last]-orig4-sum(data$MCD_Dividends),2),big.mark=",")),
	paste0("Dividends = $",prettyNum(round(sum(data$MCD_Dividends),0),big.mark=","))," ",
	paste0("Annual Return (Total) = ",return4a),
	paste0("Annual Return (Appreciation Only) = ",return4),
	paste0("Return (Total - 1 year) = ",return1year4a),
	paste0("Return (Appreciation Only - 1 year) = ",return1year4),"",
	paste0("Current Dividend Yield = ",divi4))

jpeg(fileName4,width=1200,height=800,quality=100)
par(mar=c(6,5,5,5))
plot(data$MCD_Price*data$MCD_Shares,type="l",lwd=3,col=col.9[6],ylim=c(0,maxmax4),
	main="MCD Value",xaxt="n",xlab="",yaxt="n",ylab="")
axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
axis(2,at=seq(0,maxmax4,by=2000),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax4,by=2000),big.mark=","))),las=2)
axis(4,at=seq(0,maxmax4,by=2000),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax4,by=2000),big.mark=","))),las=2)
for (i in seq(0,maxmax4,by=1000))
	{abline(h=i,lty=3,col="lightgray")}
legend("topleft",leg4,pch=15,col="white")
dev.off()

###### XOM
fileName5<-paste0("XOM_",gsub("-","",today),".jpeg")

maxmax5<-ceiling(max(data$XOM_Price*data$XOM_Shares)/2000)*2000
orig5<-6300.76
current5<-paste0("$",prettyNum(round(data$XOM_Price[last]*data$XOM_Shares[last],0),big.mark=","))
divi5<-paste0(round(100*(data$XOM_Dividends[max(which(data$XOM_Dividends>0))]*4/data$XOM_Shares[length(data$XOM_Shares)])/data$XOM_Price[length(data$XOM_Price)],2),"%")
return5<-paste0(round(100*((data$XOM_Price[length(data$XOM_Price)]-data$XOM_Price[1])/data$XOM_Price[1])/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year5<-paste0(round(100*(data$XOM_Price[last]-data$XOM_Price[yearago])/data$XOM_Price[yearago],2),"%")
return5a<-paste0(round((100*(data$XOM_Price[last]*data$XOM_Shares[last]-orig5)/orig5)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year5a<-paste0(round(100*(data$XOM_Price[last]*data$XOM_Shares[last]-data$XOM_Price[yearago]*data$XOM_Shares[yearago])/(data$XOM_Price[yearago]*data$XOM_Shares[yearago]),2),"%")

leg5<-c(paste0("Original Value = $",prettyNum(round(orig5,0),big.mark=",")),
	paste0("Current Value = ",current5),"",
	paste0("Appreciation = $",prettyNum(round(data$XOM_Price[last]*data$XOM_Shares[last]-orig5-sum(data$XOM_Dividends),0),big.mark=",")),
	paste0("Dividends = $",prettyNum(round(sum(data$XOM_Dividends),0),big.mark=",")),"",
	paste0("Annual Return (Total) = ",return5a),
	paste0("Annual Return (Appreciation Only) = ",return5),
	paste0("Return (Total - 1 year) = ",return1year5a),
	paste0("Return (Appreciation Only - 1 year) = ",return1year5),"",
	paste0("Dividend Yield = ",divi5))

jpeg(fileName5,width=1200,height=800,quality=100)
par(mar=c(6,5,5,5))
plot(data$XOM_Price*data$XOM_Shares,type="l",lwd=3,col=col.9[7],ylim=c(0,maxmax5),
	main="XOM Value",xaxt="n",xlab="",yaxt="n",ylab="")
axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
axis(2,at=seq(0,maxmax5,by=2000),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax5,by=2000),big.mark=","))),las=2)
axis(4,at=seq(0,maxmax5,by=2000),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax5,by=2000),big.mark=","))),las=2)
for (i in seq(0,maxmax5,by=1000))
	{abline(h=i,lty=3,col="lightgray")}
legend("topleft",leg5,pch=15,col="white")
dev.off()

###### MSFT
fileName6<-paste0("MSFT_",gsub("-","",today),".jpeg")

maxmax6<-ceiling(max(data$MSFT_Price*data$MSFT_Shares)/500)*500
orig6<-1800.45
current6<-paste0("$",prettyNum(round(data$MSFT_Price[last]*data$MSFT_Shares[last],0),big.mark=","))
divi6<-paste0(round(100*(data$MSFT_Dividends[max(which(data$MSFT_Dividends>0))]*4/data$MSFT_Shares[length(data$MSFT_Shares)])/data$MSFT_Price[length(data$MSFT_Price)],2),"%")
return6<-paste0(round(100*((data$MSFT_Price[length(data$MSFT_Price)]-data$MSFT_Price[1])/data$MSFT_Price[1])/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year6<-paste0(round(100*(data$MSFT_Price[last]-data$MSFT_Price[yearago])/data$MSFT_Price[yearago],2),"%")
return6a<-paste0(round((100*(data$MSFT_Price[last]*data$MSFT_Shares[last]-orig6)/orig6)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year6a<-paste0(round(100*(data$MSFT_Price[last]*data$MSFT_Shares[last]-data$MSFT_Price[yearago]*data$MSFT_Shares[yearago])/(data$MSFT_Price[yearago]*data$MSFT_Shares[yearago]),2),"%")

leg6<-c(paste0("Original Value = $",prettyNum(round(orig6,0),big.mark=",")),
	paste0("Current Value = ",current6),"",
	paste0("Appreciation = $",prettyNum(round(data$MSFT_Price[last]*data$MSFT_Shares[last]-orig6-sum(data$MSFT_Dividends),0),big.mark=",")),
	paste0("Dividends = $",round(sum(data$MSFT_Dividends),0))," ",
	paste0("Annual Return (Total) = ",return6a),
	paste0("Annual Return (Appreciation Only) = ",return6),
	paste0("Return (Total - 1 year) = ",return1year6a),
	paste0("Return (Appreciation Only - 1 year) = ",return1year6),"",
	paste0("Dividend Yield = ",divi6))

jpeg(fileName6,width=1200,height=800,quality=100)
par(mar=c(6,5,5,5))
plot(data$MSFT_Price*data$MSFT_Shares,type="l",lwd=3,col=col.9[8],ylim=c(0,maxmax6),
	main="MSFT Value",xaxt="n",xlab="",yaxt="n",ylab="")
axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
axis(2,at=seq(0,maxmax6,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax6,by=500),big.mark=","))),las=2)
axis(4,at=seq(0,maxmax6,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax6,by=500),big.mark=","))),las=2)
for (i in seq(0,maxmax6,by=250))
	{abline(h=i,lty=3,col="lightgray")}
legend("topleft",leg6,pch=15,col="white")
dev.off()

###### BP
fileName7<-paste0("BP_",gsub("-","",today),".jpeg")

maxmax7<-ceiling(max(data$BP_Price*data$BP_Shares)/500)*500
orig7<-1038.82
current7<-paste0("$",prettyNum(round(data$BP_Price[last]*data$BP_Shares[last],0),big.mark=","))
divi7<-paste0(round(100*(data$BP_Dividends[max(which(data$BP_Dividends>0))]*4/data$BP_Shares[length(data$BP_Shares)])/data$BP_Price[length(data$BP_Price)],2),"%")
return7<-paste0(round(100*((data$BP_Price[length(data$BP_Price)]-data$BP_Price[1])/data$BP_Price[1])/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year7<-paste0(round(100*(data$BP_Price[last]-data$BP_Price[yearago])/data$BP_Price[yearago],2),"%")
return7a<-paste0(round((100*(data$BP_Price[last]*data$BP_Shares[last]-orig7)/orig7)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year7a<-paste0(round(100*(data$BP_Price[last]*data$BP_Shares[last]-data$BP_Price[yearago]*data$BP_Shares[yearago])/(data$BP_Price[yearago]*data$BP_Shares[yearago]),2),"%")

leg7<-c(paste0("Original Value = $",prettyNum(round(orig7,0),big.mark=",")),
	paste0("Current Value = ",current7),"",
	paste0("Appreciation = $",round(data$BP_Price[last]*data$BP_Shares[last]-orig7-sum(data$BP_Dividends),0)),
	paste0("Dividends = $",round(sum(data$BP_Dividends),0))," ",
	paste0("Annual Return (Total) = ",return7a),
	paste0("Annual Return (Appreciation Only) = ",return7),
	paste0("Return (Total - 1 year) = ",return1year7a),
	paste0("Return (Appreciation Only - 1 year) = ",return1year7),"",
	paste0("Dividend Yield = ",divi7))

jpeg(fileName7,width=1200,height=800,quality=100)
par(mar=c(6,5,5,5))
plot(data$BP_Price*data$BP_Shares,type="l",lwd=3,col=col.9[8],ylim=c(0,maxmax7),
	main="BP Value",xaxt="n",xlab="",yaxt="n",ylab="")
axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
axis(2,at=seq(0,maxmax7,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax7,by=500),big.mark=","))),las=2)
axis(4,at=seq(0,maxmax7,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax7,by=500),big.mark=","))),las=2)
for (i in seq(0,maxmax7,by=250))
	{abline(h=i,lty=3,col="lightgray")}
legend("topleft",leg7,pch=15,col="white")
dev.off()

###### GE
fileName8<-paste0("GE_",gsub("-","",today),".jpeg")

maxmax8<-ceiling(max(data$GE_Price*data$GE_Shares)/500)*500
orig8<-1671.87
current8<-paste0("$",prettyNum(round(data$GE_Price[last]*data$GE_Shares[last],0),big.mark=","))
divi8<-paste0(round(100*(data$GE_Dividends[max(which(data$GE_Dividends>0))]*4/data$GE_Shares[length(data$GE_Shares)])/data$GE_Price[length(data$GE_Price)],2),"%")
return8<-paste0(round(100*((data$GE_Price[length(data$GE_Price)]-data$GE_Price[1])/data$GE_Price[1])/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year8<-paste0(round(100*(data$GE_Price[last]-data$GE_Price[yearago])/data$GE_Price[yearago],2),"%")
return8a<-paste0(round((100*(data$GE_Price[last]*data$GE_Shares[last]-orig8)/orig8)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year8a<-paste0(round(100*(data$GE_Price[last]*data$GE_Shares[last]-data$GE_Price[yearago]*data$GE_Shares[yearago])/(data$GE_Price[yearago]*data$GE_Shares[yearago]),2),"%")

leg8<-c(paste0("Original Value = $",prettyNum(round(orig8,0),big.mark=",")),
	paste0("Current Value = ",current8),"",
	paste0("Appreciation = $",prettyNum(round(data$GE_Price[last]*data$GE_Shares[last]-orig8-sum(data$GE_Dividends),0),big.mark=",")),
	paste0("Dividends = $",round(sum(data$GE_Dividends),0))," ",
	paste0("Annual Return (Total) = ",return8a),
	paste0("Annual Return (Appreciation Only) = ",return8),
	paste0("Return (Total - 1 year) = ",return1year8a),
	paste0("Return (Appreciation Only - 1 year) = ",return1year8),"",
	paste0("Dividend Yield = ",divi8))

jpeg(fileName8,width=1200,height=800,quality=100)
par(mar=c(6,5,5,5))
plot(data$GE_Price*data$GE_Shares,type="l",lwd=3,col=col.9[9],ylim=c(0,maxmax8),
	main="GE Value",xaxt="n",xlab="",yaxt="n",ylab="")
axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
axis(2,at=seq(0,maxmax8,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax8,by=500),big.mark=","))),las=2)
axis(4,at=seq(0,maxmax8,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax8,by=500),big.mark=","))),las=2)
for (i in seq(0,maxmax8,by=250))
	{abline(h=i,lty=3,col="lightgray")}
legend("topleft",leg8,pch=15,col="white")
dev.off()

###### CSCO
fileName9<-paste0("CSCO_",gsub("-","",today),".jpeg")

maxmax9<-ceiling(max(data$CSCO_Price*data$CSCO_Shares)/500)*500
orig9<-2394
current9<-paste0("$",prettyNum(round(data$CSCO_Price[last]*data$CSCO_Shares[last],0),big.mark=","))
divi9<-paste0(round(100*(data$CSCO_Dividends[max(which(data$CSCO_Dividends>0))]*4/data$CSCO_Shares[length(data$CSCO_Shares)])/data$CSCO_Price[length(data$CSCO_Price)],2),"%")
return9<-paste0(round(100*((data$CSCO_Price[length(data$CSCO_Price)]-data$CSCO_Price[1])/data$CSCO_Price[1])/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year9<-paste0(round(100*(data$CSCO_Price[last]-data$CSCO_Price[yearago])/data$CSCO_Price[yearago],2),"%")
return9a<-paste0(round((100*(data$CSCO_Price[last]*data$CSCO_Shares[last]-orig9)/orig9)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year9a<-paste0(round(100*(data$CSCO_Price[last]*data$CSCO_Shares[last]-data$CSCO_Price[yearago]*data$CSCO_Shares[yearago])/(data$CSCO_Price[yearago]*data$CSCO_Shares[yearago]),2),"%")

leg9<-c(paste0("Original Value = $",prettyNum(round(orig9,0),big.mark=",")),
	paste0("Current Value = ",current9),"",
	paste0("Appreciation = $",round(data$CSCO_Price[last]*data$CSCO_Shares[last]-orig9-sum(data$CSCO_Dividends),0)),
	paste0("Dividends = $",round(sum(data$CSCO_Dividends),0))," ",
	paste0("Annual Return (Total) = ",return9a),
	paste0("Annual Return (Appreciation Only) = ",return9),
	paste0("Return (Total - 1 year) = ",return1year9a),
	paste0("Return (Appreciation Only - 1 year) = ",return1year9),"",
	paste0("Dividend Yield = ",divi9))

jpeg(fileName9,width=1200,height=800,quality=100)
par(mar=c(6,5,5,5))
plot(data$CSCO_Price*data$CSCO_Shares,type="l",lwd=3,col=col.9[3],ylim=c(0,maxmax9),
	main="CSCO Value",xaxt="n",xlab="",yaxt="n",ylab="")
axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
axis(2,at=seq(0,maxmax9,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax9,by=500),big.mark=","))),las=2)
axis(4,at=seq(0,maxmax9,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax9,by=500),big.mark=","))),las=2)
for (i in seq(0,maxmax9,by=250))
	{abline(h=i,lty=3,col="lightgray")}
legend("topleft",leg9,pch=15,col="white")
dev.off()

###### BAC
fileName10<-paste0("BAC_",gsub("-","",today),".jpeg")

maxmax10<-ceiling(max(data$BAC_Price*data$BAC_Shares)/500)*500
orig10<-978.9
current10<-paste0("$",prettyNum(round(data$BAC_Price[last]*data$BAC_Shares[last],0),big.mark=","))
divi10<-paste0(round(100*(data$BAC_Dividends[max(which(data$BAC_Dividends>0))]*4/data$BAC_Shares[length(data$BAC_Shares)])/data$BAC_Price[length(data$BAC_Price)],2),"%")
return10<-paste0(round(100*((data$BAC_Price[length(data$BAC_Price)]-data$BAC_Price[1])/data$BAC_Price[1])/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year10<-paste0(round(100*(data$BAC_Price[last]-data$BAC_Price[yearago])/data$BAC_Price[yearago],2),"%")
return10a<-paste0(round((100*(data$BAC_Price[last]*data$BAC_Shares[last]-orig10)/orig10)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year10a<-paste0(round(100*(data$BAC_Price[last]*data$BAC_Shares[last]-data$BAC_Price[yearago]*data$BAC_Shares[yearago])/(data$BAC_Price[yearago]*data$BAC_Shares[yearago]),2),"%")

leg10<-c(paste0("Original Value = $",prettyNum(round(orig10,0),big.mark=",")),
	paste0("Current Value = ",current10),"",
	paste0("Appreciation = $",round(data$BAC_Price[last]*data$BAC_Shares[last]-orig10-sum(data$BAC_Dividends),0)),
	paste0("Dividends = $",round(sum(data$BAC_Dividends),0))," ",
	paste0("Annual Return (Total) = ",return10a),
	paste0("Annual Return (Appreciation Only) = ",return10),
	paste0("Return (Total - 1 year) = ",return1year10a),
	paste0("Return (Appreciation Only - 1 year) = ",return1year10),"",
	paste0("Dividend Yield = ",divi10))

jpeg(fileName10,width=1200,height=800,quality=100)
par(mar=c(6,5,5,5))
plot(data$BAC_Price*data$BAC_Shares,type="l",lwd=3,col=col.9[4],ylim=c(0,maxmax10),
	main="BAC Value",xaxt="n",xlab="",yaxt="n",ylab="")
axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
axis(2,at=seq(0,maxmax10,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax10,by=500),big.mark=","))),las=2)
axis(4,at=seq(0,maxmax10,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax10,by=500),big.mark=","))),las=2)
for (i in seq(0,maxmax10,by=250))
	{abline(h=i,lty=3,col="lightgray")}
legend("topleft",leg10,pch=15,col="white")
dev.off()

###### HPQ
fileName11<-paste0("HPQ_",gsub("-","",today),".jpeg")

maxmax11<-ceiling(max(data$HPQ_Price*data$HPQ_Shares)/500)*500
orig11<-1163.1
current11<-paste0("$",prettyNum(round(data$HPQ_Price[last]*data$HPQ_Shares[last],0),big.mark=","))
divi11<-paste0(round(100*(data$HPQ_Dividends[max(which(data$HPQ_Dividends>0))]*4/data$HPQ_Shares[length(data$HPQ_Shares)])/data$HPQ_Price[length(data$HPQ_Price)],2),"%")
return11<-paste0(round(100*((data$HPQ_Price[length(data$HPQ_Price)]-data$HPQ_Price[1])/data$HPQ_Price[1])/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year11<-paste0(round(100*(data$HPQ_Price[last]-data$HPQ_Price[yearago])/data$HPQ_Price[yearago],2),"%")
return11a<-paste0(round((100*(data$HPQ_Price[last]*data$HPQ_Shares[last]-orig11)/orig11)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year11a<-paste0(round(100*(data$HPQ_Price[last]*data$HPQ_Shares[last]-data$HPQ_Price[yearago]*data$HPQ_Shares[yearago])/(data$HPQ_Price[yearago]*data$HPQ_Shares[yearago]),2),"%")

leg11<-c(paste0("Original Value = $",prettyNum(round(orig11,0),big.mark=",")),
	paste0("Current Value = ",current11),"",
	paste0("Appreciation = $",round(data$HPQ_Price[last]*data$HPQ_Shares[last]-orig11-sum(data$HPQ_Dividends),0)),
	paste0("Dividends = $",round(sum(data$HPQ_Dividends),0))," ",
	paste0("Annual Return (Total) = ",return11a),
	paste0("Annual Return (Appreciation Only) = ",return11),
	paste0("Return (Total - 1 year) = ",return1year11a),
	paste0("Return (Appreciation Only - 1 year) = ",return1year11),"",
	paste0("Dividend Yield = ",divi11))

jpeg(fileName11,width=1200,height=800,quality=100)
par(mar=c(6,5,5,5))
plot(data$HPQ_Price*data$HPQ_Shares,type="l",lwd=3,col=col.9[5],ylim=c(0,maxmax11),
	main="HPQ Value",xaxt="n",xlab="",yaxt="n",ylab="")
axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
axis(2,at=seq(0,maxmax11,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax11,by=500),big.mark=","))),las=2)
axis(4,at=seq(0,maxmax11,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax11,by=500),big.mark=","))),las=2)
for (i in seq(0,maxmax11,by=250))
	{abline(h=i,lty=3,col="lightgray")}
legend("topleft",leg11,pch=15,col="white")
dev.off()

###### A
fileName12<-paste0("A_",gsub("-","",today),".jpeg")

maxmax12<-ceiling(max(data$A_Price*data$A_Shares)/50)*50
orig12<-93.21
current12<-paste0("$",prettyNum(round(data$A_Price[last]*data$A_Shares[last],0),big.mark=","))
divi12<-paste0(round(100*(data$A_Dividends[max(which(data$A_Dividends>0))]*4/data$A_Shares[length(data$A_Shares)])/data$A_Price[length(data$A_Price)],2),"%")
return12<-paste0(round(100*((data$A_Price[length(data$A_Price)]-data$A_Price[1])/data$A_Price[1])/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year12<-paste0(round(100*(data$A_Price[last]-data$A_Price[yearago])/data$A_Price[yearago],2),"%")
return12a<-paste0(round((100*(data$A_Price[last]*data$A_Shares[last]-orig12)/orig12)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year12a<-paste0(round(100*(data$A_Price[last]*data$A_Shares[last]-data$A_Price[yearago]*data$A_Shares[yearago])/(data$A_Price[yearago]*data$A_Shares[yearago]),2),"%")

leg12<-c(paste0("Original Value = $",prettyNum(round(orig12,0),big.mark=",")),
	paste0("Current Value = ",current12),"",
	paste0("Appreciation = $",round(data$A_Price[last]*data$A_Shares[last]-orig12-sum(data$A_Dividends),0)),
	paste0("Dividends = $",round(sum(data$A_Dividends),0))," ",
	paste0("Annual Return (Total) = ",return12a),
	paste0("Annual Return (Appreciation Only) = ",return12),
	paste0("Return (Total - 1 year) = ",return1year12a),
	paste0("Return (Appreciation Only - 1 year) = ",return1year12),"",
	paste0("Dividend Yield = ",divi12))

jpeg(fileName12,width=1200,height=800,quality=100)
par(mar=c(6,5,5,5))
plot(data$A_Price*data$A_Shares,type="l",lwd=3,col=col.9[6],ylim=c(0,maxmax12),
	main="A Value",xaxt="n",xlab="",yaxt="n",ylab="")
axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
axis(2,at=seq(0,maxmax12,by=50),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax12,by=50),big.mark=","))),las=2)
axis(4,at=seq(0,maxmax12,by=50),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax12,by=50),big.mark=","))),las=2)
for (i in seq(0,maxmax12,by=25))
	{abline(h=i,lty=3,col="lightgray")}
legend("topleft",leg12,pch=15,col="white")
dev.off()

###### PRO
#fileName13<-paste0("PRO_",gsub("-","",today),".jpeg")

#maxmax13<-ceiling(max(data$PRO_Price*data$PRO_Shares)/500)*500
#orig13<-1756
#current13<-paste0("$",prettyNum(round(data$PRO_Price[last]*data$PRO_Shares[last],0),big.mark=","))
#sale1<-1721
#divi13<-paste0(round(100*(data$PRO_Dividends[max(which(data$PRO_Dividends>0),1)]*4/data$PRO_Shares[length(data$PRO_Shares)])/data$PRO_Price[length(data$PRO_Price)],2),"%")
#return13<-paste0(round(100*((data$PRO_Price[last]*data$PRO_Shares[last]+sale1-orig13)/(data$PRO_Price[77]*data$PRO_Shares[77]))/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[77],units="days"))/365),2),"%")
#return1year13<-paste0(round(100*(data$PRO_Price[last]-data$PRO_Price[yearago])/data$PRO_Price[yearago],2),"%")
#return13a<-return13
#return1year13a<-return1year13

#leg13<-c(paste0("Original Value = $",prettyNum(round(orig13,0),big.mark=",")),"",paste0("Sale 1 = $",prettyNum(sale1,big.mark=",")),
#	paste0("Appreciation = $",round(1720.96-1756+data$PRO_Price[last]*data$PRO_Shares[last]-sum(data$PRO_Dividends),0)),
#	paste0("Dividends = $",round(sum(data$PRO_Dividends),2))," ",
#	paste0("Annual Return (Total) = ",return13a),
#	paste0("Annual Return (Appreciation Only) = ",return13),
#	paste0("Return (Total - 1 year) = ",return1year13a),
#	paste0("Return (Appreciation Only - 1 year) = ",return1year13),"",
#	paste0("Dividend Yield = ",divi13))

## Accounting for the sale
#data$PRO_Value<-data$PRO_Shares*data$PRO_Price
#data$PRO_Value1<-data$PRO_Value
#data$PRO_Value1[data$Date>=as.Date("2013-03-24")]<-data$PRO_Value1[data$Date>=as.Date("2013-03-24")]+sale1

#jpeg(fileName13,width=1200,height=800,quality=100)
#par(mar=c(6,5,5,5))
#plot(data$PRO_Value,type="l",lwd=3,col=col.9[7],ylim=c(0,maxmax13),
#	main="PRO Value",xaxt="n",xlab="",yaxt="n",ylab="")
#lines(data$PRO_Value1,col="red")
#axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
#axis(2,at=seq(0,maxmax13,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax13,by=500),big.mark=","))),las=2)
#axis(4,at=seq(0,maxmax13,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax13,by=500),big.mark=","))),las=2)
#for (i in seq(0,maxmax13,by=250))
#	{abline(h=i,lty=3,col="lightgray")}
#legend("topleft",leg13,pch=15,col="white")
#legend("topright",c("Current Stock Value","Stock Value + Sale Cash"),col=c(col.9[7],"red"),pch=15)
#dev.off()

###### C
fileName14<-paste0("C_",gsub("-","",today),".jpeg")

maxmax14<-ceiling(max(data$C_Price*data$C_Shares)/500)*500
orig14<-1292.88
current14<-paste0("$",prettyNum(round(data$C_Price[last]*data$C_Shares[last],0),big.mark=","))
divi14<-paste0(round(100*(data$C_Dividends[max(which(data$C_Dividends>0))]*4/data$C_Shares[length(data$C_Shares)])/data$C_Price[length(data$C_Price)],2),"%")
return14<-paste0(round(100*((data$C_Price[length(data$C_Price)]-data$C_Price[1])/data$C_Price[1])/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year14<-paste0(round(100*(data$C_Price[last]-data$C_Price[yearago])/data$C_Price[yearago],2),"%")
return14a<-paste0(round((100*(data$C_Price[last]*data$C_Shares[last]-orig14)/orig14)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[1],units="days"))/365),2),"%")
return1year14a<-paste0(round(100*(data$C_Price[last]*data$C_Shares[last]-data$C_Price[yearago]*data$C_Shares[yearago])/(data$C_Price[yearago]*data$C_Shares[yearago]),2),"%")

leg14<-c(paste0("Original Value = $",prettyNum(round(orig14,0),big.mark=",")),
	paste0("Current Value = ",current14),"",
	paste0("Appreciation = $",round(data$C_Price[last]*data$C_Shares[last]-orig14-sum(data$C_Dividends),0)),
	paste0("Dividends = $",round(sum(data$C_Dividends),0))," ",
	paste0("Annual Return (Total) = ",return14a),
	paste0("Annual Return (Appreciation Only) = ",return14),
	paste0("Return (Total - 1 year) = ",return1year14a),
	paste0("Return (Appreciation Only - 1 year) = ",return1year14),"",
	paste0("Dividend Yield = ",divi14))

jpeg(fileName14,width=1200,height=800,quality=100)
par(mar=c(6,5,5,5))
plot(data$C_Price*data$C_Shares,type="l",lwd=3,col=col.9[8],ylim=c(0,maxmax14),
	main="C Value",xaxt="n",xlab="",yaxt="n",ylab="")
axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
axis(2,at=seq(0,maxmax14,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax14,by=500),big.mark=","))),las=2)
axis(4,at=seq(0,maxmax14,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax14,by=500),big.mark=","))),las=2)
for (i in seq(0,maxmax14,by=250))
	{abline(h=i,lty=3,col="lightgray")}
legend("topleft",leg14,pch=15,col="white")
dev.off()

###### CEO
fileName15<-paste0("CEO_",gsub("-","",today),".jpeg")

maxmax15<-ceiling(max(data$CEO_Price*data$CEO_Shares)/200)*200
orig15<-1345.6
current15<-paste0("$",prettyNum(round(data$CEO_Price[last]*data$CEO_Shares[last],0),big.mark=","))
divi15<-paste0(round(100*sum(data$CEO_Dividends[data$Date>=yearago1])/(data$CEO_Price[last]*data$CEO_Shares[last]),2),"%")
return15<-paste0(round(100*((data$CEO_Price[length(data$CEO_Price)]-data$CEO_Price[248])/data$CEO_Price[248])/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[248],units="days"))/365),2),"%")
return1year15<-paste0(round(100*(data$CEO_Price[last]-data$CEO_Price[yearago])/data$CEO_Price[yearago],2),"%")
return15a<-paste0(round((100*(data$CEO_Price[last]*data$CEO_Shares[last]+sum(data$CEO_Dividends)-orig15)/orig15)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[248],units="days"))/365),2),"%")
return1year15a<-paste0(round(100*(data$CEO_Price[last]*data$CEO_Shares[last]+sum(data$CEO_Dividends[data$Date>=yearago1])-data$CEO_Price[yearago]*data$CEO_Shares[yearago])/(data$CEO_Price[yearago]*data$CEO_Shares[yearago]),2),"%")

leg15<-c(paste0("Original Value = $",prettyNum(round(orig15,0),big.mark=",")),
	paste0("Current Value = ",current15),"",
	paste0("Appreciation = $",round(data$CEO_Price[last]*data$CEO_Shares[last]-orig15,0)),
	paste0("Dividends = $",round(sum(data$CEO_Dividends),0))," ",
	paste0("Annual Return (Total) = ",return15a),
	paste0("Annual Return (Appreciation Only) = ",return15),
	paste0("Return (Total - 1 year) = ",return1year15a),
	paste0("Return (Appreciation Only - 1 year) = ",return1year15),"",
	paste0("Dividend Yield = ",divi15))

jpeg(fileName15,width=1200,height=800,quality=100)
par(mar=c(6,5,5,5))
plot(data$CEO_Price*data$CEO_Shares,type="l",lwd=3,col=col.9[5],ylim=c(0,maxmax15),
	main="CEO Value",xaxt="n",xlab="",yaxt="n",ylab="")
axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
axis(2,at=seq(0,maxmax15,by=200),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax15,by=200),big.mark=","))),las=2)
axis(4,at=seq(0,maxmax15,by=200),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax15,by=200),big.mark=","))),las=2)
for (i in seq(0,maxmax15,by=100))
	{abline(h=i,lty=3,col="lightgray")}
legend("topleft",leg15,pch=15,col="white")
dev.off()

###### GZPFY
fileName16<-paste0("GZPFY_",gsub("-","",today),".jpeg")

maxmax16<-ceiling(max(data$GZPFY_Price*data$GZPFY_Shares)/200)*200
orig16<-904.88
current16<-paste0("$",prettyNum(round(data$GZPFY_Price[last]*data$GZPFY_Shares[last],0),big.mark=","))
divi16<-paste0(round(100*sum(data$GZPFY_Dividends[data$Date>=yearago1])/(data$GZPFY_Price[last]*data$GZPFY_Shares[last]),2),"%")
return16<-paste0(round(100*((data$GZPFY_Price[length(data$GZPFY_Price)]-data$GZPFY_Price[248])/data$GZPFY_Price[248])/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[248],units="days"))/365),2),"%")
return1year16<-paste0(round(100*(data$GZPFY_Price[last]-data$GZPFY_Price[yearago])/data$GZPFY_Price[yearago],2),"%")
return16a<-paste0(round((100*(data$GZPFY_Price[last]*data$GZPFY_Shares[last]+sum(data$GZPFY_Dividends)-orig16)/orig16)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[248],units="days"))/365),2),"%")
return1year16a<-paste0(round(100*(data$GZPFY_Price[last]*data$GZPFY_Shares[last]+sum(data$GZPFY_Dividends[data$Date>=yearago1])-data$GZPFY_Price[yearago]*data$GZPFY_Shares[yearago])/(data$GZPFY_Price[yearago]*data$GZPFY_Shares[yearago]),2),"%")

leg16<-c(paste0("Original Value = $",prettyNum(round(orig16,0),big.mark=",")),
	paste0("Current Value = ",current16),"",
	paste0("Appreciation = $",round(data$GZPFY_Price[last]*data$GZPFY_Shares[last]-orig16,0)),
	paste0("Dividends = $",round(sum(data$GZPFY_Dividends),0))," ",
	paste0("Annual Return (Total) = ",return16a),
	paste0("Annual Return (Appreciation Only) = ",return16),
	paste0("Return (Total - 1 year) = ",return1year16a),
	paste0("Return (Appreciation Only - 1 year) = ",return1year16),
	paste0("Dividend Yield = ",divi16))

jpeg(fileName16,width=1200,height=800,quality=100)
par(mar=c(6,5,5,5))
plot(data$GZPFY_Price*data$GZPFY_Shares,type="l",lwd=3,col=col.9[5],ylim=c(0,maxmax16),
	main="GZPFY Value",xaxt="n",xlab="",yaxt="n",ylab="")
axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
axis(2,at=seq(0,maxmax16,by=200),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax16,by=200),big.mark=","))),las=2)
axis(4,at=seq(0,maxmax16,by=200),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax16,by=200),big.mark=","))),las=2)
for (i in seq(0,maxmax16,by=100))
	{abline(h=i,lty=3,col="lightgray")}
legend("topleft",leg16,pch=15,col="white")
dev.off()

###### KING
#fileName17<-paste0("KING_",gsub("-","",today),".jpeg")

#maxmax17<-ceiling(max(data$KING_Price*data$KING_Shares)/200)*200
#orig17<-1000.74
#current17<-paste0("$",prettyNum(round(data$KING_Price[last]*data$KING_Shares[last],0),big.mark=","))
#divi17<-paste0(round(100*sum(data$KING_Dividends[data$Date>=yearago1])/(data$KING_Price[last]*data$KING_Shares[last]),2),"%")
#return17<-paste0(round(100*((data$KING_Price[length(data$KING_Price)]-data$KING_Price[255])/data$KING_Price[255])/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[255],units="days"))/365),2),"%")
#return1year17<-paste0(round(100*(data$KING_Price[last]-data$KING_Price[yearago])/data$KING_Price[yearago],2),"%")
#return17a<-paste0(round((100*(data$KING_Price[last]*data$KING_Shares[last]+sum(data$KING_Dividends)-orig17)/orig17)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[255],units="days"))/365),2),"%")
#return1year17a<-paste0(round(100*(data$KING_Price[last]*data$KING_Shares[last]+sum(data$KING_Dividends[data$Date>=yearago1])-data$KING_Price[yearago]*data$KING_Shares[yearago])/(data$KING_Price[yearago]*data$KING_Shares[yearago]),2),"%")

#leg17<-c(paste0("Original Value = $",prettyNum(round(orig17,0),big.mark=",")),
#	paste0("Current Value = ",current17),"",
#	paste0("Appreciation = $",round(data$KING_Price[last]*data$KING_Shares[last]-orig17,0)),
#	paste0("Dividends = $",round(sum(data$KING_Dividends),0))," ",
#	paste0("Annual Return (Total) = ",return17a),
#	paste0("Annual Return (Appreciation Only) = ",return17),
#	paste0("Return (Total - 1 year) = ",return1year17a),
#	paste0("Return (Appreciation Only - 1 year) = ",return1year17),
#	paste0("Dividend Yield = ",divi17))

#jpeg(fileName17,width=1200,height=800,quality=100)
#par(mar=c(6,5,5,5))
#plot(data$KING_Price*data$KING_Shares,type="l",lwd=3,col=col.9[5],ylim=c(0,maxmax17),
#	main="KING Value",xaxt="n",xlab="",yaxt="n",ylab="")
#axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
#axis(2,at=seq(0,maxmax17,by=200),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax17,by=200),big.mark=","))),las=2)
#axis(4,at=seq(0,maxmax17,by=200),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax17,by=200),big.mark=","))),las=2)
#for (i in seq(0,maxmax17,by=100))
#	{abline(h=i,lty=3,col="lightgray")}
#legend("topleft",leg17,pch=15,col="white")
#dev.off()

###### NFLX
fileName18<-paste0("NFLX_",gsub("-","",today),".jpeg")

maxmax18<-ceiling(max(data$NFLX_Price*data$NFLX_Shares)/500)*500
orig18<-3794.8
current18<-paste0("$",prettyNum(round(data$NFLX_Price[last]*data$NFLX_Shares[last],0),big.mark=","))
divi18<-paste0(round(100*sum(data$NFLX_Dividends[data$Date>=yearago1])/(data$NFLX_Price[last]*data$NFLX_Shares[last]),2),"%")
return18<-paste0(round(100*((data$NFLX_Price[length(data$NFLX_Price)]-94.87)/94.87)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[305],units="days"))/365),2),"%")
return1year18<-paste0(round(100*(data$NFLX_Price[last]-data$NFLX_Price[yearago])/data$NFLX_Price[yearago],2),"%")
return18a<-paste0(round((100*(data$NFLX_Price[last]*data$NFLX_Shares[last]+sum(data$NFLX_Dividends)-orig18)/orig18)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[305],units="days"))/365),2),"%")
return1year18a<-paste0(round(100*(data$NFLX_Price[last]*data$NFLX_Shares[last]+sum(data$NFLX_Dividends[data$Date>=yearago1])-data$NFLX_Price[yearago]*data$NFLX_Shares[yearago])/(data$NFLX_Price[yearago]*data$NFLX_Shares[yearago]),2),"%")

leg18<-c(paste0("Original Value = $",prettyNum(round(orig18,0),big.mark=",")),
	paste0("Current Value = ",current18),"",
	paste0("Appreciation = $",round(data$NFLX_Price[last]*data$NFLX_Shares[last]-orig18,0)),
	paste0("Dividends = $",round(sum(data$NFLX_Dividends),0))," ",
	paste0("Annual Return (Total) = ",return18a),
	paste0("Annual Return (Appreciation Only) = ",return18),
	paste0("Return (Total - 1 year) = ",return1year18a),
	paste0("Return (Appreciation Only - 1 year) = ",return1year18),
	paste0("Dividend Yield = ",divi18))

jpeg(fileName18,width=1200,height=800,quality=100)
par(mar=c(6,5,5,5))
plot(data$NFLX_Price*data$NFLX_Shares,type="l",lwd=3,col=col.9[5],ylim=c(0,maxmax18),
	main="NFLX Value",xaxt="n",xlab="",yaxt="n",ylab="")
axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
axis(2,at=seq(0,maxmax18,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax18,by=500),big.mark=","))),las=2)
axis(4,at=seq(0,maxmax18,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax18,by=500),big.mark=","))),las=2)
for (i in seq(0,maxmax18,by=250))
	{abline(h=i,lty=3,col="lightgray")}
legend("topleft",leg18,pch=15,col="white")
dev.off()

###### FB
fileName19<-paste0("FB_",gsub("-","",today),".jpeg")

maxmax19<-ceiling(max(data$FB_Price*data$FB_Shares)/500)*500
orig19<-4825
current19<-paste0("$",prettyNum(round(data$FB_Price[last]*data$FB_Shares[last],0),big.mark=","))
divi19<-paste0(round(100*sum(data$FB_Dividends[data$Date>=yearago1])/(data$FB_Price[last]*data$FB_Shares[last]),2),"%")
return19<-paste0(round(100*((data$FB_Price[length(data$FB_Price)]-120.625)/120.625)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[345],units="days"))/365),2),"%")
return1year19<-paste0(round(100*(data$FB_Price[last]-data$FB_Price[yearago])/data$FB_Price[yearago],2),"%")
return19a<-paste0(round((100*(data$FB_Price[last]*data$FB_Shares[last]+sum(data$FB_Dividends)-orig19)/orig19)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[345],units="days"))/365),2),"%")
return1year19a<-paste0(round(100*(data$FB_Price[last]*data$FB_Shares[last]+sum(data$FB_Dividends[data$Date>=yearago1])-data$FB_Price[yearago]*data$FB_Shares[yearago])/(data$FB_Price[yearago]*data$FB_Shares[yearago]),2),"%")

leg19<-c(paste0("Original Value = $",prettyNum(round(orig19,0),big.mark=",")),
         paste0("Current Value = ",current19),"",
         paste0("Appreciation = $",round(data$FB_Price[last]*data$FB_Shares[last]-orig19,0)),
         paste0("Dividends = $",round(sum(data$FB_Dividends),0))," ",
         paste0("Annual Return (Total) = ",return19a),
         paste0("Annual Return (Appreciation Only) = ",return19),
         paste0("Return (Total - 1 year) = ",return1year19a),
         paste0("Return (Appreciation Only - 1 year) = ",return1year19),
         paste0("Dividend Yield = ",divi19))

jpeg(fileName19,width=1200,height=800,quality=100)
par(mar=c(6,5,5,5))
plot(data$FB_Price*data$FB_Shares,type="l",lwd=3,col=col.9[5],ylim=c(0,maxmax19),
     main="FB Value",xaxt="n",xlab="",yaxt="n",ylab="")
axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
axis(2,at=seq(0,maxmax19,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax19,by=500),big.mark=","))),las=2)
axis(4,at=seq(0,maxmax19,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax19,by=500),big.mark=","))),las=2)
for (i in seq(0,maxmax19,by=250))
{abline(h=i,lty=3,col="lightgray")}
legend("topleft",leg19,pch=15,col="white")
dev.off()

###### AMZN
fileName20<-paste0("AMZN_",gsub("-","",today),".jpeg")

maxmax20<-ceiling(max(data$AMZN_Price*data$AMZN_Shares)/500)*500
orig20<-4870
current20<-paste0("$",prettyNum(round(data$AMZN_Price[last]*data$AMZN_Shares[last],0),big.mark=","))
divi20<-paste0(round(100*sum(data$AMZN_Dividends[data$Date>=yearago1])/(data$AMZN_Price[last]*data$AMZN_Shares[last]),2),"%")
return20<-paste0(round(100*((data$AMZN_Price[length(data$AMZN_Price)]-974)/974)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[384],units="days"))/365),2),"%")
return1year20<-paste0(round(100*(data$AMZN_Price[last]-data$AMZN_Price[yearago])/data$AMZN_Price[yearago],2),"%")
return20a<-paste0(round((100*(data$AMZN_Price[last]*data$AMZN_Shares[last]+sum(data$AMZN_Dividends)-orig20)/orig20)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[384],units="days"))/365),2),"%")
return1year20a<-paste0(round(100*(data$AMZN_Price[last]*data$AMZN_Shares[last]+sum(data$AMZN_Dividends[data$Date>=yearago1])-data$AMZN_Price[yearago]*data$AMZN_Shares[yearago])/(data$AMZN_Price[yearago]*data$AMZN_Shares[yearago]),2),"%")

leg20<-c(paste0("Original Value = $",prettyNum(round(orig20,0),big.mark=",")),
         paste0("Current Value = ",current20),"",
         paste0("Appreciation = $",round(data$AMZN_Price[last]*data$AMZN_Shares[last]-orig20,0)),
         paste0("Dividends = $",round(sum(data$AMZN_Dividends),0))," ",
         paste0("Annual Return (Total) = ",return20a),
         paste0("Annual Return (Appreciation Only) = ",return20),
         paste0("Return (Total - 1 year) = ",return1year20a),
         paste0("Return (Appreciation Only - 1 year) = ",return1year20),
         paste0("Dividend Yield = ",divi20))

jpeg(fileName20,width=1200,height=800,quality=100)
par(mar=c(6,5,5,5))
plot(data$AMZN_Price*data$AMZN_Shares,type="l",lwd=3,col=col.9[5],ylim=c(0,maxmax20),
     main="AMZN Value",xaxt="n",xlab="",yaxt="n",ylab="")
axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
axis(2,at=seq(0,maxmax20,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax20,by=500),big.mark=","))),las=2)
axis(4,at=seq(0,maxmax20,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax20,by=500),big.mark=","))),las=2)
for (i in seq(0,maxmax20,by=250))
{abline(h=i,lty=3,col="lightgray")}
legend("topleft",leg20,pch=15,col="white")
dev.off()

###### TCEHY
fileName21<-paste0("TCEHY_",gsub("-","",today),".jpeg")

maxmax21<-ceiling(max(data$TCEHY_Price*data$TCEHY_Shares)/500)*500
orig21<-5070
current21<-paste0("$",prettyNum(round(data$TCEHY_Price[last]*data$TCEHY_Shares[last],0),big.mark=","))
divi21<-paste0(round(100*sum(data$TCEHY_Dividends[data$Date>=yearago1])/(data$TCEHY_Price[last]*data$TCEHY_Shares[last]),2),"%")
return21<-paste0(round(100*((data$TCEHY_Price[length(data$TCEHY_Price)]-50.7)/50.7)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[400],units="days"))/365),2),"%")
return1year21<-paste0(round(100*(data$TCEHY_Price[last]-data$TCEHY_Price[yearago])/data$TCEHY_Price[yearago],2),"%")
return21a<-paste0(round((100*(data$TCEHY_Price[last]*data$TCEHY_Shares[last]+sum(data$TCEHY_Dividends)-orig21)/orig21)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[400],units="days"))/365),2),"%")
return1year21a<-paste0(round(100*(data$TCEHY_Price[last]*data$TCEHY_Shares[last]+sum(data$TCEHY_Dividends[data$Date>=yearago1])-data$TCEHY_Price[yearago]*data$TCEHY_Shares[yearago])/(data$TCEHY_Price[yearago]*data$TCEHY_Shares[yearago]),2),"%")

leg21<-c(paste0("Original Value = $",prettyNum(round(orig21,0),big.mark=",")),
         paste0("Current Value = ",current21),"",
         paste0("Appreciation = $",round(data$TCEHY_Price[last]*data$TCEHY_Shares[last]-orig21,0)),
         paste0("Dividends = $",round(sum(data$TCEHY_Dividends),0))," ",
         paste0("Annual Return (Total) = ",return21a),
         paste0("Annual Return (Appreciation Only) = ",return21),
         paste0("Return (Total - 1 year) = ",return1year21a),
         paste0("Return (Appreciation Only - 1 year) = ",return1year21),
         paste0("Dividend Yield = ",divi21))

jpeg(fileName21,width=1200,height=800,quality=100)
par(mar=c(6,5,5,5))
plot(data$TCEHY_Price*data$TCEHY_Shares,type="l",lwd=3,col=col.9[5],ylim=c(0,maxmax21),
     main="TCEHY Value",xaxt="n",xlab="",yaxt="n",ylab="")
axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
axis(2,at=seq(0,maxmax21,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax21,by=500),big.mark=","))),las=2)
axis(4,at=seq(0,maxmax21,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax21,by=500),big.mark=","))),las=2)
for (i in seq(0,maxmax21,by=250))
{abline(h=i,lty=3,col="lightgray")}
legend("topleft",leg21,pch=15,col="white")
dev.off()

###### AAPL
fileName22<-paste0("AAPL_",gsub("-","",today),".jpeg")

maxmax22<-ceiling(max(data$AAPL_Price*data$AAPL_Shares)/500)*500
orig22<-3979.5
current22<-paste0("$",prettyNum(round(data$AAPL_Price[last]*data$AAPL_Shares[last],0),big.mark=","))
divi22<-paste0(round(100*sum(data$AAPL_Dividends[data$Date>=yearago1])/(data$AAPL_Price[last]*data$AAPL_Shares[last]),2),"%")
return22<-paste0(round(100*((data$AAPL_Price[length(data$AAPL_Price)]-159.18)/159.18)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[406],units="days"))/365),2),"%")
return1year22<-paste0(round(100*(data$AAPL_Price[last]-data$AAPL_Price[yearago])/data$AAPL_Price[yearago],2),"%")
return22a<-paste0(round((100*(data$AAPL_Price[last]*data$AAPL_Shares[last]+sum(data$AAPL_Dividends)-orig22)/orig22)/(as.numeric(difftime(data$Date[length(data$Date)],data$Date[406],units="days"))/365),2),"%")
return1year22a<-paste0(round(100*(data$AAPL_Price[last]*data$AAPL_Shares[last]+sum(data$AAPL_Dividends[data$Date>=yearago1])-data$AAPL_Price[yearago]*data$AAPL_Shares[yearago])/(data$AAPL_Price[yearago]*data$AAPL_Shares[yearago]),2),"%")

leg22<-c(paste0("Original Value = $",prettyNum(round(orig22,0),big.mark=",")),
         paste0("Current Value = ",current22),"",
         paste0("Appreciation = $",round(data$AAPL_Price[last]*data$AAPL_Shares[last]-orig22,0)),
         paste0("Dividends = $",round(sum(data$AAPL_Dividends),0))," ",
         paste0("Annual Return (Total) = ",return22a),
         paste0("Annual Return (Appreciation Only) = ",return22),
         paste0("Return (Total - 1 year) = ",return1year22a),
         paste0("Return (Appreciation Only - 1 year) = ",return1year22),
         paste0("Dividend Yield = ",divi22))

jpeg(fileName22,width=1200,height=800,quality=100)
par(mar=c(6,5,5,5))
plot(data$AAPL_Price*data$AAPL_Shares,type="l",lwd=3,col=col.9[5],ylim=c(0,maxmax22),
     main="AAPL Value",xaxt="n",xlab="",yaxt="n",ylab="")
axis(1,at=seq(1,length(data$Date),by=4),labels=data$Date[seq(1,length(data$Date),by=4)],las=2)
axis(2,at=seq(0,maxmax22,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax22,by=500),big.mark=","))),las=2)
axis(4,at=seq(0,maxmax22,by=500),labels=paste0("$",gsub(" ","",prettyNum(seq(0,maxmax22,by=500),big.mark=","))),las=2)
for (i in seq(0,maxmax22,by=250))
{abline(h=i,lty=3,col="lightgray")}
legend("topleft",leg22,pch=15,col="white")
dev.off()


