###### Settings
library(readxl)
library(RColorBrewer)
library(ggplot2)
library(tidyr)
library(stringr)
library(scales)
library(dplyr)
options(scipen=10)
green.9<-brewer.pal(9,"Greens")
blue.9<-brewer.pal(9,"Blues")
red.9<-brewer.pal(9,"Reds")
purple.9<-brewer.pal(9,"Purples")
orange.9<-brewer.pal(9,"Oranges")

thous<-function(x) {
  x<-x/1000
  str_c("$",x,"K")
}

###### Date
today<-as.Date(substr(Sys.time(),0,10))
today1<-gsub("-","",today)
curr_month<-gsub("-","",substr(Sys.time(),0,7))

###### Loading data
setwd("C:/Users/green/Desktop")
data<-read_excel("Invest.xlsx")

###### Subdirectory
subdir<-paste0("C:/Users/green/Desktop/Investment/NetWorth/NetWorth_",curr_month)
dir.create(subdir)
setwd(subdir)

###### Formatting Data
data1<-data[,c("Date","Schwab Checking","Ally","US Bank","Kat China Savings",
	"Schwab Savings","EastWest PHP","EastWest USD","Apt. Equity","Roth IRA Cash",
	"Roth IRA Stock","Schwab Cash","Schwab Stock","Vanguard Funds","Vanguard IRA",
	"Google 401K","Treasury","PS 401K","Giller Loan Gold","Etrade","Kat IRA","Kat 401K",
	"Shorepoint","Kat RH 401K","Loper 529","4362 High Meadow","6999 Knollwood","436 Greenbrier",
	"Le Gran Unit 1402")]
colnames(data1)<-gsub(" ","",colnames(data1))
colnames(data1)[colnames(data1)=="4362HighMeadow"]<-"X4362HighMeadow"
colnames(data1)[colnames(data1)=="6999Knollwood"]<-"X6999Knollwood"
colnames(data1)[colnames(data1)=="436Greenbrier"]<-"X436Greenbrier"

data1$Cash<-data1$SchwabChecking+data1$SchwabSavings+data1$Ally+data1$USBank+
data1$KatChinaSavings+data1$SchwabSavings+data1$EastWestPHP+data1$EastWestUSD+data1$SchwabCash
data1$Retire_401K<-data1$PS401K+data1$Kat401K+data1$Google401K+data1$KatRH401K
data1$Retire_IRA<-data1$RothIRACash+data1$RothIRAStock+data1$VanguardIRA+data1$KatIRA
data1$Stock<-data1$SchwabStock+data1$VanguardFunds+data1$Etrade
data1$Other<-data1$Treasury+data1$GillerLoanGold
data1$Property<-data1$Apt.Equity+data1$Shorepoint+data1$X4362HighMeadow+data1$X6999Knollwood+data1$X436Greenbrier+data1$LeGranUnit1402
data1$College529<-data1$Loper529

data2<-data1[,c("Property","Retire_401K","Retire_IRA","Stock","Other","Cash","College529")]
rownames(data2)<-data1$Date
data3<-as.matrix(t(data2))

## ggplot2 format
data2a<-cbind(data$Date,data2)
colnames(data2a)<-c("date","Property","Retire_401K","Retire_IRA","Stock","Other","Cash","College_529")

data4<-gather(data2a, condition, measurement, Property:College_529)
data4$date<-as.Date(data4$date)
data4$measurement[data4$measurement<0]<-0

###### Plotting Raw Numbers
maxmax<-ceiling(max(apply(data3,2,sum))/50000)*50000

theme_plot<-theme(
  axis.text.x=element_text(size=16,colour="black",face="bold"),
  panel.background=element_rect(fill="white",colour="gray50"),
  panel.grid.minor=element_line(colour="gray90"),
  panel.grid.major=element_line(colour="gray90"),
  axis.text.y=element_text(size=16,colour="black",face="bold"),
  axis.title.x=element_text(size=16,face="bold"),
  axis.title.y=element_text(size=16,face="bold"),
  legend.title=element_blank(),
  legend.text=element_text(size=16,face="bold"),
  legend.position="bottom",
  plot.title=element_text(size=20,face="bold"),
  strip.text.x=element_text(size=18,face="bold")
)

fileName<-paste0("NetWorth_",gsub("-","",today),".png")
ggplot(data=data4,aes(x=date,y=measurement,fill=condition)) +
  geom_area(colour="black",size=0.5,alpha=0.6) +
  scale_y_continuous(breaks=seq(0,4000000,by=200000),labels=thous) +
  scale_x_date(date_breaks="1 year",labels=date_format("%b\n%Y")) + 
  xlab("") + ylab("") + theme_plot
ggsave(fileName,height=8,width=13)

###### Plotting proportions
fileName1<-paste0("NetWorthProportion_",gsub("-","",today),".png")
ggplot(data=data4,aes(x=date,y=measurement,fill=condition)) +
  geom_area(position="fill",colour="black",size=0.5,alpha=0.6) +
  scale_y_continuous(breaks=seq(0,1,by=.1),labels=percent) +
  scale_x_date(date_breaks="1 year",labels=date_format("%b\n%Y")) + 
  xlab("") + ylab("") + theme_plot
ggsave(fileName1,height=8,width=13)

###### Plotting lines
fileName2<-paste0("InvestmentClasses_",gsub("-","",today),".png")
ggplot(data=data4,aes(x=date,y=measurement,colour=condition,group=condition)) +
  geom_line(size=2) +
  scale_y_continuous(breaks=seq(0,1000000,by=100000),labels=thous) +
  scale_x_date(date_breaks="1 year",labels=date_format("%b\n%Y")) + 
  xlab("") + ylab("") + theme_plot
ggsave(fileName2,height=8,width=13)
