###### Settings
library(XLConnect)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
options(scipen=10)
col.9<-brewer.pal(9,"Blues")

thous<-function(x) {
  x<-x/1000
  str_c("$",x,"K")
}

###### Date
today<-as.Date(substr(Sys.time(),0,10))
today1<-gsub("-","",today)

###### Loading data
data<-readWorksheet(loadWorkbook("C:/Users/Jon/Desktop/StockList2.xlsx"),sheet=1)
index<-readWorksheet(loadWorkbook("C:/Users/Jon/Desktop/Invest.xlsx"),sheet=1)

###### Create subdirectory
newdir<-paste0("C:/Users/Jon/Desktop/Investment/StockTracking/Images_",today1)
dir.create(newdir)
setwd(newdir)

###### Formatting data
index$Date<-as.Date(index$Date)
index<-index[!is.na(index$Date),]
index$Index2<-index$Vanguard.Funds+index$Vanguard.IRA+index$Google.401K+index$PS.401K+index$Kat.401K+index$Kat.RH.401K+index$Loper.529
index1<-index[,c("Date","Index2")]

data$Date<-as.Date(data$Date)
data<-data[!is.na(data$Date),]
data$AAPL_Value<-data$AAPL_Price*data$AAPL_Shares
data$AMZN_Value<-data$AMZN_Price*data$AMZN_Shares
data$BAC_Value<-data$BAC_Price*data$BAC_Shares
data$C_Value<-data$C_Price*data$C_Shares
data$CEO_Value<-data$CEO_Price*data$CEO_Shares
data$CVX_Value<-data$CVX_Price*data$CVX_Shares
data$FB_Value<-data$FB_Price*data$FB_Shares
data$GZPFY_Value<-data$GZPFY_Price*data$GZPFY_Shares
data$MSFT_Value<-data$MSFT_Price*data$MSFT_Shares
data$NFLX_Value<-data$NFLX_Price*data$NFLX_Shares
data$TCEHY_Value<-data$TCEHY_Price*data$TCEHY_Shares
data$XOM_Value<-data$XOM_Price*data$XOM_Shares
data$A_Value<-data$A_Price*data$A_Shares
data$BP_Value<-data$BP_Price*data$BP_Shares
data$CSCO_Value<-data$CSCO_Price*data$CSCO_Shares
data$GE_Value<-data$GE_Price*data$GE_Shares
data$GOOG_Value<-data$GOOG_Price*data$GOOG_Shares
data$HPQ_Value<-data$HPQ_Price*data$HPQ_Shares
data$MCD_Value<-data$MCD_Price*data$MCD_Shares
data$PEP_Value<-data$PEP_Price*data$PEP_Shares
data$CAR_Value<-data$CAR_Price*data$CAR_Shares
data$PRO_Value<-data$PRO_Price*data$PRO_Shares
data$KING_Value<-data$KING_Price*data$KING_Shares
data<-merge(data,index1,by.x="Date",by.y="Date",all.x=TRUE)
data$Index_Value<-data$Index1+data$Index2

data1<-data[,c(1,grep("Value",colnames(data)))]
colnames(data1)<-gsub("_Value","",colnames(data1))
data2<-gather(data1,condition,measurement,AAPL:Index)

###### Plotting
theme_plot2 <- theme(
  axis.text.x  = element_text(size=16,color='black',face='bold'),
  panel.background = element_rect(fill='white',colour='gray50'),
  panel.grid.minor = element_line('gray90'),
  panel.grid.major = element_line(colour = 'gray90'),
  axis.text.y = element_text(size=16,color='black',face='bold'),
  axis.title.x = element_text(size=16,face='bold'),
  axis.title.y = element_text(size=16,face='bold'),
  legend.title=element_blank(),
  legend.text = element_text(size=16,face='bold'),
  legend.position = "bottom",
  plot.title=element_text(size=20,face='bold'),
  strip.text.x = element_text(size=18,face='bold')
)

fileName<-paste0("AggregateStocks_",gsub("-","",today),".jpeg")
ggplot(data=data2,aes(x=Date,y=measurement,fill=condition)) +
  geom_area(colour="black",size=0.5,alpha=0.6) +
  ggtitle("Aggregate Stock Value by Ticker") +
  scale_y_continuous(labels=thous) +
  scale_x_date(date_breaks="12 months",date_labels="%b\n%Y") + 
  xlab("") + ylab("") + theme_plot2
ggsave(fileName,height=8,width=13)

fileName1<-paste0("AggregateStocksProp_",gsub("-","",today),".jpeg")
ggplot(data=data2,aes(x=Date,y=measurement,fill=condition)) +
  geom_area(colour="black",size=0.5,alpha=0.6,position="fill") +
  ggtitle("Proportion of Aggregate Stock Value by Ticker") +
  scale_y_continuous(labels=scales::percent) +
  scale_x_date(date_breaks="12 months",date_labels="%b\n%Y") + 
  xlab("") + ylab("") + theme_plot2
ggsave(fileName1,height=8,width=13)

fileName1a<-paste0("AggregateStocksPropNoIndex_",gsub("-","",today),".jpeg")
ggplot(data=data2[data2$condition!="Index",],aes(x=Date,y=measurement,fill=condition)) +
  geom_area(colour="black",size=0.5,alpha=0.6,position="fill") +
  ggtitle("Proportion of Aggregate Stock Value by Ticker") +
  scale_y_continuous(labels=scales::percent) +
  scale_x_date(date_breaks="12 months",date_labels="%b\n%Y") + 
  xlab("") + ylab("") + theme_plot2
ggsave(fileName1a,height=8,width=13)

fileName2<-paste0("StocksLine_",gsub("-","",today),".jpeg")
ggplot(data=data2[data2$condition!="Index",],aes(x=Date,y=measurement,group=condition,colour=condition)) +
  geom_line(size=2) +
  ggtitle("Stock Value by Ticker") +
  scale_y_continuous(labels=thous) +
  scale_x_date(date_breaks="12 months",date_labels="%b\n%Y") + 
  xlab("") + ylab("") + theme_plot2
ggsave(fileName2,height=8,width=13)

fileName3<-paste0("StocksFacetLine_",gsub("-","",today),".jpeg")
ggplot(data=data2,aes(x=Date,y=measurement,group=condition,colour=condition)) +
  geom_line(size=2) +
  facet_wrap(~condition,scales="free_y") +
  ggtitle("Stock Value by Ticker") +
  scale_y_continuous(labels=thous) +
  scale_x_date(date_labels="%b\n%Y") + 
  xlab("") + ylab("") + theme_plot2 +
  expand_limits(y=0)
ggsave(fileName3,height=8,width=13)

