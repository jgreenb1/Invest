###### Settings
library(readxl)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(googlesheets4)
options(scipen=10)
col.9<-brewer.pal(9,"Blues")

thous<-function(x) {
  x<-x/1000
  str_c("$",x,"K")
}

curr_month<-gsub("-","",substr(Sys.time(),0,7))

###### Date
today<-as.Date(substr(Sys.time(),0,10))
today1<-gsub("-","",today)

###### Authentication (this allows me to use auth cache if only one token)
options(gargle_oauth_email = TRUE)

###### Loading data
## 965 Shorepoint
data1<-read_sheet("https://docs.google.com/spreadsheets/d/1urqMhVFVYbbzplj7C5O5UZJJrP46bU2ClhQEkLrsDW8/edit#gid=1688995611",sheet="965 Shorepoint Equity")
## 4362 High Meadow
data2<-read_sheet("https://docs.google.com/spreadsheets/d/1urqMhVFVYbbzplj7C5O5UZJJrP46bU2ClhQEkLrsDW8/edit#gid=1142935786",sheet="4362 High Meadow Equity")
## 45 Eisenhower - Le Gran Unit 909
data3<-read_sheet("https://docs.google.com/spreadsheets/d/1urqMhVFVYbbzplj7C5O5UZJJrP46bU2ClhQEkLrsDW8/edit#gid=1289989391",sheet="45 Eisenhower Street - Le Gran U909")
## 6999 Knollwood
data4<-read_sheet("https://docs.google.com/spreadsheets/d/1urqMhVFVYbbzplj7C5O5UZJJrP46bU2ClhQEkLrsDW8/edit#gid=1032920525",sheet="6999 Knollwood")
## 436 Greenbrier
data5<-read_sheet("https://docs.google.com/spreadsheets/d/1urqMhVFVYbbzplj7C5O5UZJJrP46bU2ClhQEkLrsDW8/edit#gid=1032920525",sheet="436 Greenbrier Equity")
## 45 Eisenhower - Le Gran Unit 1402
data6<-read_sheet("https://docs.google.com/spreadsheets/d/1urqMhVFVYbbzplj7C5O5UZJJrP46bU2ClhQEkLrsDW8/edit#gid=1032920525",sheet="45 Eisenhower Street - Le Gran U1402")

###### Setting directory
subdir<-paste0("C:/Users/green/Desktop/Investment/NetWorth/NetWorth_",curr_month)
dir.create(subdir)
setwd(subdir)

###### Formatting data
data1$Property<-"965ShorepointCourt"
data2$Property<-"4362HighMeadow"
data3$Property<-"45EisenhowerU909"
data4$Property<-"6999Knollwood"
data5$Property<-"436Greenbrier"
data6$Property<-"45EisenhowerU1402"

data<-rbind(data1,data2,data3,data4,data5,data6)
data<-data[!is.na(data$Date),]

data$Date<-as.Date(data$Date,format="%m/%d/%Y")
data$`Principal Owed`<-round(as.numeric(gsub('[$,]', '', data$`Principal Owed`)),0)
data$`Principal Payment`<-round(as.numeric(gsub('[$,]', '', data$`Principal Payment`)),0)
data$`Remaining Principal`<-round(as.numeric(gsub('[$,]', '', data$`Remaining Principal`)),0)
data$Interest<-round(as.numeric(gsub('[$,]', '', data$Interest)),0)
data$`Total Interest Paid`<-round(as.numeric(gsub('[$,]', '', data$`Total Interest Paid`)),0)
data$Equity<-round(as.numeric(gsub('[$,]', '', data$Equity)),0)
data$`Total Interest Paid`<-round(as.numeric(gsub('[$,]', '', data$`Total Interest Paid`)),0)
data$Equity<-round(as.numeric(gsub('[$,]', '', data$Equity)),0)
data$`Equity + Renovation`<-round(as.numeric(gsub('[$,]', '', data$`Equity + Renovation`)),0)

## Data Manipulation
combo<-data[,c("Date","Property","Remaining Principal","Equity + Renovation")]
colnames(combo)<-c("Date","Property","Debt","Equity")
combo1<-gather(combo,condition,measurement,Debt:Equity)
combo1$MONTH<-substr(combo1$Date,0,7)
maxdate<-as.Date(min(max(data1$Date[!is.na(data1$Date)]),max(data2$Date[!is.na(data2$Date)]),max(data3$Date[!is.na(data3$Date)]),max(data4$Date[!is.na(data4$Date)]),max(data5$Date[!is.na(data5$Date)])))
combo2<-combo1 %>% group_by(MONTH,condition) %>% summarise(measurement=sum(measurement))
combo2$Date<-as.Date(paste0(combo2$MONTH,"-01"))
combo2<-combo2[combo2$Date<=maxdate,]

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

fileName1<-paste0("RealEstateEquityDebt_",gsub("-","",today),".jpeg")
ggplot(data=combo1,aes(x=Date,y=measurement,fill=condition)) +
  geom_area(colour="black",size=0.5,alpha=0.6) +
  facet_wrap(~Property) +
  ggtitle("Real Estate Equity and Debt") +
  scale_y_continuous(labels=thous) +
  scale_x_date(date_breaks="12 months",date_labels="%b\n%Y") + 
  xlab("") + ylab("") + theme_plot2
ggsave(fileName1,height=8,width=13)

fileName1a<-paste0("RealEstateEquityDebt2_",gsub("-","",today),".jpeg")
ggplot(data=combo1,aes(x=Date,y=measurement,fill=condition)) +
  geom_area(colour="black",size=0.5,alpha=0.6) +
  facet_wrap(~Property,scales="free_y") +
  ggtitle("Real Estate Equity and Debt") +
  scale_y_continuous(labels=thous) +
  scale_x_date(date_breaks="12 months",date_labels="%b\n%Y") + 
  xlab("") + ylab("") + theme_plot2
ggsave(fileName1a,height=8,width=13)

fileName2<-paste0("RealEstateEquityDebtProp_",gsub("-","",today),".jpeg")
ggplot(data=combo1,aes(x=Date,y=measurement,fill=condition)) +
  geom_area(colour="black",size=0.5,alpha=0.6,position="fill") +
  facet_wrap(~Property) +
  ggtitle("Real Estate Equity and Debt") +
  scale_y_continuous(labels=scales::percent) +
  scale_x_date(date_breaks="12 months",date_labels="%b\n%Y") + 
  xlab("") + ylab("") + theme_plot2
ggsave(fileName2,height=8,width=13)

fileName3<-paste0("TotalRealEstateEquityDebt_",gsub("-","",today),".jpeg")
ggplot(data=combo2,aes(x=Date,y=measurement,fill=condition)) +
  geom_area(colour="black",size=0.5,alpha=0.6) +
  ggtitle("Total Real Estate Equity and Debt") +
  scale_y_continuous(labels=thous) +
  scale_x_date(date_breaks="12 months",date_labels="%b\n%Y") + 
  xlab("") + ylab("") + theme_plot2
ggsave(fileName3,height=8,width=13)

fileName4<-paste0("TotalRealEstateEquityDebtProp_",gsub("-","",today),".jpeg")
ggplot(data=combo2,aes(x=Date,y=measurement,fill=condition)) +
  geom_area(position="fill",colour="black",size=0.5,alpha=0.6) +
  ggtitle("Proportion of Total Real Estate Equity and Debt") +
  scale_y_continuous(labels=scales::percent) +
  scale_x_date(date_breaks="12 months",date_labels="%b\n%Y") + 
  xlab("") + ylab("") + theme_plot2
ggsave(fileName4,height=8,width=13)

