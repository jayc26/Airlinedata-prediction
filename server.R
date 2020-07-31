#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
air.df<-read.csv(paste("SixAirlinesData.csv",sep = ""))
air1 = air.df[,6:17]

#odt.df<-read.csv(paste("odt.csv",sep = ""))
avf.df<-read.csv(paste("b1.csv",sep = ""))
delay.df<-read.csv(paste("flight.csv",sep = ""))
sl<-summary(avf.df)
library(compareDF)
cta = by(avf.df$Ifare,avf.df$Navg,mean)
m=lm(avf.df$Ifare~avf.df$Navg)
lm(formula = avf.df$Ifare~avf.df$Navg )
#ms=summary(m)
#me=mean(avf.df$v2017)
#mew=avf.df$v2017
#msp=predict.lm(m,me=data.frame(x=10))


r=table(avf.df$Rank,avf.df$Aname)


sapply(avf.df, function(x) sum(is.na(x)))
sapply(avf.df, function(x) length(unique(x)))
 
model1 <- lm(Afare ~ Ifare + Navg, data = avf.df)
odtt <- read.table("odt.csv", sep=",", header=TRUE)
# convert to local data frame
odtt <- table(odtt$ItinFare)
b<-by(air.df$PRICE_ECONOMY,air.df$AIRLINE,mean)
by(air.df$PRICE_PREMIUM,air.df$AIRLINE,mean)
by(air.df$SEATS_ECONOMY,air.df$AIRLINE,mean)
by(air.df$SEATS_PREMIUM,air.df$AIRLINE,mean)
by(air.df$WIDTH_ECONOMY,air.df$AIRLINE,mean)
by(air.df$WIDTH_PREMIUM,air.df$AIRLINE,mean)
t<-table(air.df$INTERNATIONAL)
t.test(PRICE_PREMIUM~AIRCRAFT,alternative="less",data=air.df)

t.test(PRICE_ECONOMY~AIRCRAFT,alternative="less", data=air.df)

t.test(PRICE_ECONOMY~INTERNATIONAL, alternative="less", data = air.df)

t.test(PRICE_PREMIUM~INTERNATIONAL, alternative="less", data = air.df)


fit1<- lm(PRICE_ECONOMY~ PITCH_ECONOMY + WIDTH_ECONOMY + FLIGHT_DURATION +PRICE_RELATIVE+ INTERNATIONAL+PITCH_ECONOMY, data = air.df)
summary(fit1)

fit2<- lm(PRICE_PREMIUM~ PITCH_PREMIUM + WIDTH_PREMIUM + FLIGHT_DURATION +PRICE_RELATIVE+ INTERNATIONAL+PITCH_PREMIUM , data = air.df)
summary(fit2)

library(formattable)

le=mean(air.df$PRICE_ECONOMY)
l=mean(air.df$PRICE_PREMIUM)
prediction<-predict(fit2,l=data.frame(x=100))
pree<- prediction - air.df$PRICE_PREMIUM 
prep<-percent(pree)
pren<-predict(fit1,le=data.frame(x=100))
preen<- pren - air.df$PRICE_ECONOMY
prees<-percent(preen)
#prep1<- 1000-prep
#prees1<- 1000-prees




#bp=plot(air.df$FLIGHT_DURATION~air.df$PRICE_ECONOMY,main="Price of Economy seat with flight duration",xlab = "Flight Duration in Hours", ylab="Price of seats in $", cex=1.1)
#bs=hist(air.df$PRICE_PREMIUM,main = paste("" ),xlab = "",col = 3)


##Individual boxplots for comparable parameters





# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   output$a<-renderPlot({ 
     #paste(bs)
     library(car)
     scatterplotMatrix(
       air.df[
         ,c("FLIGHT_DURATION","PRICE_ECONOMY","PRICE_PREMIUM")], 
       spread=FALSE, smoother.args=list(lty=2),
       main="Scatter Plot Matrix", diagonal = "histogram")
     
    # library(car)
    #scatterplot(air.df$PRICE_ECONOMY,air.df$FLIGHT_DURATION,main="Price of Economy seat with flight duration",ylab = "Flight Duration in Hours", xlab="Price of seats in $",cex=1.1,pch=19)
     #library(ggvis)
     #ggvis(~PRICE_ECONOMY,~PRICE_PREMIUM,fill=~PRICE_RELATIVE,data=air.df)
     
     
   })
   output$pd<-renderPlot({ 
     
      library(car)
     scatterplot(air.df$PRICE_ECONOMY,air.df$FLIGHT_DURATION,main="Price of Economy seat with flight duration",ylab = "Flight Duration in Hours", xlab="Price of seats in $",cex=1.1,pch=19)
     
     
   })
   output$pdl<-renderPlot({ 
     library(corrgram)
     corrgram(air1, order=TRUE, lower.panel=panel.shade,
              upper.panel=panel.pie, text.panel=panel.txt,
              main="Corrgram of Airline data intercorrelations")
   
   })
   output$c<-renderPlot({ 
     #paste(bs)
     plot(air.df$FLIGHT_DURATION~air.df$PRICE_PREMIUM,main="Price of Premium seat with flight duration",xlab = "Price of seats in $", ylab="Flight Duration in Hours", cex=1.1,col="green")
     
     
   })
   output$e<-renderPlot({
     boxplot(air.df$PRICE_ECONOMY~air.df$AIRLINE,yaxt="n", horizontal=TRUE,main="Price of Economy seats with Airlines",xlab="Price in $", ylab="Name of the aircraft company",col=c("blue","skyblue","orange","blue","pink","red"))
     axis(side=2, at=c(1,2,3,4,5,6), labels = c("AFR","BriA","Delta","Jet","SA","Vir"))
     
     
   })
   output$pe<-renderPlot({
     boxplot(pren~air.df$AIRLINE,yaxt="n", horizontal=TRUE,main="Predicted Price of Economy seats with Airlines",xlab="Price in $", ylab="Name of the aircraft company",col=c("blue","skyblue","orange","blue","pink","red"))
     axis(side=2, at=c(1,2,3,4,5,6), labels = c("AFR","BriA","Delta","Jet","SA","Vir"))
     
     
   })
   output$p<-renderPlot({
     boxplot(air.df$PRICE_PREMIUM~air.df$AIRLINE,yaxt="n", horizontal=TRUE,main="Price of Premium seats with Airlines company",xlab="Price in $", ylab="Name of the aircraft company",col=c("blue","skyblue","orange","blue","pink","red"))
     axis(side=2, at=c(1,2,3,4,5,6), labels = c("AFR","BriA","Delta","Jet","SA","Vir"))
     
     
   })
   
   output$pp<-renderPlot({
     #plot(air.df$AIRLINE~prediction,main="Price of Economy seat with flight duration",xlab = "Flight Duration in Hours", ylab="Price of seats in $", cex=1.1)
     
     #plot(air.df$AIRLINE~prediction,lty=1.8,col="red",main="Price of Economy seat with flight duration",ylab = "Flight Duration in Hours", xlab="Price of airlines in $",cex=1.1)
     pre=length(prediction)
     #library(car)
     #scatterplot(air.df$AIRLINE,prediction,main="Predicted Price of Economy seats",ylab = "Aircraft", xlab="Price of seats in $",x=462, y=462,cex=1.1,pch=19)
     boxplot(prediction~air.df$AIRLINE,yaxt="n", horizontal=TRUE,main="Predicted Price of Premium seats with Airlines",xlab="Price in $", ylab="Name of the aircraft company",col=c("blue","skyblue","orange","blue","pink","red"))
     axis(side=2, at=c(1,2,3,4,5,6), labels = c("AFR","BriA","Delta","Jet","SA","Vir"))
     
    
     
   })
   output$pi<-renderPlot({
     boxplot(air.df$PRICE_PREMIUM~air.df$AIRCRAFT,yaxt="n", horizontal=TRUE,main="Price of Premium seats with Aircraft company",xlab="Price in $", ylab="Name of the aircraft company",col=c("blue","red"))
     axis(side=2, at=c(1,2), labels = c("Boeing", "Airbus"))
     
   })
   output$ae<-renderPlot({
     labels=c("International","Domestic")
     x=c(461,39)
     table(air.df$INTERNATIONAL)
     pie(x,labels,main="Type of Airlines",col = rainbow(length(x)))
     
   })
   output$d<-renderTable({
     library(tidyverse)
     #v=avf.df$Rank
     #e=avf.df$Aname
    #v[with(v,order(v,e))]
     
     avf.df %>% select(1,3,8)
    # x=c("Rank","Airport Name","Passengers")
     #table(avf.df$Rank,avf.df$Aname,avf.df$v2017, colnames(x))
     #table(avf.df$Rank,avf.df$Aname)
     #tableOutput(avf.df$Rank~avf.df$Aname)
     #with(avf.df$Rank,table(avf.df$Aname, dnn=c("Rank","Airport")))
     #summary(tbl)
     #paste("HelloT",table(avf.df$Rank,avf.df$Aname))
   })
   output$delay<-renderValueBox({
     library(formattable)
     delay_rate_value <-mean(delay.df$Dep_Delay, na.rm=TRUE)
     valueBox(
       percent(delay_rate_value/100)
       ,subtitle = "Departure Delay Rate 2016-17"
       ,color = 'blue'
       ,icon = icon("bell", lib= 'font-awesome')
     )
     #paste("HelloT",table(avf.df$Rank,avf.df$Aname))
   })
   
   output$adelay<-renderValueBox({
     library(formattable)
     delay_rate_value <-mean(delay.df$Arr_Delay, na.rm=TRUE)
     valueBox(
       percent(delay_rate_value/100)
       ,subtitle = "Arrival Delay Rate 2016-2017"
       ,color = 'yellow'
       ,icon = icon("bell", lib= 'font-awesome')
     )
     #paste("HelloT",table(avf.df$Rank,avf.df$Aname))
   })
   output$cancel<-renderValueBox({
     library(formattable)
     delay_rate_value <-mean(delay.df$Security_Delay, na.rm=TRUE)
     valueBox(
       percent(delay_rate_value/100)
       ,subtitle = "Security Delay Rate 2016-2017"
       ,color = 'red'
       ,icon = icon("bell", lib= 'font-awesome')
     )
     #paste("HelloT",table(avf.df$Rank,avf.df$Aname))
   })
   output$da<-renderPlot({ 
     #paste(bs)
     plot(delay.df$Scheduled_Departure~delay.df$Departure_Time,main="Departure Analysis",xlab = "Scheduled Departure", ylab="Departure Time", cex=1.1,col="red")
     
     
   })
   output$af<-renderPlot({ 
     #paste(bs)
     plot(avf.df$Afare~avf.df$Ifare,main="Airport Fare Analysis",xlab = "Average Fare", ylab="Itenary Fare", cex=1.1)
     
     
   })
   output$pas<-renderPlot({ 
     #paste(bs)
     dc<-gsub(",","",avf.df$v2017)
     dc<-as.numeric(dc)
     library(car)
     scatterplot(avf.df$Rank,dc,main="Number of Passengers by Airport Rank",ylab = "Number of Passangers", xlab="Rank",cex=1.1,pch=10,col = "green",border="red",bg="black")
     
     #hist(dc,main="Airport Passanger Analysis By Rank",xlab = "Rank", ylab="Number of Passangers",cex=1.1,xlim=c(0,400),ylim=c(0,400000))
     #axis(side=2, at=1:300000)
     #axis(side=1,at=1:500)
     #boxplot(avf.df$v2017~avf.df$Aname,yaxt="n", horizontal=TRUE,main="Price of Premium seats with Aircraft company",xlab="Price in $", ylab="Name of the aircraft company", col=c("blue","red"))
     #axis(side=2, at=c(1,2), labels = c("Boeing", "Airbus"))     
     
   })
   output$afn<-renderPlot({ 
     #paste(bs)
     plot(avf.df$Afare~avf.df$Navg,main="Average Fare Analysis",xlab = "Average Fare", ylab="US National Average Fare",col="green", cex=1.1)
     
     
   })
   output$afni<-renderPlot({ 
     #paste(bs)
     plot(avf.df$Navg~avf.df$Ifare,main="Average Fare Analysis",xlab = "Average Itenary Fare", ylab="US National Average Fare", col="blue",cex=1.1)
     
     
   })
   output$rp<-renderPlot({
    # library(tidyverse)
     
     #avf.df %>% select(1,3)
    #avf.df[order(avf.df$v2017)]
     dc<-gsub(",","",avf.df$v2017)
     dc<-as.numeric(dc)
     dcl=rev(sort(dc))
     dcle=length(dcl)
     hist(dcl,xlim = c(100000,800000), breaks = 10, main = paste("Total Passangers At Popular Airports" ),xlab = "Passengers", ylab="Number",col = "blue")
     
     #sort(avf.df$Aname,decreasing = TRUE)
   })
   output$npp<-renderPlot({
     tm<-table(avf.df$v2017,avf.df$Aname)
     #n<-avf.df$Aname
     barplot(tm,beside = TRUE,main = "Passangers", col="yellow")
     
   })
   output$seats<-renderPlot({
     #x<-c("AFR","BriA","Delta","Jet","SA","Vir")
     #ta<-as.table(air.df$SEATS_TOTAL,x)
     boxplot(air.df$SEATS_TOTAL~air.df$AIRLINE,yaxt="n", horizontal=TRUE,main="Number of seats per Airline",xlab="Number Of Seats", ylab="Name of the aircraft company" ,col=c("blue","skyblue","orange","blue","pink","red"))
     axis(side=2, at=c(1,2,3,4,5,6), labels = c("AFR","BriA","Delta","Jet","SA","Vir"))
     
     #n<-avf.df$Aname
     #barplot(ta,beside = TRUE,main = "Number of Seats", col="yellow")
     
   })
   output$dan<-renderPlot({ 
     #paste(bs)
     plot(delay.df$Scheduled_Arrival~delay.df$Arrival_Time,main="Arrival Analysis",xlab = "Scheduled Arrival", ylab="Arrival Time",col="blue", cex=1.1)
     
     
   })
   output$dep<-renderPlot({ 
     #da<-table(delay.df$Departure_Time,delay.df$Dep_Delay)
     #n<-avf.df$Aname
     da<-delay.df$Dep_Delay
     #x=c(0,5000)
     barplot(da,main = "Departure Delay 10% data", col="yellow", ylim = c(-200,800),xlim = c(1,500),ylab = "In Minutes")
    #plot(da,col="blue", xlab = "Number Of Flights",ylab = "Departure Delay",xlim = c(-20,5000))
     #library(e1071)
     #dat=data.frame(delay.df$Scheduled_Departure,delay.df$Departure_Time)
     
     #boxplot(delay.df$Scheduled_Departure,delay.df$Departure_Time,yaxt="n", horizontal=TRUE,main="Number of seats per Airline",xlab="Number Of Seats", ylab="Name of the aircraft company" ,col="blue")
     
   })
   output$arr<-renderPlot({ 
     #da<-table(delay.df$Arrival_Time,delay.df$Arrival_Delay)
     #n<-avf.df$Aname
     da<-delay.df$Arrival_Delay
     barplot(da,beside = TRUE,main = "Arrival Delay 10% data", col="blue",ylim = c(-200,800),xlim = c(1,500),ylab = "In Minutes")
     
     
   })
   output$ch<-renderPlot({
    
       boxplot(prep~air.df$AIRLINE,yaxt="n", horizontal=TRUE,main="Predicted Price Increase/Decrease of Premium seats of Airlines",xlab="Price in $", ylab="Name of the aircraft company" ,col=c("blue","skyblue","orange","blue","pink","red"))
       axis(side=2, at=c(1,2,3,4,5,6), labels = c("AFR","BriA","Delta","Jet","SA","Vir"))
       
       
     
     #paste(prep)
   })
   output$peh<-renderPlot({
     
     boxplot(prees~air.df$AIRLINE,yaxt="n", horizontal=TRUE,main="Predicted Price Increase/Decrease of Economy seats of Airlines",xlab="Price in $", ylab="Name of the aircraft company",col=c("blue","skyblue","orange","blue","pink","red"))
     axis(side=2, at=c(1,2,3,4,5,6), labels = c("AFR","BriA","Delta","Jet","SA","Vir"))
     
     
     
     #paste(prep)
   })
   output$pehl<-renderText({
     #dcp<-gsub(",","",avf.df$v2017)
     #dcpr<-as.numeric(dcp)
     #paste(dcpr)
    # forecast::forecast(fit1,h=200)
   })
   output$gg<-renderPlot({
     dcp<-gsub(",","",avf.df$v2017)
     dcpr<-as.numeric(dcp)
     pase<-density(dcpr,from = 1, to=20000)
     plot(pase)
     library(ggvis)
     
     #ggvis(~PRICE_ECONOMY,~PRICE_PREMIUM,fill=~PRICE_RELATIVE,data=air.df)
     
     #ggvis(~PRICE_ECONOMY,~PRICE_PREMIUM,fill=~AIRLINE,data=air.df)
     
    # ggvis(~PRICE_PREMIUM,~WIDTH_PREMIUM,fill=~PRICE_RELATIVE,data=air.df)
     
     #Interaction between the price quantities
     
     #ggvis(~PRICE_ECONOMY,~PRICE_RELATIVE,fill=~PRICE_PREMIUM,data=air.df)
     
     #ggvis(~PRICE_ECONOMY,~PRICE_PREMIUM,fill=~PRICE_RELATIVE,data=air.df)
     #dc<-gsub(",","",avf.df$v2017)
     #ggvis(~PRICE_ECONOMY,~PRICE_PREMIUM,fill=~INTERNATIONAL,data=air.df)
     
     
     #mp<-mean(avf.df$v2017)
     #curve(dnorm(x=avf.df$v2017,mean = mp,sd=200),xlim = c(0,200000),xname = "Passangers")
     #abline(h=0)
     #library(dplyr)
     #a<-avf.df %>% pull(v2017)
     #plot(pnorm(a,mean = mp,sd=200)~avf.df$Aname,cex=1.1)
     #paste(prees)
     
   
   
    
    
     
    # boxplot(air.df$FLIGHT_DURATION, main="Duration of Flights",ylab="Hours")
     cor(air1)
    # x<-air.df[,c("FLIGHT_DURATION","LAMBDA","INTERNATIONAL","N", "MONTH","QUALITY","WIDTH_ECONOMY", "WIDTH_PREMIUM","SEATS_ECONOMY", "SEATS_PREMIUM", "PITCH_ECONOMY", "PITCH_PREMIUM")]
     #y<-air.df[,c("PRICE_ECONOMY","PRICE_PREMIUM","PRICE_RELATIVE")]
     #cor(x,y)
     #cov(x,y)
     #var(x,y)
     
     #Visualizing relation through corrplots
     
     #library(corrplot)
     #corrplot(corr=cor(air1[,c(2,6:17)],use = "complete.obs"), method = "ellipse")
     #library(gplots)
     #corrplot.mixed(corr=cor(air1[,c(2,6:17)],use = "complete.obs"), upper = "ellipse", tl.pos = "lt", col = colorpanel(50, "red", "gray60", "blue4"))
     
   })
   output$g1<-renderPlot({
     ggvis(~PRICE_ECONOMY,~PRICE_PREMIUM,fill=~PRICE_RELATIVE,data=air.df)
     
   })
   output$g3<-renderPlot({
      ggvis(~PRICE_PREMIUM,~WIDTH_PREMIUM,fill=~PRICE_RELATIVE,data=air.df)
     
   })
   output$g4<-renderPlot({
     ggvis(~PRICE_ECONOMY,~PRICE_RELATIVE,fill=~PRICE_PREMIUM,data=air.df)
     
   })
   output$g5<-renderPlot({
     ggvis(~PRICE_ECONOMY,~PRICE_PREMIUM,fill=~INTERNATIONAL,data=air.df)
     
   })
   output$g6<-renderPlot({
     ggvis(~PRICE_ECONOMY,~PRICE_PREMIUM,fill=~AIRLINE,data=air.df)
     
   })
   #ggvis(~PRICE_ECONOMY,~PRICE_PREMIUM,fill=~AIRLINE,data=air.df)
   
   output$pf1<-renderPlot({
     par(mfrow=c(1,2),col=c("blue"))
     with(air.df, boxplot(air.df$PRICE_ECONOMY,main="Price of Economy seats",ylab="Price in $"),col="yellow")
     with(air.df, boxplot(air.df$PRICE_PREMIUM,main="Price of Premium seats",ylab="Price in $"),col=c("blue"))
     par(mfrow=c(1,1),col=c("blue"))
     
   })
   output$pf2<-renderPlot({
     par(mfrow=c(1,2))
     with(air.df, boxplot(air.df$WIDTH_ECONOMY,main="Width of Economy seats",ylab="Width in inches"))
     with(air.df, boxplot(air.df$WIDTH_PREMIUM,main="width of Premium seats",ylab="Width in inches"))
     par(mfrow=c(1,1))
   })
   output$pf3<-renderPlot({
     par(mfrow=c(1,2))
     with(air.df, boxplot(air.df$WIDTH_ECONOMY,main="Width of Economy seats",ylab="Width in inches"))
     with(air.df, boxplot(air.df$WIDTH_PREMIUM,main="width of Premium seats",ylab="Width in inches"))
     par(mfrow=c(1,1))
     
   })
   output$pf4<-renderPlot({
     par(mfrow=c(1,2))
     with(air.df, boxplot(air.df$SEATS_ECONOMY,main="No. of Economy seats",ylab="Count"))
     with(air.df, boxplot(air.df$SEATS_PREMIUM,main="No. of Premium seats",ylab="Count"))
     par(mfrow=c(1,1))
   })
   output$tpr<-renderValueBox({
     library(formattable)
     lad <-mean(delay.df$Late_Aircraft_Delay, na.rm=TRUE)
     
     valueBox(
       percent(lad/100)
       ,subtitle = "Late Aircraft Delay"
       ,color = 'light-blue'
       ,icon = icon("", lib= 'font-awesome')
     )
     #paste("HelloT",table(avf.df$Rank,avf.df$Aname))
   })
   output$cdel<-renderValueBox({
     library(formattable)
     cdel_rate_value <-mean(delay.df$Carrier_Delay, na.rm=TRUE)
     valueBox(
       percent(cdel_rate_value/100)
       ,subtitle = "Carrier Delay"
       ,color = 'purple'
       ,icon = icon("", lib= 'font-awesome')
     )
     #paste("HelloT",table(avf.df$Rank,avf.df$Aname))
   })
   output$nasdel<-renderValueBox({
     
     library(formattable)
     nas_rate_value <-mean(delay.df$NAS_Delay, na.rm=TRUE)
     
     valueBox(
       percent(nas_rate_value/100)
       ,subtitle = "NAS Delay"
       ,color = 'lime'
       ,icon = icon("", lib= 'font-awesome')
     )
     #paste("HelloT",table(avf.df$Rank,avf.df$Aname))
   })

})
