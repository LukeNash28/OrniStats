library(readxl)
library(dplyr)
library(kableExtra)
library(DT)
library(forecast)
library(ggplot2)
library(tidyverse)
library(caret)
library(lubridate)
library(pracma)
library(tidyr)
library(nortest)
library(MASS)
library(KScorrect)
install.packages("semEff")

AMROBDates = read.csv("/Users/lukenash1/Documents/OrniStats/Private/American Robin/AMROB Year Count.csv")
AMROBDates = subset(AMROBDates, discovery_year != '2023')

etsAMROB <- ets(AMROBDates$record_count)
arimaAMROB <- auto.arima(AMROBDates$record_count)

etsForecast <- forecast(etsAMROB, h=5, level = c(50,80))
arimaForecast <- forecast(arimaAMROB, h=5, level = c(50,80))

fcextract <- function(fcast){
  dflo <- data.frame(fcast$lower)
  dfhi <- data.frame(fcast$upper)
  dfcastn <- data.frame(fcast$mean, dflo$X50., dfhi$X50.,dflo$X80., dfhi$X80.)
  names(dfcastn)<- c('forecast','lo50', 'hi50', 'lo80', 'hi80')
  return(dfcastn)
}

pd<-fcextract(arimaForecast)
pd$date<-c('2023','2024','2025','2026','2027')
pd[pd<0] = 0
pd

ggplot(pd, aes(x=as.numeric(date), y=forecast))+
  geom_line(data=AMROBDates,aes(x=as.numeric(discovery_year),y=as.numeric(record_count)),color="red",linewidth=1.5)+
  geom_ribbon(aes(ymin=lo80, ymax=hi80,group=1), alpha=.50, fill='pink')+
  geom_ribbon(aes(ymin=lo50, ymax=hi50,group=1), alpha=.25, fill="#AA336A")+
  geom_line(aes(group=1), color='red', linewidth = 1.5)+
  theme_classic()+
  theme(legend.position = 'none')+
  xlab("Year")+
  ylab("Number of records")

ggsave("AMROB Forecast Graph.jpg")

ggplot(AMROBDates, aes(x= discovery_year, y=record_count))+
  geom_bar(stat="identity",color="black", fill="red")+
  geom_smooth(aes(x=discovery_year, y = record_count), se=FALSE, method="loess", color="black")+
  theme_classic()+
  xlab("Year")+
  ylab("Number of records")

set.seed(101)
train.data = AMROBDates[1:68, ]
test.data = AMROBDates[69:73, ]

rmse_list <- c()
r2_list <- c()

for (deg in 1:10) {
  model <- lm(data = train.data, formula= record_count ~ poly(discovery_year, deg,raw=TRUE))
  predictions <- model %>% predict(test.data)
  RMSE_val <- RMSE(predictions, test.data$record_count)
  print(RMSE_val)
  R2_val <- R2(predictions, test.data$record_count)
  print(R2_val)
}

final_model <- lm(data = train.data, formula = record_count ~ poly(discovery_year, 1, raw=TRUE))
summary(final_model)

ggplot(AMROBDates, aes(discovery_year, record_count))+
  geom_bar(stat = "identity", fill="red", colour = "black")+
  geom_smooth(method = lm, se=FALSE, formula = y ~ poly(x, 1, raw=TRUE), color = "black")+
  theme_classic()+
  xlab("Year")+
  ylab("Number of records")+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))

ggsave("AMROB Trend Plot.jpg")

AMROBArrivals <- read.csv("/Users/lukenash1/Documents/OrniStats/Private/American Robin/AMROB Arrival by Location.csv")
AMROBArrivals <- subset(AMROBArrivals, discovery != "NULL")
AMROBArrivals <- AMROBArrivals[-22, ]
AMROBArrivals$discovery <- as.Date(AMROBArrivals$discovery, format="%d/%m/%Y")

AMROBArrivalsLoc <- AMROBArrivals
AMROBArrivalsLoc$discovery <- update(AMROBArrivalsLoc$discovery, year = 2023)

for (i in seq_along(AMROBArrivalsLoc$discovery)) {
  if (AMROBArrivalsLoc$discovery[i] > as.Date("2023-07-31")) {
  AMROBArrivalsLoc$discovery[i] <- update(AMROBArrivalsLoc$discovery[i], year=2022)
  }
}

AMROBArrivalsLoc <- AMROBArrivalsLoc[order(AMROBArrivalsLoc$discovery), ]
AMROBArrivalsLoc$rolling_meanLat = movavg(AMROBArrivalsLoc$lat, n=7, type="e")
AMROBArrivalsLoc$rolling_meanLong = movavg(AMROBArrivalsLoc$long, n=7, type="e")
AMROBArrivalsLoc

ggplot(AMROBArrivalsLoc, aes(x=discovery, y=lat))+
  geom_point(color="red", size=3)+
  geom_smooth(aes(x=discovery,y=rolling_meanLat), method="loess", se=FALSE, colour="black")+
  theme_classic()+
  xlab("Discovery Date")+
  ylab("Latitude")+
  scale_x_date(date_labels = "%B")

ggsave("/Users/lukenash1/Documents/OrniStats/Private/AMROB Arrival by Latitude.jpg")

ggplot(AMROBArrivalsLoc, aes(x=discovery, y=abs(long)))+
  geom_point(color="red", size=3)+
  geom_smooth(aes(x=discovery,y=rolling_meanLong), method="loess", se=FALSE, colour="black")+
  theme_classic()+
  xlab("Discovery Date")+
  ylab("Degrees west of Greenwich Meridian")

ggsave("/Users/lukenash1/Documents/OrniStats/Private/AMROB Arrival by Longitude.jpg")

AMROBArrivals$rolling_meanDate = movavg(yday(AMROBArrivals$discovery), n=7, type="e")

ggplot(AMROBArrivals, aes(discovery, yday(discovery)))+
  geom_point(color="red", size = 3)+
  theme_classic()+
  xlab("Year")+
  ylab("Day of Year of Discovery")

ggsave("/Users/lukenash1/Documents/OrniStats/Private/AMROB Overall Arrival over Time.jpg")

AMROBEvol = subset(AMROBArrivals, 200 < yday(discovery) & yday(discovery) < 350)
AMROBEvol$rolling_meanDate = movavg(yday(AMROBEvol$discovery), n=7, type="e")

ggplot(AMROBEvol, aes(discovery, yday(discovery)))+
  geom_point(color="red", size = 3)+
  geom_smooth(aes(discovery,rolling_meanDate), method="loess", se=FALSE, colour="black")+
  theme_classic()+
  xlab("Year")+
  ylab("Day of Year of Discovery")

ggsave("/Users/lukenash1/Documents/OrniStats/Private/AMROB Autumn Arrival Over Time.jpg")

AMROBArrivals$rolling_meanLat = movavg(AMROBArrivals$lat, n=7, type="e")
AMROBArrivals$rolling_meanLong = movavg(AMROBArrivals$long, n=7, type="e")

ggplot(AMROBArrivals, aes(discovery, lat))+
  geom_point(color="red", size=3)+
  geom_smooth(aes(x=discovery,y=rolling_meanLat), method="loess", se=FALSE, colour="black")+
  theme_classic()+
  xlab("Year")+
  ylab("Latitude")

ggsave("/Users/lukenash1/Documents/OrniStats/Private/AMROB Latitude Over Time.jpg")

ggplot(AMROBArrivals, aes(discovery, abs(long)))+
  geom_point(color="red", size=3)+
  geom_smooth(aes(x=discovery, y=abs(rolling_meanLong)), method="loess", se=FALSE, colour="black")+
  theme_classic()+
  xlab("Year")+
  ylab("Degrees west of the Greenwich Meridian")

ggsave("/Users/lukenash1/Documents/OrniStats/Private/AMROB Longitude Over Time.jpg")

AMROBClusters = read.csv("/Users/lukenash1/Documents/OrniStats/Private/American Robin/AMROB Cluster Table.csv")
AMROBClusters$date <- AMROBArrivals$discovery

SIrishClust = subset(AMROBClusters, labels == 4)
clusterZero = subset(AMROBClusters, labels == 0 & discovery != 18)

clusterZero$area <- ifelse(!(clusterZero$X %in% c(13, 35)), "Ireland", "Hebs")
SIrishClust$area <- rep("S Irish Sea", 14)
clusterPlotting <- rbind(SIrishClust, clusterZero)

ggplot(clusterPlotting, aes(area, discovery, color=area, group=area))+
  geom_jitter(width=0, alpha = 1.5, size=2)+
  scale_color_manual(values=c("red","orange", "blue"))+
  theme_classic()+
  labs(x="Cluster Area", y="Date of Discovery (days after August 1st)")+
  theme(legend.position = "none")+
  scale_x_discrete(labels=c("Outer Hebrides", "Western Ireland", "SW England and Wales"))

testFrame <- subset(clusterPlotting, area != "Hebs")
t.test(discovery~area, data=testFrame)

ggsave("/Users/lukenash1/Documents/OrniStats/Private/American Robin/AMROB Cluster Graph.jpg")

AMROBlenStay <- AMROB_table
AMROBlenStay$Date.of.Discovery <- as.Date(AMROBlenStay$Date.of.Discovery, format = "%d/%m/%Y")
AMROBlenStay$Date.of.Discovery <- update(AMROBlenStay$Date.of.Discovery, year = 2023)
AMROBlenStay <- subset(AMROBlenStay, !is.na(Date.of.Discovery))

for (i in seq_along(AMROBlenStay$Date.of.Discovery)) {
  if (AMROBlenStay$Date.of.Discovery[i] > as.Date("2023-07-31")) {
    AMROBlenStay$Date.of.Discovery[i] <- update(AMROBlenStay$Date.of.Discovery[i], year=2022)
  }
}

AMROBlenStay$rolling_meanStay = movavg(AMROBlenStay$Stay.Length, n=7, type="e")

AMROBStay <- AMROBlenStay %>%
  group_by(Stay.Length) %>%
  summarise(count = n())

ggplot(AMROBlenStay, aes(Date.of.Discovery, Stay.Length))+
  geom_point(color="red", size=2)+
  theme_classic()

ggsave("/Users/lukenash1/Documents/OrniStats/Private/American Robin/AMROB Stay over Seasons.jpg")

ggplot(AMROBStay, aes(Stay.Length, count))+
  geom_bar(stat="identity", position="dodge", color="black", fill="red")+
  geom_smooth(method="loess", se=FALSE, color="black")+
  theme_classic()+
  labs(x="Length of stay (days)", y="Frequency")+
  scale_y_continuous(n.breaks=12)

ggsave("/Users/lukenash1/Documents/OrniStats/Private/American Robin/AMROB Length of Stay.jpg")

lenStay <- AMROBStay$count
fit1
LcKS(lenStay, "pexp")

AMROBArrivals$rolling_meanStay = movavg(AMROBArrivals$stay_length, n=7, type="e")

ggplot(AMROBArrivals, aes(discovery, stay_length))+
  geom_point(color="red", size=2)+
  geom_smooth(aes(discovery,rolling_meanStay), method="loess", se=FALSE, color="black")+
  theme_classic()+
  labs(x="Year",y="Length of stay (days)")

ggsave("/Users/lukenash1/Documents/OrniStats/Private/American Robin/AMROB Stay Over Time.jpg")

AMROBArrivalsLat = AMROBArrivals[order(AMROBArrivals$lat), ]
AMROBArrivalsLat$rolling_meanStay = movavg(AMROBArrivalsLat$stay_length, n=7, type="e")

ggplot(AMROBArrivalsLat, aes(abs(lat), stay_length))+
  geom_point(color='red', size=2)+
  geom_smooth(aes(abs(lat), rolling_meanStay), method="loess", se=F, color="black")+
  theme_classic()+
  labs(x="Latitude", y="Length of stay (days)")

ggsave("/Users/lukenash1/Documents/OrniStats/Private/American Robin/AMROB Stay Over Latitude.jpg")

AMROBArrivalsLong = AMROBArrivals[order(AMROBArrivals$long), ]
AMROBArrivalsLong$rolling_meanStay = movavg(AMROBArrivalsLong$stay_length, n=7, type="e")

ggplot(AMROBArrivalsLong, aes(abs(long), stay_length))+
  geom_point(color='red', size=2)+
  geom_smooth(aes(abs(long), rolling_meanStay), method="loess", se=F, color="black")+
  theme_classic()+
  labs(x="Degrees west of Greenwich Meridian", y="Length of stay (days)")

AMROBTest = AMROBArrivalsLong[-c(14,35)]
train.data = AMROBTest[1:30, ]
test.data = AMROBTest[31:34, ]

for (deg in 1:10) {
  model <- lm(data = train.data, formula= stay_length ~ poly(abs(long), deg,raw=TRUE))
  predictions <- model %>% predict(test.data)
  RMSE_val <- RMSE(predictions, test.data$stay_length)
  print(RMSE_val)
  R2_val <- R2(predictions, test.data$stay_length)
  print(R2_val)
}

final_model <- lm(data = AMROBTest, formula = stay_length ~ poly(abs(long), 3, raw=T))
summary(final_model)

ggplot(AMROBArrivalsLong, aes(abs(long), stay_length))+
  geom_point(color='red', size=2)+
  geom_smooth(aes(abs(long), rolling_meanStay),method="loess",se=FALSE,color='black')+
  theme_classic()+
  labs(x="Degrees west of Greenwich Meridian", y="Length of stay (days)")

ggsave("/Users/lukenash1/Documents/OrniStats/Private/American Robin/AMROB Stay Over Longitude.jpg")

AMROBArrivalsDay <- AMROBArrivals
AMROBArrivalsDay$discovery <- as.Date(AMROBArrivalsDay$discovery)
AMROBArrivalsDay$discovery <- update(AMROBArrivalsDay$discovery, year = 2023)

for (i in seq_along(AMROBArrivalsDay$discovery)) {
  if (AMROBArrivalsDay$discovery[i] > as.Date("2023-07-31")) {
    AMROBArrivalsDay$discovery[i] <- update(AMROBArrivalsDay$discovery[i], year=2022)
  }
}

AMROBArrivalsDay$rolling_meanStay = movavg(AMROBArrivalsDay$stay_length, n=7, type="e")

ggplot(AMROBArrivalsDay, aes(discovery, stay_length))+
  geom_point(color='red', size=2)+
  theme_classic()+
  labs(x="Month", y="Length of stay (days)")

ggsave("/Users/lukenash1/Documents/OrniStats/Private/American Robin/AMROB Stay Over Year Day.jpg")

model <- lm(data = AMROBArrivalsDay, formula = stay_length ~ poly(discovery, 2, raw=T))
summary(model)

as.logical("No")

McLeod.Li.test(arimaAMROB, AMROBDates$record_count)
install.packages("TSA")
library(TSA)

AMROB <- subset(OrniStatsRecords, Qualifier == "AMROB")

returnsList = c()

for (num in 1:max(AMROB$Record.No.)) {
  recordsSep <- subset(AMROB, Record.No. == num)
  siteList <- c()
  returnCounter <- 0
  
  for (i in 1:nrow(recordsSep)){
    subsetValue <- recordsSep$Subset[i]
    site <- recordsSep$Site[i]
    
    if (length(subsetValue) > 0 && !is.na(subsetValue)) {
      if (grepl("/", site)) {
        sites <- unlist(strsplit(site, "/"))
      }
      
      for (loc in sites) {
          if (loc %in% siteList) {
            returnCounter <- returnCounter + 1
            break
          } else {
            siteList <- c(siteList, loc)
          }
        }
      }
  }
  returnsList[num] <- returnCounter
}

OrniStatsRecords = data.frame(read_excel("/Users/lukenash1/Documents/OrniStats/Public/Rare Bird Data - Main.xlsx", sheet = "Accepted Records"))
AMROB_table = data.frame(subset(OrniStatsRecords, Qualifier == "AMROB" & "Year of Discovery" != 2023))

