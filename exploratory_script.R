library(readr)
library(dplyr)
library(lubridate)


if (!exists("storm_data")){
        storm_data <- read_csv("repdata-data-StormData.csv")
        storm_data$BGN_DATE <- mdy_hms(storm_data$BGN_DATE)
}
health_storm_data <- NULL
grouped_by_EVTYPE <- NULL
sum_health <- NULL
arrange_data <- NULL

health_storm_data <- storm_data %>% filter(as.Date(BGN_DATE) > as.Date("2001/01/01")) %>% select(EVTYPE, FATALITIES, INJURIES)
health_storm_data$EVTYPE <- ifelse(health_storm_data$EVTYPE=="HIGH WIND", "WIND", health_storm_data$EVTYPE)
health_storm_data$EVTYPE <- ifelse(health_storm_data$EVTYPE=="TSTM WIND", "THUNDERSTORM WIND", health_storm_data$EVTYPE)
health_storm_data$EVTYPE <- ifelse(health_storm_data$EVTYPE=="HURRICANE/TYPHOON", "HURRICANE", health_storm_data$EVTYPE)
health_storm_data$EVTYPE <- ifelse(health_storm_data$EVTYPE=="EXCESSIVE HEAT", "HEAT", health_storm_data$EVTYPE)
health_storm_data$EVTYPE <- ifelse(health_storm_data$EVTYPE=="TSTM WIND/HAIL", "WIND-HAIL", health_storm_data$EVTYPE)
health_storm_data$EVTYPE <- ifelse(health_storm_data$EVTYPE=="WILD/FOREST FIRE", "WILDFIRE", health_storm_data$EVTYPE)
health_storm_data$EVTYPE <- ifelse(health_storm_data$EVTYPE=="URBAN/SML STREAM FLD", "URBAN SML STREAM FLD", health_storm_data$EVTYPE)
health_storm_data$EVTYPE <- ifelse(health_storm_data$EVTYPE=="RECORD HEAT", "HEAT", health_storm_data$EVTYPE)
health_storm_data$EVTYPE <- ifelse(health_storm_data$EVTYPE=="WND", "WIND", health_storm_data$EVTYPE)
health_storm_data$EVTYPE <- ifelse(health_storm_data$EVTYPE=="STRONG WIND", "WIND", health_storm_data$EVTYPE)

health_storm_data <- health_storm_data %>% mutate(fat_inj_sum = FATALITIES + INJURIES)
grouped_by_EVTYPE <- health_storm_data %>% group_by(EVTYPE)
sum_health <- summarise(grouped_by_EVTYPE, Sum = sum(fat_inj_sum))
arrange_data <- arrange(sum_health, desc(Sum))
top10 <- arrange_data[1:10,]
par(mai=c(1.7,1.6,0.82,0.42)+.1, mgp=c(5.2,0.5,0))
arplot(top10$Sum, names.arg=top10$EVTYPE, las=2, cex.names =  .5, xlab = "Weather Events", 
       ylab = "Fatalities & Injuries", main = "Injuries & Fatalities from \nTop 10 Weather Events 2001 - 2011")