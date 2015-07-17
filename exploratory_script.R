library(readr)
library(dplyr)
library(lubridate)


storm_data <- read_csv("repdata-data-StormData.csv", col_types = c("ccccccccccccccccccccccccccccccccccccc"))
##storm_data <- read.csv2("repdata-data-StormData.csv", sep = ",")
storm_data$BGN_DATE <- mdy_hms(storm_data$BGN_DATE)
storm_data$FATALITIES <- as.numeric(storm_data$FATALITIES)
storm_data$INJURIES <- as.numeric(storm_data$INJURIES)
storm_data$PROPDMG <- as.numeric(storm_data$PROPDMG)
storm_data$PROPDMGEXP <- as.character(storm_data$PROPDMGEXP)
storm_data$CROPDMG <- as.numeric(storm_data$CROPDMG)
storm_data$CROPDMGEXP <- as.character(storm_data$CROPDMGEXP)
storm_data$EVTYPE <- as.character(storm_data$EVTYPE)

storm_data$EVTYPE <- ifelse(storm_data$EVTYPE=="HIGH WIND", "WIND", storm_data$EVTYPE)
storm_data$EVTYPE <- ifelse(storm_data$EVTYPE=="TSTM WIND", "THUNDERSTORM WIND", storm_data$EVTYPE)
storm_data$EVTYPE <- ifelse(storm_data$EVTYPE=="HURRICANE/TYPHOON", "HURRICANE", storm_data$EVTYPE)
storm_data$EVTYPE <- ifelse(storm_data$EVTYPE=="EXCESSIVE HEAT", "HEAT", storm_data$EVTYPE)
storm_data$EVTYPE <- ifelse(storm_data$EVTYPE=="TSTM WIND/HAIL", "WIND-HAIL", storm_data$EVTYPE)
storm_data$EVTYPE <- ifelse(storm_data$EVTYPE=="WILD/FOREST FIRE", "WILDFIRE", storm_data$EVTYPE)
storm_data$EVTYPE <- ifelse(storm_data$EVTYPE=="URBAN/SML STREAM FLD", "URBAN SML STREAM FLD", storm_data$EVTYPE)
storm_data$EVTYPE <- ifelse(storm_data$EVTYPE=="RECORD HEAT", "HEAT", storm_data$EVTYPE)
storm_data$EVTYPE <- ifelse(storm_data$EVTYPE=="WND", "WIND", storm_data$EVTYPE)
storm_data$EVTYPE <- ifelse(storm_data$EVTYPE=="STRONG WIND", "WIND", storm_data$EVTYPE)


dmg_exponent <- sort(unique(c(storm_data$PROPDMGEXP, storm_data$CROPDMGEXP)))
dmg_multipler <- c(0,0,0,0,1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000, 1000000000, 100, 100, 1000, 1000000, 1000000)
dmg_multipler_df = data.frame(dmg_exponent, dmg_multipler)

storm_data <- storm_data %>% 
        mutate(CROPDMG_MULT = dmg_multipler_df$dmg_multipler[match(CROPDMGEXP, dmg_multipler_df$dmg_exponent)])
storm_data <- storm_data %>% 
        mutate(PROPDMG_MULT = dmg_multipler_df$dmg_multipler[match(PROPDMGEXP, dmg_multipler_df$dmg_exponent)])

storm_data$PROPDMG <- storm_data$PROPDMG * storm_data$PROPDMG_MULT
storm_data$CROPDMG <- storm_data$CROPDMG * storm_data$CROPDMG_MULT

storm_data <- storm_data %>% mutate(ECONDMG=PROPDMG + CROPDMG)

## DO NOT INCLUDE IN Rmd DOCUMENT!!
health_storm_data <- NULL
grouped_by_EVTYPE <- NULL
sum_health <- NULL
arrange_data <- NULL

health_storm_data <- storm_data %>% 
        filter(as.Date(BGN_DATE) > as.Date("2001/01/01")) %>% 
        select(EVTYPE, FATALITIES, INJURIES)

health_storm_data <- health_storm_data %>% mutate(fat_inj_sum = FATALITIES + INJURIES)
grouped_by_EVTYPE <- health_storm_data %>% group_by(EVTYPE)
sum_health <- summarise(grouped_by_EVTYPE, Sum = sum(fat_inj_sum))
arrange_data <- arrange(sum_health, desc(Sum))
top10health <- arrange_data[1:10,]
par(mai=c(1.4,1.7,0.82,0.42)+.1, mgp=c(4,0.5,0))
barplot(top10health$Sum, names.arg=top10health$EVTYPE, las=2, cex.names =  .5, 
        xlab = "Injuries & Fatalities", ylab = "", 
        main = "Injuries & Fatalities from \nTop 10 Weather Events 2001 - 2011", horiz = TRUE)


economic_storm_data <- storm_data %>% 
        filter(as.Date(BGN_DATE) > as.Date("2001/01/01")) %>% 
        select(EVTYPE, ECONDMG)

grouped_by_EVTYPE_ECON <- economic_storm_data %>% group_by(EVTYPE)
sum_econ <- summarise(grouped_by_EVTYPE_ECON, Sum = sum(ECONDMG))

arrange_data_econ <- arrange(sum_econ, desc(Sum))
top10econ <- arrange_data_econ[1:10,]
par(mai=c(1.4,1.7,0.82,0.42)+.1, mgp=c(4,0.5,0))
barplot(top10econ$Sum, names.arg=top10econ$EVTYPE, las=2, cex.names =  .5, 
       xlab = "Economic Damage", ylab = "", 
       main = "Economic Damage from \nTop 10 Weather Events 2001 - 2011", horiz = TRUE)
