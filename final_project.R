# Downloading data
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "stormdata.csv.bz2", method = "curl")

#reading data into a file in R
x <- read.csv("stormdata.csv.bz2")

# studying impact on population health 
# two relevant variables -  FATALITIES and INJURIES, 

library(dplyr)
library(ggplot2)

# step 1 : trying to find max fatality event types 
eventwise_fat <- with(x, tapply(FATALITIES, EVTYPE, sum))
eventwise_inj <- with(x, tapply(INJURIES, EVTYPE, sum))
eventwise_prop <- with(x, tapply(PROPDMG, EVTYPE, sum))
eventwise_crop <- with(x, tapply(CROPDMG, EVTYPE, sum))
eventwise_fat <- eventwise_fat[order(eventwise_fat, decreasing = TRUE)]
eventwise_inj <- eventwise_inj[order(eventwise_inj, decreasing = TRUE)]
eventwise_prop <- eventwise_prop[order(eventwise_prop, decreasing = TRUE)]
eventwise_crop <- eventwise_crop[order(eventwise_crop, decreasing = TRUE)]

# step 2 : selecting events for detailed analysis 
# Exploratory data analysis 
par(mfrow = c(1, 4))
boxplot(eventwise_fat, main = "fatalities")
boxplot(eventwise_inj, main = "injuries")
boxplot(eventwise_prop, main = "property damage")
boxplot(eventwise_crop, main = "crop damage")
#dev.off()
tail(quantile(eventwise_fat, prob = seq(0, 1, length.out = 985)), 5)
tail(quantile(eventwise_inj, prob = seq(0, 1, length.out = 985)), 5)
tail(quantile(eventwise_crop, prob = seq(0, 1, length.out = 985)), 5)
tail(quantile(eventwise_prop, prob = seq(0, 1, length.out = 985)), 5)


# step 3 : Selecting significant events
maxfat_events <- names(eventwise_fat[1:10])
maxinj_events <- names(eventwise_inj[1:10])
x_final <- subset(x, EVTYPE %in% c(maxfat_events, maxinj_events))


#  step 4: new dataset for plotting 
l <- length(x_final$FATALITIES)
effect_count <- c(x_final$FATALITIES, x_final$INJURIES)
effect_type <- c(rep("FATALITIES", l), rep("INJURIES", l))
#x_final <- as.character(x_final$EVTYPE)
event_type <- c(x_final$EVTYPE, x_final$EVTYPE)
x3 <- data.frame(Effect_count = effect_count, Effect_type = effect_type, Event_type =  event_type)

x3$Event_type <- as.factor(x3$Event_type)
levelnames <- as.numeric(levels(x3$Event_type))
levelnames <- levels(x$EVTYPE)[levelnames]
levels(x3$Event_type) <- levelnames

#plotting event wise total fatalities
g <- ggplot(x3) + geom_bar(aes(x = Event_type, y = Effect_count, fill = Event_type), stat = "identity")
g <- g + facet_wrap( ~ Effect_type, scales = "free")
g <- g + theme(axis.text.x = element_blank())
g <- g + xlab("Event Types") + ylab("Count of human incidents")
g


# plotting property damanges

# step 5 : Selecting significant events
maxprop_events <- names(eventwise_prop[1:10])
maxcrop_events <- names(eventwise_crop[1:10])
x_final2 <- subset(x, EVTYPE %in% c(maxprop_events, maxcrop_events))
l <- length(x_final2$PROPDMG)

#  step 6: new dataset for plotting 
effect_count <- c(x_final2$PROPDMG, x_final2$CROPDMG)
effect_type <- c(rep("PROPERTY DAMAGE", l), rep("CROP DAMAGE", l))
event_type <- c(x_final2$EVTYPE, x_final2$EVTYPE)
x3 <- data.frame(Effect_count = effect_count, Effect_type = effect_type, Event_type =  event_type)

x3$Event_type <- as.factor(x3$Event_type)
levelnames <- as.numeric(levels(x3$Event_type))
levelnames <- levels(x$EVTYPE)[levelnames]
levels(x3$Event_type) <- levelnames

#plotting event wise total damage
g <- ggplot(x3) + geom_bar(aes(x = Event_type, y = Effect_count, fill = Event_type), stat = "identity")
g <- g + facet_wrap( ~ Effect_type, scales = "free")
g <- g + theme(axis.text.x = element_blank())
g <- g + xlab("Event Types") + ylab("Damage")
g

