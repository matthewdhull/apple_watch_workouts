library(XML)
library(dplyr)
library(tidyverse)
library(xts)
library(zoo)
library(spacetime)
library(lubridate)
library(RColorBrewer)
library(viridis)
library(mice)
library(reshape2)
require(gridExtra)

### LOAD 
setwd("/Users/matthewhull/r/apple_watch_workouts")
xml <- xmlParse("export.xml")
wkdf <- XML:::xmlAttrsToDataFrame(xml["//Workout"])
rdf <- XML:::xmlAttrsToDataFrame(xml["//Record"])
hdf <- subset.data.frame(rdf, type=="HKQuantityTypeIdentifierHeartRate")

# convert time/date to POSIXct
hdf$posixStartDate <- as.POSIXct(hdf$startDate)
hdf$posixEndDate <- as.POSIXct(hdf$endDate)
hdf$posixCreationDate <- as.POSIXct(hdf$creationDate)

wkdf$posixStartDate <- as.POSIXct(wkdf$startDate)
wkdf$posixEndDate <- as.POSIXct(wkdf$endDate)
wkdf$posixCreationDate <- as.POSIXct(wkdf$creationDate)

# create intervals for workout times so we can match heart rates
wkdf$interval <- interval(wkdf$posixStartDate, wkdf$posixEndDate)
wkdf$tDistance <- as.character(wkdf$totalDistance)
wkdf$tDistance <- as.numeric(wkdf$tDistance)
wkdf$totalDistance <- wkdf$tDistance
wkdf$tDistance <- NULL

# remove duplicates, 0 duration
wkdf <- wkdf[!duplicated(wkdf$posixStartDate), ]
wkdf <- subset(wkdf, duration!=0)
# tag ids to each workout to allow membership matching in heart rate df
wkdf$id <- seq(1, nrow(wkdf))
wkdf$id <- as.factor(wkdf$id)


hdf$workout_id <- NA
# column for the day only
hdf$day <- format(hdf$posixStartDate, "%y-%m-%d")
wkdf$day <- format(wkdf$posixStartDate, "%y-%m-%d")

# subset HR only on days that a workout occured
workout_hdf <- subset.data.frame(hdf, day %in% wkdf$day)

b <- which(workout_hdf$posixStartDate %within% wkdf$interval)
workout_hr <- workout_hdf[b,]

# add workout ids to hr values within time interval of workout record
# needs vectorization
for (i in 1:nrow(workout_hr)) {
  for (j in 1:nrow(wkdf)){
    if (workout_hr[i,10] %within% wkdf[j,16]){
      workout_hr[i,13] <- wkdf[j,17]
    }
  }
}



# factor ids, then group by id, calculate mean hr
workout_hr$workout_id <- as.factor(workout_hr$workout_id)
workout_hr$hr_value <- as.character(workout_hr$value)
workout_hr$hr_value <- as.integer(workout_hr$hr_value)
avg.hr <- aggregate(hr_value ~ workout_id, data=workout_hr, FUN=mean)

# duration to numeric
wkdf$tDuration <- as.character(wkdf$duration)
wkdf$tDuration <- as.numeric(wkdf$tDuration)
wkdf$duration <- wkdf$tDuration
wkdf$tDuration <- NULL


# totalEnergyBurned to numeric
wkdf$totalEnergyBurned <- as.character(wkdf$totalEnergyBurned)
wkdf$totalEnergyBurned <- as.numeric(wkdf$totalEnergyBurned)

#merge & sort
merged_df <- merge.data.frame(x=wkdf, y=workout_hr, by.x="id", by.y="workout_id", all.x=TRUE)
merged_df <- merged_df[,c(1:10,14,28,15,29,16,30,17,18,20:21,30:32)]
merged_df <- merged_df[merged_df$workoutActivityType != "HKWorkoutActivityTypeRowing",]
merged_df <- arrange(merged_df, posixStartDate.x)

merged_df$workoutType <- NA
merged_df$workoutType[merged_df$workoutActivityType == "HKWorkoutActivityTypeRunning"] <- "running"
merged_df$workoutType[merged_df$workoutActivityType == "HKWorkoutActivityTypeSwimming"] <- "swimming"
merged_df$workoutType[merged_df$workoutActivityType == "HKWorkoutActivityTypeCycling"] <- "cycling"
merged_df$workoutType[merged_df$workoutActivityType == "HKWorkoutActivityTypeElliptical"] <- "elliptical"
merged_df$workoutType[merged_df$workoutActivityType == "HKWorkoutActivityTypeOther"] <- "other"
merged_df$workoutType[merged_df$workoutActivityType == "HKWorkoutActivityTypeWalking"] <- "walking"

df <- merged_df




# plot

# match app logo colors, darker colors for points
# used this site to darken colors:
# http://www.simonbattersby.com/blog/hex-to-rgb-rgb-to-hex-and-colour-shade-selector/
nike_color <- rgb(204/255, 255/255, 54/255)
nike_color_dark <- "#a3cc2b"
strava_color <- rgb(248/255,52/255,9/255)
strava_color_dark <- "#c62a07"
apple_watch_color <- rgb(223/255,223/255,223/255)
apple_watch_color_dark <- "#b2b2b2"
app_colors <- c(apple_watch_color, nike_color, strava_color)
app_colors_dark <- c(apple_watch_color_dark, nike_color_dark, strava_color_dark)


# zissou palette
z_blue <- rgb(18/255,138/255,206/255)
z_darkblue <- rgb(21/255,37/255,88/255)
z_red <- rgb(238/255,51/255,22/255)
z_gray <- rgb(182/255,195/255,198/255)
  


ggplot(df, aes(x=sourceName.x,y=totalDistance)) +
  geom_jitter(aes(color=sourceName.x), alpha=.75, show.legend = F) +
  scale_color_manual(values=app_colors_dark) +
  geom_boxplot(fill=app_colors, alpha=.7) +  
  stat_summary(geom="text", fun.y=median,
               aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.5), size=3.5) +    
  theme_bw() +
  scale_x_discrete(labels=c("Apple Workout", "Nike+ Run Club", "Strava")) +  
  xlab("Workout App") +
  ylab("Distance (mi)") +
  ggtitle("Workout Distance per App")
  

# workout duration distribution by workout type
# rowing & swimming are too small to consider
ggplot(subset(df, ! workoutType %in% c("swimming")), aes(x=duration, y=..count..)) + 
  geom_histogram(aes(fill=workoutType), color='gray28', binwidth = 7, alpha=.75, show.legend = F) +
  scale_fill_viridis(discrete = T) +
  facet_wrap(~ workoutType) +
  xlab("Duration (min)") +
  ggtitle("Workout Duration by Workout Type")


# Heart Rate Density by workout Type
ggplot(subset(df,!is.na(hr_value)), aes(hr_value,y=..density..)) +
  geom_density(aes(fill=workoutType),alpha=.7, show.legend=F) +
  facet_wrap(~ workoutType) +
  scale_fill_viridis(discrete=T) +
  xlab("Heart Rate") +
  ggtitle("Heart Rate Density by Workout Type") 
  
  
  
# fit a model to predict missing HR values
# taken from http://www.stat.columbia.edu/~gelman/arm/missing.pdf
  
hr_df <- subset.data.frame(df, subset=is.na(workoutType)==F, select=c(workoutType,duration,totalDistance,totalEnergyBurned,sourceName.x,hr_value))

lm.imputation <- lm(hr_value ~ workoutType + duration + totalDistance + totalEnergyBurned + sourceName.x, data=hr_df)

pred.hr <- predict(lm.imputation, hr_df)

impute <- function (a, a.impute){ 
  ifelse (is.na(a), a.impute, a)
}

hr.imp <- impute(df$hr_value, pred.hr)

df$imputed_hr <- round(hr.imp,0)

# deterministic imputation of hr
p1 <- ggplot(mapping=aes(x=pred.hr, y=df$imputed_hr)) + 
  geom_point(alpha=.4) +  
  ylab("Imputed HR") +
  xlab("Regression Prediction") +
  ggtitle("Deterministic Imputation of HR Variable")

# Heart Rate Density by workout Type
p3 <- ggplot(df, aes(y=..density..)) +
  geom_density(aes(x=imputed_hr, fill="imputed"), alpha=.5) +  
  geom_density(aes(x=hr_value, fill="measured"), alpha=.7,na.rm=T) +
  scale_fill_manual(values=c(rgb(28/255,112/255,107/255), rgb(252/255,230/255,30/255))) +  
  facet_wrap(~ workoutType) +
  xlab("Heart Rate") +
  ggtitle("Heart Rate Density by Workout Type, Deterministic Imputation") +
  guides(fill=guide_legend(title="HR Data Source"))

# random imputation of hr

pred.random.hr <- rnorm(nrow(hr_df), predict(lm.imputation, hr_df))
n_missing <- length(df$hr_value[is.na(df$hr_value)])
draw <- sample(pred.random.hr, n_missing)
random.hr.imp <- impute(df$hr_value, draw)
df$random_imputed_hr <- round(random.hr.imp,0)

p2 <- ggplot(mapping=aes(x=pred.random.hr, y=df$random_imputed_hr)) + 
  geom_point(alpha=.4) +
  ylab("Imputed HR") +
  xlab("Regression Prediction") +
  ggtitle("Random Imputation of HR Variable")

p4 <- ggplot(df, aes(y=..density..)) +
  geom_density(aes(x=random_imputed_hr, fill="R-imputed"), alpha=.5) +  
  geom_density(aes(x=hr_value, fill="measured"), alpha=.7,na.rm=T) +
  scale_fill_manual(values=c(rgb(28/255,112/255,107/255), rgb(252/255,230/255,30/255))) +  
  facet_wrap(~ workoutType) +
  xlab("Heart Rate") +
  ggtitle("Heart Rate Density by Workout Type, Random Imputation") +
  guides(fill=guide_legend(title="HR Data Source"))

# compare both imputation methods
grid.arrange(p1,p2,p3,p4)


final_df <- df[,c(1,3,5,7,9,26,24)]
colnames(final_df) <- c("id","duration","distance","energy","source","hr","activity")
# categorical to discrete numeric
# from: https://medium.com/data-design/visiting-categorical-features-and-encoding-in-decision-trees-53400fa65931
# 

final_df$source <- as.numeric(final_df$source)
# originally making factors of all workout types but wanted to only differentiate
# between a workout of 'other' vs. any other type
# final_df$activity <- as.numeric(as.factor(final_df$activity))
final_df$activity[final_df$activity!='other'] <- 1
final_df$activity[final_df$activity=='other'] <- 0
final_df$activity <- as.numeric(final_df$activity)

save(final_df, file="apple_health_data.Rdata")
write_csv(final_df, "apple_health_data.csv")

  
  