# sport_df output from preprocessing_fitfiles.RMD
# prototyping a single workout plot that summarizes a few stats

library(ggplot2)
library(gridExtra)
library(reshape2)


load("data/after_effects_data.Rdata")


cycle <- subset(sport_df, sourceName.x == 'Strava')
tc <- as.data.frame(cycle[,c(1,2,4,6,9:11)])

# attrs w/ similar scale 
t1 <- tc[,c(1,2,4,5)]
t1$col <- seq(1:nrow(t1))
t1 <- as.data.frame(t1)
t1n <- rownames(t1)
# complete cases only
t1 <- t1[which(complete.cases(t1)),] 

# pipeline to after effects.
library(jsonlite)
ae_df <- t1[1:5,] #just a snippet for now
ae_df$id <- ae_df$col
ae_df$col <- NULL
ae_df <- ae_df[,c(2:4)]
ae_df$totalDistance <- round(ae_df$totalDistance, 2)
colnames(ae_df) <- c("mi", "mph", "mets")
aejson <- toJSON(ae_df)
write(aejson, "data/cycle_workouts.json")


# convert the data from wide to long, 
# produces a plot of every workout on the selected mets.  
t2 <- reshape::melt.data.frame(t1, "col", measure.vars = c("duration", "totalDistance", "mph", "mets"))

# sequence below just subsets to 20 workouts, increase as needed.
t3 <- t2[t2$col %in% seq(1:20),]
ggplot(t3, aes(variable,value)) +
  geom_col(fill='lightgreen') +
  coord_flip() +
  theme_dark() +
  theme(panel.grid = element_blank()) +
  facet_wrap(~col,nrow=10)