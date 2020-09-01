library(tidyverse)
load("data/after_effects_data.RData")

# build out for schema for Empty Polygon Structure load in C4d Points, X, Y, Z.



prepare_for_c4d <- function(workout=NULL, x_spacing, y_scale, z,sorted=T) {
  
  cols <- c("totalDistance", "workoutType")
  dist_df <- dplyr::select(sport_df, cols)
  dist_df <- dplyr::filter(dist_df, totalDistance>0) 
  dist_df$totalDistance <- dist_df$totalDistance*y_scale #scale by factor for c4d
  

  
  
  df <- dplyr::filter(dist_df, workoutType==workout)
  
  if (sorted) {
    df$totalDistance <- sort(df$totalDistance,decreasing = T)
  }
  
  c4d_cols <- c("Points", "X","Y","Z")  
  
  z <- z
  x_spacing <- x_spacing
  ids <- 1:nrow(df)
  
  df <- add_column(df,ids,.before=1)
  x_vals <- seq.int(0,(nrow(df)*x_spacing)-1,by=x_spacing)
  df <- tibble::add_column(df,x_vals,.before=2)
  df <- tibble::add_column(df,rep(z,nrow(df)),.before=4)
  df['workoutType'] <- NULL
  colnames(df) <- c4d_cols  
  return (df)
}

cycling_df <- prepare_for_c4d(workout="cycling", x_spacing=35, y_scale=10, z=0)
running_df <- prepare_for_c4d(workout="running", x_spacing=35, y_scale=10, z=0)

write_csv(cycling_df,"data/c4d_cycling.csv")
write_csv(running_df,"data/c4d_running.csv")
