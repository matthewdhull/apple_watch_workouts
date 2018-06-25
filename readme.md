# Readme

**Author**: Matthew Hull

## Using the Apple Watch Workout Pipeline

![Alt text](img/pipeline.png "Pipeline")

- All watch workouts are automatically synced through the HealthFit app on iPhone
- Sources currently are Strava, Apple Activity, and Nike+
- After sync, HealthFit uploads exports each workout as a fitfile to iCloud with the extension .fit
- Run fitparse_process.py to grab all fitfiles and combine them with a common format into fit.csv
- Preprocess and clean aggregated data from fit.csv using preprocessing_fitfiles.Rmd. Using knit to pdf on the R Markdown workbook produces exploratory plots as well.
- Output of the preprocessing is fit_data.csv
- Use random_tree.r to build random trees and random forests using classification on the fit_data.csv file.