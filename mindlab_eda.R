#we change working directory
setwd("~/PSYT/Mindlab Report")

#we load jsonlite library
library("jsonlite")

#we load the stringr library
library("stringr")


#we load the jsonfile and flatten the dataframe
data <- fromJSON("All.json", flatten = TRUE)
data <- data[["results"]]

#create vector of character dailyMeditation from 0 to 21
daily_meditations <- paste("dailyMeditation", as.character(0:20), sep = "_")

#we consider the actual day.
#we take UUID and deviceName
df_actual_day <- data[,c("data.UUID","data.deviceName")] 

for(words in daily_meditations)
{
  #create regex patterns for daily_meditations in practicePerformanceCountByActualDayKey
  pat <- paste0("data\\.practicePerformanceCountByActualDayKey\\.[0-9]+\\.", words)
  
  #find regex matches
  binary_matches <- grepl(pat, names(data), perl = TRUE)
  
  #create sub_dataframe based on matches
  sub_df <- data[,binary_matches]
  df_actual_day[[words]] <- apply(sub_df, 1, function(x){any(x >= 1, na.rm = TRUE)})
}