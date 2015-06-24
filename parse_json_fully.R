setwd("~/PSYT/Mindlab Report")

#load tidyjson
require("tidyjson")

#load dplyr
require("dplyr")

#read json
data <- read_json("All.json") 

#extract results array
data_df <- data %>% 
  enter_object("results") %>% #  enter "results" attribute
  gather_array %>% 
  spread_values( #  parse createdAt column
    createdAt = jstring("createdAt"),
    updatedAt = jstring("updatedAt")
  ) %>% 
  enter_object("data") %>% # enter data array
  spread_values(
    UUID = jstring("UUID"), 
    deviceModel = jstring("deviceModel"), 
    deviceName = jstring("deviceName")
    ) %>% 
  enter_object("practicePerformances") %>% # enter "practicePerformances" attribute
  gather_array  %>% #  stack the array
  spread_values(
    actualDay = jnumber("actualDay"),
    nominalDay = jnumber("nominalDay"),
    practice = jstring("practice"),
    timestamp = jnumber("timestamp")
  ) 



#column to identify dailyMeditations
data_df[["isMeditation"]] <- grepl("dailyMeditation\\_[0-9]+", data_df[["practice"]], perl = TRUE)

#column to identity reviews
data_df[["isReview"]] <- grepl("review\\_[0-9]+", data_df[["practice"]], perl = TRUE)

#format datetime strings
data_df[["createdAt"]] <- as.POSIXct(data_df[["createdAt"]] , format = "%Y-%m-%dT%H:%M:%OSZ")
data_df[["updatedAt"]] <- as.POSIXct(data_df[["updatedAt"]] , format = "%Y-%m-%dT%H:%M:%OSZ") 

#adjusted timestamp
data_df[["adjusted_timestamp"]] <- as.POSIXct(data_df[["timestamp"]] + 10e8, origin="1970-01-01") 

data_df <- unique(data_df

write.csv(data_df, file = "data_cleaned.csv", row.names = FALSE)

