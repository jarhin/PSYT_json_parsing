setwd("~/PSYT/Mindlab Report")

#load tidyjson
library("tidyjson")

#load dplyr
library("dplyr")

#load tidyr
library("tidyr")

#read json
data <- read_json("All.json") 

#read vodafone table UUIDs
vodafone <- read.csv2("Vodafone_UUIDs.csv")

#read nuffield table UUIDs
nuffield <- read.csv2("Nuffield_UUIDs.csv")


#create table practice_performances
practice_performances <- data %>% 
  enter_object("results") %>% #  enter "results" attribute
  gather_array %>% 
  spread_values( #  parse createdAt & updatedAt columns
    createdAt = jstring("createdAt"),
    updatedAt = jstring("updatedAt")
  ) %>% 
  enter_object("data") %>% # enter data array
  spread_values(  # parse UUID
    UUID = jstring("UUID")
  ) %>% 
  enter_object("practicePerformances") %>% # enter "practicePerformances" attribute
  gather_array  %>% #  stack the array
  spread_values( #  parse actualDays, nominalDays, practice & timestamp 
    actualDay = jnumber("actualDay"),
    nominalDay = jnumber("nominalDay"),
    practice = jstring("practice"),
    timestamp = jnumber("timestamp")
  ) 


practice_performances[["document.id"]] <- NULL
practice_performances[["array.index"]] <- NULL

#we adjust the timestamp as it seems to be missing a 1 digit from timestamps
practice_performances[["adjusted_timestamp"]] <- practice_performances[["timestamp"]] + 
  10e8

#convert adjusted timestamp to time object
practice_performances[["adjusted_timestamp"]] <- as.POSIXct(
  practice_performances[["adjusted_timestamp"]], origin = "1970-01-01"
  )

#remove duplicate rows 
practice_performances <- unique(practice_performances)

#create table practice_performances_engagement
#basis for counting engagements
practice_performances_engagement <- practice_performances %>% 
  group_by(UUID, practice, actualDay, nominalDay, timestamp) %>% #group by UUID, practice, actualDay, nominalDay, adjusted_timestamp
  summarise(
    pre_counts = 1, # 1 signifiy row in extracted data
    first_createdAt = min(createdAt), #  extract first creation of practice 
    first_updatedAt = min(updatedAt)  #  extract first update of practice
    ) 


#format datetime strings
practice_performances[["createdAt"]] <- as.POSIXct(practice_performances[["createdAt"]] , format = "%Y-%m-%dT%H:%M:%OSZ")
practice_performances[["updatedAt"]] <- as.POSIXct(practice_performances[["updatedAt"]] , format = "%Y-%m-%dT%H:%M:%OSZ") 

#format other datetime objects
practice_performances_engagement[["first_createdAt"]] <- as.POSIXct(practice_performances_engagement[["first_createdAt"]] , format = "%Y-%m-%dT%H:%M:%OSZ")
practice_performances_engagement[["first_updatedAt"]] <- as.POSIXct(practice_performances_engagement[["first_updatedAt"]] , format = "%Y-%m-%dT%H:%M:%OSZ") 


#####################################################################################################
#
#basis for all review tables!
#review data 
review_data <- data %>% 
  enter_object("results") %>% #  enter "results" attribute
  gather_array %>% 
  enter_object("data") %>% # enter data array
  spread_values(  # parse UUID
    UUID = jstring("UUID")
  )

review_data$document.id <- NULL

####################################################################################################

#create review_0 table
review_0 <- review_data %>% 
  enter_object("review_0") %>% # enter review_0
  spread_values( # parse aware, feetground & noticethoughts keys
    aware = jnumber("aware"),
    feetground = jnumber("feetground"),
    noticethoughts = jnumber("noticethoughts")
  )

review_0$array.index <- NULL
review_0 <- unique(review_0) # remove duplicate rows

###################################################################################################

#create review_1 table 
review_1 <- review_data %>% 
  enter_object("review_1") %>% # enter review_1 object
  spread_values( # parse diffsitnoreact, nocritemo & windhair 
    diffsitnoreact = jnumber("diffsitnoreact"),
    nocritemo = jnumber("nocritemo"),
    windhair = jnumber("windhair")
  )

review_1$array.index <- NULL
review_1 <- unique(review_1) # remove duplicate rows

##################################################################################################

# create review_2 table
review_2 <- review_data %>% 
  enter_object("review_2") %>% # enter review_2 object
  spread_values( # parse difffindwords, fooddrink & nocritthoughts
    difffindwords = jnumber("difffindwords"),
    fooddrink = jnumber("fooddrink"),
    nocritthoughts = jnumber("nocritthoughts")
  )

review_2$array.index <- NULL
review_2 <- unique(review_2) # remove duplicate rows

#################################################################################################
#

#create review_3 table
review_3 <- review_data %>% 
  enter_object("review_3") %>% # enter review-3 field
  spread_values( # parse acceptthoughts, easyfindwords & noticeautopilot
    acceptthoughts = jnumber("acceptthoughts"),
    easyfindwords = jnumber("easyfindwords"),
    noticeautopilot = jnumber("noticeautopilot")
  )

review_3$array.index <- NULL
review_3 <- unique(review_3) # remove duplicate rows

###################################################################################################

#
# create review_4 table
review_4 <- review_data %>% 
  enter_object("review_4") %>% # enter review_4 object
  spread_values( # parse bewithdiffemo, explainself & focused
    bewithdiffemo = jnumber("bewithdiffemo"),
    explainself = jnumber("explainself"),
    focused = jnumber("focused")
  )

review_4$array.index <- NULL
review_4 <- unique(review_4) # remove duplicate rows

#################################################################################################


#create review_5 table 
review_5 <- review_data %>% 
  enter_object("review_5") %>% #  enter review_5 object
  spread_values( #  parse aware, feetground & noticethoughts
    aware = jnumber("aware"),
    feetground = jnumber("feetground"),
    noticethoughts = jnumber("noticethoughts")
  )

review_5$array.index <- NULL
review_5 <- unique(review_5) # remove duplicate rows

####################################################################################################
#
#
#review_6
review_6 <- review_data %>% 
  enter_object("review_6") %>%
  spread_values(
    diffsitnoreact = jnumber("diffsitnoreact"),
    nocritemo = jnumber("nocritemo"),
    windhair = jnumber("windhair")
  )

review_6$array.index <- NULL
review_6 <- unique(review_6)
#################################################################################################
#review_7
review_7 <- review_data %>% 
  enter_object("review_7") %>%
  spread_values(
    difffindwords = jnumber("difffindwords"),
    fooddrink = jnumber("fooddrink"),
    nocritthoughts = jnumber("nocritthoughts")
  )

review_7$array.index <- NULL
review_7 <- unique(review_7)

######################################################################################
#
#review_8
review_8 <- review_data %>% 
  enter_object("review_8") %>%
  spread_values(
    acceptthoughts = jnumber("acceptthoughts"),
    easyfindwords = jnumber("easyfindwords"),
    noticeautopilot = jnumber("noticeautopilot")
  )

review_8$array.index <- NULL
review_8 <- unique(review_8)

######################################################################################
#
#review_9
review_9 <- review_data %>% 
  enter_object("review_9") %>%
  spread_values(
    bewithdiffemo = jnumber("bewithdiffemo"),
    explainself = jnumber("explainself"),
    focused = jnumber("focused")
  )

review_9$array.index <- NULL
review_9 <- unique(review_9)

######################################################################################
#
#review_10
review_10 <- review_data %>% 
  enter_object("review_10") %>%
  spread_values(
    aware = jnumber("aware"),
    feetground = jnumber("feetground"),
    noticethoughts = jnumber("noticethoughts")
  )

review_10$array.index <- NULL
review_10 <- unique(review_10)

######################################################################################
#
#review_11
review_11 <- review_data %>% 
  enter_object("review_11") %>%
  spread_values(
    diffsitnoreact = jnumber("diffsitnoreact"),
    nocritemo = jnumber("nocritemo"),
    windhair = jnumber("windhair")
  )

review_11$array.index <- NULL
review_11 <- unique(review_11)

######################################################################################
#
#review_12
review_12 <- review_data %>% 
  enter_object("review_12") %>%
  spread_values(
    difffindwords = jnumber("difffindwords"),
    fooddrink = jnumber("fooddrink"),
    nocritthoughts = jnumber("nocritthoughts")
  )

review_12$array.index <- NULL
review_12 <- unique(review_12)

######################################################################################
#
#review_13
review_13 <- review_data %>% 
  enter_object("review_13") %>%
  spread_values(
    acceptthoughts = jnumber("acceptthoughts"),
    easyfindwords = jnumber("easyfindwords"),
    noticeautopilot = jnumber("noticeautopilot")
  )

review_13$array.index <- NULL
review_13 <- unique(review_13)

######################################################################################
#
#review_14
review_14 <- review_data %>% 
  enter_object("review_14") %>%
  spread_values(
    bewithdiffemo = jnumber("bewithdiffemo"),
    explainself = jnumber("explainself"),
    focused = jnumber("focused")
  )

review_14$array.index <- NULL
review_14 <- unique(review_14)

######################################################################################
#
#review_15
review_15 <- review_data %>% 
  enter_object("review_15") %>%
  spread_values(
    aware = jnumber("aware"),
    feetground = jnumber("feetground"),
    noticethoughts = jnumber("noticethoughts")
  )

review_15$array.index <- NULL
review_15 <- unique(review_15)


######################################################################################
#
#review_16
review_16 <- review_data %>% 
  enter_object("review_16") %>%
  spread_values(
    diffsitnoreact = jnumber("diffsitnoreact"),
    nocritemo = jnumber("nocritemo"),
    windhair = jnumber("windhair")
  )

review_16$array.index <- NULL
review_16 <- unique(review_16)


######################################################################################
#
#review_17
review_17 <- review_data %>% 
  enter_object("review_17") %>%
  spread_values(
    difffindwords = jnumber("difffindwords"),
    fooddrink = jnumber("fooddrink"),
    nocritthoughts = jnumber("nocritthoughts")
  )

review_17$array.index <- NULL
review_17 <- unique(review_17)


######################################################################################
#
#review_18
review_18 <- review_data %>% 
  enter_object("review_18") %>%
  spread_values(
    acceptthoughts = jnumber("acceptthoughts"),
    easyfindwords = jnumber("easyfindwords"),
    noticeautopilot = jnumber("noticeautopilot")
  )

review_18$array.index <- NULL
review_18 <- unique(review_18)

######################################################################################
#
#review_19
review_19 <- review_data %>% 
  enter_object("review_19") %>%
  spread_values(
    bewithdiffemo = jnumber("bewithdiffemo"),
    explainself = jnumber("explainself"),
    focused = jnumber("focused")
  )

review_19$array.index <- NULL
review_19 <- unique(review_19)

######################################################################################
#

starting_answers <- data %>% 
  enter_object("results") %>% #  enter "results" attribute
  gather_array %>% 
  enter_object("data") %>% # enter data array
  spread_values(  # parse UUID
    UUID = jstring("UUID"),
    useful = jnumber("useful")
  ) %>% 
  enter_object("startingAnswers") %>%
  spread_values(
    benefits_balance = jlogical("benefits/balance"),
    benefits_calmness = jlogical("benefits/calmness"),
    benefits_control = jlogical("benefits/control"),
    benefits_creativity = jlogical("benefits/creativity"),
    benefits_focus = jlogical("benefits/focus"),
    benefits_happiness = jlogical("benefits/happiness"),
    clearthinking = jnumber("clearthinking"),
    closetopeople = jnumber("closetopeople"),
    dealingproblems = jnumber("dealingproblems"),
    difficultiespiling = jnumber("difficultiespiling"),
    goingmyway = jnumber("goingmyway"),
    makingupmind = jnumber("makingupmind"),
    motivation = jnumber("motivation"),
    nocontrol = jnumber("nocontrol"),
    obstacles_1 = jstring("obstacles/1"),
    obstacles_2 = jstring("obstacles/2"),
    obstacles_3 = jstring("obstacles/3"),
    optimistic = jnumber("optimistic"),
    problemconfident = jnumber("problemconfident"),
    relaxed = jnumber("relaxed"),
    solutions_1 = jstring("solutions/1"),
    solutions_2 = jstring("solutions/2"),
    solutions_3 = jstring("solutions/3"),
    startDate.day = jnumber("startDate", "day"),
    startDate.month = jnumber("startDate", "month"),
    startDate.year = jnumber("startDate", "year")
  )


starting_answers[["document.id"]] <- NULL
starting_answers[["array.index"]] <- NULL 

starting_answers <- unique(starting_answers)
############################################################################

finishing_answers <- data %>% 
  enter_object("results") %>% #  enter "results" attribute
  gather_array %>% 
  enter_object("data") %>% # enter data array
  spread_values(  # parse UUID
    UUID = jstring("UUID")
  ) %>% 
  enter_object("finishingAnswers") %>%
  spread_values(
    clearthinking = jnumber("clearthinking"),
    closetopeople = jnumber("closetopeople"),
    dealingproblems = jnumber("dealingproblems"),
    difficultiespiling = jnumber("difficultiespiling"),
    goingmyway = jnumber("goingmyway"),
    makingupmind = jnumber("makingupmind"),
    nocontrol = jnumber("nocontrol"),
    optimistic = jnumber("optimistic"),
    problemconfident = jnumber("problemconfident"),
    relaxed = jnumber("relaxed"),
    useful = jnumber("useful")
  )


finishing_answers[["document.id"]] <- NULL
finishing_answers[["array.index"]] <- NULL 

finishing_answers <- unique(finishing_answers)


#############################################################################

#
#
device <- data %>%
  enter_object("results") %>%
  gather_array() %>%
  enter_object("data") %>% 
  spread_values(  # parse UUID, deviceName and deviceModel
    UUID = jstring("UUID"), 
    deviceModel = jstring("deviceModel"), 
    deviceName = jstring("deviceName")
  )

device[["document.id"]] <- NULL
device[["array.index"]] <- NULL

device <- unique(device)

#############################################################################
#
#
#
practice <- data %>% 
  enter_object("results") %>% #  enter "results" attribute
  gather_array %>% 
  enter_object("data") %>% # enter data array
  enter_object("practicePerformances") %>% # enter "practicePerformances" attribute
  gather_array  %>% #  stack the array
  spread_values( #  parse practice 
    practice = jstring("practice")
  ) 

practice[["document.id"]] <- NULL
practice[["array.index"]] <- NULL

#remove duplicate rows
practice <- unique(practice)


#column to identify dailyMeditations
practice[["is_Meditation"]] <- grepl("dailyMeditation\\_[0-9]+", practice[["practice"]], perl = TRUE)

#column to identity reviews
practice[["is_Review"]] <- grepl("review\\_[0-9]+", practice[["practice"]], perl = TRUE)


###############################################################################################

#remove large unneeded objects:
#data and review_data
rm(list = c("data", "review_data"))

#export workspace as csv files
for(i in ls())
{
    write.table(
    get(i), 
    file = paste0(i, ".csv"), 
    quote = TRUE, 
    dec = ".", 
    sep =",", 
    na = "", 
    row.names = FALSE,
    )
}


# 
# #engagament calculations by practice
# vodafone %>% 
#   left_join(practice_performances_engagement) %>% 
#   group_by(practice) %>% 
#   summarise(counts = sum(pre_counts)) %>% 
#   arrange(-counts) %>% 
#   as.data.frame()
# 
# nuffield %>% 
#   left_join(practice_performances_engagement) %>% 
#   group_by(practice) %>% 
#   summarise(counts = sum(pre_counts)) %>% 
#   arrange(-counts) %>% 
#   as.data.frame()
# 
# #engagament calculations by nominalDay
# vodafone %>% 
#   left_join(practice_performances_engagement) %>% 
#   left_join(practice) %>% filter(is_Meditation) %>% 
#   group_by(nominalDay) %>% 
#   summarise(counts = sum(pre_counts)) %>% 
#   as.data.frame()
# 
# nuffield %>% 
#   left_join(practice_performances_engagement) %>% 
#   left_join(practice) %>% 
#   filter(is_Meditation) %>% 
#   group_by(nominalDay) %>% 
#   summarise(counts = sum(pre_counts)) %>% 
#   as.data.frame()
# 
# #Engagement – Any Practice (excluding reviews) by Week
# vodafone %>% 
#   left_join(practice_performances_engagement) %>% 
#   left_join(practice) %>% 
#   filter(is_Review == FALSE, actualDay >=0) %>% 
#   mutate(week_number = floor(actualDay/7) + 1) %>% 
#   group_by(week_number) %>% 
#   summarise(counts = sum(pre_counts)) %>% 
#   as.data.frame()
# 
# nuffield %>% 
#   left_join(practice_performances_engagement) %>% 
#   left_join(practice) %>% 
#   filter(is_Review == FALSE, actualDay >=0) %>% 
#   mutate(week_number = floor(actualDay/7) + 1) %>% 
#   group_by(week_number) %>% 
#   summarise(counts = sum(pre_counts)) %>% 
#   as.data.frame()
# 
# 
# 
# #Practices – Histograms Showing Distribution by user
# library(ggvis)
# 
# vodafone %>% 
#   left_join(practice_performances_engagement) %>% 
#   group_by(UUID) %>% 
#   summarise(counts = sum(pre_counts)) %>% 
#   compute_bin(~counts, width = 5, center = 2.5)
# 
# nuffield %>% 
#   left_join(practice_performances_engagement) %>% 
#   group_by(UUID) %>% 
#   summarise(counts = sum(pre_counts)) %>% 
#   compute_bin(~counts, width = 5, center = 2.5)
