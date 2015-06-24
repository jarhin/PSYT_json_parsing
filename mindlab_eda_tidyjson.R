setwd("~/PSYT/Mindlab Report")

#load tidyjson
library("tidyjson")

#load dplyr
library("dplyr")

#load tidyr
library("tidyr")

#load ggplot2
library("ggplot2")

#load scales
library("scales")


#ggplot constants
ggplot_width = 10
ggplot_height = 7
ggplot_units = "in"

#read json
data <- read_json("All.json") 

#extract results array
data_df <- data %>% 
  enter_object("results") %>% #  enter "results" attribute
  gather_array %>% #  stack the array
  enter_object("data") %>% #  enter the data field
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


#################################
#
#TODO:
#deal with missing dates in plots. (make them zero counts!)
#
##################
##########################################################################################################

#dailyMeditation counted by distinct UUID
count_meditations <- data_df %>% 
  filter(isMeditation == TRUE) %>% 
  group_by(practice) %>% 
  summarise(UUID_counts = n_distinct(UUID)) %>%
  separate(practice, into = c("activity", "number"), sep = "\\_") 

#make numbers column integer
count_meditations[["number"]] <- as.integer(count_meditations[["number"]])

#sort by number column
count_meditations <- count_meditations %>% arrange(number)

ggplot(data = count_meditations, aes(number, UUID_counts)) + 
  geom_line(stat = "identity") + geom_point(stat = "identity") + xlab("\nDaily Meditation Number") + 
  ylab("Distinct UUID counts\n") + ggtitle("Distinct UUID counts vs Daily Meditation Number")
ggsave("plot_uud_counts_vs_daily_meditation_number.jpeg", width = ggplot_width, height = ggplot_height, units = ggplot_units)


###########################################################################################################

#meditation counts (distinct UUID) by actual day 
count_meditation_actual_day <- data_df %>% 
  filter(isMeditation == TRUE, actualDay >= 0) %>% 
  mutate(actual_day_weeks = floor(actualDay/7)) %>% 
  group_by(actual_day_weeks) %>% 
  summarise(meditationCounts = n_distinct(UUID)) 

ggplot(data = count_meditation_actual_day, aes(x = actual_day_weeks, y = meditationCounts)) + 
  geom_line(stat = "identity") + geom_point(stat = "identity") + 
  scale_x_continuous("\nActual Day in weeks") + 
  ylab("Meditation Counts in distinct UUIDS\n") + 
  ggtitle("Meditation Counts in distinct UUIDS by Actual Day in weeks")
ggsave("plot_meditation_counts_by_actual_day.jpeg", width = ggplot_width, height = ggplot_height, units = ggplot_units)


###########################################################################################################


#meditation counts (distinct UUID) by nominal day 
count_meditation_nominal_day <- data_df %>% 
  filter(isMeditation == TRUE, nominalDay >= 0) %>% 
  group_by(nominalDay) %>% 
  summarise(meditationCounts = n_distinct(UUID)) 

ggplot(data = count_meditation_nominal_day, aes(x = nominalDay, y = meditationCounts)) + 
  geom_line(stat = "identity") + 
  geom_point(stat = "identity") + 
  scale_x_continuous("\nNominal Day") + 
  ylab("Meditation Counts in distinct UUIDS\n") + 
  ggtitle("Meditation Counts in distinct UUIDS by Nominal Day")
ggsave("plot_meditation_counts_by_nominal_day.jpeg", width = ggplot_width, height = ggplot_height, units = ggplot_units)


############################################################################################################

#count distinct UUID by date for reviews
count_review <- data_df %>% 
  filter(isReview == TRUE) %>% 
  group_by(practice) %>% 
  summarise(UUID_counts = n_distinct(UUID)) %>%
  separate(practice, into = c("activity", "number"), sep = "\\_")

#make numbers column integer
count_review[["number"]] <- as.integer(count_review[["number"]])

#sort by numbers column
count_review <- count_review %>% arrange(number)


ggplot(data = count_review, aes(number, UUID_counts)) + 
  geom_line(stat = "identity") +
  geom_point(stat = "identity") +
  xlab("\nReview Number") + 
  ylab("Distinct UUID counts\n") + ggtitle("Distinct UUID counts vs Review Number")
ggsave("plot_uud_counts_vs_review_number.jpeg", width = ggplot_width, height = ggplot_height, units = ggplot_units)


#############################################################################################################  


#popular practices by distinct UUID (Top 20)
practices_popular <- data_df %>% 
  group_by(practice) %>%
  summarise(UUID_counts = n_distinct(UUID)) %>%
  arrange(desc(UUID_counts)) %>%
  top_n(20)

#code practices as factors for reordering  
practices_popular[["practice"]] <- as.factor(practices_popular[["practice"]])  
  
ggplot(data = practices_popular, aes(x = reorder(practice, -UUID_counts), UUID_counts, y = UUID_counts)) +
  geom_bar(stat = "identity") + xlab("\nPractise") + ylab("Count (Distinct UUIDs)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggtitle("Top 20 Practises via distinct UUIDs")
ggsave("plot_popular_practises_by_uuid.jpeg", width = ggplot_width, height = ggplot_height, units = ggplot_units)
  
  
##############################################################################################################


#popular practices by distinct UUID 
#excluding reviews
practices_popular_no_review <- data_df %>% 
  filter(isReview == FALSE) %>%
  group_by(practice) %>%
  summarise(UUID_counts = n_distinct(UUID)) %>%
  arrange(desc(UUID_counts)) %>%
  top_n(20)

practices_popular_no_review[["practice"]] <- as.factor(practices_popular_no_review[["practice"]])  
  
ggplot(data = practices_popular_no_review, aes(x = reorder(practice, -UUID_counts), UUID_counts, y = UUID_counts)) +
  geom_bar(stat = "identity") + xlab("\nPractise") + ylab("Count (Distinct UUIDs)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggtitle("Top 20 Practises excluding reviews via distinct UUIDs")
ggsave("plot_popular_practises_no_review_by_uuid.jpeg", width = ggplot_width, height = ggplot_height, units = ggplot_units)
    
  
##################################################################################################################
# 
# practice_UUID <- unique(data_df[,c("UUID", "practice")])
# practice_UUID[["UUID"]] <- as.factor(practice_UUID[["UUID"]])
# practice_UUID[["practice"]] <- as.factor(practice_UUID[["practice"]])
# 
# uud_practise_table <- table(practice_UUID[["UUID"]], practice_UUID[["practice"]])
# user_progress <- margin.table(uud_practise_table, 1)
# practice_progress <- margin.table(uud_practise_table,2)
# 
# 


#number practices completed per user
practices_number_per_UUID <- data_df %>% 
  group_by(UUID) %>%
  summarise(practice_counts = n_distinct(practice))

#histogram
ggplot(data = practices_number_per_UUID, aes(practice_counts)) + 
  geom_histogram(binwidth = 5) + 
  scale_x_continuous("\nPractice Counts", breaks = seq(0, 50, 10)) + 
  ylab("Number of distinct UUIDs\n") + 
  ggtitle("Histogram of the distribution of Practice Completed")
ggsave("plot_histogram_practice_counts.jpeg", width = ggplot_width, height = ggplot_height, units = ggplot_units)
  
#cumulative frequency
#histogram
ggplot(data = practices_number_per_UUID, aes(practice_counts)) + 
  stat_ecdf(binwidth = 5) + 
  scale_x_continuous("\nPractice Counts", breaks = seq(0, 50, 10)) + 
  scale_y_continuous("Cumulative Frequency of distinct UUIDs\n", labels = percent) + 
  ggtitle("Cumulative Frequency distribution of Practice Completed")
ggsave("plot_cumulative_frequency_practice_counts.jpeg", width = ggplot_width, height = ggplot_height, units = ggplot_units)



###################################################################################################################


averages_time_between_visits <- data_df %>% 
  group_by(UUID) %>% 
  filter(n() > 1) %>% 
  mutate(timestamp_diff = timestamp - lag(timestamp)) %>%
  summarise(
    mean_time = mean(timestamp_diff, na.rm = TRUE), 
    median_time = median(timestamp_diff, na.rm = TRUE)
    ) %>% 
    gather(summary_time, time, mean_time:median_time) 
    
    
ggplot(data = averages_time_between_visits, aes(x = time, fill = summary_time)) + 
  geom_histogram(binwidth = 1000) + 
  scale_x_continuous("\nTime (seconds ??)",limits = c(0, 40000)) + 
  ylab("Count\n") + 
  ggtitle("Histogram of the average time between UUID visits per UUID") + 
  scale_fill_discrete(name = "Averages", breaks = c("mean_time", "median_time"), labels = c("Mean", "Median"))
ggsave("plot_average_time_between_visits.jpeg", width = ggplot_width, height = ggplot_height, units = ggplot_units)
  

################################################################################################




