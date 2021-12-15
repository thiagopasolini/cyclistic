library(tidyverse)
library(lubridate)

load("C:/data_science/Portfolio/Case_Study_1_Bike_Share/data/data2_cleaned.RData")


### Step 6.1 - Filtering
# Using only variables to the count/frequency study



glimpse(data3_cleaned)

data4_filtered_1count <- select(data3_cleaned,
                               rideable_type, 
                               member_casual,
                               hour,
                               day_of_week,
                               month_number)


# Confering
head(data4_filtered_1count)
tail(data4_filtered_1count)

rm(data3_cleaned)

### Step 7.1 - Analyzing



## Exploring by tables

# Member and Casual Table

table_count1_mc <- data4_filtered_1count %>% 
  group_by(member_casual) %>% 
  summarise(count = n())

# Member and Casual by Hour Table

table_count2_mc_hour <- data4_filtered_1count %>% 
  group_by(member_casual, hour) %>% 
  summarise(count = n())

# Member and Casual by Day Table

table_count3_mc_day <- data4_filtered_1count %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(count = n())

# Member and Casual by Month Table

table_count4_mc_month <- data4_filtered_1count %>% 
  group_by(member_casual, month_number) %>% 
  summarise(count = n())

# Member and Casual by Ride Type Table

table_count5_mc_ride <- data4_filtered_1count %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(count = n())


table_count1_mc
table_count2_mc_hour
table_count3_mc_day
table_count4_mc_month
table_count5_mc_ride

## Exploring by visuals

# Member and Casual Viz

# Bar
ggplot(data=table_count1_mc,
       aes(x = member_casual, y = count, fill = member_casual)) +
  geom_bar(stat="identity", width = 0.7) +
  labs(title="Casual and Member",
       caption = "Data from jan/20 to oct/21",
       x = "",
       y = "Counts") +
  theme(plot.title = element_text(hjust=0.5), 
        axis.text.x = element_text(angle = 0), legend.position="none",
        panel.background = element_rect(fill = "gray88")) +
  scale_y_continuous(labels = scales::comma)

# Donuts
donuts <- data.frame(table_length1_mc$member_casual, 
                     as.numeric(table_length1_mc$mean)/60
)

donuts$fraction <- donuts$as.numeric.table_length1_mc.mean..60 / 
  sum(donuts$as.numeric.table_length1_mc.mean..60)
donuts$ymax <- cumsum(donuts$fraction)
donuts$ymin <- c(0, head(donuts$ymax, n = -1))
donuts$labelposition <- (donuts$ymax + donuts$ymin) / 2
donuts$label <- paste0(donuts$table_length1_mc.member_casual,
                       "\n value: ", 
                       round(donuts$fraction*100, digits = 2))

# Hour Viz

ggplot(data=table_count2_mc_hour,
       aes(x = hour, y = count, fill = member_casual)) +
  facet_grid(~member_casual) +
  geom_bar(stat="identity",
           position="dodge") +
  labs(title="Casual and Member by Hour",
       caption = "Data from jan/20 to oct/21",
       x = "",
       y = "Counts") +
  geom_text(aes(label=count), vjust=1.6, color="white",
            position = position_dodge(0.9), size=2,) +
  theme(plot.title = element_text(hjust=0.5), 
        axis.text.x = element_text(angle = 45),
        panel.background = element_rect(fill = "gray88")) +
  scale_y_continuous(labels = scales::comma)

# Day Viz

ggplot(data=table_count3_mc_day,
       aes(x = day_of_week, y = count, fill = member_casual)) +
  facet_grid(~member_casual) +
  geom_bar(stat="identity",
           position="dodge") +
  labs(title="Casual and Member by Week Day",
       caption = "Data from jan/20 to oct/21",
       x = "",
       y = "Counts") +
  geom_text(aes(label=count), vjust=1.6, color="white",
            position = position_dodge(0.9), size=2,) +
  theme(plot.title = element_text(hjust=0.5), 
        axis.text.x = element_text(angle = 0),
        panel.background = element_rect(fill = "gray88")) +
  scale_y_continuous(labels = scales::comma)

# Month Viz

table_count4_mc_month$month_number <- month.abb[table_count4_mc_month$month_number]

ggplot(data=table_count4_mc_month,
       aes(x = factor(month_number, 
                      levels = c("Jan", "Feb", "Mar", 
                                 "Apr", "May", "Jun",
                                 "Jul", "Aug", "Sep",
                                 "Oct", "Nov", "Dec")), 
           y = count, fill = member_casual)) +
  facet_grid(~member_casual) +
  geom_bar(stat="identity",
           position="dodge") +
  labs(title="Casual and Member by Month",
       caption = "Data from jan/20 to oct/21",
       x = "",
       y = "Counts") +
  geom_text(aes(label=count), vjust=1.6, color="white",
            position = position_dodge(0.9), size=2,) +
  theme(plot.title = element_text(hjust=0.5), 
        axis.text.x = element_text(angle = 45),
        panel.background = element_rect(fill = "gray88")) +
  scale_y_continuous(labels = scales::comma)

# Rideable Bike Type Viz

ggplot(data=table_count5_mc_ride,
       aes(x = rideable_type, y = count, fill = member_casual)) +
  facet_grid(~member_casual) +
  geom_bar(stat="identity",
           position="dodge") +
  labs(title="Casual and Member by Bike Type",
       caption = "Data from jan/20 to oct/21",
       x = "",
       y = "Counts") +
  geom_text(aes(label=count), vjust=1.6, color="white",
            position = position_dodge(0.9), size=2,) +
  theme(plot.title = element_text(hjust=0.5), 
        axis.text.x = element_text(angle = 0),
        panel.background = element_rect(fill = "gray88")) +
  scale_y_continuous(labels = scales::comma)
