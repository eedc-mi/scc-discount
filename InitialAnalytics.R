#-----------------------
# Exploratory Analytics
#-----------------------

# Prepare Session
#-----------------

# Load Libraries

library(tidyverse)
library(lubridate)
library(stringr)
library(reshape2)
library(here)

# Set Working Directory

setwd(here())

# Read in Data

d1 <- read_rds("data.rds")
dim(d1)

# Omit NA values in rental_discount and rental_revenue columns

d2 <- d1 %>% drop_na(rental_discount, rental_revenue)

# Omit values of zero in the rental_revenue column

d2 <- d2[!(d2$rental_revenue == 0),]

# Omit positive discount values (inflation charges)

d2 <- d2[!(d2$rental_discount > 0),]

# Make discount amounts all positive

d2$rental_discount <- abs(d2$rental_discount)

# Omit values of zero in the total_event_attendance column

d2 <- d2[!(d2$total_event_attendance == 0),]

# Create bins for total_event_attendence

d2$tea_bins <- cut(d2$total_event_attendance, c(0,5000,10000,15000,20000,25000,
                                                30000,35000,40000,45000,50000))

# Create bins for total_revenue

d2$tr_bins <- cut(d2$total_revenue, c(0,50000,100000,150000,200000,250000,
                                      300000,350000,400000,450000,500000,
                                      550000,600000,650000))

# Add Percentage Discount column

d2$percentage_discount <- (d2$rental_discount)/(d2$rental_revenue)*100

# Sort Start Dates and Dates Booked into months

event_month <- months(d2$start_date)
month_booked <- months(d2$date_booked)

d2$event_month <- event_month
d2$month_booked <- month_booked

# Add year_booked column

d2$year_booked <- as.factor(year(d2$date_booked))

# Add advance_booking column (days booked in advance of event)

d2$advance_booking <- as.numeric(d2$start_date - d2$date_booked)

d2$ab_bins <- cut(d2$advance_booking, c(0,100,200,300,400,500,600,700,800,900))

# Add number_of_days column (number of days of the event)

d2$number_of_days <- as.numeric(d2$end_date - d2$start_date + 1)

# Add column that displays count of booked_spaces

dtest <- d2
dtest2 <- unnest(dtest, booked_spaces)

dcount <- aggregate(data.frame(count = dtest2$event_id),
                    list(value = dtest2$event_id), length)

d2$space_count <- dcount$count

# Add columns indicating if the event booked a hall, salon, and/or a room

d2 <- d2 %>%
  mutate(
    uses_hall = as.logical(
      map(
        booked_spaces,
        function(list) { 
          any(
            map_lgl(
              list,
              function (string) {
                grepl("Hall", string)
              }
            )
          )
        }
      )
    )
  ) 

d2 <- d2 %>%
  mutate(
    uses_salon = as.logical(
      map(
        booked_spaces,
        function(list) { 
          any(
            map_lgl(
              list,
              function (string) {
                grepl("Salon", string)
              }
            )
          )
        }
      )
    )
  ) 

d2 <- d2 %>%
  mutate(
    uses_room = as.logical(
      map(
        booked_spaces,
        function(list) { 
          any(
            map_lgl(
              list,
              function (string) {
                grepl("Room", string)
              }
            )
          )
        }
      )
    )
  ) 

# Create a subset of the type variable

dtypesubset <- d2[!(d2$type == "Private/Social (SOC)"),]

# Full Sample Stats
#-------------------

d2 %>%
  summarise(mean = mean(rental_discount),
            median = median(rental_discount),
            standard_dev = sd(rental_discount),
            min = min(rental_discount),
            max = max(rental_discount),
            n = n())

d2 %>%
  summarise(mean = mean(percentage_discount),
            median = median(percentage_discount),
            standard_dev = sd(percentage_discount),
            min = min(percentage_discount),
            max = max(percentage_discount),
            n = n())

# Subset Stats
#--------------

# Function to use for summary stats

summary_stats <- function(df, category, discount) {
  category <- enquo(category)
  discount <- enquo(discount)
  
  df %>%
    group_by(!!category) %>%
    summarize(Mean = mean(!!discount),
              Median = median(!!discount),
              Standard_Deviation = sd(!!discount),
              Min = min(!!discount),
              Max = max(!!discount),
              n = n())
}

# Function to use for summary stats that involve a month variable

summary_stats_month <- function(df, category, discount) {
  category <- enquo(category)
  discount <- enquo(discount)
  
  df %>%
    group_by(!!category) %>%
    summarize(Mean = mean(!!discount),
              Median = median(!!discount),
              Standard_Deviation = sd(!!discount),
              Min = min(!!discount),
              Max = max(!!discount),
              n = n()) %>%
    mutate(category = factor(!!!category,
                                levels = c("January", "February", "March", "April",
                                           "May", "June", "July", "August",
                                           "September", "October", "November", 
                                           "December"))) %>%
    arrange(category) %>%
    select(-category)
}

# Summary stats for variables

summary_by_type <- summary_stats(d2, type, percentage_discount)

summary_by_teabins <- summary_stats(d2, tea_bins, percentage_discount)

summary_by_abbins <- summary_stats(d2, ab_bins, percentage_discount)

summary_by_trbins <- summary_stats(d2, tr_bins, percentage_discount)

summary_by_numberofdays <- summary_stats(d2, number_of_days, percentage_discount)

summary_by_spacecount <- summary_stats(d2, space_count, percentage_discount)

summary_by_useshall <- summary_stats(d2, uses_hall, percentage_discount)

summary_by_usessalon <- summary_stats(d2, uses_salon, percentage_discount)

summary_by_usesroom <- summary_stats(d2, uses_room, percentage_discount)

summary_by_yearbooked <- summary_stats(d2, year_booked, percentage_discount)

# Summary stats for month variables

summary_by_eventmonth <- summary_stats_month(d2, event_month, percentage_discount)

summary_by_monthbooked <- summary_stats_month(d2, month_booked, percentage_discount)

# Function to create barcharts

create_barchart <- function(df, category, y) {
  df %>%
  ggplot(aes(x = category, y = y)) +
    geom_bar(stat = "identity") +
    xlab(colnames(df[1])) + ylab(paste(colnames(df[2]), " Percent Discount"))
}

# Function to create month variable barcharts

create_month_barchart <- function(df, category, y) {
  df %>%
  mutate(category = factor(category,
                           levels = c("January", "February", "March", "April",
                                      "May", "June", "July", "August",
                                      "September", "October", "November", 
                                      "December"))) %>%
  arrange(category) %>%
  ggplot(aes(x = category, y = y)) +
    geom_bar(stat = "identity") +
    xlab(colnames(df[1])) + ylab(paste(colnames(df[2]), " Percent Discount"))
}

# Barcharts

bartype <- create_barchart(summary_by_type, summary_by_type$type, summary_by_type$Mean)

create_barchart(summary_by_teabins, summary_by_teabins$tea_bins, summary_by_teabins$Mean)

create_barchart(summary_by_abbins, summary_by_abbins$ab_bins, summary_by_abbins$Mean)

create_barchart(summary_by_trbins, summary_by_trbins$tr_bins, summary_by_trbins$Mean)

create_barchart(summary_by_numberofdays, summary_by_numberofdays$number_of_days, summary_by_numberofdays$Mean)

create_barchart(summary_by_spacecount, summary_by_spacecount$space_count, summary_by_spacecount$Mean)

create_barchart(summary_by_useshall, summary_by_useshall$uses_hall, summary_by_useshall$Mean)

create_barchart(summary_by_usessalon, summary_by_usessalon$uses_salon, summary_by_usessalon$Mean)

create_barchart(summary_by_usesroom, summary_by_usesroom$uses_room, summary_by_usesroom$Mean)

create_barchart(summary_by_yearbooked, summary_by_yearbooked$year_booked, summary_by_yearbooked$Mean)

# Barcharts for month variables

create_month_barchart(summary_by_eventmonth, summary_by_eventmonth$event_month, summary_by_eventmonth$Mean)

create_month_barchart(summary_by_monthbooked, summary_by_monthbooked$month_booked, summary_by_monthbooked$Mean)

#----------------------------------------------------------------------------

# Rental Discount by year_booked and type, total event attendance, advanced booking,
# total revenue, number of days of the event, number of spaces booked,
# event_month, and month_booked

d2 %>%
  group_by(year_booked, type) %>%
  summarise(mean_discount = mean(rental_discount),
            median = median(rental_discount),
            standard_dev = sd(rental_discount),
            min = min(rental_discount),
            max = max(rental_discount),
            n = n()) %>%
  ggplot(aes(x = type, y = mean_discount, fill = year_booked)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Event Type") + ylab("Mean Rental Discount")



d2 %>%
  group_by(year_booked, tr_bins) %>%
  summarise(mean_discount = mean(rental_discount),
            median = median(rental_discount),
            standard_dev = sd(rental_discount),
            min = min(rental_discount),
            max = max(rental_discount),
            n = n()) %>%
  ggplot(aes(x = tr_bins, y = mean_discount, fill = year_booked)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Total Revenue") + ylab("Mean Rental Discount")

d2 %>%
  group_by(year_booked, space_count) %>%
  summarise(mean_discount = mean(rental_discount),
            median = median(rental_discount),
            standard_dev = sd(rental_discount),
            min = min(rental_discount),
            max = max(rental_discount),
            n = n()) %>%
  ggplot(aes(x = space_count, y = mean_discount, fill = year_booked)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Number of Spaces Booked") + ylab("Mean Rental Discount")

d2 %>%
  group_by(year_booked, event_month) %>%
  summarise(mean_discount = mean(rental_discount),
            median = median(rental_discount),
            standard_dev = sd(rental_discount),
            min = min(rental_discount),
            max = max(rental_discount),
            n = n()) %>%
  mutate(event_month = factor(event_month,
                              levels = c("January", "February", "March", "April",
                                         "May", "June", "July", "August",
                                         "September", "October", "November", 
                                         "December"))) %>%
  arrange(event_month) %>%
  ggplot(aes(x = event_month, y = mean_discount, fill = year_booked)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Event Month") + ylab("Mean Rental Discount")

d2 %>%
  group_by(year_booked, month_booked) %>%
  summarise(mean_discount = mean(rental_discount),
            median = median(rental_discount),
            standard_dev = sd(rental_discount),
            min = min(rental_discount),
            max = max(rental_discount),
            n = n()) %>%
  mutate(month_booked = factor(month_booked,
                               levels = c("January", "February", "March", "April",
                                          "May", "June", "July", "August",
                                          "September", "October", "November", 
                                          "December"))) %>%
  arrange(month_booked) %>%
  ggplot(aes(x = month_booked, y = mean_discount, fill = year_booked)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Month Booked") + ylab("Mean Rental Discount")

# Percentage Discount by year_booked and type, total event attendance, advanced booking,
# total revenue, number of days of the event, number of spaces booked,
# event_month, and month_booked

d2 %>%
  group_by(year_booked, type) %>%
  summarise(mean_percentage_discount = mean(percentage_discount),
            median = median(percentage_discount),
            standard_dev = sd(percentage_discount),
            min = min(percentage_discount),
            max = max(percentage_discount),
            n = n()) %>%
  ggplot(aes(x = type, y = mean_percentage_discount, fill = year_booked)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Event Type") + ylab("Mean Percent Discount")

d2 %>%
  group_by(year_booked, tr_bins) %>%
  summarise(mean_percentage_discount = mean(percentage_discount),
            median = median(percentage_discount),
            standard_dev = sd(percentage_discount),
            min = min(percentage_discount),
            max = max(percentage_discount),
            n = n())

d2 %>%
  group_by(year_booked, space_count) %>%
  summarise(mean_percentage_discount = mean(percentage_discount),
            median = median(percentage_discount),
            standard_dev = sd(percentage_discount),
            min = min(percentage_discount),
            max = max(percentage_discount),
            n = n()) %>%
  ggplot(aes(x = space_count, y = mean_percentage_discount, fill = year_booked)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Number of Spaces") + ylab("Mean Percent Discount")

#----------------------------------------------------------------------

d2 %>%
  group_by(year_booked, event_month) %>%
  summarise(mean_percentage_discount = mean(percentage_discount),
            median = median(percentage_discount),
            standard_dev = sd(percentage_discount),
            min = min(percentage_discount),
            max = max(percentage_discount),
            n = n()) %>%
  mutate(event_month = factor(event_month,
                              levels = c("January", "February", "March", "April",
                                         "May", "June", "July", "August",
                                         "September", "October", "November", 
                                         "December"))) %>%
  arrange(event_month) %>%
  ggplot(aes(x = event_month, y = mean_percentage_discount, fill = year_booked)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Event Month") + ylab("Mean Percent Discount")

dtypesubset %>%
  group_by(year_booked, event_month) %>%
  summarise(mean_percentage_discount = mean(percentage_discount),
            median = median(percentage_discount),
            standard_dev = sd(percentage_discount),
            min = min(percentage_discount),
            max = max(percentage_discount),
            n = n()) %>%
  mutate(event_month = factor(event_month,
                              levels = c("January", "February", "March", "April",
                                         "May", "June", "July", "August",
                                         "September", "October", "November", 
                                         "December"))) %>%
  arrange(event_month) %>%
  ggplot(aes(x = event_month, y = mean_percentage_discount, fill = year_booked)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Event Month") + ylab("Mean Percent Discount")

#--------------------------------------------------------------------------

d2 %>%
  group_by(year_booked, month_booked) %>%
  summarise(mean_percentage_discount = mean(percentage_discount),
            median = median(percentage_discount),
            standard_dev = sd(percentage_discount),
            min = min(percentage_discount),
            max = max(percentage_discount),
            n = n()) %>%
  mutate(month_booked = factor(month_booked,
                               levels = c("January", "February", "March", "April",
                                          "May", "June", "July", "August",
                                          "September", "October", "November", 
                                          "December"))) %>%
  arrange(month_booked) %>%
  ggplot(aes(x = month_booked, y = mean_percentage_discount, fill = year_booked)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Month Booked") + ylab("Mean Percent Discount")

#--------------------------------------------

# Total event counts by type per month

event_counts <- d2 %>%
  group_by(type, event_month) %>%
  summarise(number_of_events = n()) %>%
  mutate(event_month = factor(event_month,
                               levels = c("January", "February", "March", "April",
                                          "May", "June", "July", "August",
                                          "September", "October", "November", 
                                          "December"))) %>%
  arrange(event_month)

# Scatterplots

# Advanced Booking

d2 %>%
  group_by(advance_booking, type) %>%
  summarise(mean_percentage_discount = mean(percentage_discount),
            median = median(percentage_discount),
            standard_dev = sd(percentage_discount),
            min = min(percentage_discount),
            max = max(percentage_discount),
            n = n()) %>%
  ggplot(aes(x = advance_booking, y = mean_percentage_discount)) +
  geom_point(aes(colour=factor(type)), stat = "identity")

dtypesubset %>%
  group_by(advance_booking, type) %>%
  summarise(mean_percentage_discount = mean(percentage_discount),
            median = median(percentage_discount),
            standard_dev = sd(percentage_discount),
            min = min(percentage_discount),
            max = max(percentage_discount),
            n = n()) %>%
  ggplot(aes(x = advance_booking, y = mean_percentage_discount)) +
  geom_point(aes(colour=factor(type)), stat = "identity")

# Total Event Attendance

d2 %>%
  group_by(total_event_attendance, type) %>%
  summarise(mean_percentage_discount = mean(percentage_discount),
            median = median(percentage_discount),
            standard_dev = sd(percentage_discount),
            min = min(percentage_discount),
            max = max(percentage_discount),
            n = n()) %>%
  ggplot(aes(x = total_event_attendance, y = mean_percentage_discount)) +
  geom_point(aes(colour=factor(type)), stat = "identity")

# Total Revenue

dtypesubset %>%
  group_by(total_revenue, type) %>%
  summarise(mean_percentage_discount = mean(percentage_discount),
            median = median(percentage_discount),
            standard_dev = sd(percentage_discount),
            min = min(percentage_discount),
            max = max(percentage_discount),
            n = n()) %>%
  ggplot(aes(x = total_revenue, y = mean_percentage_discount)) +
  geom_point(aes(colour=factor(type)), stat = "identity")

# Rental Revenue

dtypesubset %>%
  group_by(rental_revenue, type) %>%
  summarise(mean_percentage_discount = mean(percentage_discount),
            median = median(percentage_discount),
            standard_dev = sd(percentage_discount),
            min = min(percentage_discount),
            max = max(percentage_discount),
            n = n()) %>%
  ggplot(aes(x = rental_revenue, y = mean_percentage_discount)) +
  geom_point(aes(colour=factor(type)), stat = "identity")

