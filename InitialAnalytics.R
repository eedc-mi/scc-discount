#-----------------------
# Exploratory Analytics
#-----------------------

# Prepare Session
#-----------------

# Set Working Directory

thisDir <- dirname(parent.frame(2)$ofile)
setwd(thisDir)

# Load Libraries

library(tidyverse)
library(lubridate)
library(stringr)

# Read in Data

d1 <- read_rds("data.rds")
dim(d1)

# Omit NA values in rental_discount and rental_revenue columns

d2 <- d1 %>% drop_na(rental_discount, rental_revenue)

# Omit values of zero in the rental_revenue column

d2 <- d2[!(d2$rental_revenue == 0),]

# Make discount amounts all positive

d2$rental_discount <- abs(d2$rental_discount)

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

# Add advance_booking column (days booked in advance of event)

d2$advance_booking <- as.numeric(d2$start_date - d2$date_booked)

d2$ab_bins <- cut(d2$advance_booking, c(0,100,200,300,400,500,600,700,800,900))

# Add number_of_days column (number of days of the event)

d2$number_of_days <- as.numeric(d2$end_date - d2$start_date + 1)

# Full Sample Stats
#-------------------

mean(d2$rental_discount)
median(d2$rental_discount)
sd(d2$rental_discount)
min(d2$rental_discount)
max(d2$rental_discount)

# Subset Stats
#--------------

# Rental Discount by type, total event attendance, advanced booking,
# total revenue, number of days of the event, event_month, and month_booked

d2 %>%
  group_by(type) %>%
  summarise(mean = mean(rental_discount),
            median = median(rental_discount),
            standard_dev = sd(rental_discount),
            min = min(rental_discount),
            max = max(rental_discount),
            n = n())

d2 %>%
  group_by(tea_bins) %>%
  summarise(mean = mean(rental_discount),
            median = median(rental_discount),
            standard_dev = sd(rental_discount),
            min = min(rental_discount),
            max = max(rental_discount),
            n = n())

d2 %>%
  group_by(ab_bins) %>%
  summarise(mean = mean(rental_discount),
            median = median(rental_discount),
            standard_dev = sd(rental_discount),
            min = min(rental_discount),
            max = max(rental_discount),
            n = n())

d2 %>%
  group_by(tr_bins) %>%
  summarise(mean = mean(rental_discount),
            median = median(rental_discount),
            standard_dev = sd(rental_discount),
            min = min(rental_discount),
            max = max(rental_discount),
            n = n())

d2 %>%
  group_by(number_of_days) %>%
  summarise(mean = mean(rental_discount),
            median = median(rental_discount),
            standard_dev = sd(rental_discount),
            min = min(rental_discount),
            max = max(rental_discount),
            n = n())

d2 %>%
  group_by(event_month) %>%
  summarise(mean = mean(rental_discount),
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
  arrange(event_month)

d2 %>%
  group_by(month_booked) %>%
  summarise(mean = mean(rental_discount),
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
  arrange(month_booked)

# Percentage Discount by type, total event attendance, advanced booking,
# total revenue, number of days of the event, event_month, and month_booked

d2 %>%
  group_by(type) %>%
  summarise(mean = mean(percentage_discount),
            median = median(percentage_discount),
            standard_dev = sd(percentage_discount),
            min = min(percentage_discount),
            max = max(percentage_discount),
            n = n())

d2 %>%
  group_by(tea_bins) %>%
  summarise(mean = mean(percentage_discount),
            median = median(percentage_discount),
            standard_dev = sd(percentage_discount),
            min = min(percentage_discount),
            max = max(percentage_discount),
            n = n())

d2 %>%
  group_by(ab_bins) %>%
  summarise(mean = mean(percentage_discount),
            median = median(percentage_discount),
            standard_dev = sd(percentage_discount),
            min = min(percentage_discount),
            max = max(percentage_discount),
            n = n())

d2 %>%
  group_by(tr_bins) %>%
  summarise(mean = mean(percentage_discount),
            median = median(percentage_discount),
            standard_dev = sd(percentage_discount),
            min = min(percentage_discount),
            max = max(percentage_discount),
            n = n())

d2 %>%
  group_by(number_of_days) %>%
  summarise(mean = mean(percentage_discount),
            median = median(percentage_discount),
            standard_dev = sd(percentage_discount),
            min = min(percentage_discount),
            max = max(percentage_discount),
            n = n())

d2 %>%
  group_by(event_month) %>%
  summarise(mean = mean(percentage_discount),
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
  arrange(event_month)

d2 %>%
  group_by(month_booked) %>%
  summarise(mean = mean(percentage_discount),
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
  arrange(month_booked)



