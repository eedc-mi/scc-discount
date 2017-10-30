#-----------------------
# Presentation Creation
#-----------------------

# Prepare Session
#-----------------

# Load Libraries

library(tidyverse)
library(lubridate)
library(stringr)
library(reshape2)
library(officer)
library(knitr)
library(rvg)
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

#--------------------------------------
# Powerpoint Creation
#--------------------------------------

# Set Captions

intro <- ""
cap1 <- "Percentage Discount Summary Statistics by Event Type"
cap2 <- "Percentage Discount Summary Statistics by Year Booked"
cap3 <- "Mean Percentage Discount by Event Type"
cap4 <- "Percentage Discount Summary Statistics by Event Month"
cap5 <- "Percentage Discount Summary Statistics by Month Booked"
cap6 <- "Mean Percentage Discount by Number of Days Booked in Advance"

# Build Slide Deck

pres <- read_pptx()

pres <- pres %>%
  # Title Slide
  add_slide(layout = "Title Slide", master = "Office Theme") %>%
    ph_with_text(type = "ctrTitle", str = "SCC Room Rental Discount History") %>% 
    ph_with_text(type = "subTitle", str = "Exploratory Analytics") %>%
  
  # Summary Slide
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_text(type = "title", index = 1, str = "Summary") %>%
    ph_with_text(type="body", str = intro ) %>% 
    ph_with_text(type = "sldNum", str = "1" ) %>%
  
  # Slide with Chart 1
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_text(type = "title", index = 1, str = cap1) %>% 
    ph_with_table(type = "body", value = head(summary_by_type)) %>%
    ph_with_text(type = "sldNum", str = "2" ) %>%
  
  # Slide with Chart 2
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_text(type = "title", index = 1, str = cap2) %>% 
    ph_with_table(type = "body", value = head(summary_by_yearbooked)) %>%
    ph_with_text(type = "sldNum", str = "3" ) %>%
    
  # Slide with Barchart (Event Type)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_text(type = "title", index = 1, str = cap3) %>% 
    ph_with_vg(code = print(create_barchart(summary_by_type,
                                            summary_by_type$type,
                                            summary_by_type$Mean)), type = "body") %>%
    ph_with_text(type = "sldNum", str = "4" ) %>%
    
  # Slide with Chart 4
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_text(type = "title", index = 1, str = cap4) %>% 
    ph_with_table(type = "body", index = 1, value = head(summary_by_eventmonth)) %>%
    ph_with_text(type = "sldNum", str = "5" ) %>%
    
  # Slide with Chart 5
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_text(type = "title", index = 1, str = cap5) %>% 
    ph_with_table(type = "body", index = 1, value = head(summary_by_monthbooked)) %>%
    ph_with_text(type = "sldNum", str = "6" ) %>%

  # Slide with Chart 6
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_text(type = "title", index = 1, str = cap6) %>% 
    ph_with_vg(code = print(d2 %>%
                              group_by(advance_booking, type) %>%
                              summarise(mean_percentage_discount = mean(percentage_discount),
                                        median = median(percentage_discount),
                                        standard_dev = sd(percentage_discount),
                                        min = min(percentage_discount),
                                        max = max(percentage_discount),
                                        n = n()) %>%
                              ggplot(aes(x = advance_booking, y = mean_percentage_discount)) +
                              geom_point(aes(colour=factor(type)), stat = "identity")),
               type = "body") %>%
    ph_with_text(type = "sldNum", str = "7" )
    
  # Save Powerpoint
  print(pres, target = "SCC Room Rental Discount History.pptx") %>%
    invisible()
    
