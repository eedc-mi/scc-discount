#-----------------------------------------------------------------------------------
# Presentation Creation
#-----------------------------------------------------------------------------------

#-----------------
# Prepare Session
#-----------------

# Load Libraries

library(tidyverse)
library(lubridate)
library(stringr)
library(reshape2)
library(officer)
library(flextable)
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

d2$percentage_discount <- (d2$rental_discount)/(d2$total_revenue_pre_discount)*100

# Sort Start Dates and Dates Booked into months

event_month <- months(d2$start_date)
month_booked <- months(d2$date_booked)

d2$event_month <- event_month
d2$month_booked <- month_booked

# Add year_booked column

d2$year_booked <- as.factor(year(d2$date_booked))

# Add event_year column

d2$event_year <- as.factor(year(d2$start_date))

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

# Conventions only subset

convention_stats <- subset(d2, type == "Convention (CONV)")
convention_stats_original <- subset(d1, type == "Convention (CONV)")

# Tradeshows only subset

trsh_stats <- subset(d2, type == "Consumer Tradeshow (TRSH)")

# Concerts only subset

con_stats <- subset(d2, type == "Concert (CON)")

#-------------------
# Full Sample Stats
#-------------------

discountvalue_summary <- d2 %>%
  summarise(Discount = "Dollar Value",
            Mean = round(mean(rental_discount), digits = 0),
            Median = round(median(rental_discount), digits = 0),
            Number_of_Events = n())

percent_discount_summary <- d2 %>%
  summarise(Discount = "Percent",
            Mean = round(mean(percentage_discount), digits = 0),
            Median = round(median(percentage_discount), digits = 0),
            Number_of_Events = n())

summary_discount <- rbind(discountvalue_summary, percent_discount_summary)

# Function to use for summary stats

summary_stats <- function(df, category, discount) {
  category <- enquo(category)
  discount <- enquo(discount)
  
  df %>%
    group_by(!!category) %>%
    summarize(Mean = round(mean(!!discount), digits = 0),
              Median = round(median(!!discount), digits = 0),
              Number_of_Events = n())
}

# Function to use for summary stats that involve a month variable

summary_stats_month <- function(df, category, discount) {
  category <- enquo(category)
  discount <- enquo(discount)
  
  df %>%
    group_by(!!category) %>%
    summarize(Mean = round(mean(!!discount), digits = 0),
              Median = round(median(!!discount), digits = 0),
              Number_of_Events = n()) %>%
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

summary_by_eventyear <- summary_stats(d2, event_year, percentage_discount)

summary_by_foodbeveragerevenue <- summary_stats(d2, food_beverage_revenue, percentage_discount)

# Summary stats for month variables

summary_by_eventmonth <- summary_stats_month(d2, event_month, percentage_discount)

summary_by_monthbooked <- summary_stats_month(d2, month_booked, percentage_discount)

conventionsummary_by_eventmonth <- summary_stats_month(convention_stats, event_month, percentage_discount)

# Total event counts by type per year

event_counts_year <- d2 %>%
  group_by(type, event_year) %>%
  summarise(number_of_events = n())

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

# Total event counts by type for December and June

december_counts <- subset(event_counts, event_month == "December" | event_month == "June")

# Function to create barcharts

create_barchart <- function(df, category, y) {
  df %>%
    ggplot(aes(x = category, y = y)) +
    geom_bar(stat = "identity", fill = "chartreuse3") +
    ggtitle(paste(colnames(df[2]), "Percentage Discount by", colnames(df[1]))) +
    theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
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
    geom_bar(stat = "identity", fill = "chartreuse3") +
    ggtitle(paste(colnames(df[2]), "Percentage Discount by", colnames(df[1]))) +
    theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
    xlab(colnames(df[1])) + ylab(paste(colnames(df[2]), " Percent Discount"))
}

#---------------------
# Powerpoint Creation
#---------------------

# Set Captions

intro <- "The following tables and charts show rental discount patterns over the past few years in relation to different variables."
intro2 <- "From initial analyses, there are not many conclusive patterns for discounting room rentals. Instead, a great deal of variation exists between discounts, regardless of which variable is being observed."
intro3 <- "One strong trend does exist with conventions being discounted more heavily in January and February."
intro4 <- "The most important overarching observation is that there exists a relationship between the dollar value of the rental discount and the total event revenue before discount, but this relationship disappears when replacing dollar value discount with percentage discount."
cap1 <- "Percentage discount varies between 3% and 16% based on the type of event with concerts receiving the lowest percentage discount."
cap2 <- "Percentage discount varies each year with 2016 showing the highest average and median discount. It is important to note that 2017 has far fewer events recorded than the years previous because it has not reached year end."
cap3 <- "This is potentially due to the majority of events during these months being Private/Social or Meeting events."
cap4 <- "December and June events appear to be discounted the most."
cap5 <- "Events booked in January, April, August, October, and December have median discount rates of 0%. The highest number of events booked in a single month is January, with 118 more events than the next highest month March."
cap6 <- "The number of days an event was booked in advance had no influence on the discount."
cap7 <- "Analysis was conducted with both dollar values and percentages to show any discrepancies in patterns."
cap8 <- "Discount percentages vary by event type when grouped by the year the event was booked as well."
cap9 <- "Conventions and Consumer Tradeshows appear to be the only types that show correlation between rental revenue and mean percentage discount. Note, Private/Social events were excluded from this graph."
cap10 <- "In most cases, 2016 appears to have had higher average discounting than 2015. Note that January of 2017 has passed, but appears to not have discounted any events."
cap11 <- "This graph shows the same variables as the previous, but excludes all Private/Social events."
cap12 <- "Almost all event months show average discount rates between 10% and 20%."
cap13 <- "There appears to be no relationship between food and beverage revenues and percentage discount."
cap14 <- "This chart shows average percentage discounts for just Conventions. January conventions are discounted the most out of all months, and there are no conventions held in August or December to show."
cap15 <- "This plot shows that the dollar value of the food and beverage revenue has some correlation with the dollar value of the rental discount for conventions and private/social events."
cap16 <- "Total event attendance has a slight trend when looking at the dollar value of the rental discount applied to conventions specifically."
cap17 <- "The rental discount dollar value seems to show some trend when plotted against total revenue before discounts, especially for conventions."
cap18 <- "Private/Social events are discounted more than any other event type; many are given room rentals at a 100% discount."
cap19 <- "When looking at percentage discount compared with total revenue before discount, the patten displayed in the dollar value discount plot disappears."

# Set font style

text_prop <- fp_text(font.size = 16)
level_1 <- fp_text(font.size = 14, bold = TRUE)
level_2 <- fp_text(font.size = 14)
level_3 <- fp_text(font.size = 12)

# Create flextables from summary tables to ensure they will fit on slides properly

ft_discount <- flextable(summary_discount) %>%
  add_header(top = TRUE, Discount = "Room Rental Discount Summary Statistics",
             Mean = "", Median = "", Number_of_Events = "") %>%
  merge_at(i = 1, j = 1:4, part = "header") %>%
  fontsize(part = "header", size = 16) %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  padding(padding = 3, part = "all") %>%
  bg(bg = "chartreuse3", part = "header") %>%
  color(color = "white", part = "header") %>%
  color(color = "darkgreen", part = "body") %>%
  border(border = fp_border(color = "darkblue"), part = "all") %>%
  autofit() %>%
  width(j = 1, width = 3) %>%
  width(j = 2, width = 1.5) %>%
  width(j = 3, width = 1.5) %>%
  width(j = 4, width = 3)

ft_type <- flextable(summary_by_type) %>%
  add_header(top = TRUE, type = "Percentage Discount Summary Statistics by Event Type",
             Mean = "", Median = "", Number_of_Events = "") %>%
  merge_at(i = 1, j = 1:4, part = "header") %>%
  fontsize(part = "header", size = 16) %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  padding(padding = 3, part = "all") %>%
  bg(bg = "chartreuse3", part = "header") %>%
  color(color = "white", part = "header") %>%
  color(color = "darkgreen", part = "body") %>%
  border(border = fp_border(color = "darkblue"), part = "all") %>%
  autofit() %>%
  width(j = 1, width = 3) %>%
  width(j = 2, width = 1.5) %>%
  width(j = 3, width = 1.5) %>%
  width(j = 4, width = 3)

ft_eventyear <- flextable(summary_by_eventyear) %>%
  add_header(top = TRUE, event_year = "Percentage Discount Summary Statistics by Year Booked",
             Mean = "", Median = "", Number_of_Events = "") %>%
  merge_at(i = 1, j = 1:4, part = "header") %>%
  fontsize(part = "header", size = 16) %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  padding(padding = 3, part = "all") %>%
  bg(bg = "chartreuse3", part = "header") %>%
  color(color = "white", part = "header") %>%
  color(color = "darkgreen", part = "body") %>%
  border(border = fp_border(color = "darkblue"), part = "all") %>%
  autofit() %>%
  width(j = 1, width = 3) %>%
  width(j = 2, width = 1.5) %>%
  width(j = 3, width = 1.5) %>%
  width(j = 4, width = 3)

ft_eventmonth <- flextable(summary_by_eventmonth) %>%
  add_header(top = TRUE, event_month = "Percentage Discount Summary Statistics by Event Month",
             Mean = "", Median = "", Number_of_Events = "") %>%
  merge_at(i = 1, j = 1:4, part = "header") %>%
  fontsize(part = "header", size = 16) %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  padding(padding = 3, part = "all") %>%
  bg(bg = "chartreuse3", part = "header") %>%
  color(color = "white", part = "header") %>%
  color(color = "darkgreen", part = "body") %>%
  border(border = fp_border(color = "darkblue"), part = "all") %>%
  autofit() %>%
  width(j = 1, width = 3) %>%
  width(j = 2, width = 1.5) %>%
  width(j = 3, width = 1.5) %>%
  width(j = 4, width = 3)

ft_monthbooked <- flextable(summary_by_monthbooked) %>%
  add_header(top = TRUE, month_booked = "Percentage Discount Summary Statistics by Month Booked",
             Mean = "", Median = "", Number_of_Events = "") %>%
  merge_at(i = 1, j = 1:4, part = "header") %>%
  fontsize(part = "header", size = 16) %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  padding(padding = 3, part = "all") %>%
  bg(bg = "chartreuse3", part = "header") %>%
  color(color = "white", part = "header") %>%
  color(color = "darkgreen", part = "body") %>%
  border(border = fp_border(color = "darkblue"), part = "all") %>%
  autofit() %>%
  width(j = 1, width = 3) %>%
  width(j = 2, width = 1.5) %>%
  width(j = 3, width = 1.5) %>%
  width(j = 4, width = 3)

ft_decembercounts <- flextable(december_counts) %>%
  add_header(top = TRUE, type = "Number of Events by Type in June and December",
             event_month = "", number_of_events = "") %>%
  merge_at(i = 1, j = 1:3, part = "header") %>%
  fontsize(part = "header", size = 16) %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  padding(padding = 3, part = "all") %>%
  bg(bg = "chartreuse3", part = "header") %>%
  color(color = "white", part = "header") %>%
  color(color = "darkgreen", part = "body") %>%
  border(border = fp_border(color = "darkblue"), part = "all") %>%
  autofit() %>%
  width(j = 1, width = 3) %>%
  width(j = 2, width = 3) %>%
  width(j = 3, width = 3)

# Build Slide Deck

pres <- read_pptx(file.path("V:", "Economic Intelligence", "Shaw Conference Centre",
                            "Projects", "Discount Analysis", "template.pptx"))

pres <- pres %>%
  # Title Slide
  add_slide(layout = "Title Slide", master = "Office Theme") %>%
  ph_with_text(type = "ctrTitle", str = "SCC Room Rental Discount Historical Patterns") %>% 
  ph_with_text(type = "subTitle", str = "Exploratory Analytics") %>%
  
  # Definitions Slide
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "body") %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "Mean - the average of all observation values",
              style = level_1) %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "Median - the value that is the midpoint of the observations when they are sorted from smallest to largest",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Can be helpful to show if the data is skewed to one side, meaning that more of the data is closer to the maximum value or the minimum value in the data set",
              style = level_2) %>%
  ph_add_par(level = 3) %>%
  ph_add_text(str = "If median is close to the max observation value, most of the observations appear closer to the maximum observation value in the data set.",
              style = level_3) %>%
  ph_add_par(level = 3) %>%
  ph_add_text(str = "If median is close to the min observation value, most of the observations appear closer to the minimum observation value in the data set.",
              style = level_3) %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "Correlation - the relationship between two variables",
              style = level_1) %>%
  ph_with_text(type = "title", index = 1, str = "Definitions") %>%
  ph_with_img_at(src = "skewness_diagram.jpg", width = 6, height = 2, left = 2, top = 5) %>%
  ph_with_text(type = "sldNum", str = "1" ) %>%
  
  # Data Cleaning Slide
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "body") %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "Data from USI",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "2940 events (135 of these are conventions)",
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Includes all events from USI that occured between January 2015 and the end of June 2017",
              style = level_2) %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "Usable Data for Analysis",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "565 events (103 of these are conventions)",
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Events with NA (missing) values in the following variables were omitted:",
              style = level_2) %>%
  ph_add_par(level = 3) %>%
  ph_add_text(str = "rental_discount",
              style = level_3) %>%
  ph_add_par(level = 3) %>%
  ph_add_text(str = "rental_revenue",
              style = level_3) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Events with values of zero in the following variables were omitted:",
              style = level_2) %>%
  ph_add_par(level = 3) %>%
  ph_add_text(str = "rental_revenue",
              style = level_3) %>%
  ph_add_par(level = 3) %>%
  ph_add_text(str = "total_event_attendance",
              style = level_3) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Events with positive rental_discount values (inflation charges) were omitted",
              style = level_2) %>%
  ph_with_text(type = "title", index = 1, str = "Data Cleaning Process") %>%
  ph_with_text(type = "sldNum", str = "2" ) %>%
  
  # Variables that couldn't be used Slide
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "body") %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "Variables that could not be analyzed due to missing data:",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "actual_economic_impact",
              style = level_2) %>%
  ph_add_par(level = 3) %>%
  ph_add_text(str = "Only 77/2940 events had values for this variable",
              style = level_3) %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "Variables that could not be analyzed due to complexity/variable format issues:",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "booked_spaces",
              style = level_2) %>%
  ph_add_par(level = 3) %>%
  ph_add_text(str = "Because of how spaces are named (ex. Hall A, Hall B, or Hall AB), and because they are presented in a string format, it is difficult to utilize this variable for comparison of rental discounts",
              style = level_3) %>%
  ph_add_par(level = 3) %>%
  ph_add_text(str = "Analysis was attempted by creating binary variables that indicate whether or not a hall, salon, or room was booked; this analysis did not give much insight however, because multiple types of spaces are usually booked for each event.",
              style = level_3) %>%
  ph_with_text(type = "title", index = 1, str = "Variables Omitted from Analysis") %>%
  ph_with_text(type = "sldNum", str = "3" ) %>%
    
  # Variables created/added for analysis Slide
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "body") %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "total_revenue_pre_discount",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "= rental_revenue + other_revenue + food_beverage_revenue",
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Total revenue before any discount was applied",
              style = level_2) %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "percentage_discount",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "= rental_discount/total_revenue_pre_discount",
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Rental discount as a percentage of total revenue before the discount",
              style = level_2) %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "event_month",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "= month of start_date",
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "The month an event took place",
              style = level_2) %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "month_booked",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "= month of date_booked",
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "The month an event was booked",
              style = level_2) %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "event_year",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "= year of start_date",
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "The year an event took place",
              style = level_2) %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "advance_booking",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "= start_date - date_booked",
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "How many days in advance and event was booked",
              style = level_2) %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "space_count",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "= sum of the spaces in the list",
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Number of rooms/salons/halls booked for an event",
              style = level_2) %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "number_of_days",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "= end_date - start_date + 1",
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Duration of an event in days",
              style = level_2) %>%
  ph_with_text(type = "title", index = 1, str = "Variables Added for Analysis") %>%
  ph_with_text(type = "sldNum", str = "4" ) %>%
  
  # Expectations Slide
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "body") %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "Seasonality due to the recurring nature of events",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Events that occur during the same time of year each year should show high correlation",
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Each month when compared across different years should in theory produce similar average percentage discount values because of this",
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Some months are assumed to be slower than others for certain event types, and therefore it would be expected that events taking place during slow months would be discounted more heavily",
              style = level_2) %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "Correlation between percentage discount and the type of event",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Each event type has certain defining characteristics which would in theory inform the size, cost, and services provided, therefore also affecting the percentage discount received",
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Each event type should theoretically have its own specific discount range due to these defining characteristics",
              style = level_2) %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "Correlation between percentage discount and amount of food and beverage revenue",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Under the assumption that food and beverage revenue forms the majority of profits, events with higher food and beverage revenue should receive higher discounts on room rentals",
              style = level_2) %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "Correlation between the number of spaces (halls, salons, and rooms) booked and the percentage discount",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "It is common for discounts to increase as more products or services are purchased",
              style = level_2) %>%
  ph_with_text(type = "title", index = 1, str = "Data Expectations") %>%
  ph_with_text(type = "sldNum", str = "5" ) %>%
  
  # Summary Slide
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "body") %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = intro,
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = intro2,
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = intro3,
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = intro4,
              style = level_2) %>%
  ph_with_text(type = "title", index = 1, str = "Summary") %>%
  ph_with_text(type = "sldNum", str = "6" ) %>%
  
  # Slide with Summary Table
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap7, type = "title", style = text_prop) %>% 
  ph_with_flextable(value = ft_discount, type = "body", index = 1) %>%
  ph_with_text(type = "sldNum", str = "7" ) %>%
  
  # Slide with Type Table
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap1, type = "title", style = text_prop) %>% 
  ph_with_flextable(value = ft_type, type = "body", index = 1) %>%
  ph_with_text(type = "sldNum", str = "8" ) %>%
  
  # Slide with Barchart (Event Type)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap18, type = "title", style = text_prop) %>%
  ph_with_vg(code = print(create_barchart(summary_by_type,
                                          summary_by_type$type,
                                          summary_by_type$Mean)), type = "body") %>%
  ph_with_text(type = "sldNum", str = "9" ) %>%
  
  # Slide with Event Year Table
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap2, type = "title", style = text_prop) %>%
  ph_with_flextable(value = ft_eventyear, type = "body", index = 1) %>%
  ph_with_text(type = "sldNum", str = "10" ) %>%
  
  # Slide with Event Month Table
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap12, type = "title", style = text_prop) %>% 
  ph_with_flextable(value = ft_eventmonth, type = "body", index = 1) %>%
  ph_with_text(type = "sldNum", str = "11" ) %>%
  
  # Slide with Barchart (Event Month)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap4, type = "title", style = text_prop) %>% 
  ph_with_vg(code = print(create_month_barchart(summary_by_eventmonth,
                                                summary_by_eventmonth$event_month,
                                                summary_by_eventmonth$Mean)), type = "body") %>%
  ph_with_text(type = "sldNum", str = "12" ) %>%
  
  # Slide with December Counts Table
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap3, type = "title", style = text_prop) %>% 
  ph_with_flextable(value = ft_decembercounts, type = "body", index = 1) %>%
  ph_with_text(type = "sldNum", str = "13" ) %>%
  
  # Slide with Barchart (Event Month - Just Conventions)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap14, type = "title", style = text_prop) %>% 
  ph_with_vg(code = print(create_month_barchart(conventionsummary_by_eventmonth,
                                                conventionsummary_by_eventmonth$event_month,
                                                conventionsummary_by_eventmonth$Mean)), type = "body") %>%
  ph_with_text(type = "sldNum", str = "14" ) %>%
  
  # Slide with Scatterplot (Advanced Booking and Type)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap6, type = "title", style = text_prop) %>% 
  ph_with_vg(code = print(d2 %>%
                            group_by(advance_booking, type) %>%
                            ggplot(aes(x = advance_booking, y = percentage_discount)) +
                            geom_point(aes(colour=factor(type)), stat = "identity") +
                            theme(legend.position = "top", legend.direction = "horizontal") +
                            labs(color = "") +
                            ggtitle("Percentage Discount by Number of Days Booked in Advance, Grouped by Event Type") +
                            theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))),
             type = "body") %>%
  ph_with_text(type = "sldNum", str = "15" ) %>%
  
  # Slide with Bar Chart (Event Type and Event Year)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap8, type = "title", style = text_prop) %>% 
  ph_with_vg(code = print(d2 %>%
                            group_by(event_year, type) %>%
                            summarise(mean_percentage_discount = mean(percentage_discount),
                                      median = median(percentage_discount),
                                      standard_dev = sd(percentage_discount),
                                      min = min(percentage_discount),
                                      max = max(percentage_discount),
                                      n = n()) %>%
                            ggplot(aes(x = type, y = mean_percentage_discount, fill = event_year)) +
                            scale_fill_manual(values = c("chartreuse3", "goldenrod1", "darkblue")) +
                            geom_bar(stat = "identity", position = position_dodge()) +
                            theme(legend.position = "top", legend.direction = "horizontal") +
                            labs(fill="") +
                            xlab("Event Type") + ylab("Mean Percent Discount") +
                            ggtitle("Mean Percentage Discount by Event Type, Grouped by Event Year") +
                            theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))),
             type = "body") %>%
  ph_with_text(type = "sldNum", str = "16" ) %>%
  
  # Slide with Bar Chart (Event Year and Event Month)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap10, type = "title", style = text_prop) %>% 
  ph_with_vg(code = print(d2 %>%
                            group_by(event_year, event_month) %>%
                            summarise(mean_percentage_discount = mean(percentage_discount),
                                      median = median(percentage_discount),
                                      standard_dev = sd(percentage_discount),
                                      min = min(percentage_discount),
                                      max = max(percentage_discount),
                                      n = n()) %>%
                            mutate(event_month = factor(event_month,
                                                        levels = c("January", "February",
                                                                   "March", "April", "May",
                                                                   "June", "July", "August",
                                                                   "September", "October",
                                                                   "November", "December"))) %>%
                            arrange(event_month) %>%
                            ggplot(aes(x = event_month,
                                       y = mean_percentage_discount,
                                       fill = event_year)) +
                            scale_fill_manual(values = c("chartreuse3", "goldenrod1", "darkblue")) +
                            geom_bar(stat = "identity", position = position_dodge()) +
                            theme(legend.position = "top", legend.direction = "horizontal") +
                            labs(fill="") +
                            xlab("Event Month") + ylab("Mean Percent Discount") +
                            ggtitle("Mean Percentage Discount by Event Month, Grouped by Event Year") +
                            theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))),
             type = "body") %>%
  ph_with_text(type = "sldNum", str = "17" ) %>%
  
  # Slide with Bar Chart (Event Year and Event Month - Private/Social)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap11, type = "title", style = text_prop) %>% 
  ph_with_vg(code = print(dtypesubset %>%
                            group_by(event_year, event_month) %>%
                            summarise(mean_percentage_discount = mean(percentage_discount),
                                      median = median(percentage_discount),
                                      standard_dev = sd(percentage_discount),
                                      min = min(percentage_discount),
                                      max = max(percentage_discount),
                                      n = n()) %>%
                            mutate(event_month = factor(event_month,
                                                        levels = c("January", "February",
                                                                   "March", "April","May",
                                                                   "June", "July", "August",
                                                                   "September", "October",
                                                                   "November", "December"))) %>%
                            arrange(event_month) %>%
                            ggplot(aes(x = event_month,
                                       y = mean_percentage_discount,
                                       fill = event_year)) +
                            scale_fill_manual(values = c("chartreuse3", "goldenrod1", "darkblue")) +
                            geom_bar(stat = "identity", position = position_dodge()) +
                            theme(legend.position = "top", legend.direction = "horizontal") +
                            labs(fill="") +
                            xlab("Event Month") + ylab("Mean Percent Discount") +
                            ggtitle("Mean Percentage Discount by Event Month, Grouped by Event Year") +
                            theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))),
             type = "body") %>%
  ph_with_text(type = "sldNum", str = "18" ) %>%
  
  # Slide with Scatterplot (Total Event Attendance vs Rental Discount (Conventions Only))
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap16, type = "title", style = text_prop) %>% 
  ph_with_vg(code = print(convention_stats %>%
                            group_by(total_event_attendance, type) %>%
                            ggplot(aes(x = total_event_attendance, y = rental_discount)) +
                            geom_point(aes(colour=factor(type)), stat = "identity") +
                            theme(legend.position = "top", legend.direction = "horizontal") +
                            geom_smooth(method = "lm", se = FALSE) +
                            labs(color = "") +
                            scale_y_continuous(labels = scales::dollar) +
                            ggtitle("Rental Discount by Total Event Attendance for Conventions") +
                            theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))),
             type = "body") %>%
  ph_with_text(type = "sldNum", str = "19" ) %>%
  
  # Slide with Scatterplot (Food/Beverage Revenue and Type w/ Rental Discount)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap15, type = "title", style = text_prop) %>% 
  ph_with_vg(code = print(d2 %>%
                            group_by(food_beverage_revenue, type) %>%
                            ggplot(aes(x = food_beverage_revenue, y = rental_discount)) +
                            geom_point(aes(colour=factor(type)), stat = "identity") +
                            theme(legend.position = "top", legend.direction = "horizontal") +
                            labs(color = "") +
                            scale_x_continuous(labels = scales::dollar) +
                            scale_y_continuous(labels = scales::dollar) +
                            ggtitle("Rental Discount by Food/Beverage Revenue, Grouped by Event Type") +
                            theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))),
            type = "body") %>%
  ph_with_text(type = "sldNum", str = "20" ) %>%
  
  # Slide with Scatterplot (Food/Beverage Revenue w/ Percentage Discount)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap13, type = "title", style = text_prop) %>% 
  ph_with_vg(code = print(d2 %>%
                            group_by(food_beverage_revenue, type) %>%
                            ggplot(aes(x = food_beverage_revenue, y = percentage_discount)) +
                            geom_point(aes(colour=factor(type)), stat = "identity") +
                            theme(legend.position = "top", legend.direction = "horizontal") +
                            labs(color = "") +
                            scale_x_continuous(labels = scales::dollar) +
                            ggtitle("Percentage Discount by Food/Beverage Revenue, Grouped by Event Type") +
                            theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))),
             type = "body") %>%
  ph_with_text(type = "sldNum", str = "21" ) %>%
  
  

  # Slide with Scatterplot (Total Revenue w/ Rental Discount)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap17, type = "title", style = text_prop) %>% 
  ph_with_vg(code = print(d2 %>%
                            group_by(total_revenue_pre_discount, type) %>%
                            ggplot(aes(x = total_revenue_pre_discount, y = rental_discount)) +
                            geom_point(aes(colour = factor(type)), stat = "identity") +
                            theme(legend.position = "top", legend.direction = "horizontal") +
                            labs(color = "") +
                            scale_x_continuous(labels = scales::dollar) +
                            scale_y_continuous(labels = scales::dollar) +
                            ggtitle("Rental Discount by Total Revenue Pre Discount, Grouped by Event Type") +
                            theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))),
             type = "body") %>%
  ph_with_text(type = "sldNum", str = "22" ) %>%
  
  # Slide with Scatterplot (Total Revenue w/ Percentage Discount)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap19, type = "title", style = text_prop) %>% 
  ph_with_vg(code = print(d2 %>%
                            group_by(total_revenue_pre_discount, type) %>%
                            ggplot(aes(x = total_revenue_pre_discount, y = percentage_discount)) +
                            geom_point(aes(colour = factor(type)), stat = "identity") +
                            theme(legend.position = "top", legend.direction = "horizontal") +
                            labs(color = "") +
                            scale_x_continuous(labels = scales::dollar) +
                            ggtitle("Percentage Discount by Total Revenue Pre Discount, Grouped by Event Type") +
                            theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))),
             type = "body") %>%
  ph_with_text(type = "sldNum", str = "23" ) %>%

# Save Powerpoint
print(pres, target = "SCC Room Rental Template.pptx") %>%
  invisible()

