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
library(flextable)
#library(ReporteRs)
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

# Full Sample Stats
#-------------------

discountvalue_summary <- d2 %>%
  summarise(Discount = "Dollar Value",
            Mean = mean(rental_discount),
            Median = median(rental_discount),
            Standard_Deviation = sd(rental_discount),
            Min = min(rental_discount),
            Max = max(rental_discount),
            n = n())

percent_discount_summary <- d2 %>%
  summarise(Discount = "Percent",
            Mean = mean(percentage_discount),
            Median = median(percentage_discount),
            Standard_Deviation = sd(percentage_discount),
            Min = min(percentage_discount),
            Max = max(percentage_discount),
            n = n())

summary_discount <- rbind(discountvalue_summary, percent_discount_summary)

# Function to use for summary stats

summary_stats <- function(df, category, discount) {
  category <- enquo(category)
  discount <- enquo(discount)
  
  df %>%
    group_by(!!category) %>%
    summarize(Mean = round(mean(!!discount), digits = 3),
              Median = round(median(!!discount), digits = 3),
              Standard_Deviation = round(sd(!!discount), digits = 3),
              Min = round(min(!!discount), digits = 3),
              Max = round(max(!!discount), digits = 3),
              n = n())
}

# Function to use for summary stats that involve a month variable

summary_stats_month <- function(df, category, discount) {
  category <- enquo(category)
  discount <- enquo(discount)
  
  df %>%
    group_by(!!category) %>%
    summarize(Mean = round(mean(!!discount), digits = 3),
              Median = round(median(!!discount), digits = 3),
              Standard_Deviation = round(sd(!!discount), digits = 3),
              Min = round(min(!!discount), digits = 3),
              Max = round(max(!!discount), digits = 3),
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

december_counts <- subset(event_counts, event_month == "December")

# Function to create barcharts

create_barchart <- function(df, category, y) {
  df %>%
    ggplot(aes(x = category, y = y)) +
    geom_bar(stat = "identity", fill = "chartreuse3") +
    ggtitle(paste(colnames(df[2]), "Percentage Discount by", colnames(df[1]))) +
    theme(plot.title = element_text(size = 20, face = "bold")) +
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
    theme(plot.title = element_text(size = 20, face = "bold")) +
    xlab(colnames(df[1])) + ylab(paste(colnames(df[2]), " Percent Discount"))
}

#--------------------------------------
# Powerpoint Creation
#--------------------------------------

# Set Captions

intro <- "The following tables and charts show rental discount patterns over the past few years in relation to different variables."
cap1 <- "Private/Social events are discounted much more than any other event type; many are discounted 100%"
cap2 <- "Percentage discount varies each year with 2016 showing the highest average and median discount. It is important to note that 2017 has far fewer events recorded than the years previous because it is not yet the end of the year."
cap3 <- "This is potentially due to the majority of events during this month being Private/Social events"
cap4 <- "December events appear to be discounted the most"
cap5 <- "Note: Events held in January, April, August, October, and December have median discount rates of 0%"
cap6 <- "The number of days an event was booked in advance had some influence on the discount, but only for certain event types"
cap7 <- ""

content_title2 <- "Percentage Discount Summary Statistics by Year Booked"

# Set font styles

text_prop <- fp_text(font.size = 16)
subtext_prop <- fp_text(font.size = 12)

# Create flextables from summary tables to ensure they will fit on slides

ft_discount <- flextable(summary_discount) %>%
  add_header(top = TRUE, Discount = "Discount Summary Statistics",
             Mean = "", Median = "", Standard_Deviation = "", Max = "", Min = "", n = "") %>%
  merge_at(i = 1, j = 1:7, part = "header") %>%
  fontsize(part = "header", size = 16) %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  padding(padding = 3, part = "all") %>%
  bg(bg = "chartreuse3", part = "header") %>%
  color(color = "white", part = "header") %>%
  color(color = "darkgreen", part = "body") %>%
  border(border = fp_border(color = "goldenrod1"), part = "all") %>%
  autofit() %>%
  width(j = 1, width = 2.5)

ft_type <- flextable(summary_by_type) %>%
  add_header(top = TRUE, type = "Percentage Discount Summary Statistics by Event Type",
             Mean = "", Median = "", Standard_Deviation = "", Max = "", Min = "", n = "") %>%
  merge_at(i = 1, j = 1:7, part = "header") %>%
  fontsize(part = "header", size = 16) %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  padding(padding = 3, part = "all") %>%
  bg(bg = "chartreuse3", part = "header") %>%
  color(color = "white", part = "header") %>%
  color(color = "darkgreen", part = "body") %>%
  border(border = fp_border(color = "goldenrod1"), part = "all") %>%
  autofit() %>%
  width(j = 1, width = 2.5)

ft_yearbooked <- flextable(summary_by_yearbooked) %>%
  add_header(top = TRUE, year_booked = "Percentage Discount Summary Statistics by Year Booked",
             Mean = "", Median = "", Standard_Deviation = "", Max = "", Min = "", n = "") %>%
  merge_at(i = 1, j = 1:7, part = "header") %>%
  fontsize(part = "header", size = 16) %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  padding(padding = 3, part = "all") %>%
  bg(bg = "chartreuse3", part = "header") %>%
  color(color = "white", part = "header") %>%
  color(color = "darkgreen", part = "body") %>%
  border(border = fp_border(color = "goldenrod1"), part = "all") %>%
  autofit() %>%
  width(j = 1, width = 2.5)
  
ft_eventmonth <- flextable(summary_by_eventmonth) %>%
  add_header(top = TRUE, event_month = "Percentage Discount Summary Statistics by Event Month",
             Mean = "", Median = "", Standard_Deviation = "", Max = "", Min = "", n = "") %>%
  merge_at(i = 1, j = 1:7, part = "header") %>%
  fontsize(part = "header", size = 16) %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  padding(padding = 3, part = "all") %>%
  bg(bg = "chartreuse3", part = "header") %>%
  color(color = "white", part = "header") %>%
  color(color = "darkgreen", part = "body") %>%
  border(border = fp_border(color = "goldenrod1"), part = "all") %>%
  autofit() %>%
  width(j = 1, width = 2.5)

ft_monthbooked <- flextable(summary_by_monthbooked) %>%
  add_header(top = TRUE, month_booked = "Percentage Discount Summary Statistics by Month Booked",
             Mean = "", Median = "", Standard_Deviation = "", Max = "", Min = "", n = "") %>%
  merge_at(i = 1, j = 1:7, part = "header") %>%
  fontsize(part = "header", size = 16) %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  padding(padding = 3, part = "all") %>%
  bg(bg = "chartreuse3", part = "header") %>%
  color(color = "white", part = "header") %>%
  color(color = "darkgreen", part = "body") %>%
  border(border = fp_border(color = "goldenrod1"), part = "all") %>%
  autofit() %>%
  width(j = 1, width = 2.5)

ft_decembercounts <- flextable(december_counts) %>%
  add_header(top = TRUE, type = "Number of Events by Type in December",
             event_month = "", number_of_events = "") %>%
  merge_at(i = 1, j = 1:3, part = "header") %>%
  fontsize(part = "header", size = 16) %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  padding(padding = 3, part = "all") %>%
  bg(bg = "chartreuse3", part = "header") %>%
  color(color = "white", part = "header") %>%
  color(color = "darkgreen", part = "body") %>%
  border(border = fp_border(color = "goldenrod1"), part = "all") %>%
  autofit() %>%
  width(j = 1, width = 2.5)

# Build Slide Deck

pres <- read_pptx()

pres <- pres %>%
  # 1. Title Slide
  add_slide(layout = "Title Slide", master = "Office Theme") %>%
    ph_with_text(type = "ctrTitle", str = "SCC Room Rental Discount Historical Patterns") %>% 
    ph_with_text(type = "subTitle", str = "Exploratory Analytics") %>%
  
  # 2. Summary Slide
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_text(type = "title", index = 1, str = "Summary") %>%
    ph_with_text(type="body", str = intro ) %>% 
    ph_with_text(type = "sldNum", str = "1" ) %>%
  
  # 3. Slide with Type Table
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_empty(type = "title") %>%
    ph_add_par() %>%
    ph_add_text(str = cap1, type = "title", style = text_prop) %>% 
    ph_with_flextable(value = ft_discount, type = "body", index = 1) %>%
    ph_with_text(type = "sldNum", str = "2" ) %>%
  
  # 3. Slide with Type Table
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_empty(type = "title") %>%
    ph_add_par() %>%
    ph_add_text(str = cap1, type = "title", style = text_prop) %>% 
    ph_with_flextable(value = ft_type, type = "body", index = 1) %>%
    ph_with_text(type = "sldNum", str = "2" ) %>%
  
  # 4. Slide with Barchart (Event Type)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_empty(type = "title") %>%
    ph_add_par() %>%
    ph_remove(type = "title", id_chr = 2) %>% 
    ph_with_vg(code = print(create_barchart(summary_by_type,
                                            summary_by_type$type,
                                            summary_by_type$Mean)), type = "body") %>%
    ph_with_text(type = "sldNum", str = "3" ) %>%
  
  # 5. Slide with Year Booked Table
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_empty(type = "title") %>%
    ph_add_par() %>%
    ph_add_text(str = cap2, type = "title", style = text_prop) %>%
    ph_with_flextable(value = ft_yearbooked, type = "body", index = 1) %>%
    ph_with_text(type = "sldNum", str = "4" ) %>%
    
  # 6. Slide with Barchart (Event Month)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_empty(type = "title") %>%
    ph_add_par() %>%
    ph_add_text(str = cap4, type = "title", style = text_prop) %>% 
    ph_with_vg(code = print(create_month_barchart(summary_by_eventmonth,
                                                  summary_by_eventmonth$event_month,
                                                  summary_by_eventmonth$Mean)), type = "body") %>%
    ph_with_text(type = "sldNum", str = "5" ) %>%

  # 7. Slide with December Counts Table
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_empty(type = "title") %>%
    ph_add_par() %>%
    ph_add_text(str = cap3, type = "title", style = text_prop) %>% 
    ph_with_flextable(value = ft_decembercounts, type = "body", index = 1) %>%
    ph_with_text(type = "sldNum", str = "6" ) %>%
  
  # 8. Slide with Event Month Table
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_empty(type = "title") %>%
    ph_add_par() %>%
    ph_add_text(str = cap4, type = "title", style = text_prop) %>% 
    ph_with_flextable(value = ft_eventmonth, type = "body", index = 1) %>%
    ph_with_text(type = "sldNum", str = "7" ) %>%
    
  # 9. Slide with Month Booked Table
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_empty(type = "title") %>%
    ph_add_par() %>%
    ph_add_text(str = cap5, type = "title", style = text_prop) %>% 
    ph_with_flextable(value = ft_monthbooked, type = "body", index = 1) %>%
    ph_with_text(type = "sldNum", str = "8" ) %>%

  # 10. Slide with Scatterplot (Advanced Booking and Type)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_empty(type = "title") %>%
    ph_add_par() %>%
    ph_add_text(str = cap6, type = "title", style = text_prop) %>% 
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
    ph_with_text(type = "sldNum", str = "9" )
    
  # Save Powerpoint
  print(pres, target = "SCC Room Rental Discount History.pptx") %>%
    invisible()
    
