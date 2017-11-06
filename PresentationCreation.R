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

# Conventions only subset

convention_stats <- subset(d2, type == "Convention (CONV)")

#-------------------
# Full Sample Stats
#-------------------

discountvalue_summary <- d2 %>%
  summarise(Discount = "Dollar Value",
            Mean = round(mean(rental_discount), digits = 3),
            Median = round(median(rental_discount), digits = 3),
            Standard_Deviation = round(sd(rental_discount), digits = 3),
            Min = round(min(rental_discount), digits = 3),
            Max = round(max(rental_discount), digits = 3),
            n = n())

percent_discount_summary <- d2 %>%
  summarise(Discount = "Percent",
            Mean = round(mean(percentage_discount), digits = 3),
            Median = round(median(percentage_discount), digits = 3),
            Standard_Deviation = round(sd(percentage_discount), digits = 3),
            Min = round(min(percentage_discount), digits = 3),
            Max = round(max(percentage_discount), digits = 3),
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

summary_by_foodbeveragerevenue <- summary_stats(d2, food_beverage_revenue, percentage_discount)

# Summary stats for month variables

summary_by_eventmonth <- summary_stats_month(d2, event_month, percentage_discount)

summary_by_monthbooked <- summary_stats_month(d2, month_booked, percentage_discount)

conventionsummary_by_eventmonth <- summary_stats_month(convention_stats, event_month, percentage_discount)

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

# Total event counts by type for December

december_counts <- subset(event_counts, event_month == "December")

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

intro <- "The following tables and charts show rental discount patterns over the past few years in relation to different variables. From initial analyses, there are not many conclusive patterns for discounting room rentals. Instead, a great deal of variation exists between discounts, regardless of which variable is being observed. One strong trend does exist with Conventions being discounted more heavily in January and February."
cap1 <- "Private/Social events are discounted much more than any other event type; many are discounted 100%."
cap2 <- "Percentage discount varies each year with 2016 showing the highest average and median discount. It is important to note that 2017 has far fewer events recorded than the years previous because it has not reached year end."
cap3 <- "This is potentially due to the majority of events during this month being Private/Social events."
cap4 <- "December events appear to be discounted the most."
cap5 <- "Events booked in January, April, August, October, and December have median discount rates of 0%. The highest number of events booked in a single month is January, with 118 more events than the next highest month March."
cap6 <- "The number of days an event was booked in advance had very little influence if any on the discount."
cap7 <- "Dollar value summary statistics may not be as useful when looking at patterns due to the variety of event sizes, therefore most further analysis was conducted with discount percentages."
cap8 <- "Discount percentages vary by event type when grouped by the year the event was booked as well."
cap9 <- "Conventions and Consumer Tradeshows appear to be the only types that show correlation between rental revenue and mean percentage discount. Note, Private/Social events were excluded from this graph."
cap10 <- "In most cases, 2016 appears to have had higher average discounting than 2015. Note that there are multiple months that have passed in 2017 that appear to not have discounted any events."
cap11 <- "This graph shows the same variables as the previous, but excludes all Private/Social events."
cap12 <- "Almost all event months show average discount rates between 20% and 50%, December is the only outlier."
cap13 <- "There appears to be no relationship between food and beverage revenues and percentage discount."
cap14 <- "This chart shows average percentage discounts for just Conventions. February conventions are discounted the most out of all months, and there are no conventions held in August or December to show."

# Set font style

text_prop <- fp_text(font.size = 16)

# Create flextables from summary tables to ensure they will fit on slides properly

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
  border(border = fp_border(color = "darkblue"), part = "all") %>%
  autofit() %>%
  width(j = 1, width = 3.25)

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
  border(border = fp_border(color = "darkblue"), part = "all") %>%
  autofit() %>%
  width(j = 1, width = 3.25)

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
  border(border = fp_border(color = "darkblue"), part = "all") %>%
  autofit() %>%
  width(j = 1, width = 3.25)

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
  border(border = fp_border(color = "darkblue"), part = "all") %>%
  autofit() %>%
  width(j = 1, width = 3.25)

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
  border(border = fp_border(color = "darkblue"), part = "all") %>%
  autofit() %>%
  width(j = 1, width = 3.25)

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
  
  # Summary Slide
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_text(type = "title", index = 1, str = "Summary") %>%
  ph_with_text(type = "body", str = intro) %>%
  ph_with_text(type = "sldNum", str = "1" ) %>%
  
  # Slide with Summary Table
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap7, type = "title", style = text_prop) %>% 
  ph_with_flextable(value = ft_discount, type = "body", index = 1) %>%
  ph_with_text(type = "sldNum", str = "2" ) %>%
  
  # Slide with Type Table
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap1, type = "title", style = text_prop) %>% 
  ph_with_flextable(value = ft_type, type = "body", index = 1) %>%
  ph_with_text(type = "sldNum", str = "3" ) %>%
  
  # Slide with Barchart (Event Type)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_remove(type = "title", id_chr = 2) %>% 
  ph_with_vg(code = print(create_barchart(summary_by_type,
                                          summary_by_type$type,
                                          summary_by_type$Mean)), type = "body") %>%
  ph_with_text(type = "sldNum", str = "4" ) %>%
  
  # Slide with Year Booked Table
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap2, type = "title", style = text_prop) %>%
  ph_with_flextable(value = ft_yearbooked, type = "body", index = 1) %>%
  ph_with_text(type = "sldNum", str = "5" ) %>%
  
  # Slide with Event Month Table
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap12, type = "title", style = text_prop) %>% 
  ph_with_flextable(value = ft_eventmonth, type = "body", index = 1) %>%
  ph_with_text(type = "sldNum", str = "6" ) %>%
  
  # Slide with Barchart (Event Month)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap4, type = "title", style = text_prop) %>% 
  ph_with_vg(code = print(create_month_barchart(summary_by_eventmonth,
                                                summary_by_eventmonth$event_month,
                                                summary_by_eventmonth$Mean)), type = "body") %>%
  ph_with_text(type = "sldNum", str = "7" ) %>%
  
  # Slide with December Counts Table
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap3, type = "title", style = text_prop) %>% 
  ph_with_flextable(value = ft_decembercounts, type = "body", index = 1) %>%
  ph_with_text(type = "sldNum", str = "8" ) %>%
  
  # Slide with Barchart (Event Month - Just Conventions)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap14, type = "title", style = text_prop) %>% 
  ph_with_vg(code = print(create_month_barchart(conventionsummary_by_eventmonth,
                                                conventionsummary_by_eventmonth$event_month,
                                                conventionsummary_by_eventmonth$Mean)), type = "body") %>%
  ph_with_text(type = "sldNum", str = "9" ) %>%
  
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
  ph_with_text(type = "sldNum", str = "10" ) %>%
  
  # Slide with Bar Chart (Event Type and Year Booked)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap8, type = "title", style = text_prop) %>% 
  ph_with_vg(code = print(d2 %>%
                            group_by(year_booked, type) %>%
                            summarise(mean_percentage_discount = mean(percentage_discount),
                                      median = median(percentage_discount),
                                      standard_dev = sd(percentage_discount),
                                      min = min(percentage_discount),
                                      max = max(percentage_discount),
                                      n = n()) %>%
                            ggplot(aes(x = type, y = mean_percentage_discount, fill = year_booked)) +
                            scale_fill_manual(values = c("chartreuse3", "goldenrod1", "darkblue")) +
                            geom_bar(stat = "identity", position = position_dodge()) +
                            theme(legend.position = "top", legend.direction = "horizontal") +
                            labs(fill="") +
                            xlab("Event Type") + ylab("Mean Percent Discount") +
                            ggtitle("Mean Percentage Discount by Event Type, Grouped by Year Booked") +
                            theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))),
             type = "body") %>%
  ph_with_text(type = "sldNum", str = "11" ) %>%
  
  # Slide with Scatterplot (Rental Revenue and Type - Private/Social)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap9, type = "title", style = text_prop) %>% 
  ph_with_vg(code = print(dtypesubset %>%
                            group_by(rental_revenue, type) %>%
                            ggplot(aes(x = rental_revenue, y = percentage_discount)) +
                            geom_point(aes(colour=factor(type)), stat = "identity") +
                            theme(legend.position = "top", legend.direction = "horizontal") +
                            labs(color = "") +
                            ggtitle("Percentage Discount by Rental Revenue, Grouped by Event Type") +
                            theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))),
             type = "body") %>%
  ph_with_text(type = "sldNum", str = "12" ) %>%
  
  
  # Slide with Bar Chart (Year Booked and Event Month)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap10, type = "title", style = text_prop) %>% 
  ph_with_vg(code = print(d2 %>%
                            group_by(year_booked, event_month) %>%
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
                                       fill = year_booked)) +
                            scale_fill_manual(values = c("chartreuse3", "goldenrod1", "darkblue")) +
                            geom_bar(stat = "identity", position = position_dodge()) +
                            theme(legend.position = "top", legend.direction = "horizontal") +
                            labs(fill="") +
                            xlab("Event Month") + ylab("Mean Percent Discount")+
                            ggtitle("Mean Percentage Discount by Event Month, Grouped by Year Booked") +
                            theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))),
             type = "body") %>%
  ph_with_text(type = "sldNum", str = "13" ) %>%
  
  # Slide with Bar Chart (Year Booked and Event Month - Private/Social)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = cap11, type = "title", style = text_prop) %>% 
  ph_with_vg(code = print(dtypesubset %>%
                            group_by(year_booked, event_month) %>%
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
                                       fill = year_booked)) +
                            scale_fill_manual(values = c("chartreuse3", "goldenrod1", "darkblue")) +
                            geom_bar(stat = "identity", position = position_dodge()) +
                            theme(legend.position = "top", legend.direction = "horizontal") +
                            labs(fill="") +
                            xlab("Event Month") + ylab("Mean Percent Discount") +
                            ggtitle("Mean Percentage Discount by Event Month, Grouped by Year Booked") +
                            theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))),
             type = "body") %>%
  ph_with_text(type = "sldNum", str = "14" ) %>%
  
  # Slide with Scatterplot (Food/Beverage Revenue and Type)
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
                            ggtitle("Percentage Discount by Food/Beverage Revenue, Grouped by Event Type") +
                            theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))),
             type = "body") %>%
  ph_with_text(type = "sldNum", str = "15" )

# Save Powerpoint
print(pres, target = "SCC Room Rental Template.pptx") %>%
  invisible()

