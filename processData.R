library(tidyverse)
library(lubridate)
library(stringr)
library(here)

setwd(here())

dataPath <- file.path(
  "V:", 
  "Economic Intelligence", 
  "Shaw Conference Centre", 
  "Projects", 
  "Discount Analysis", 
  "Data",
  "update_jan2018"
)

resPath <- file.path(dataPath, "res.csv")
# USI -> Events -> EEDC - discount analysis - events
eventPath <- file.path(dataPath, "events.csv")
itemsPath <- file.path(dataPath, "so_items")
itemsFiles <- lapply(list.files(itemsPath), function(x) file.path(itemsPath, x))

resData <- read_csv(resPath)
eventData <- read_csv(eventPath)
itemsData <- bind_rows(lapply(itemsFiles, read_csv))

fixName <- function(string) {
   newName <- unlist(strsplit(string, "-"))[1]
   newName <- trimws(tolower(newName))
   newName <- str_replace_all(newName, " ", "_")
   newName <- str_replace_all(newName, "/", "_")
   
   return(newName)
}

names(resData) <- sapply(names(resData), fixName)
names(eventData) <- sapply(names(eventData), fixName)
names(itemsData) <- sapply(names(itemsData), fixName)

glBev <- c("Beverage -Concession Revenue (recording Sales) (BEVERAGE2)",
           "Beverage Revenue (BEVERAGE)")

glCorkage <- c("Corkage Beverage Revenue (CORKAGE)")

glFacilityfee <- c("Facility Fees - Special Event Revenue (FACILITY)")

glFood <- c("Food - Concession Revenue (recording sales) (FOOD2)",
            "Food Revenue (FOOD)")

glGratuity <- c("Gratuity Revenue (GRATUITY)")

glLabour <- c("BQT Labour Revenue (LABOUR)", 
              "Linen Revenue (LINEN)",
              "Misc Operating Supplies - Banquet Revenue (SUPPLIES)")

glRental <- c("Room Rental Revenue (RENTAL)")

glSecurity <- c("Miscellaneous-Special Event Revenue (SPEVENT)")

glSundry <- c("Maintenance Revenue (MAINTENANCE)", 
              "Event Planning Revenue, Other Recovery (EVENTS)",
              "Event Planning Revenue-Group Registration (EVENTS2)", 
              "Equipment Revenue (EQUIPMENT)",
              "Kitchen Labour Revenue (LABOUR2)", 
              "E&S Labour Revenue (LABOUR3)", 
              "Guest Experience Labour (Revenue & Expense) (LABOUR5)", 
              "Grierson Hill Parking - Sundry Revenue (PARKING)",
              "Photocopy - Sundry Revenue (COPY)", 
              "Sponsorship, Sales (SPONSORSHIP)")

# Other reveneue excluding Gratuity
glOther <- c(glFacilityfee, glSecurity, glSundry, glCorkage, glLabour)

eventData <- eventData %>% 
  mutate_if(grepl("date", names(.)), ymd)

tib <- left_join(
  itemsData,
  resData %>%
    filter(! is.na(resource_code)) %>%
    select(type, resource_code, resource_code_description, gl_distribution_scheme),
  by = c("resource_type" = "type", "resource_code")
)


tib <- tib %>%
  mutate(
    gl_distribution_scheme = case_when(
      is.na(gl_distribution_scheme) & resource_type_description == "Gratuity" ~ "Gratuity Revenue (GRATUITY)",
      is.na(gl_distribution_scheme) & resource_type_description == "Discount" ~ "Room Rental Revenue (RENTAL)",
      is.na(gl_distribution_scheme) & resource_type_description == "Conversion" ~ "Security Labour Revenue (LABOUR4)",
      TRUE ~ gl_distribution_scheme
    )
  )

tib <- tib %>%
  mutate(
    revenue_group = case_when(
      gl_distribution_scheme %in% glRental & resource_type_description == "Discount" ~ "rental_discount",
      gl_distribution_scheme %in% glRental ~ "rental_revenue",
      gl_distribution_scheme %in% c(glFood, glBev) ~ "food_beverage_revenue",
      gl_distribution_scheme %in% glOther ~ "other_revenue"
    )
  )

tib <- tib %>%
  filter(! is.na(revenue_group)) %>%
  group_by(event_id, revenue_group) %>%
  summarize(revenue = sum(extended_charge, na.rm = TRUE)) %>%
  spread(revenue_group, revenue, fill = 0) %>%
  mutate(
    total_revenue = rental_revenue + other_revenue + rental_discount + food_beverage_revenue,
    total_revenue_pre_discount = rental_revenue + other_revenue + food_beverage_revenue
  )

data <- left_join(eventData, tib, by = "event_id") %>%
  mutate(total_event_attendance = as.integer(total_event_attendance)) %>%
  filter(start_date < dmy("01-10-17"))

write_rds(data, "data.rds")