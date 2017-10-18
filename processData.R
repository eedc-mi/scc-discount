library(tidyverse)
library(lubridate)
library(stringr)

thisDir <- dirname(parent.frame(2)$ofile)
setwd(thisDir)

dataPath <- file.path(
  "V:", 
  "Economic Intelligence", 
  "Shaw Conference Centre", 
  "Projects", 
  "Discount Analysis", 
  "Data")

# USI -> GL Transaction Inquiry -> EEDC - discount analysis - gl
glPath <- file.path(dataPath, "gl.csv")
resPath <- file.path(dataPath, "res.csv")
# USI -> Events -> EEDC - discount analysis - events
eventPath <- file.path(dataPath, "events.csv")

glData <- read_csv(glPath)
resData <- read_csv(resPath)
eventData <- read_csv(eventPath)

fixName <- function(string) {
   newName <- unlist(strsplit(string, "-"))[1]
   newName <- trimws(tolower(newName))
   newName <- str_replace_all(newName, " ", "_")
   newName <- str_replace_all(newName, "/", "_")
   
   return(newName)
}

names(glData) <- sapply(names(glData), fixName)
names(resData) <- sapply(names(resData), fixName)
names(eventData) <- sapply(names(eventData), fixName)

glData$amount <- -1 * glData$amount

eventData <- eventData %>% 
  mutate_if(grepl("date", names(.)), ymd) %>%
  mutate(booked_spaces = str_split(booked_spaces, ";")) %>%
  mutate(booked_spaces = map(booked_spaces, trimws))

tib <- left_join(
  glData,
  resData %>%
    filter(! is.na(resource_code)) %>%
    select(type, resource_code, resource_code_description, gl_distribution_scheme),
  by = c("type", "resource_code")
)
  
tib <- tib %>%
  mutate(
    gl_distribution_scheme = case_when(
      is.na(gl_distribution_scheme) & resource_type_description == "Gratuity" ~ "Gratuity (GRATUITY)",
      is.na(gl_distribution_scheme) & resource_type_description == "Discount" ~ "Room Rental Revenue (RENTAL)",
      is.na(gl_distribution_scheme) & resource_type_description == "Conversion" ~ "Security Labour Revenue (LABOUR4)",
      TRUE ~ gl_distribution_scheme
    )
  )

tib <- tib %>%
  mutate(
    revenue_group = case_when(
      gl_distribution_scheme == "Room Rental Revenue (RENTAL)" & resource_type_description == "Discount" ~ "rental_discount",
      gl_distribution_scheme == "Room Rental Revenue (RENTAL)" ~ "rental_revenue",
      gl_distribution_scheme == "Food Revenue (FOOD)" | gl_distribution_scheme == "Beverage Revenue (BEVERAGE)" ~ "food_beverage_revenue",
      TRUE ~ "other_revenue"
    )
  )

tib <- tib %>%
  filter(gl_account_type == "Revenue (50)") %>%
  group_by(event_id, revenue_group) %>%
  summarize(revenue = sum(amount, na.rm = TRUE)) %>%
  spread(revenue_group, revenue, fill = 0) %>%
  mutate(total_revenue = rental_revenue + other_revenue + rental_discount + food_beverage_revenue)

data <- left_join(eventData, tib, by = "event_id")

write_rds(data, "data.rds")
