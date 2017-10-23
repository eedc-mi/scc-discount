#-----------------------
# Exploratory Analytics
#-----------------------

# Prepare Session
#-----------------

# Set Working Directory

setwd("C:/Users/klangeste/OneDrive - Edmonton Economic Development Corporation/Github/scc-discount")

# Load Libraries

library(tidyverse)
library(lubridate)
library(stringr)

# Read in Data

d1 <- read_rds("data.rds")
dim(d1)

# Replace NA values with 0

na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

discount <- na.zero(d1$rental_discount)

# Make discount amounts all positive

discount <- abs(discount)

# Summary Statistics
#--------------------

# Full Sample

mean(discount)

median(discount)

sd(discount)

min(discount)

max(discount)

# Sort Start Dates and Dates Booked into months

event_month <- months(d1$start_date)

month_booked <- months(d1$date_booked)

d1$event_month <- event_month
d1$month_booked <- month_booked

#event_type <- d1$type

d2 <- cbind(discount,event_type,event_month,month_booked)

# Subset by event type

ENT <- subset(d2, event_type == "Entertainment (ENT)")
CONV <- subset(d2, event_type == "Convention (CONV)")
MTG <- subset(d2, event_type == "Meeting (MTG)")
TRSH <- subset(d2, event_type == "Consumer Tradeshow (TRSH)")
CON <- subset(d2, event_type == "Concert (CON)")
SCOM <- subset(d2, event_type == "Community (SCOM)")
EP <- subset(d2, event_type == "Event Planning (EP)")
ER <- subset(d2, event_type == "Event Registration (ER)")
INT <- subset(d2, event_type == "Internal (INT)")
MISC <- subset(d2, event_type == "Mkiscellaneous (MISC)")
SOC <- subset(d2, event_type == "Private/Social (SOC)")
GRP <- subset(d2, event_type == "ZZZ Retired - Group (GRP)")

# Subset by event month

emJAN <- subset(d2, event_month == "January")
emFEB <- subset(d2, event_month == "February")
emMAR <- subset(d2, event_month == "March")
emAPR <- subset(d2, event_month == "April")
emMAY <- subset(d2, event_month == "May")
emJUN <- subset(d2, event_month == "June")
emJUL <- subset(d2, event_month == "July")
emAUG <- subset(d2, event_month == "August")
emSEP <- subset(d2, event_month == "September")
emOCT <- subset(d2, event_month == "October")
emNOV <- subset(d2, event_month == "November")
emDEC <- subset(d2, event_month == "December")

# Subset by month booked

mbJAN <- subset(d2, month_booked == "January")
mbFEB <- subset(d2, month_booked == "February")
mbMAR <- subset(d2, month_booked == "March")
mbAPR <- subset(d2, month_booked == "April")
mbMAY <- subset(d2, month_booked == "May")
mbJUN <- subset(d2, month_booked == "June")
mbJUL <- subset(d2, month_booked == "July")
mbAUG <- subset(d2, month_booked == "August")
mbSEP <- subset(d2, month_booked == "September")
mbOCT <- subset(d2, month_booked == "October")
mbNOV <- subset(d2, month_booked == "November")
mbDEC <- subset(d2, month_booked == "December")

# Subset stats

mean(ENT$discount)

median(ENT$discount)

sd(ENT$discount)

min(ENT$discount)

max(ENT$discount)

# Find all events with positive rental_discount

rental_discount_positive <- filter(d1, d1$rental_discount > 0)

# Find all events with value of 0 for rental_revenue but have a value for rental_discount

rental_revenue_zero <- filter(d1, d1$rental_revenue == 0 & d1$rental_discount != 0)

# Find all events with value of NA for actual_economic_impact

actual_economic_impact_NA <- filter(d1, is.na(d1$actual_economic_impact))

# Find all events with value of 0 for total_event_attendance

tea_zero <- filter(d1, d1$total_event_attendance == 0)

# Determine how many spaces are being rented

# dtest <- d1

# dtest$booked_spaces_split <- split(d1$booked_spaces, row(d1))

# dtest$number_of_spaces <- lengths(dtest$booked_spaces_split)

# t(apply(dtest, 1, unlist))

dtest2 <- unnest(dtest, booked_spaces)

data.frame(table(dtest2$post_as))

dcount <- aggregate(data.frame(count = dtest2$event_id),
                    list(value = dtest2$event_id), length)

dtest$space_count <- dcount$count


class(d1$booked_spaces)



booked_spaces_df <- data.frame(matrix(unlist(d1$booked_spaces)),
                               nrow=count(nrow(d1), byrow=T, stringsAsFactors=FALSE))




