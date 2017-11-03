library(tidyverse)
library(lubridate)
library(stringr)
library(here)
library(officer)

setwd(here())

tib <- read_rds("data.rds")

proposedDiscountMatrix <- cbind(
  fbRev = c(0, 60001, 90001, 105001, 120001, 150001),
  discount = c(0, -.10, -.25, -.35, -.45, -.55)
)

offPeakMonths <- c(1, 6, 7, 8, 12)
allMonths <- seq(1, 12, 1)

makeDiscountMatrix <- function(fbStart, fbStep, dStart, dStep, numSteps) {
  x = c(0, seq(fbStart, (fbStart + fbStep * numSteps), by = fbStep))
  y = c(0, seq(dStart, (dStart + dStep * numSteps), by = dStep))
  
  cbind(x, y)
}

findDiscount <- function(x, discountMatrix) {
  if (is.na(x))
    return(NA)
  
  index <- findInterval(x, discountMatrix[, 1])
  as.double(discountMatrix[index, 2])
}

applyDiscount <- function(dataMatrix, discountMatrix) {
  dataMatrix <- cbind(
    dataMatrix, 
    new_discount = as.numeric(
      map(
        dataMatrix[, "food_beverage_revenue"], 
        findDiscount, 
        discountMatrix = discountMatrix
      )
    )
  )
  
  dataMatrix[, c("event_id", "new_discount")]
}

simStartConditions <- function(data, fbStartSeq, dStartSeq, fbStep, dStep, numSteps, dMonths) {
  grid <- expand.grid(rev = fbStartSeq, disc = dStartSeq)
  
  dMatrixList <- mapply(
    makeDiscountMatrix, 
    fbStart = grid$rev, 
    dStart = grid$disc, 
    MoreArgs = list(fbStep = fbStep, dStep = dStep, numSteps = numSteps),
    SIMPLIFY = FALSE
  )
  
  dataMatrix <- data.matrix(
    data %>% select(event_id, rental_revenue, food_beverage_revenue)
  )
  
  matrixList <- lapply(dMatrixList, applyDiscount, dataMatrix = dataMatrix)
  
  tib <- as.tibble(
    cbind(
      grid[rep(seq_len(nrow(grid)), each = nrow(matrixList[[1]])),],
      do.call("rbind", matrixList)
    )
  )
  
  data %>%
    left_join(tib, by = "event_id") %>%
    filter(month(start_date) %in% dMonths) %>%
    mutate(
      new_rental_revenue = case_when(
        month(start_date) %in% dMonths ~ rental_revenue + (rental_revenue * new_discount),
        TRUE ~ rental_revenue + rental_discount
      )
    ) %>%
    group_by(rev, disc, type) %>%
    summarize(new_rental_revenue = sum(new_rental_revenue, na.rm = TRUE))
  
}

simStepConditions <- function(data, fbStart, dStart, fbStepSeq, dStepSeq, numSteps, dMonths) {
  grid <- expand.grid(rev = fbStepSeq, disc = dStepSeq)
  
  dMatrixList <- mapply(
    makeDiscountMatrix, 
    fbStep = grid$rev, 
    dStep = grid$disc, 
    MoreArgs = list(fbStart = fbStart, dStart = dStart, numSteps = numSteps),
    SIMPLIFY = FALSE
  )
  
  dataMatrix <- data.matrix(
    data %>% select(event_id, rental_revenue, food_beverage_revenue)
  )
  
  matrixList <- lapply(dMatrixList, applyDiscount, dataMatrix = dataMatrix)
  
  tib <- as.tibble(
    cbind(
      grid[rep(seq_len(nrow(grid)), each = nrow(matrixList[[1]])),],
      do.call("rbind", matrixList)
    )
  )
  
  data %>%
    left_join(tib, by = "event_id") %>%
    filter(month(start_date) %in% dMonths) %>%
    mutate(
      new_rental_revenue = case_when(
        month(start_date) %in% dMonths ~ rental_revenue + (rental_revenue * new_discount),
        TRUE ~ rental_revenue + rental_discount
      )
    ) %>%
    group_by(rev, disc, type) %>%
    summarize(new_rental_revenue = sum(new_rental_revenue, na.rm = TRUE))
  
}

proposedResult <- as.tibble(
  applyDiscount(
    data.matrix(
      tib %>% select(event_id, rental_revenue, food_beverage_revenue)
    ), 
    proposedDiscountMatrix
  )
)

proposedAllMonths <- tib %>%
  left_join(proposedResult, by = "event_id")

proposedResultAll <- proposedAllMonths %>%
  mutate(
    new_rental_revenue = case_when(
      month(start_date) %in% allMonths ~ rental_revenue + (rental_revenue * new_discount),
      TRUE ~ rental_revenue + rental_discount
    ),
    old_rental_revenue = rental_revenue + rental_discount
  ) %>%
  group_by(type) %>%
  summarize(
    new_rental_revenue = sum(new_rental_revenue, na.rm = TRUE),
    old_rental_revenue = sum(old_rental_revenue, na.rm = TRUE)
  )

proposedOffPeak <- tib %>%
  left_join(proposedResult, by = "event_id") %>%
  filter(month(start_date) %in% offPeakMonths)

proposedResultOff <- proposedOffPeak %>%
  mutate(
    new_rental_revenue = case_when(
      month(start_date) %in% allMonths ~ rental_revenue + (rental_revenue * new_discount),
      TRUE ~ rental_revenue + rental_discount
    ),
    old_rental_revenue = rental_revenue + rental_discount
  ) %>%
  group_by(type) %>%
  summarize(
    new_rental_revenue = sum(new_rental_revenue, na.rm = TRUE),
    old_rental_revenue = sum(old_rental_revenue, na.rm = TRUE)
  )

minFbRev <- seq(10000, 100000, by = 1000)
minDisc <- seq(-0.1, -.30, by = -0.05)
sim <- simStartConditions(tib, minFbRev, minDisc, 30000, -0.1, 5, offPeakMonths)
simAll <- simStartConditions(tib, minFbRev, minDisc, 30000, -0.1, 5, allMonths)

stepSeq <- seq(5000, 50000, by = 1000)
discSeq <- seq(-.05, -.30, by = -0.05)
simStep <- simStepConditions(tib, 30000, -.10, stepSeq, discSeq, 5, offPeakMonths)

# Conventions only, all months
convSimPlotAll <- ggplot(
  simAll %>% filter(type == "Convention (CONV)"), 
  aes(x = rev, y = new_rental_revenue, color = factor(disc))) +
  geom_line() + 
  geom_hline(
    yintercept = c(
      first(proposedResultAll %>% filter(type == "Convention (CONV)") %>% select(new_rental_revenue)),
      first(proposedResultAll %>% filter(type == "Convention (CONV)") %>% select(old_rental_revenue))
    ),
    linetype = "dashed"
  ) +
  labs(
    x = "Minimum food and beverage revenue",
    y = "Simulated revenue",
    color = "Starting\nrental discount",
    title = "The choice of minimum F&B revenue and initial discount has a large effect on revenue",
    subtitle = paste0(
      "Effect of starting discount conditions on convention rental revenue, Jan 2015 - Jun 2017 events\n",
      "Five discount levels, increases by 10% for every $30,000 in F&B revenue"
    )
  ) +
  annotate(
    "text", 
    c(min(sim$rev), min(sim$rev)),
    c(
      first(proposedResultAll %>% filter(type == "Convention (CONV)") %>% select(new_rental_revenue)),
      first(proposedResultAll %>% filter(type == "Convention (CONV)") %>% select(old_rental_revenue))
    ),
    vjust = -0.5,
    hjust = 0,
    label = c("Proposed model", "Actual revenue")
  ) + 
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar)

#Conventions only, off-peak months
convSimPlotOff <- ggplot(
  sim %>% filter(type == "Convention (CONV)"), 
  aes(x = rev, y = new_rental_revenue, color = factor(disc))) +
  geom_line() + 
  geom_hline(
    yintercept = c(
      first(proposedResultOff %>% filter(type == "Convention (CONV)") %>% select(new_rental_revenue)),
      first(proposedResultOff %>% filter(type == "Convention (CONV)") %>% select(old_rental_revenue))
    ),
    linetype = "dashed"
  ) +
  labs(
    x = "Minimum food and beverage revenue",
    y = "Simulated revenue",
    color = "Starting\nrental discount",
    title = "The choice of minimum F&B revenue and initial discount has a large effect on revenue",
    subtitle = paste0(
      "Effect of starting discount conditions on convention rental revenue, Jan 2015 - Jun 2017 events, off-peak only\n",
      "Five discount levels, increases by 10% for every $30,000 in F&B revenue"
    )
  ) +
  annotate(
    "text", 
    c(min(sim$rev), min(sim$rev)),
    c(
      first(proposedResultOff %>% filter(type == "Convention (CONV)") %>% select(new_rental_revenue)),
      first(proposedResultOff %>% filter(type == "Convention (CONV)") %>% select(old_rental_revenue))
    ),
    vjust = -0.5,
    hjust = 0,
    label = c("Proposed model", "Actual revenue")
  ) + 
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar)

cdfPlot <- ggplot(
  rbind(
    tib %>% mutate(type = "All"),
    tib %>% filter(type == "Convention (CONV)" & month(start_date) %in% offPeakMonths) %>%
      mutate(type = "Convention (CONV), Off-Peak Only"),
    tib %>% filter(type == "Convention (CONV)")
  ), 
  aes(food_beverage_revenue, color = type)) +
  stat_ecdf() +
  geom_vline(xintercept = proposedDiscountMatrix[2, "fbRev"], linetype = "dashed") +
  annotate(
    "text",
    proposedDiscountMatrix[2, "fbRev"],
    0.25,
    hjust = -0.25,
    label = "Proposed discount cutoff"
  ) +
  labs(
    x = "Food and beverage revenue",
    y = "Fraction of events",
    color = "Event type",
    title = "Half of off-peak conventions have < $50,000 in F&B revenue",
    subtitle = "Fraction of Jan 2015 - Jun 2017 events with given amounts of food and beverage revenue"
  ) +
  scale_x_continuous(limits = c(0, 200000), labels = scales::dollar)

barPlotAll <- ggplot(
  proposedAllMonths %>%
    group_by(type) %>%
    summarize(
      count_total = n(),
      count_old = sum(rental_discount < 0, na.rm = TRUE),
      count_new = sum(new_discount < 0, na.rm = TRUE)
    ) %>%
    mutate(percent_old = count_old / count_total, percent_new = count_new / count_total) %>%
    select(type, percent_old, percent_new) %>%
    filter(percent_old > 0) %>%
    gather(key = percent, value = value, percent_old:percent_new),
  aes(x = type, y = value, fill = percent)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Event type",
    y = "Fraction of events",
    fill = "Discount model",
    title = "Applied all year, proposed model would result in fewer discounted room rates",
    subtitle = "Fraction of Jan 2015 - Jun 2017 events receiving a discount by event type"
  ) +
  scale_fill_discrete(labels = c("Proposed model", "Historical")) +
  coord_flip()

barPlotOff <- ggplot(
  proposedAllMonths %>%
    filter(month(start_date) %in% offPeakMonths) %>%
    group_by(type) %>%
    summarize(
      count_total = n(),
      count_old = sum(rental_discount < 0, na.rm = TRUE),
      count_new = sum(new_discount < 0, na.rm = TRUE)
    ) %>%
    mutate(percent_old = count_old / count_total, percent_new = count_new / count_total) %>%
    select(type, percent_old, percent_new) %>%
    filter(percent_old > 0) %>%
    gather(key = percent, value = value, percent_old:percent_new),
  aes(x = type, y = value, fill = percent)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Event type",
    y = "Fraction of events",
    fill = "Discount model",
    title = "In off-peak times, proposed model would\nresult in about the same number of discounted conventions",
    subtitle = "Fraction of Jan 2015 - Jun 2017 events receiving a discount by event type, off-peak only"
  ) +
  scale_fill_discrete(labels = c("Proposed model", "Historical")) +
  coord_flip()

pres <- read_pptx("SCC Room Rental Discount History.pptx")

pres <- pres %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with_vg(code = print(convSimPlotOff), type = "body")

print(pres, target = "SCC Room Rental Discount History.pptx")

