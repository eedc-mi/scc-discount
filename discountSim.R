library(tidyverse)
library(lubridate)
library(stringr)
library(here)

setwd(here())

tib <- read_rds("data.rds")

proposedDiscountMatrix <- cbind(
  fbRev = c(0, 60001, 90001, 105001, 120001, 150001),
  discount = c(0, -.10, -.25, -.35, -.45, -.55)
)

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

simStartConditions <- function(data, fbStartSeq, dStartSeq, fbStep, dStep, numSteps) {
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
    mutate(new_rental_revenue = rental_revenue + (rental_revenue * new_discount)) %>%
    group_by(rev, disc, type) %>%
    summarize(new_rental_revenue = sum(new_rental_revenue, na.rm = TRUE))
  
}

simStepConditions <- function(data, fbStart, dStart, fbStepSeq, dStepSeq, numSteps) {
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
    mutate(new_rental_revenue = rental_revenue + (rental_revenue * new_discount)) %>%
    group_by(rev, disc, type) %>%
    summarize(new_rental_revenue = sum(new_rental_revenue, na.rm = TRUE))
  
}

proposedDiscResult <- as.tibble(
  applyDiscount(
    data.matrix(
      tib %>% select(event_id, rental_revenue, food_beverage_revenue)
    ), 
    proposedDiscountMatrix
  )
)

proposedDiscAll <- tib %>%
  left_join(proposedDiscResult, by = "event_id")

proposedDiscResult <- proposedDiscAll %>%
  mutate(
    new_rental_revenue = rental_revenue + (rental_revenue * new_discount),
    old_rental_revenue = rental_revenue + rental_discount
  ) %>%
  group_by(type) %>%
  summarize(
    new_rental_revenue = sum(new_rental_revenue, na.rm = TRUE),
    old_rental_revenue = sum(old_rental_revenue, na.rm = TRUE)
  )

minFbRev <- seq(30000, 100000, by = 1000)
minDisc <- seq(-0.1, -.30, by = -0.05)
sim <- simStartConditions(tib, minFbRev, minDisc, 30000, -0.1, 5)

stepSeq <- seq(5000, 50000, by = 1000)
discSeq <- seq(-.05, -.30, by = -0.05)
simStep <- simStepConditions(tib, 30000, -.10, stepSeq, discSeq, 5)

ggplot(sim %>% filter(type == "Convention (CONV)"), aes(x = rev, y = new_rental_revenue, color = factor(disc))) +
  geom_line() + 
  geom_hline(
    yintercept = c(
      first(proposedDiscResult %>% filter(type == "Convention (CONV)") %>% select(new_rental_revenue)),
      first(proposedDiscResult %>% filter(type == "Convention (CONV)") %>% select(old_rental_revenue))
    ),
    linetype = "dashed"
  ) +
  labs(
    x = "Minimum food and beverage revenue",
    y = "Simulated revenue",
    color = "Starting\nrental discount",
    title = "Effect of different discount conditions on historical rental revenue",
    subtitle = "Convention rental revenue only, , Jan 2015 - Jun 2017 events"
  ) +
  annotate(
    "text", 
    c(min(sim$rev), min(sim$rev)),
    c(
      first(proposedDiscResult %>% filter(type == "Convention (CONV)") %>% select(new_rental_revenue)),
      first(proposedDiscResult %>% filter(type == "Convention (CONV)") %>% select(old_rental_revenue))
    ),
    vjust = -0.5,
    hjust = 0,
    label = c("Proposed model", "Actual revenue")
  ) + 
  scale_x_continuous(labels = scales::comma)

ggplot(
  sim %>% 
    group_by(rev, disc) %>% 
    summarize(new_rental_revenue = sum(new_rental_revenue)), 
  aes(x = rev, y = new_rental_revenue, color = factor(disc))) +
  geom_line() + 
  geom_hline(
    yintercept = c(
      first(proposedDiscResult %>% summarize(sum(new_rental_revenue))),
      first(proposedDiscResult %>% summarize(sum(old_rental_revenue)))
    ),
    linetype = "dashed"
  ) + 
  labs(
    x = "Minimum food and beverage revenue",
    y = "Simulated revenue",
    color = "Starting\nrental discount",
    title = "Effect of different discount conditions on historical rental revenue",
    subtitle = "All rental revenue, Jan 2015 - Jun 2017 events"
  ) +
  annotate(
    "text", 
    c(min(sim$rev), min(sim$rev)),
    c(
      first(proposedDiscResult %>% summarize(sum(new_rental_revenue))),
      first(proposedDiscResult %>% summarize(sum(old_rental_revenue)))
    ),
    vjust = -0.5,
    hjust = 0,
    label = c("Proposed model", "Actual revenue")
  ) + 
  scale_x_continuous(labels = scales::comma)

ggplot(tib, aes(food_beverage_revenue)) +
  geom_histogram() +
  geom_vline(xintercept = proposedDiscountMatrix[2, "fbRev"], linetype = "dashed") +
  scale_x_continuous(labels = scales::comma)

ggplot(
  rbind(
    tib %>% mutate(type = "All"),
    tib %>% filter(type == "Convention (CONV)")
  ), 
  aes(food_beverage_revenue, color = type)) +
  stat_ecdf() +
  geom_vline(xintercept = proposedDiscountMatrix[2, "fbRev"], linetype = "dashed") +
  annotate(
    "text",
    c(proposedDiscountMatrix[2, "fbRev"], proposedDiscountMatrix[2, "fbRev"]),
    c(0.25, 0.25),
    hjust = c(1.1, -0.25),
    label = c("No discount", "Discount")
  ) +
  labs(
    x = "Food and beverage revenue",
    y = "Fraction of events",
    color = "Event type",
    title = "Under the proposed model, > 75% of conventions would not have received a discount",
    subtitle = "Fraction of Jan 2015 - Jun 2017 events with given amounts of food and beverage revenue"
  ) +
  scale_x_continuous(limits = c(0, 200000), labels = scales::comma)

ggplot(
  proposedDiscAll %>%
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
  coord_flip()


