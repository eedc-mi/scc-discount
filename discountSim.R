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

simStartConditions <- function(data, minRevSeq, minDiscountSeq, fbStep, dStep, numSteps) {
  grid <- expand.grid(rev = minRevSeq, disc = minDiscountSeq)
  
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
    proposedDiscountTable
  )
)

proposedDiscResult <- tib %>%
  left_join(proposedDiscResult, by = "event_id") %>%
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
simStep <- simStepConditions(tib, 30000, -.10, fbStepSeq, discStepSeq, 5)

ggplot(filter(sim, type == "Convention (CONV)"), aes(x = rev, y = new_rental_revenue, color = factor(disc))) +
  geom_line() + 
  geom_hline(
    yintercept = c(
      first(proposedDiscResult %>% filter(type == "Convention (CONV)") %>% select(new_rental_revenue)),
      first(proposedDiscResult %>% filter(type == "Convention (CONV)") %>% select(old_rental_revenue))
    ),
    linetype = "dashed"
  ) +
  ggtitle("Simulation results") +
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
  )
 


