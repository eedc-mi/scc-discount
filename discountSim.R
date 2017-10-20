library(tidyverse)
library(lubridate)
library(stringr)

thisDir <- dirname(parent.frame(2)$ofile)
setwd(thisDir)

tib <- read_rds("data.rds")

proposedDiscountTable <- tibble(
  fbRev = c(0, 60001, 90001, 105001, 120001, 150001),
  discount = c(0, -.10, -.25, -.35, -.45, -.55)
)

makeDiscountTable <- function(fbStart, fbStep, dStart, dStep, numSteps) {
  x = seq(fbStart, (fbStart + fbStep * numSteps), by = fbStep)
  y = seq(dStart, (dStart + dStep * numSteps), by = dStep)
  
  tib <- rbind(
    tibble(fbRev = 0, discount = 0),
    tibble(fbRev = x, discount = y)
  )
  
  return(tib)
}

findDiscount <- function(x, discountTable) {
  if (is.na(x))
    return(NA)
  
  index <- findInterval(x, discountTable$fbRev)
  return(as.double(discountTable[index, 2]))
}

applyDiscount <- function(data, discountTable) {
  return(
    data %>%
      rowwise() %>%
      mutate(
        new_discount = rental_revenue * findDiscount(food_beverage_revenue, discountTable),
        new_rental_revenue = rental_revenue + new_discount
      ) %>%
      group_by(type) %>%
      summarize(
        new_rental_revenue = sum(new_rental_revenue, na.rm = TRUE)
      )
  )
}

simStartConditions <- function(data, minRevSeq, minDiscountSeq, fbStep, dStep, numSteps) {
  grid <- expand.grid(rev = minRevSeq, disc = minDiscountSeq)
  
  dTableList <- mapply(
    makeDiscountTable, 
    fbStart = grid$rev, 
    dStart = grid$disc, 
    MoreArgs = list(fbStep = fbStep, dStep = dStep, numSteps = numSteps),
    SIMPLIFY = FALSE
  )
  
  tibList <- lapply(dTableList, applyDiscount, data = data)
  
  return(
    as.tibble(
      cbind(
        grid[rep(seq_len(nrow(grid)), each = nrow(tibList[[1]])),],
        bind_rows(tibList)
      )
    )
  )
}

minFbRev <- seq(0, 100000, by = 1000)
minDisc <- seq(-0.1, -.50, by = -0.1)

sim <- simStartConditions(tib, minFbRev, minDisc, 30000, -0.1, 5)

ggplot(filter(sim, type == "Convention (CONV)"), aes(x = rev, y = new_rental_revenue, color = factor(disc))) +
  geom_line()

proposedDiscountResult <- applyDiscount(tib, proposedDiscountTable)





