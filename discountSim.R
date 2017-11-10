library(tidyverse)
library(lubridate)
library(stringr)
library(here)
library(officer)

setwd(here())

pptPath <- file.path(
  "V:", 
  "Economic Intelligence", 
  "Shaw Conference Centre", 
  "Projects", 
  "Discount Analysis"
)

tib <- read_rds("data.rds")

proposedDiscountMatrix <- cbind(
  fbRev = c(0, 60001, 90001, 105001, 120001, 150001),
  discount = c(0, -.10, -.25, -.35, -.45, -.55)
)

offPeakMonths <- c(1, 7, 8, 12)
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
    new_rental_revenue = rental_revenue + (rental_revenue * new_discount),
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
    new_rental_revenue = rental_revenue + (rental_revenue * new_discount),
    old_rental_revenue = rental_revenue + rental_discount
  ) %>%
  group_by(type) %>%
  summarize(
    new_rental_revenue = sum(new_rental_revenue, na.rm = TRUE),
    old_rental_revenue = sum(old_rental_revenue, na.rm = TRUE)
  )

minFbRev <- seq(0, 100000, by = 1000)
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
  scale_y_continuous(labels = scales::dollar) +
  theme(plot.title = element_text(face = "bold"))

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
    title = "Effect of starting discount conditions on convention rental revenue",
    subtitle = paste0(
      "Jan 2015 - Jun 2017 events, off-peak only\n", 
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
  scale_y_continuous(labels = scales::dollar) + 
  theme(plot.title = element_text(face = "bold"))

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
    title = "Fraction of Jan 2015 - Jun 2017 events with given amounts of food and beverage revenue"
  ) +
  scale_x_continuous(limits = c(0, 200000), labels = scales::dollar) + 
  theme(plot.title = element_text(face = "bold"))

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
    title = "Fraction of Jan 2015 - Jun 2017 events receiving a discount by event type"
  ) +
  scale_fill_discrete(labels = c("Proposed model", "Actual")) +
  coord_flip() + 
  theme(plot.title = element_text(face = "bold"))

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
    title = "Fraction of Jan 2015 - Jun 2017 events with a discount, off-peak only"
  ) +
  scale_fill_discrete(labels = c("Proposed model", "Actual")) +
  coord_flip() + 
  theme(plot.title = element_text(face = "bold"))

barPlotOffDollars <- ggplot(
  proposedAllMonths %>%
    filter(month(start_date) %in% offPeakMonths) %>%
    group_by(type) %>%
    summarize(
      sum_old = sum(rental_discount, na.rm = TRUE),
      sum_new = sum((new_discount * rental_revenue), na.rm = TRUE)
    ) %>%
    select(type, sum_old, sum_new) %>%
    filter(sum_old < 0) %>%
    gather(key = discount, value = value, sum_old:sum_new),
  aes(x = type, y = -1 * value, fill = discount)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "Event type",
    y = "Total room rental discount",
    fill = "Discount model",
    title = "Total rental discounts for Jan 2015 - Jun 2017 events, off-peak only"
  ) +
  scale_fill_discrete(labels = c("Proposed model", "Actual")) +
  scale_y_continuous(labels = scales::dollar) +
  coord_flip() + 
  theme(plot.title = element_text(face = "bold"))

discScatter <- ggplot(
  proposedAllMonths %>%
    filter(type == "Convention (CONV)", rental_discount < 0) %>%
    mutate(
      new_rental_discount = -1 * (rental_revenue * new_discount),
      old_rental_discount = -1 * rental_discount,
      is_off_peak = month(start_date) %in% offPeakMonths
    ),
  aes(
    x = old_rental_discount, y = new_rental_discount, 
    size = total_event_attendance, color = is_off_peak)) +
  geom_point() +
  geom_abline(linetype = "dashed") + 
  labs(
    x = "Actual discount",
    y = "Proposed discount",
    size = "Total attendance",
    color = "Event month",
    title = "Actual vs. proposed discounts for Jan 2015 - Jun 2017 conventions"
  ) +
  scale_color_discrete(labels = c("Peak", "Off-peak")) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) + 
  theme(plot.title = element_text(face = "bold"))

discRevScatter <- ggplot(
  proposedAllMonths %>%
    filter(type == "Convention (CONV)", rental_discount < 0) %>%
    mutate(
      new_rental_discount = -1 * (rental_revenue * new_discount),
      food_beverage_revenue = food_beverage_revenue,
      is_off_peak = month(start_date) %in% offPeakMonths
    ),
  aes(
    x = food_beverage_revenue, y = new_rental_discount, 
    size = total_event_attendance, color = is_off_peak)) +
  geom_point(shape = 1) +
  labs(
    x = "Food and beverage revenue",
    y = "Proposed discount",
    size = "Total attendance",
    color = "Event month",
    title = "F&B revenue vs. proposed discounts for Jan 2015 - Jun 2017 conventions"
  ) +
  scale_color_discrete(labels = c("Peak", "Off-peak")) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) + 
  theme(plot.title = element_text(face = "bold")) 

discFlexTable <- flextable(as.tibble(proposedDiscountMatrix)) %>%
  set_header_labels(fbRev = "Amount of F&B Revenue", discount = "Rental Discount") %>%
  align(align = "center", part = "all") %>%
  padding(padding = 3, part = "all") %>%
  bg(bg = "chartreuse3", part = "header") %>%
  color(color = "white", part = "header") %>%
  color(color = "darkgreen", part = "body") %>%
  border(border = fp_border(color = "darkblue"), part = "all") %>%
  autofit() %>%
  width(j = 1, width = 3) %>%
  width(j = 2, width = 3)

ppt <- read_pptx("SCC Room Rental Template.pptx")

ppt <- ppt %>%
  add_slide(layout = "Title Slide", master = "Office Theme") %>%
  ph_with_text(type = "ctrTitle", str = "Evaluation of Proposed Discount Model") %>% 
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(
    str = paste(
      "Below is the proposed convention discount model for off-peak months.",
      "This analysis considers Jan, Jul, Aug, and Dec 'off-peak'.",
      "The choice of off-peak months is not obvious (e.g. Feb slow overall but busy for conventions)",
      "and has a large impact on the results in the next slides."
    ), type = "title", style = fp_text(font.size = 16)) %>%
  ph_with_flextable(value = discFlexTable, type = "body", index = 1) %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(
    str = paste(
      "For any discount model, a key factor is how many events it would apply to.",
      "Based on past events, the proposed model would apply to relatively few events",
      "as half of off-peak conventions have < $50,000 in F&B revenue"
    ),
    type = "title", style = fp_text(font.size = 16)
  ) %>%
  ph_with_vg(code = print(cdfPlot), type = "body", index = 1) %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(
    str = "In off-peak times, proposed model would result in about the same number of discounted conventions",
    type = "title", style = fp_text(font.size = 16)
  ) %>%
  ph_with_vg(code = print(barPlotOff), type = "body", index = 1) %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(
    str = "However, the proposed model would result in far lower discounts for off-peak conventions",
    type = "title", style = fp_text(font.size = 16)
  ) %>%
  ph_with_vg(code = print(barPlotOffDollars), type = "body", index = 1) %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(
    str = paste(
      "By simulating the proposed model, we can see how it would have affected actual revenue (dashed lines below),",
      "and to show how actual revenue would have changed given different F&B cutoffs and starting discounts (coloured lines below.)",
      "Proposed model would result in higher overall rental prices for convention clients, hence higher revenue." 
    ),
    type = "title", style = fp_text(font.size = 16)
  ) %>%
  ph_with_vg(code = print(convSimPlotOff), type = "body", index = 1) %>%
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(
    str = paste(
      "To consider: is the goal of discounting conventions more revenue, higher attendance, or other?",
      "As shown below, proposed model would not give discounts to some larger conventions with low",
      "food and beverage revenue."
    ),
    type = "title", style = fp_text(font.size = 16)
  ) %>%
  ph_with_vg(code = print(discRevScatter), type = "body", index = 1)

print(ppt, target = "SCC Room Rental Template.pptx")

