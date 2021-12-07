#global.R
#

library(tidyverse)


set.seed(998)
raw <- read_csv("mammographic_masses.csv",
                col_names = c("BI-RADS", "Age", "numShape", "numMargin",
                              "numDensity", "numSeverity"), na = "?") %>%
  mutate(
    "Shape" = cut(numShape, breaks = 4, labels = c("round", "oval",
                                                   "lobular", "irregular")),
    "Margin" = cut(numMargin, breaks = 5,
                   labels = c("circumscribed","microlobulated", "obscured",
                              "ill-defined", "spiculated")),
    "Density" = cut(numDensity,
                    breaks = 4, labels = c("high", "iso", "low",
                                           "fat-containing"),
                    ordered_result = TRUE),
    "Severity" = cut(numSeverity, breaks = 2,
                     labels = c("benign", "maligant"))
  ) %>% select(1:2, 7:10)

split <- raw %>% na.omit() %>% select(2:6)

