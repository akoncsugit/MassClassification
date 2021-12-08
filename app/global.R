########################################################################;
#Author:  Ashley Ko                                                     ;
#Title: ST558 Project 3 global environment helper                       ;
#Program Purpose: To complete ST558 project 3 requirements              ;
#Date: 2021/12/05                                                       ;
########################################################################;

# Required packages
library(tidyverse)

# Sets seed for reproducibility
set.seed(998)

# Reads in "mammographic_masses.csv" and produces raw data frame
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

# Creates data frame for plotting and modeling
split <- raw %>% na.omit() %>% select(2:6)
