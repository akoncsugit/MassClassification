library(tidyverse)

set.seed(998)

mammo_masses <- read_csv("mammographic_masses.csv",
                       col_names = c("BI-RADS", "Age", "Shape", "Margin",
                                     "Density", "Severity"), na = "?")

masses <- mammo_masses %>% mutate("ID" = c(1:961),
         "Shape" = cut(Shape, breaks = 4,
                       labels = c("round", "oval", "lobular", "irregular")),
         "Margin" = cut(Margin, breaks = 5,
                        labels = c("circumscribed","microlobulated", "obscured",
                                   "ill-defined", "spiculated")),
         "Density" = cut(Density,
                         breaks = 4, labels = c("high", "iso", "low",
                                                "fat-containing"),
                         ordered_result = TRUE),
         "Severity" = cut(Severity, breaks = 2,
                          labels = c("benign", "maligant"))) %>% select(ID, 1:6)
massSum <- summary(masses)
massNoNA <- masses %>% na.omit()

massNoNASum <- summary(massNoNA)

g <- ggplot(massNoNA, aes(Severity))

