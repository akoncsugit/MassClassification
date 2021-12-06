# ST558Project3


The app2 folder is the primary app as of this evening.

# Brief Introduction
The goal of this app is to classify mammographic masses as either begin or malign.
The data comes from: http://archive.ics.uci.edu/ml/datasets/mammographic+mass.


Citation:

M. Elter, R. Schulz-Wendtland and T. Wittenberg (2007)
The prediction of breast cancer biopsy outcomes using two CAD approaches that both emphasize an intelligible decision process.
Medical Physics 34(11), pp. 4164-4172

# List of Packages
shiny, shinydashboard, markdown, shinyWidgets, markdown, DT, tidyverse

# Code for Installing All the Packages

```R
install.packages(c("shiny", "shinydashboard", "markdown", "shinyWidgets", "markdown", "DT", "tidyverse"))
```

# Code to Launch App

```R
shiny::runGitHub(repo = "ST558Project3", username = "akoncsugit",
ref = "main", subdir = "/app2/", launch.browser = TRUE)
```

