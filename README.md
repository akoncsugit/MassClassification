# Mammographic Mass Lesion Classification Using Supervised Learning

# App Description

This `Shiny` app allows users to explore mammographic mass data and fit models for predicting mass 
status through repeated cross-validated supervised learning models.

# About the data

The data used in this app is from a study of mammographic mass lesions performed at University 
Erlangen-Nuremberg between 2003 and 2006. The research was performed with the goal of reducing 
unnecessary biopsies by using computer-aided diagnosis systems to predict the `Severity` status, 
benign or malignant, of lesions.

Citation:
*M. Elter, R. Schulz-Wendtland and T. Wittenberg (2007)*
*The prediction of breast cancer biopsy outcomes using two CAD approaches that both emphasize an*
*intelligible decision process.*
*Medical Physics 34(11), pp. 4164-4172* 
[Data Source](http://archive.ics.uci.edu/ml/datasets/mammographic+mass)

# List of Packages

`caret`, `rpart.plot`, `shiny`, `shinythemes`, `shinyWidgets`, `tidyverse`


# Code for Installing All the Packages

```R
install.packages(c("caret", "rpart.plot", "shiny", "shinythemes", "shinyWidgets", "tidyverse"))
```

# Code to Launch App

```R
shiny::runGitHub(repo = "MassClassification", username = "akoncsugit",
ref = "main", subdir = "/app/", launch.browser = TRUE)
```

