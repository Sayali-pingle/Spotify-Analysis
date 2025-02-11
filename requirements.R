# requirements.R
if (!require(pak)) install.packages("pak")
pak::pkg_install(c(
  "tidyverse",
  "shiny",
  "spotifyr",
  "shinydashboard",
  "plotly",
  "DT",
  "shinyWidgets",
  "waiter",
  "markdown",
  "dplyr",
  "ggplot2",
  "rsconnect"
)) 
