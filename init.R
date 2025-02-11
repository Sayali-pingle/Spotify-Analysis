# init.R
my_packages = c("tidyverse", "shiny", "spotifyr", "shinydashboard", "plotly", 
                "DT", "shinyWidgets", "waiter", "markdown", "dplyr", "ggplot2")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
}

invisible(sapply(my_packages, install_if_missing)) 