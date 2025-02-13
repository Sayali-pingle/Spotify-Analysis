# init.R
# Remove or comment out the pak-related code
# options(repos = c(CRAN = "https://cloud.r-project.org"))
# if (!require("pak")) install.packages("pak")
# pak::pkg_install(c("shiny", "shinydashboard", "tidyverse", "DT", "plotly", "waiter", "spotifyr"))

# Instead, use this:
options(repos = c(RSPM = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"))
required_packages <- c("shiny", "shinydashboard", "DT", "plotly", "waiter", "spotifyr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages) 
