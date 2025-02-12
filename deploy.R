install.packages("rsconnect")
library(rsconnect)

# Read from environment variables
rsconnect::setAccountInfo(
    name = Sys.getenv("SHINYAPPS_NAME"),
    token = Sys.getenv("SHINYAPPS_TOKEN"),
    secret = Sys.getenv("SHINYAPPS_SECRET")
)

# Deploy the app
rsconnect::deployApp() 
