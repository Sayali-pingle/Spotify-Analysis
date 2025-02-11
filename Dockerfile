FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

# Install R packages
RUN R -e "install.packages(c('shiny', 'spotifyr', 'tidyverse', 'shinydashboard', 'plotly', 'DT', 'shinyWidgets', 'waiter', 'rsconnect'))"

# Create directory for environment file
RUN mkdir -p /usr/local/etc/R/

# Copy app files
COPY . /srv/shiny-server/app/

# Copy .Renviron to the R environment directory
COPY .Renviron /usr/local/etc/R/Renviron.site

# Set permissions
RUN chmod 644 /usr/local/etc/R/Renviron.site

# Expose port
EXPOSE 3838

# Run app
CMD ["/usr/bin/shiny-server"]