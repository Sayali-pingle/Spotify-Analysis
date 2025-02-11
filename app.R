# At the very top of app.R
if (!require(pak)) install.packages("pak")
required_packages <- c(
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
  "ggplot2"
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    pak::pkg_install(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Function to retrieve and analyze Spotify data for a given artist
artist_details <- function(artist_name, client_id, secret_id) {
  # Reset and set up authentication
  Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
  Sys.setenv(SPOTIFY_CLIENT_SECRET = secret_id)
  
  # Get new access token
  access_token <- get_spotify_access_token(
    client_id = client_id,
    client_secret = secret_id
  )
  
  print("Getting artist info...")
  artist_search <- search_spotify(artist_name, type = "artist")
  
  if (nrow(artist_search) == 0) {
    print("No artist found in search")
    return(NULL)
  }
  
  artist_info <- get_artist(artist_search$id[1])
  
  # Get all singles with pagination to get the complete count
  print("Getting singles...")
  offset <- 0
  limit <- 50
  all_singles <- data.frame()
  
  repeat {
    singles_batch <- get_artist_albums(
      artist_info$id,
      include_groups = "single",
      limit = limit,
      offset = offset,
      market = "US"
    )
    
    if (nrow(singles_batch) == 0) break
    
    all_singles <- rbind(all_singles, singles_batch)
    if (nrow(singles_batch) < limit) break
    
    offset <- offset + limit
  }
  
  # Get collaborations
  print("Getting collaborations...")
  offset <- 0
  all_collabs <- data.frame()
  
  repeat {
    collabs_batch <- get_artist_albums(
      artist_info$id,
      include_groups = "appears_on",
      limit = limit,
      offset = offset,
      market = "US"
    )
    
    if (nrow(collabs_batch) == 0) break
    
    all_collabs <- rbind(all_collabs, collabs_batch)
    if (nrow(collabs_batch) < limit) break
    
    offset <- offset + limit
  }
  
  # Process collaborations to find featured artists
  collab_artists <- unique(unlist(strsplit(all_collabs$name, " featuring | feat. | feat | ft. | ft | with | & ")))
  collab_artists <- collab_artists[!grepl(artist_info$name, collab_artists, ignore.case = TRUE)]
  
  # Get regular albums for total tracks count
  print("Getting albums...")
  albums <- get_artist_albums(
    artist_info$id,
    include_groups = "album",
    limit = 50,
    market = "US"
  )
  
  # Get top tracks for visualizations
  print("Getting top tracks...")
  top_tracks <- get_artist_top_tracks(artist_info$id)
  
  # Process tracks by year
  top_tracks$year <- format(as.Date(top_tracks$album.release_date), "%Y")
  yearly_top_tracks <- top_tracks %>%
    group_by(year) %>%
    slice_max(order_by = popularity, n = 1) %>%
    ungroup() %>%
    arrange(desc(year)) %>%
    select(year, name, popularity) %>%
    rename(
      "Year" = year,
      "Top Track" = name,
      "Popularity Score" = popularity
    )
  
  total_singles <- nrow(all_singles)
  total_collabs <- nrow(all_collabs)
  
  # Calculate total tracks (singles + album tracks + collaborations)
  total_tracks <- nrow(all_singles) + 
    sum(albums$total_tracks) + 
    nrow(all_collabs)
  
  # Make sure to get the preview URL
  preview_url <- top_tracks$preview_url[1]
  print(paste("Preview URL:", preview_url)) # Debug print
  
  # Create collaboration summary with renamed column
  collab_summary <- data.frame(
    "Top.Collab.Songs" = collab_artists[1:min(length(collab_artists), 10)],  # Match the column name exactly
    stringsAsFactors = FALSE
  )
  
  result <- list(
    artist_id = artist_info$id,
    artist_name = artist_info$name,
    artist_image = artist_info$images$url[1],  # High quality image
    followers = artist_info$followers$total,    # Total followers count
    popularity = artist_info$popularity,        # Artist popularity
    top_track_preview = preview_url,
    top_track_name = top_tracks$name[1],
    total_singles = total_singles,
    total_collabs = total_collabs,
    total_albums = nrow(albums),
    avg_duration = mean(top_tracks$duration_ms) / 60000,
    yearly_top_tracks = yearly_top_tracks,
    total_tracks = total_tracks,  # Add total tracks count
    
    # Create collaboration summary
    collab_summary = collab_summary,
    
    # Your existing plots...
    popularity_plot = ggplot(top_tracks, aes(x = reorder(name, popularity), y = popularity)) +
      geom_col(aes(fill = popularity)) +
      scale_fill_gradient(low = "#69b3a2", high = "#2D5D7B") +
      coord_flip() +
      labs(
        x = "Track Name",
        y = "Popularity Score"
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        legend.position = "none"
      ),
    
    duration_plot = ggplot(top_tracks, aes(x = reorder(name, duration_ms), y = duration_ms/60000)) +
      geom_col(fill = "#69b3a2") +
      coord_flip() +
      labs(
        x = "Track Name",
        y = "Duration (minutes)"
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white")
      ),
    
    # Add a new plot for release types distribution
    release_types_plot = ggplot(
      data.frame(
        type = c("Singles", "Albums", "Collaborations"),
        count = c(total_singles, nrow(albums), total_collabs)
      ),
      aes(x = reorder(type, count), y = count)
    ) +
      geom_col(fill = "#69b3a2") +
      coord_flip() +
      labs(
        x = "Type",
        y = "Count"
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white")
      )
  )
  
  return(result)
}

# First, add a function to format followers count
format_followers <- function(n) {
  if (n < 1000) {
    return(as.character(n))
  } else if (n < 1000000) {
    return(paste0(round(n/1000, 2), " K"))
  } else if (n < 1000000000) {
    return(paste0(round(n/1000000, 2), " M"))
  } else {
    return(paste0(round(n/1000000000, 2), " B"))
  }
}

# Shiny UI
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Spotify Artist Analytics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    textInput("artist_name", "Enter Artist Name", ""),
    actionBttn(
      "analyze_button",
      "Analyze",
      style = "gradient",
      color = "success"
    )
  ),
  dashboardBody(
    use_waiter(),
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #1E1E1E; }
        .box { background-color: #2D2D2D; color: white; }
        .box-header {
          color: white !important;
          font-size: 16px !important;
          font-weight: bold !important;
        }
        .value-box { 
          margin-bottom: 4px;
          width: 70% !important;  /* Increased from 50% to 70% */
          margin-left: auto !important;
          margin-right: 0 !important;
        }
        .artist-container { 
          display: flex; 
          padding: 15px;
          align-items: flex-start;
        }
        .artist-left { 
          width: 35%;
          padding-right: 20px;
        }
        .artist-right { 
          width: 65%;
        }
        .artist-image {
          width: 100%;
          border-radius: 8px;
        }
        .small-value-box {
          width: 70% !important;  /* Increased from 50% to 70% */
          height: 94px !important;
          margin-left: auto !important;
          margin-right: 0 !important;
        }
        .section-box {
          margin-bottom: 15px;
        }
        .small-box h3 {
          font-size: 22px !important;
          margin-top: 7px !important;
          margin-bottom: 5px !important;
        }
        .small-box p {
          font-size: 11px !important;
        }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "dashboard",
        # Section 1: Artist Profile and Release Distribution
        fluidRow(
          # Left: Artist Profile and KPIs
          box(
            width = 6,
            status = "success",
            solidHeader = TRUE,
            title = uiOutput("artistNameTitle"),
            class = "section-box",
            div(
              class = "artist-container",
              div(
                class = "artist-left",
                div(uiOutput("artistImage"))
              ),
              div(
                class = "artist-right",
                div(class = "small-value-box", valueBoxOutput("popularity_box", width = 12)),
                div(class = "small-value-box", valueBoxOutput("followers_box", width = 12)),
                div(class = "small-value-box", valueBoxOutput("total_tracks_box", width = 12)),
                div(class = "small-value-box", valueBoxOutput("duration_box", width = 12))
              )
            )
          ),
          # Right: Release Distribution
          box(
            width = 6,
            status = "success",
            title = "Distribution of Releases",
            class = "section-box",
            plotlyOutput("releaseTypesPlot")
          )
        ),
        
        # Section 2: Popular Tracks and Duration Charts
        fluidRow(
          # Left: Popular Tracks Chart
          box(
            width = 6,
            status = "success",
            title = "Top Tracks by Popularity",
            class = "section-box",
            plotlyOutput("popularityPlot")
          ),
          # Right: Duration Chart
          box(
            width = 6,
            status = "success",
            title = "Track Durations",
            class = "section-box",
            plotlyOutput("durationPlot")
          )
        ),
        
        # Section 3: Top Tracks by Year and Collaborations
        fluidRow(
          # Left: Top Tracks by Year
          box(
            width = 6,
            status = "success",
            title = "Top Track by Year",
            class = "section-box",
            DTOutput("yearlyTable")
          ),
          # Right: Collaborations Table
          box(
            width = 6,
            status = "success",
            title = "Top Collaborators",
            class = "section-box",
            DTOutput("collabTable")
          )
        )
      ),
      tabItem(
        tabName = "about",
        box(
          width = 12,
          includeMarkdown("about.md")
        )
      )
    )
  )
)

# Shiny Server
server <- function(input, output) {
  w <- Waiter$new(
    html = spin_flower(),
    color = transparent(.5)
  )
  
  result <- reactiveVal(NULL)
  
  observeEvent(input$analyze_button, {
    req(input$artist_name)
    w$show()
    
    tryCatch({
      details <- artist_details(
        input$artist_name,
        Sys.getenv("SPOTIFY_CLIENT_ID"),
        Sys.getenv("SPOTIFY_CLIENT_SECRET")
      )
      
      if (is.null(details)) {
        showNotification("Artist not found", type = "error")
        w$hide()
        return()
      }
      
      result(details)
      
      # Update value boxes with new metrics
      output$popularity_box <- renderValueBox({
        valueBox(
          paste0(result()$popularity, "%"),
          "Popularity",
          icon = icon("star"),
          color = "green",
          width = 12
        )
      })
      
      output$followers_box <- renderValueBox({
        valueBox(
          format_followers(result()$followers),
          "Total Followers",
          icon = icon("users"),
          color = "green",
          width = 12
        )
      })
      
      output$total_tracks_box <- renderValueBox({
        valueBox(
          paste0(result()$total_tracks),
          "Total Tracks",
          icon = icon("music"),
          color = "green"
        )
      })
      
      output$duration_box <- renderValueBox({
        valueBox(
          paste0(round(result()$avg_duration, 2), " min"),
          "Average Duration",
          icon = icon("clock"),
          color = "green",
          width = 12
        )
      })
      
      # Update plots
      output$popularityPlot <- renderPlotly({
        ggplotly(details$popularity_plot)
      })
      
      output$durationPlot <- renderPlotly({
        ggplotly(details$duration_plot)
      })
      
      output$yearlyTable <- renderDT({
        req(result())
        datatable(
          result()$yearly_top_tracks,
          options = list(
            pageLength = 10,
            dom = 't',  # Only show the table, no search/pagination
            ordering = TRUE,
            rownames = FALSE,
            columnDefs = list(
              list(className = 'dt-center', targets = '_all')
            )
          ),
          style = 'bootstrap',
          class = 'compact',
          rownames = FALSE
        ) %>%
          formatStyle(
            columns = 1:3,
            backgroundColor = 'rgb(25, 25, 25)',
            color = 'white'
          )
      })
      
      output$artistImage <- renderUI({
        req(result())
        tags$img(
          src = result()$artist_image,
          class = "artist-image"
        )
      })
      
      output$collabTable <- renderDT({
        req(result())
        datatable(
          result()$collab_summary,
          options = list(
            pageLength = 5,
            dom = 't',
            ordering = TRUE,
            rownames = FALSE
          ),
          style = 'bootstrap',
          class = 'compact'
        ) %>%
          formatStyle(
            columns = 'Top.Collab.Songs',  # Fixed column name to match the actual data
            backgroundColor = 'rgb(25, 25, 25)',
            color = 'white'
          )
      })
      
      output$releaseTypesPlot <- renderPlotly({
        req(result())
        ggplotly(result()$release_types_plot) %>%
          layout(
            plot_bgcolor = "black",
            paper_bgcolor = "black",
            font = list(color = "white")
          )
      })
      
      output$artistNameTitle <- renderUI({
        req(result())
        HTML(paste(result()$artist_name))
      })
      
    }, error = function(e) {
      print(e)
      showNotification(
        paste("Error:", e$message),
        type = "error"
      )
    })
    
    w$hide()
  })
}

# Run the Shiny App
shinyApp(ui, server)