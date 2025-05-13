# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(gdtools)
#library(data.table)
library(arrow)
library(fst)

# Register font
register_gfont("Roboto") # IF using ggiraph

# Load the dataset
df <- data.frame(open_dataset('all_leg.parquet'))
#df <- read.fst('df.fst')
#df <- read.csv('df.csv')

#df <- fread("all_leg.csv")
# Data currently has the following variables
    # bill_id	
    # bill_url	
    # bill_number	
    # bill_type	
    # bill_chamber	
    # bill_no	
    # bill_name	
    # congress	
    # swi_issue_id	
    # issue_name	
    # sponsor_name	
    # sponsor_person_id	
    # sponsor_party	
    # cosponsor_name	
    # cosponsor_person_id	
    # cosponsor_party

# Create bill_search_item column
#df <- df %>%
#  mutate(bill_search_item = paste0(bill_number, " - ", congress, "th congress"))

df <- df %>%
  mutate(bill_search_item = bill_number)


# Define UI for the application
ui <- fluidPage(
  
  # Link to external CSS file
  includeCSS("styles.css"),
  
  # Title section

  
  fluidRow(
    # Selection Panel
    column(
      width = 12,  # Initial width
      addGFontHtmlDependency(family = c("Roboto")),
      id = "sidebar",
      tags$div(
        id = "sidebar-content",
        tags$div(
          class = "grid-container",
          
          # Filter by Congress Version
          selectInput("congress_version",
                      "Congress",
                      choices = sort(unique(df$congress), decreasing = TRUE),
                      selected = max(df$congress)),
          # Filter by Chamber
          selectInput("selected_chamber",
                      "Chamber",
                      choices = c("House", "Senate"),
                      selected = "House"),
          # Filter by Issue
          selectInput("selected_issue",
                      "Issue Area",
                      choices = c("All", sort(unique(df$issue_name))),
                      selected = "All"),
          # Search for specific bills
          selectizeInput("search_bills",
                         "Search Bills",
                         choices = NULL,  # Set choices to NULL initially
                         selected = "All"),  # Enable server-side processing
          
          selectizeInput("search_sponsor",
                         "Search Sponsor",
                         choices = NULL,  # Set choices to NULL initially
                         selected = "All"
                         ),
          
          selectizeInput("search_cosponsor",
                         "Search Cosponsor",
                         choices = NULL,  # Set choices to NULL initially
                         selected = "All"
                         )
          
        )
      )
    ),
    
    # Main Panel
    column(
      width = 12,  # Initial width
      id = "main",
      titlePanel(textOutput("main_title")),
      tags$h3(id = "subtitle", textOutput("subtitle")),
      girafeOutput("plot", width = "1000px"),
      tags$p (HTML("<b>Methodology</b><br>
                   Data is provided by <a href='https://www.congress.gov' target='_blank'>congress.gov</a>. Cosponsor data is derived from each bill’s Republican and Democratic cosponsors.")),
      tags$script(
        "Shiny.addCustomMessageHandler('openURL', function(url) { window.open(url); });"
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  # Reactive filtered dataset
  filtered_data <- reactive({
    df %>%
      filter(
        congress == input$congress_version,
        bill_chamber == input$selected_chamber,
        if (input$selected_issue != "All") issue_name == input$selected_issue else TRUE,
        if (input$search_sponsor != "All") sponsor_name == input$search_sponsor else TRUE,
        if (input$search_cosponsor != "All") cosponsor_name == input$search_cosponsor else TRUE
      )
  })
  
  # Preload options for default Congress and Chamber
  observe({
    filtered_df <- df %>%
      filter(
        congress == max(df$congress),  # Default Congress: 118th
        bill_chamber == "House"        # Default Chamber: House
      )
    
    updateSelectizeInput(session, "search_bills", 
                         choices = c("All", sort(unique(filtered_df$bill_search_item))), 
                         selected = "All", server = TRUE)
    
    updateSelectizeInput(session, "search_sponsor", 
                         choices = c("All", sort(unique(filtered_df$sponsor_name))), 
                         selected = "All", server = TRUE)
    
    updateSelectizeInput(session, "search_cosponsor", 
                         choices = c("All", sort(unique(filtered_df$cosponsor_name))), 
                         selected = "All", server = TRUE)
  })
  
  # Update filters dynamically based on Congress
  observeEvent(input$congress_version, {
    filtered_df <- df %>%
      filter(congress == input$congress_version)
    
    updateSelectizeInput(session, "search_bills", 
                         choices = c("All", sort(unique(filtered_df$bill_search_item))), 
                         selected = "All", server = TRUE)
    updateSelectizeInput(session, "search_sponsor", 
                         choices = c("All", sort(unique(filtered_df$sponsor_name))), 
                         selected = "All", server = TRUE)
    updateSelectizeInput(session, "search_cosponsor", 
                         choices = c("All", sort(unique(filtered_df$cosponsor_name))), 
                         selected = "All", server = TRUE)
  })
  
  # Update filters dynamically based on Chamber
  observeEvent(input$selected_chamber, {
    filtered_df <- df %>%
      filter(
        congress == input$congress_version,
        bill_chamber == input$selected_chamber
      )
    
    updateSelectizeInput(session, "search_bills", 
                         choices = c("All", sort(unique(filtered_df$bill_search_item))), 
                         selected = "All", server = TRUE)
    updateSelectizeInput(session, "search_sponsor", 
                         choices = c("All", sort(unique(filtered_df$sponsor_name))), 
                         selected = "All", server = TRUE)
    updateSelectizeInput(session, "search_cosponsor", 
                         choices = c("All", sort(unique(filtered_df$cosponsor_name))), 
                         selected = "All", server = TRUE)
  })
  
  # Update sponsors and cosponsors based on selected Issue
  observeEvent(input$selected_issue, {
    filtered_df <- df %>%
      filter(
        congress == input$congress_version,
        bill_chamber == input$selected_chamber,
        if (input$selected_issue != "All") issue_name == input$selected_issue else TRUE
      )
    
    updateSelectizeInput(session, "search_bills", 
                         choices = c("All", sort(unique(filtered_df$bill_search_item))), 
                         selected = "All", server = TRUE)
    updateSelectizeInput(session, "search_sponsor", 
                         choices = c("All", sort(unique(filtered_df$sponsor_name))), 
                         selected = "All", server = TRUE)
    updateSelectizeInput(session, "search_cosponsor", 
                         choices = c("All", sort(unique(filtered_df$cosponsor_name))), 
                         selected = "All", server = TRUE)
  })
  
  # Update bills and cosponsors based on selected Sponsor
  observeEvent(input$search_sponsor, {
    filtered_df <- filtered_data()
    
    # Filter cosponsors based on bills sponsored by the selected sponsor
    cosponsor_bill_ids <- filtered_df %>%
      filter(sponsor_name == input$search_sponsor) %>%
      pull(bill_id) %>%
      unique()
    
    cosponsors <- df %>%
      filter(bill_id %in% cosponsor_bill_ids) %>%
      pull(cosponsor_name) %>%
      unique()
    
    updateSelectizeInput(session, "search_cosponsor", 
                         choices = c("All", sort(cosponsors)), 
                         selected = "All", server = TRUE)
    
    # Update bills dropdown to show only those sponsored by the selected sponsor
    updateSelectizeInput(session, "search_bills", 
                         choices = c("All", sort(unique(filtered_df$bill_search_item))), 
                         selected = "All", server = TRUE)
  })
  
  # Update bills dynamically if no sponsor or cosponsor is selected
  observe({
    if (input$search_sponsor == "All" && input$search_cosponsor == "All") {
      filtered_df <- df %>%
        filter(
          congress == input$congress_version,
          bill_chamber == input$selected_chamber,
          if (input$selected_issue != "All") issue_name == input$selected_issue else TRUE
        )
      
      updateSelectizeInput(session, "search_bills", 
                           choices = c("All", sort(unique(filtered_df$bill_search_item))), 
                           selected = "All", server = TRUE)
    }
  })
  
  # Main plot logic
  output$plot <- renderGirafe({
    df_filtered <- filtered_data()
    
    # Ensure cosponsor_party column has correct values
    df_filtered <- df_filtered %>%
      mutate(cosponsor_party = trimws(cosponsor_party)) %>%
      filter(cosponsor_party %in% c("Democrat", "Republican"))
    
    # Create plot_df with required columns and calculations
    plot_df <- df_filtered %>%
      group_by(bill_url, bill_name, bill_number, sponsor_name, sponsor_party, issue_name) %>%
      summarize(
        num_dem_cosponsors = n_distinct(cosponsor_person_id[cosponsor_party == "Democrat"]),
        num_rep_cosponsors = n_distinct(cosponsor_person_id[cosponsor_party == "Republican"])
      ) %>%
      ungroup() %>%
      mutate(
        tooltip = paste(
          "Bill:", bill_number, "<br>",
          "Title:", bill_name, "<br>",
          "Sponsor:", sponsor_name, "<br>",
          "Issue:", issue_name, "<br>",
          "Democratic Cosponsors:", num_dem_cosponsors, "<br>",
          "Republican Cosponsors:", num_rep_cosponsors
        )
      )
    
    # Set axis limits
    axis_limit <- if (input$selected_chamber == "Senate") 50 else 220
    
    # Plot graph
    p <- ggplot(plot_df, aes(x = num_dem_cosponsors, y = num_rep_cosponsors)) +
      geom_point_interactive(aes(tooltip = tooltip, data_id = bill_url),
                             color = ifelse(plot_df$sponsor_party == 'Republican', '#810000',
                                            ifelse(plot_df$sponsor_party == 'Democrat', '#2E598E', '#6B56AA')),
                             size = 2.5, alpha = 0.5) +
      geom_abline(intercept = 0, slope = 1, color = "gray", linewidth = 0.5, linetype = "dashed") +
      coord_cartesian(
        xlim = c(0, axis_limit),
        ylim = c(0, axis_limit)
      ) +
      labs(x = "Democratic Cosponsors",
           y = "Republican Cosponsors") +
      theme_classic(base_family = "Roboto")
    
    girafe(ggobj = p, options = list(
      opts_hover(css = "cursor:pointer;fill:yellow;stroke:gray;"),
      opts_selection(type = "single", css = "fill:yellow;stroke:gray;")
    ))
    
  })
  
  observeEvent(input$plot_selected, {
    selected_id <- input$plot_selected
    if (!is.null(selected_id)) {
      url <- paste0("https://app.legis1.com/bill/detail?id=", selected_id, "#summary")
      session$sendCustomMessage(type = 'openURL', message = url)
    }
  })
}




# Run the application
shinyApp(ui = ui, server = server)


