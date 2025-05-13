
library(shiny)
library(ggplot2)
library(dplyr)
#library(tidyverse)
library(ggiraph)
library(gdtools)
#library(fst)
library(data.table)

#Register the font
register_gfont("Roboto")
#options(scipen=999)

# Load the dataset
df <- fread("df2.csv")
#df <- read.fst('df2.fst')

# Data currently has the following variables
    # comm_content_id	
    # member_id	
    # person_id	
    # chamber	
    # content	
    # issue_name	
    # swi_issue_id	
    # pub_date	
    # display_name	
    # formal_title	
    # first_name	
    # last_name	
    # party_name	
    # us_state_id	
    # district_no



# Modify date column
#df$date <- as.Date(df$pub_date, format= "%Y-%m-%d")
#df$date_display <- format(df$date, "%B %Y")
#df$date_sort <- as.Date(paste0(format(df$date, "%Y-%m"), "-01"))

#df <- df %>%
#  group_by(date_sort, date_display, member_id, person_id, chamber, issue_name, swi_issue_id, display_name, formal_title, first_name, last_name, party_name, us_state_id, district_no) %>%
#  summarise(posts = n(), .groups = "drop") %>%
#  ungroup() %>%
#  arrange(date_sort)

#Modify party column
#df$party_name <- gsub(",.*", "", df$party_name)

#Create chart name column
#df <- df %>%
#  mutate(chart_name = paste0(substr(display_name, 1, 3), ". ", last_name, " ", sub(".*\\s(\\S+)$", "\\1", display_name)))

# Modify issue column
#df$issue_name <- gsub("\\*\\*$", "", df$issue_name)


# Define UI for the application
ui <- fluidPage(
  
  # Link to external CSS file
  includeCSS("styles.css"),
  
  # Title section
  tags$div(
    id = "custom-html-section"
    #tags$h1("Lawmaker X (Twitter) Activity by Issue")
  ),
  
  fluidRow(
    # Selection Panel
    column(
      width = 12,  # Initial width
      addGFontHtmlDependency(family = c("Roboto")), # Add font dependency
      id = "sidebar",
      tags$div(
        id = "sidebar-content",
        tags$div(
          class = "grid-container",
          # Select Issue
          selectInput("selected_issue",
                      "Issue Area",
                      choices = c("All", sort(unique(df$issue_name))),
                      selected = "All"),
          # Select Chamber
          selectInput("selected_chamber",
                      "Chamber",
                      choices = c("Both Chambers", "House", "Senate"),
                      selected = "Both Chambers"),
          # Select Party
          selectInput("selected_party",
                      "Party",
                      choices = c("All Parties", "Democrat", "Republican", "Independent"),
                      selected = "All Parties"),
          # Select Number pf Lawkmakers to display
          selectInput("num_lawmakers",
                      "Number of lawmakers",
                      choices = c("10", "20", "50"),
                      selected = "20"),

          selectInput("start_date",
                      "Start Date",
                      choices = rev(unique(df$date_display)),  # Reverse the order of choices
                      selected = tail(unique(df$date_display), 2)[1]  # Second from the last value
          ),
          selectInput("end_date",
                      "End Date",
                      choices = rev(unique(df$date_display)),  # Reverse the order of choices
                      selected = tail(unique(df$date_display), 1)  # Most recent value
          )
        ),
      ),
    ),
    
    
    # Main Panel
    column(
      width = 12,  # Initial width
      id = "main",
      titlePanel(textOutput("main_title")),
      tags$h3(id = "subtitle", textOutput("subtitle")),
      girafeOutput("plot", width = "1000px"),
      tags$script(
        "Shiny.addCustomMessageHandler('openURL', function(url) { window.open(url); });"
      )
    )
  )
)



# Define server logic
server <- function(input, output, session) {
  output$plot <- renderGirafe({
    # Filter by date
    start_date <- df %>% filter(date_display == input$start_date) %>% pull(date_sort)
    end_date <- df %>% filter(date_display == input$end_date) %>% pull(date_sort)
    
    filtered_df <- subset(df, date_sort >= unique(start_date) & date_sort <= unique(end_date))
    
    # Print filtered dataframe for debugging
    print(head(filtered_df))
    
    # Filter by selected issue
    if (input$selected_issue != "All") {
      filtered_df <- filtered_df %>% filter(issue_name == input$selected_issue)
      # Check if filtered by issue
      print(paste("Filtered by issue:", input$selected_issue))
      print(head(filtered_df))
    }
    
    # Check if the filtered dataframe is empty
    if(nrow(filtered_df) == 0) {
      print("No data available after filters.")
      return(NULL)  # Return NULL if no data to avoid errors
    }
    
    # Prepare data for plotting
    plot_df <- filtered_df %>%
      group_by(display_name, party_name, chart_name, person_id) %>%
      summarise(posts = sum(posts), .groups = "drop")
    
    print("Grouped data:")
    print(head(plot_df))
    
    # Select top lawmakers based on user input
    if (input$num_lawmakers != "All") {
      top_lawmakers <- plot_df %>%
        arrange(desc(posts)) %>%
        head(as.numeric(input$num_lawmakers))
      plot_df <- plot_df %>% filter(chart_name %in% top_lawmakers$chart_name)
    }
    
    # Set colors
    party_colors <- c("Democrat" = "#2E598E", "Republican" = "#810000", "Independent" = "#B19CD9")
    
    # Plot
    p <- ggplot(plot_df, aes(x = reorder(chart_name, posts), y = posts, fill = party_name)) +
      geom_bar_interactive(stat = "identity", aes(tooltip = paste0(display_name, ': ', posts, ' posts'), 
                                                  data_id = as.numeric(person_id))) +
      labs(x = "Lawmaker", y = "Number of Twitter Posts", fill = "Party Name") +
      coord_flip() +
      scale_fill_manual(values = party_colors) +
      theme_classic(base_family = "Roboto") +
      theme(axis.title = element_text(color="black", size = 8),
            axis.text = element_text(color="black", size = 6),
            legend.title = element_text(color="black", size = 8),
            legend.text = element_text(color="black", size = 7))
    girafe(ggobj = p, options = list(
      opts_hover(css = "cursor:pointer;fill:gray;stroke:gray;"),
      opts_selection(type = "single", css = "fill:gray;stroke:gray;")
    ))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
