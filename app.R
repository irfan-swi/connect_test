library(shiny)
library(dplyr)
library(ggplot2)
library(ggiraph)

# Sample data from the loyalty_all function
all_loyalty <- read.csv("all_loyalty.csv")

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .well {
        background-color: transparent;
        border: none;
        box-shadow: none;
      }
      .app-title {
        font-size: 20px;
      }
      .app-subtitle {
        font-size: 14px;
        color: grey;
      }
    "))
  ),
  titlePanel(
    div(
      class = "app-title",
      "Lawmaker Party Loyalty",
      div(class = "app-subtitle", "Percent of the time a lawmaker votes with their own party on divided votes")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("congress", "Select Congress", choices = sort(unique(all_loyalty$congress)), selected = 118),
      checkboxGroupInput("party", "Select Party", choices = list("Democrats" = "D", "Republicans" = "R"), selected = c("D", "R")),
      sliderInput("numValues", "Number of Lawmakers to Display", min = 10, max = 100, value = 50, step = 10)
    ),
    mainPanel(
      ggiraphOutput("loyaltyPlot", width = "100%", height = "800px")
    )
  )
)

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    all_loyalty %>%
      filter(congress == input$congress, party %in% input$party) %>%
      arrange(desc(party_loyalty)) %>%
      head(input$numValues)
  })
  
  output$loyaltyPlot <- renderggiraph({
    data <- filtered_data()
    
    font_size <- ifelse(input$numValues <= 50, 12, 10)
    point_size <- ifelse(input$numValues <= 50, 5, 3)
    segment_size <- ifelse(input$numValues <= 50, 2, 1)
    
    p <- ggplot(data, aes(x = reorder(name, party_loyalty), y = party_loyalty, color = party)) +
      geom_point_interactive(aes(tooltip = paste(name, "<br>Loyalty:", round(party_loyalty, 2))), size = point_size) +
      geom_segment(aes(xend = reorder(name, party_loyalty), yend = 0), size = segment_size) +
      scale_color_manual(values = c("D" = "blue", "R" = "red")) +
      coord_flip() +
      labs(x = "Lawmakers", y = "Party Loyalty", title = paste("Percent of Votes with Party"),subtitle = paste0(input$congress, " Congress")) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18),
        axis.text.y = element_text(size = font_size),
        axis.text.x = element_text(size = 12)
      )
    
    ggiraph(code = print(p), width_svg = 15, height_svg = 12)
  })
}

# Run the app
shinyApp(ui, server)
