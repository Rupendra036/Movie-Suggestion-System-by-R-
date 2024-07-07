library(shiny)
library(dplyr)
library(stringr)

# Define UI with a background image and logo from URL
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "
        body {
          background-image: url('https://imgix.bustle.com/inverse/0a/c8/f7/92/856d/470f/84dc/5c431ee8092b/netflixs-altered-carbon-is-some-of-the-best-new-sci-fi-tv-weve-seen-in-recent-memory.jpeg?w=2000&h=640&auto=format%2Ccompress&cs=srgb&q=70&fit=crop&crop=faces&blend=ffffff&blendAlpha=45&blendMode=normal');
          background-size: cover;
          background-repeat: no-repeat;
          background-attachment: fixed;
          background-position: center;
        }
        .titlePanel {
          text-align: center; /* Center content horizontally */
        }
        .titlePanel img {
          display: block;   /* Display image as block element */
          margin: 0 auto 10px auto; /* Set margins for top, bottom, left, right */
        }
        "
      )
    )
  ),
  
  tags$div(
    style = "position: absolute; top: 20px; right: 30px; z-index: 10000;",
    img(src = "https://cdn.theatlantic.com/assets/media/img/custom/2018/04/24/netflix-logo.png", height = 100)
  ),
  tags$div(
    style = "position: absolute; bottom: 20px; middle: 30px; z-index: 10000;",
    img(src = "https://2.bp.blogspot.com/--bUexRZr7-M/T092HCvJOdI/AAAAAAAAAEg/FJ9V0MdEH2I/s1600/film+logos+2.png", height = 200)
  ),
  
  titlePanel(
    h3("Movie Suggestion System"),
    br()
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload your movie dataset (CSV format):"),
      selectInput("type", "Select Type:",
                  choices = NULL),
      selectInput("title", "Select Title:",
                  choices = NULL)
    ),
    mainPanel(
      titlePanel(
        h3("Cast :"),
        br()
      ),
      htmlOutput("suggestions"),
      br(), br(), br(),
      titlePanel(
        h3("Description of that movie"),
        br()
      ),
      textOutput("description"),
      br(),
      h4("Type:"),
      textOutput("type_output"),
      h4("Country:"),
      textOutput("country"),
      h4("Rating:"),
      textOutput("rating")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  dataset <- reactive({
    req(input$file)
    read.csv(input$file$datapath, stringsAsFactors = FALSE)
  })
  
  observe({
    updateSelectInput(session, "type", "Select Type:",
                      choices = unique(dataset()$type))
  })
  
  observeEvent(input$type, {
    updateSelectInput(session, "title", "Select Title:",
                      choices = unique(dataset()$title[dataset()$type == input$type]))
  })
  
  output$suggestions <- renderText({
    filtered_movies <- dataset() %>%
      filter(type == input$type, title == input$title)
    
    if (nrow(filtered_movies) == 0) {
      return("No movies found for the selected type and title.")
    } else {
      cast <- filtered_movies$cast[1]
      cast <- str_replace_all(cast, paste0("\\b(", input$title, ")\\b"), "<strong>\\1</strong>")
      return(HTML(cast))
    }
  })
  
  output$description <- renderText({
    selected_movie <- dataset() %>%
      filter(type == input$type, title == input$title) %>%
      slice(1)
    
    if (nrow(selected_movie) == 0) {
      return("")
    } else {
      return(selected_movie$description)
    }
  })
  
  output$type_output <- renderText({
    selected_movie <- dataset() %>%
      filter(type == input$type, title == input$title) %>%
      slice(1)
    
    if (nrow(selected_movie) == 0) {
      return("")
    } else {
      return(selected_movie$type)  
    }
  })
  
  output$country <- renderText({
    selected_movie <- dataset() %>%
      filter(type == input$type, title == input$title) %>%
      slice(1)
    
    if (nrow(selected_movie) == 0) {
      return("")
    } else {
      return(selected_movie$country)  
    }
  })
  
  output$rating <- renderText({
    selected_movie <- dataset() %>%
      filter(type == input$type, title == input$title) %>%
      slice(1)
    
    if (nrow(selected_movie) == 0) {
      return("")
    } else {
      return(selected_movie$rating)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
