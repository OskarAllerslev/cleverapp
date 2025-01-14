# ======================================================
# app.R (Single-file Shiny app with navlistPanel)
# ======================================================
library(shiny)
library(bslib)
library(yaml)

# ---------------------------
# Load the YAML with chapters
# ---------------------------
content_data <- read_yaml("data/content.yaml")

# ---------------------------
# Define a custom theme using bslib
# ---------------------------
my_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#007bff",
  secondary = "#6c757d",
  base_font = font_google("Roboto"),
  heading_font = font_google("Montserrat")
)

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  theme = my_theme,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  titlePanel("Matematik A hhx"),
  fluidRow(
    column(
      width = 12,
      navlistPanel(
        "Kapitler / Underkapitler",
        id = "chapters_nav",
        widths = c(3, 9),
        # Dynamically create tabPanel for each chapter
        !!!lapply(sort(names(content_data$book1)), function(ch_id) {
          chapter <- content_data$book1[[ch_id]]
          tabPanel(
            title = paste(ch_id, "-", chapter$title),
            fluidPage(
              withMathJax(),
              h3(chapter$title),
              p(chapter$text),
              if (!is.null(chapter$latex)) div(withMathJax(chapter$latex)),
              if (!is.null(chapter$plot) && chapter$plot == "circle_plot") {
                plotOutput(paste0("circle_plot_output_", ch_id))
              },
              if (!is.null(chapter$exercise) && chapter$exercise == "exercise_quadratic") {
                tagList(
                  numericInput(paste0("quad_input_", ch_id), "Find rødderne af x^2 - 4:", value = 0),
                  actionButton(paste0("quad_submit_", ch_id), "Tjek"),
                  textOutput(paste0("quad_feedback_", ch_id))
                )
              }
            )
          )
        })
      )
      
    )
  )
)

# ---------------------------
# SERVER
# ---------------------------
server <- function(input, output, session) {
  
  # Render circle plot
  output$circle_plot_output <- renderPlot({
    current_tab <- input$chapters_nav
    ch_id <- strsplit(current_tab, " - ")[[1]][1]
    chapter <- content_data$book1[[ch_id]]
    if (!is.null(chapter$plot) && chapter$plot == "circle_plot") {
      plot(c(-1, 1), c(-1, 1), type = "n", main = "Cirkel")
      symbols(0, 0, circles = 1, add = TRUE)
    }
  })
  
  # Handle quadratic exercise
  observeEvent(input$quad_submit, {
    current_tab <- input$chapters_nav
    ch_id <- strsplit(current_tab, " - ")[[1]][1]
    chapter <- content_data$book1[[ch_id]]
    if (!is.null(chapter$exercise) && chapter$exercise == "exercise_quadratic") {
      val <- input$quad_input
      feedback <- if (val %in% c(-2, 2)) "Korrekt!" else "Prøv igen."
      output$quad_feedback <- renderText(feedback)
    }
  })
}

# ---------------------------
# Run the App
# ---------------------------
shinyApp(ui = ui, server = server)
