# modules/content_module.R
content_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("content_area"))
  )
}

content_server <- function(id, selected_book, content_data, selected_chapter) {
  moduleServer(id, function(input, output, session) {
    
    # If you have exercises or plots, define their loaders:
    load_exercise <- function(exercise_id, ns) {
      if (exercise_id == "exercise_quadratic") {
        tagList(
          numericInput(ns("quad_input"), "Find rødderne af x^2 - 4", value = 0),
          actionButton(ns("quad_submit"), "Tjek"),
          textOutput(ns("quad_feedback"))
        )
      } else {
        return(NULL)
      }
    }
    
    load_plot <- function(plot_id) {
      if (plot_id == "circle_plot") {
        return(function() {
          plot(c(-1,1), c(-1,1), type="n", main="Cirkel")
          symbols(0,0, circles=1, add=TRUE)
        })
      }
      return(NULL)
    }
    
    observe({
      chap <- selected_chapter()
      if (is.null(chap)) {
        message("[DEBUG content] No chapter selected yet.")
        return()
      }
      
      book       <- chap$book
      chapter_id <- chap$chapter_id
      message("[DEBUG content] selected_chapter() => book=", book,
              ", chapter_id=", chapter_id)
      
      # Validate
      if (!book %in% names(content_data)) {
        message("[DEBUG content] Book '", book, "' not in content_data!")
        return()
      }
      if (!chapter_id %in% names(content_data[[book]])) {
        message("[DEBUG content] Chapter '", chapter_id, "' not in content_data[[", book, "]]!")
        return()
      }
      
      content_item <- content_data[[book]][[chapter_id]]
      message("[DEBUG content] content_item$title = ", content_item$title)
      
      output$content_area <- renderUI({
        ns <- session$ns
        tagList(
          h3(content_item$title),
          p(content_item$text),
          if (!is.null(content_item$latex)) withMathJax(content_item$latex),
          if (!is.null(content_item$plot)) plotOutput(ns("dynamic_plot")),
          if (!is.null(content_item$exercise)) load_exercise(content_item$exercise, ns)
        )
      })
      
      if (!is.null(content_item$plot)) {
        output$dynamic_plot <- renderPlot({
          plot_fn <- load_plot(content_item$plot)
          if (!is.null(plot_fn)) plot_fn()
        })
      }
      
      # Sample exercise logic
      if (!is.null(content_item$exercise) && content_item$exercise == "exercise_quadratic") {
        observeEvent(input$quad_submit, {
          # x^2 - 4 => ±2 are roots
          feedback <- if (input$quad_input %in% c(-2, 2)) "Korrekt!" else "Prøv igen."
          output$quad_feedback <- renderText(feedback)
        })
      }
    })
  })
}
