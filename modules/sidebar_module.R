# modules/sidebar_module.R
sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("chapter_list"))
  )
}

sidebar_server <- function(id, selected_book, content_data, selected_chapter) {
  moduleServer(id, function(input, output, session) {
    
    output$chapter_list <- renderUI({
      ns <- session$ns
      book <- selected_book()
      
      message("[DEBUG sidebar] selected_book() = ", book)
      
      if (is.null(book)) {
        return(h5("Vælg en bog ovenfor."))
      }
      if (!book %in% names(content_data)) {
        return(h5("Ingen kapitler fundet for: ", book))
      }
      
      chapters_in_book <- content_data[[book]]
      if (length(chapters_in_book) == 0) {
        return(h5("Ingen kapitler tilgængelige."))
      }
      
      # Sort them if you have numeric or dotted keys (like "1.1", "1.2"):
      ch_ids <- sort(names(chapters_in_book))
      
      message("[DEBUG sidebar] chapters_in_book keys = ",
              paste(ch_ids, collapse = ", "))
      
      # Create an actionLink for each chapter
      lapply(ch_ids, function(ch_id) {
        ch_title <- chapters_in_book[[ch_id]]$title
        actionLink(
          ns(paste0("chapter_", ch_id)),
          label = ch_title,
          style = "display: block; margin-bottom: 5px;"
        )
      })
    })
    
    # Observe all chapter links
    observe({
      book <- selected_book()
      if (is.null(book) || !book %in% names(content_data)) return()
      
      chapters_in_book <- content_data[[book]]
      ch_ids <- names(chapters_in_book)
      
      for (ch_id in ch_ids) {
        observeEvent(input[[paste0("chapter_", ch_id)]], {
          message("[DEBUG sidebar] Clicked chapter ID: ", ch_id)
          selected_chapter(list(book = book, chapter_id = ch_id))
        }, ignoreInit = TRUE)
      }
    })
    
  })
}
