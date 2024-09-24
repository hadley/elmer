#' Open an interactive chat application
#'
#' Use `chatApp()` to launch an interactive shiny app for any chat instance.
#'
#' @param chat A chat object created by [new_chat_openai()] or friends.
#' @export
#' @returns A [shiny::shinyApp].
chatApp <- function(chat) {
  check_installed(c("bslib", "shiny", "shinychat"))

  ui <- bslib::page_fluid(
    shinychat::chat_ui("chat"),
    shiny::actionButton(
      "close_btn", "",
      class = "btn-close",
      style = "position: absolute; top: 10px; left: 10px;"
    )
  )
  server <- function(input, output, session) {
    for (message in chat$messages()) {
      shinychat::chat_append_message("chat", message)
    }

    shiny::observeEvent(input$chat_user_input, {
      stream <- chat$stream_async(input$chat_user_input)
      shinychat::chat_append("chat", stream)
    })

    shiny::observeEvent(input$close_btn, {
      shiny::stopApp()
    })
  }

  shiny::shinyApp(ui, server)
}
