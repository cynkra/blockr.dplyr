# Module UI
mod_ui <- function(id) {
  ns <- NS(id)
  div(
    textOutput(ns("display"))
  )
}

# Module Server
mod_server <- function(id, reactive_input) {
  moduleServer(id, function(input, output, session) {
    output$display <- renderText({
      paste("Value is:", reactive_input())
    })
  })
}

# Main App
shinyApp(
  ui = bslib::page_fluid(
    theme = bslib::bs_theme(version = 5),
    textInput("txt", "Enter text"),
    mod_ui("mymod")
  ),
  server = function(input, output, session) {
    my_reactive <- reactive({ input$txt })

    mod_server("mymod", my_reactive)
  }
)