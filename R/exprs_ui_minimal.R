#' Create a minimal UI element for a single expression
#'
#' @param id Character string, an identifier for the UI element
#' @param value Default value for the expression
#' @return A div element containing the UI components
#' @importFrom shinyAce aceEditor
#' @importFrom shiny div
#' @importFrom htmltools tagList tags
#' @examples
#' \dontrun{
#' pkgload::load_all(); run_minimal_example()
#' }
exprs_ui_minimal <- function(
  id = "",
  value = ""
) {
  setup_ace_editor(id, value)
}

#' Run minimal example app
#'
#' This function launches a minimal Shiny app that demonstrates the basic
#' expression UI functionality.
#'
#' @examples
#' \dontrun{
#' pkgload::load_all(); run_minimal_example()
#' }
#' @export
run_minimal_example <- function() {
  df <- data.frame(
    Sepal.Length = rnorm(10),
    Sepal.Width = rnorm(10),
    Species = sample(c("setosa", "versicolor"), 10, replace = TRUE)
  )

  shinyApp(
    ui = bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      shinyjs::useShinyjs(),
      div(
        class = "container mt-3",
        exprs_ui_minimal(
          id = "expr",
          value = ""
        ),
        verbatimTextOutput("value")
      )
    ),
    server = function(input, output, session) {
      # Initialize ace editor
      observe({
        initialize_ace_editor(session, "expr", colnames(df))
      })

      output$value <- renderPrint({
        input$expr
      })
    }
  )
}
