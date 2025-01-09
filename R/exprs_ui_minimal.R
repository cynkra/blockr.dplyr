#' Create a minimal UI element for a single expression
#'
#' @param id Character string, an identifier for the UI element
#' @param value Default value for the expression
#' @param key How to display the 'key' field: "suggest", "empty", or "none"
#' @param auto_complete_list List of autocompletion options
#' @return A div element containing the UI components
#' @importFrom shinyAce aceEditor
#' @importFrom shiny div
#' @importFrom htmltools tagList tags
#' @examples
#' \dontrun{
#' run_minimal_example()
#' }
exprs_ui_minimal <- function(
  id = "",
  value = NULL,
  key = c("suggest", "empty", "none"),
  auto_complete_list = NULL
) {
  key <- match.arg(key)

  tagList(
    tags$style(".mutate-expression .shiny-ace {
      border: none;
      margin: 1rem;
    }

    .mutate-expression .mutate-code {
      width: 62%;
    }"),
    div(
      class = "input-group d-flex justify-content-between mt-1 mb-3 mutate-expression",
      div(
        class = "mutate-code",
        shinyAce::aceEditor(
          outputId = paste0(id, "_val"),
          value = value,
          mode = "r",
          autoComplete = "live",
          autoCompleters = c("rlang", "static"),
          autoCompleteList = auto_complete_list,
          height = "20px",
          showPrintMargin = FALSE,
          highlightActiveLine = FALSE,
          tabSize = 2,
          theme = "tomorrow",
          maxLines = 1,
          fontSize = 14,
          showLineNumbers = FALSE
        )
      )
    )
  )
}

#' Run example app demonstrating minimal expression UI
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
  shinyApp(
    ui = bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      # Add CSS for consistent styling
      tags$head(
        tags$style("
          .exprs-container {
            position: relative;
          }
          .exprs-editor {
            width: 100%;
            border: 1px solid #ced4da;
            border-radius: 0.25rem;
          }
          .ace_editor {
            font-size: 1rem !important;
          }
        ")
      ),
      div(
        class = "container mt-3",
        exprs_ui_minimal(
          id = "expr",
          value = "Sepal.Length, Sepal.Width",
          auto_complete_list = list(data = colnames(iris))
        ),
        verbatimTextOutput("value")
      )
    ),
    server = function(input, output, session) {
      shinyAce::aceAutocomplete("expr_val")

      output$value <- renderPrint({
        input$expr_val
      })
    }
  )
}