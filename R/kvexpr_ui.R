#' Create a UI element for expressions
#'
#' This function generates a UI element for inputting expressions in a Shiny
#' application.
#' It includes two `shinyAce::aceEditor` elements for inputting the name and
#' value of a new column.
#'
#' @param id Character string, an identifier for the UI element
#' @param value_name Default name for the new column
#' @param value_val Default value for the new column
#' @param auto_complete_list List of autocompletion options, passed to
#'   shinyAce::aceEditor()
#' @return A `div` element containing the UI components
#' @importFrom shinyAce aceEditor aceAutocomplete aceTooltip
#' @importFrom shiny icon div
#' @importFrom htmltools tagList tags
#' @export
#' @examples
#' \dontrun{
#' pkgload::load_all(); run_kvexpr_ui_example()
#' }
kvexpr_ui <- function(id = "",
                     value_name = "newcol",
                     value_val = NULL,
                     auto_complete_list = NULL) {
  tagList(
    tags$style(".mutate-expression .shiny-ace {
      border: none;
      margin: 0.75rem;
    }

    .mutate-expression .mutate-column {
      width: 15%;
    }

    .mutate-expression .mutate-code {
      flex: 1;
      margin-left: 0;
    }

    .mutate-expression .mutate-equal {
      background-color: #e9ecef;
      border-color: #dee2e6;
      padding-left: 0.75rem;
      padding-right: 0.75rem;
    }

    .mutate-expression {
      border: 1px solid #dee2e6;
    }"),
    div(
      id = id,
      class = paste(
        "input-group d-flex justify-content-between mt-1 mb-3",
        "mutate-expression border rounded"
      ),
      div(
        class = "mutate-column",
        shinyAce::aceEditor(
          outputId = paste0(id, "_name"),
          # Update may be delayed when clicking submit
          debounce = 300,
          value = value_name,
          mode = "r",
          autoComplete = "disabled",
          height = "20px",
          showPrintMargin = FALSE,
          highlightActiveLine = FALSE,
          tabSize = 2,
          theme = "tomorrow",
          maxLines = 1,
          fontSize = 14,
          showLineNumbers = FALSE
        )
      ),
      div(
        class = "input-group-text mutate-equal",
        icon("equals")
      ),
      div(
        class = "mutate-code",
        shinyAce::aceEditor(
          outputId = paste0(id, "_val"),
          debounce = 300,
          value = value_val,
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

#' Run example app demonstrating the UI component
#'
#' This function launches a Shiny app that demonstrates the basic UI
#' functionality with a simple example.
#'
#' @examples
#' \dontrun{
#' pkgload::load_all(); run_kvexpr_ui_example()
#' }
#' @export
run_kvexpr_ui_example <- function() {
  shinyApp(
    ui = bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      kvexpr_ui(
        id = "myid",
        value_name = "newcol",
        value_val = "x + 1"
      )
    ),
    server = function(input, output, session) {
      shinyAce::aceAutocomplete("myid_val")
    }
  )
}
