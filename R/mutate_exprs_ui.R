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
#' @param delete_button Should a delete button be shown?
#' @param key How to display the 'key' field: "suggest", "empty", or "none"
#' @param auto_complete_list List of autocompletion options, passed to
#'   shinyAce::aceEditor()
#' @return A `div` element containing the UI components
#' @importFrom shinyAce aceEditor aceAutocomplete aceTooltip
#' @importFrom shiny icon div
#' @importFrom htmltools tagList tags
#' @export
#' @examples
#' \dontrun{
#' library(shiny)
#' library(shinyAce)
#' library(bslib)
#' shinyApp(
#'   ui = bslib::page_fluid(
#'     theme = bslib::bs_theme(version = 5),
#'     exprs_ui(
#'       id = "myid",
#'       value_name = "newcol",
#'       value_val = "x + 1",
#'       delete_button = TRUE
#'     )
#'   ),
#'   server = function(input, output, session) {
#'     shinyAce::aceAutocomplete("myid_val")
#'   }
#' )
#' }
exprs_ui <- function(id = "",
                     value_name = "newcol",
                     value_val = NULL,
                     delete_button = TRUE,
                     key = c("suggest", "empty", "none"),
                     auto_complete_list = NULL) {
  key <- match.arg(key)

  tagList(
    tags$style(".mutate-expression .shiny-ace {
      border: none;
      margin: 1rem;
    }

    .mutate-expression .mutate-column {
      width: 15%;
    }

    .mutate-expression .mutate-code {
      width: 62%;
    }

    .mutate-expression .mutate-delete:hover {
      color: var(--bs-white);
      border-color: var(--bs-danger);
      background: var(--bs-danger);
    }"),
    div(
      id = id,
      class = paste(
        "input-group d-flex justify-content-between mt-1 mb-3",
        "mutate-expression border border-dark-subtle rounded"
      ),
      if (key != "none") {
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
        )
      },
      if (key != "none") {
        div(
          class = "input-group-text mutate-equal",
          icon("equals")
        )
      },
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
      ),
      if (delete_button) {
        tags$button(
          id = paste0(id, "_rm"),
          type = "button",
          class = "btn btn-default action-button mutate-delete",
          icon("trash-can")
        )
      }
    )
  )
}
