#' Mutate key-value server module
#'
#' A Shiny module that manages a key-value pair for mutate expressions.
#' Handles user input, autocompletion, and value updates.
#'
#' @param id The module ID
#' @param get_value Function that returns initial value
#' @param get_cols Function that returns column names for autocompletion
#'
#' @return A reactive expression containing the current key-value pair
#' @importFrom shiny req showNotification NS moduleServer reactive
#' @importFrom glue glue
#' @seealso [new_transform_block()]
#' @examples
#' \dontrun{
#' serve(new_mutate_block(), list(data = mtcars))
#' }
#' @export
mod_kvexpr_server <- function(
  id,
  get_value,
  get_cols
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize reactive values
    r_value <- reactiveVal({
      get_value()
    })
    r_cols <- reactive({
      get_cols()
    })

    # Update autocomplete list when columns change
    observe({
      shinyAce::updateAceEditor(
        session,
        editorId = "pl_val",
        autoCompleteList = list(data = r_cols())
      )
    })

    # Update editor values for non-user changes
    observe({
      val <- r_value()
      shinyAce::updateAceEditor(
        session,
        editorId = "pl_val",
        value = unname(val)
      )
      shinyAce::updateAceEditor(
        session,
        editorId = "pl_name",
        value = names(val)
      )
    })

    # Handle user input
    observe({
      name <- input$pl_name
      val <- input$pl_val

      if (!is.null(name) && !is.null(val)) {
        r_value(setNames(val, name))
      }
    })

    # Return reactive value
    reactive(r_value())
  })
}

#' Create key-value UI module
#'
#' @param id The module ID
#' @param value_name Initial name value
#' @param value_val Initial value
#' @param auto_complete_list List of autocomplete options
#' @return A div containing the UI elements
#' @export
mod_kvexpr_ui <- function(id) {
  ns <- NS(id)

  div(
    kvexpr_ui(
      ns("pl")
    )
  )
}

#' Run example app demonstrating key-value functionality
#'
#' This function launches a Shiny app that demonstrates the key-value module
#' functionality with a simple example.
#'
#' @examples
#' \dontrun{
#' pkgload::load_all();run_kvexpr_example()
#' }
#' @export
run_kvexpr_example <- function() {
  shinyApp(
    ui = bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      mod_kvexpr_ui("kv"),
      verbatimTextOutput("value")
    ),
    server = function(input, output, session) {
      r_ans <- mod_kvexpr_server(
        "kv",
        get_value = function() c(newcol = "x + 1"),
        get_cols = function() c("x", "y", "z")
      )

      output$value <- renderPrint({
        val <- r_ans()
        cat(names(val), "\n")
        cat(sprintf('"%s"', unname(val)))
      })
    }
  )
}
