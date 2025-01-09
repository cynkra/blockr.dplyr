#' Flexible expression server module
#'
#' A Shiny module that manages flexible expressions.
#'
#' @param id The module ID
#' @param get_value Function that returns initial values
#' @param get_cols Function that returns column names for autocompletion
#' @param submit Whether to show a submit button (defaults to TRUE)
#'
#' @return A reactive expression containing the current expressions
#' @importFrom shiny req showNotification NS moduleServer reactive
#' @importFrom glue glue
#' @examples
#' \dontrun{
#' serve(new_flexpr_block(), list(data = mtcars))
#' }
#' @export
mod_flexpr_server <- function(
  id,
  get_value,
  get_cols,
  submit = TRUE
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize reactive values
    r_value <- reactiveVal(get_value())
    r_auto_complete_list <- reactive(get_cols())

    # Return value based on submit mode
    if (submit) {
      reactive(r_value()) |> bindEvent(input$i_submit)
    } else {
      reactive(r_value())
    }
  })
}

#' Create flexible expression UI module
#'
#' @param value Initial values
#' @param submit Whether to show submit button
#' @param auto_complete_list List of autocomplete options
#' @param ns Namespace function
#' @return A div containing the UI elements
#' @importFrom shiny NS actionButton icon div
#' @importFrom htmltools tagList
#' @export
mod_flexpr_ui <- function(
  value,
  submit = TRUE,
  auto_complete_list = NULL,
  ns = function(x) x
) {
  div(
    div(
      id = ns("exprs"),
      exprs_ui_minimal(
        id = ns("expr"),
        value = value,
        key = "none",
        auto_complete_list = auto_complete_list
      )
    ),
    div(
      class = "w-100 d-flex justify-content-end",
      div(
        class = "m-0 mb-5",
        if (submit) {
          actionButton(
            ns("i_submit"),
            label = "Submit",
            icon = icon("paper-plane"),
            class = "btn btn-primary"
          )
        }
      )
    )
  )
}

#' Run example app demonstrating flexible expression functionality
#'
#' This function launches a Shiny app that demonstrates the flexible expression
#' module functionality with a simple example.
#'
#' @examples
#' \dontrun{
#' pkgload::load_all(); run_flexpr_example()
#' }
#' @export
run_flexpr_example <- function() {
  shinyApp(
    ui = bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      mod_flexpr_ui(
        ns = NS("flexpr")
      )
    ),
    server = function(input, output, session) {
      r_ans <- mod_flexpr_server(
        "flexpr",
        get_value = function() "Sepal.Length, Sepal.Width",
        get_cols = function() colnames(iris)
      )

      observe({
        print(r_ans())
      })
    }
  )
}
