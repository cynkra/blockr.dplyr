#' Convert expression to selectable columns
#'
#' @param expr Expression string
#' @param cols Available column names
#' @return Character vector of valid column names or NULL if invalid format
#' @examples
#' # Valid cases (return column names)
#' expr_to_cols("Sepal.Length, Sepal.Width", colnames(iris))  # c("Sepal.Length", "Sepal.Width")
#' expr_to_cols("Species", colnames(iris))                    # "Species"
#'
#' # Invalid cases (return NULL)
#' expr_to_cols("paste(Sepal.Length, Sepal.Width)", colnames(iris))  # NULL (not comma-separated)
#' expr_to_cols("foo, bar", colnames(iris))                          # NULL (invalid columns)
#' expr_to_cols("", colnames(iris))                                  # NULL (empty)
#' expr_to_cols(NULL, colnames(iris))                               # NULL
#' @noRd
expr_to_cols <- function(expr, cols) {
  if (is.null(expr) || !nzchar(expr)) return(NULL)

  # Split by comma and trim whitespace
  potential_cols <- trimws(strsplit(expr, ",")[[1]])

  # Check if all are valid columns
  if (all(potential_cols %in% cols)) {
    return(potential_cols)
  }
  NULL
}

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
#' @importFrom shiny req showNotification NS moduleServer reactive updateSelectInput
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
    initial_value <- get_value()
    r_value <- reactiveVal(initial_value)
    r_cols <- reactive(get_cols())

    # Set initial mode based on value format
    initial_mode <- !is.null(expr_to_cols(initial_value, get_cols()))
    updateCheckboxInput(session, "use_expr", value = !initial_mode)

    # Update expression input when switching to expression mode
    observeEvent(input$use_expr, {
      if (isTruthy(input$use_expr)) {
        shinyAce::updateAceEditor(
          session,
          "expr",
          value = r_value()
        )
      }
    })

    # Update select input when switching to select mode
    observeEvent(input$use_expr, {
      if (!isTruthy(input$use_expr)) {
        selected <- expr_to_cols(r_value(), r_cols())
        if (!is.null(selected)) {
          updateSelectInput(
            session,
            "selected_cols",
            choices = r_cols(),
            selected = selected
          )
        }
      }
    })

    # Get current value based on mode
    r_current <- reactive({
      if (isTruthy(input$use_expr)) {
        if (!is.null(input$expr)) input$expr else ""
      } else {
        if (!is.null(input$selected_cols) && length(input$selected_cols) > 0) {
          paste(input$selected_cols, collapse = ", ")
        } else {
          ""
        }
      }
    })

    # Update value when mode changes
    observeEvent(r_current(), {
      r_value(r_current())
    }, ignoreInit = TRUE)

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
#' @importFrom shiny NS actionButton icon div selectInput checkboxInput
#' @importFrom htmltools tagList tags
#' @export
mod_flexpr_ui <- function(
  value,
  cols = c("Sepal.Length", "Sepal.Width"),
  submit = TRUE,
  ns = function(x) x
) {
  div(
    div(
      class = "mb-3",
      div(
        class = "d-flex align-items-center gap-3",
        div(
          class = "flex-grow-1",
          style = "min-width: 0;",  # prevents flex item from overflowing
          conditionalPanel(
            condition = sprintf("input['%s'] == false", ns("use_expr")),
            div(
              class = "w-100",
              selectInput(
                ns("selected_cols"),
                label = NULL,
                choices = cols,
                multiple = TRUE,
                selected = NULL,
                width = "100%"
              )
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("use_expr")),
            exprs_ui_minimal(
              id = ns("expr"),
              value = value,
              key = "none",
              auto_complete_list = list(columns = cols)
            )
          )
        ),
        div(
          class = "form-check form-switch",
          tags$input(
            class = "form-check-input",
            type = "checkbox",
            id = ns("use_expr"),
            role = "switch"
          ),
          tags$label(
            class = "form-check-label",
            `for` = ns("use_expr"),
            icon("pencil")
          )
        )
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
      div(
        class = "container mt-3",
        mod_flexpr_ui(
          value = NULL,
          cols = colnames(iris),
          ns = NS("flexpr")
        ),
        verbatimTextOutput("value")
      )
    ),
    server = function(input, output, session) {
      r_ans <- mod_flexpr_server(
        "flexpr",
        get_value = function() "paste(Sepal.Length, Sepal.Width)",
        get_cols = function() colnames(iris)
      )

      output$value <- renderPrint({
        r_ans()
      })
    }
  )
}
