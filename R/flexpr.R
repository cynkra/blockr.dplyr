#' Convert expression to selectable columns
#'
#' @param expr Expression string
#' @param cols Available column names
#' @return Character vector of valid column names or NULL if invalid format
#' @examples
#' # Valid cases (return column names)
#' df <- data.frame(x = 1:3, y = 4:6, z = 7:9)
#' expr_to_cols("x, y", colnames(df))  # c("x", "y")
#' expr_to_cols("z", colnames(df))     # "z"
#'
#' # Invalid cases (return NULL)
#' expr_to_cols("paste(x, y)", colnames(df))  # NULL (not comma-separated)
#' expr_to_cols("foo, bar", colnames(df))     # NULL (invalid columns)
#' expr_to_cols("", colnames(df))             # NULL (empty)
#' expr_to_cols(NULL, colnames(df))           # NULL
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
  get_cols_expr = get_cols,
  submit = TRUE
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize ace editor with custom completions
    observe({
      initialize_ace_editor(session, ns("expr"), get_cols_expr())
    })

    # Initialize reactive values
    r_value <- reactiveVal()
    observe({
      r_value(get_value())
    })

    # Initialize tooltips
    observe({
      shinyjs::runjs("$('[data-bs-toggle=\"tooltip\"]').tooltip()")
    })

    # Set initial mode based on value format
    observe({
      initial_mode <- !is.null(expr_to_cols(get_value(), get_cols()))
      updateCheckboxInput(session, "use_expr", value = !initial_mode)
    })

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
        selected <- expr_to_cols(r_value(), get_cols())
        if (!is.null(selected)) {
          updateSelectInput(
            session,
            "select",
            choices = get_cols(),
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
        if (!is.null(input$select) && length(input$select) > 0) {
          paste(input$select, collapse = ", ")
        } else {
          ""
        }
      }
    })

    # Update value when mode changes
    observeEvent(r_current(), {
      r_value(r_current())
    }, ignoreInit = TRUE)

    reactive(r_value())
  })
}

#' Create flexible expression UI module
#'
#' @param value Initial values
#' @param cols Available column names
#' @param submit Whether to show submit button
#' @param ns Namespace function
#'
#' @return A div containing the UI elements
#' @importFrom shiny NS actionButton icon div selectInput checkboxInput
#' @importFrom htmltools tagList tags
#' @export
mod_flexpr_ui <- function(ns = function(x) x) {
  div(
    # Add custom CSS
    tags$style("
      .input-wrapper {
        position: relative;
      }
      .mode-toggle {
        position: absolute;
        right: 10px;
        top: 50%;
        transform: translateY(-50%);
        z-index: 100;
        display: flex;
        align-items: center;
        gap: 0.5rem;
      }
      .mode-toggle .form-check-input {
        cursor: pointer;
        width: 2em;
        margin: 0;
      }
      .mode-toggle .fa-code {
        color: var(--bs-gray-600);
        font-size: 0.9em;
      }
    "),

    div(
      class = "input-wrapper",
      div(
        class = "flex-grow-1",
        style = "min-width: 0;",  # prevents flex item from overflowing
        conditionalPanel(
          condition = sprintf("input['%s'] == false", ns("use_expr")),
          div(
            class = "w-100",
            selectInput(
              ns("select"),
              label = NULL,
              choices = NULL,
              multiple = TRUE,
              selected = NULL,
              width = "100%"
            )
          )
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("use_expr")),
          div(
            class = "w-100",
            setup_ace_editor(
              ns("expr"),
              height = "20px"
            )
          )
        )
      ),
      # Toggle switch with icon
      div(
        class = "mode-toggle form-check form-switch",
        icon("code"),
        tags$input(
          class = "form-check-input",
          type = "checkbox",
          id = ns("use_expr"),
          role = "switch",
          `data-bs-toggle` = "tooltip",
          `data-bs-placement` = "left",
          title = "Toggle between selection mode (off) and expression mode (on)"
        )
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
  df <- data.frame(x = 1:10, y = 11:20, z = 21:30)

  shinyApp(
    ui = bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      shinyjs::useShinyjs(),
      div(
        class = "container mt-3",
        mod_flexpr_ui(ns = NS("flexpr")),
        verbatimTextOutput("value")
      )
    ),
    server = function(input, output, session) {
      r_ans <- mod_flexpr_server(
        "flexpr",
        get_value = function() "x, y",
        get_cols = function() colnames(df)
      )

      output$value <- renderPrint({
        r_ans()
      })
    }
  )
}

#' Run example app demonstrating custom autocompletion
#'
#' This function launches a minimal Shiny app that demonstrates the custom
#' autocompletion functionality with a simple example.
#'
#' @examples
#' \dontrun{
#' pkgload::load_all(); run_ace_example()
#' }
#' @export
run_ace_example <- function() {
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
        setup_ace_editor(
          "expr",
          height = "20px"
        ),
        verbatimTextOutput("value")
      )
    ),
    server = function(input, output, session) {
      # Initialize ace editor with custom completions
      observe({
        initialize_ace_editor(session, "expr", colnames(df))
      })

      output$value <- renderPrint({
        input$expr
      })
    }
  )
}
