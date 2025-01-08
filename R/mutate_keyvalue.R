#' Mutate key-value server module
#'
#' A Shiny module that manages key-value pairs for mutate expressions.
#' Handles user input, autocompletion, and value updates.
#'
#' @param id The module ID
#' @param get_value Function that returns initial values
#' @param get_cols Function that returns column names for autocompletion
#' @param submit Whether to show a submit button (defaults to TRUE)
#' @param multiple Whether multiple key-value pairs are allowed
#'   (defaults to TRUE)
#' @param key The key display mode: "suggest", "empty", or "none"
#'
#' @return A reactive expression containing the current key-value pairs
#' @importFrom shiny req showNotification NS moduleServer reactive
#' @importFrom glue glue
#' @seealso [new_transform_block()]
#' @examples
#' \dontrun{
#' serve(new_mutate_block(), list(data = mtcars))
#' }
#' @export
mod_keyvalue_server <- function(
  id,
  get_value,
  get_cols,
  submit = TRUE,
  multiple = TRUE,
  key = "suggest"
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize reactive values
    r_value <- reactiveVal({
      print(get_value())
      get_value()
    })

    r_auto_complete_list <- reactive({
      print(get_cols())
      get_cols()
    })

    # Update autocomplete list when columns change
    observe({
      if (!isTruthy(r_value_user())) {
        shinyAce::updateAceEditor(
          session,
          editorId = paste0("pl_", 1, "_val"),
          autoCompleteList = list(data = r_auto_complete_list())
        )
      }
    })

    # Update editor values for non-user changes
    observe({
      val <- r_value()
      if (!isTruthy(r_value_user())) {
        shinyAce::updateAceEditor(
          session,
          editorId = paste0("pl_", 1, "_val"),
          value = unname(val)
        )
        shinyAce::updateAceEditor(
          session,
          editorId = paste0("pl_", 1, "_name"),
          value = names(val)
        )
      }
    })

    r_n_max <- reactiveVal(0L)
    # Dynamically add aceAutocomplete and aceTooltip for new rows
    observe({
      n <- length(r_value())
      if (n > r_n_max()) {
        add <- (r_n_max() + 1):n
        for (i in add) {
          aceAutocomplete(paste0("pl_", i, "_val"))
          aceTooltip(paste0("pl_", i, "_val"))
        }
        r_n_max(n)
      }
    }) |>
      bindEvent(r_value())

    # Handle user input and DOM cleanup
    r_value_user <- reactiveVal(NULL)
    observe({
      ans <- get_exprs("pl_", input)
      value <- isolate(r_value())

      # Clean up previously used ids not removed from DOM
      if (length(ans) > length(value)) {
        idx <- seq_along(value)
        if (length(idx) > 0) {
          ans <- ans[idx]
        }
      }

      if (length(ans) > 0) {
        r_value_user(ans)
      }
    })

    # Update main value from user input
    observe({
      ans <- r_value_user()
      if (!is.null(ans)) {
        r_value(ans)
      }
    }) |>
      bindEvent(r_value_user(), ignoreInit = TRUE)

    # Return value based on submit mode
    if (submit) {
      reactive(r_value()) |> bindEvent(input$i_submit)
    } else {
      reactive(r_value())
    }
  })
}

#' Helper function to get input names matching a pattern
#'
#' @param prefix Prefix to match at start of input names
#' @param input Shiny input object
#' @param regex Regular expression to match in input names
#' @return Character vector of matching input names
get_input_names <- function(prefix, input, regex) {
  input_names <- grep(paste0("^", prefix), names(input), value = TRUE)
  grep(regex, input_names, value = TRUE)
}

#' Get remove button values
#'
#' @param prefix Prefix to match at start of input names
#' @param input Shiny input object
#' @return Named integer vector of remove button values
get_rms <- function(prefix, input) {
  input_names <- get_input_names(prefix, input, "_rm$")
  vapply(setNames(input_names, input_names), \(x) input[[x]], 0L)
}

#' Extract expressions from input values
#'
#' @param prefix Prefix to match at start of input names
#' @param input Shiny input object
#' @return Named character vector of expressions
get_exprs <- function(prefix, input) {
  input_names <- get_input_names(prefix, input, "_name$|_val$")
  ans <- lapply(setNames(input_names, input_names), \(x) input[[x]])
  vals <- unlist(ans[grepl("_val$", names(ans))])
  names <- unlist(ans[grepl("_name$", names(ans))])
  setNames(vals, names)
}

#' Create key-value UI module
#'
#' @param value Initial values
#' @param multiple Whether multiple key-value pairs are allowed
#' @param submit Whether to show submit button
#' @param key Key display mode
#' @param auto_complete_list List of autocomplete options
#' @param ns Namespace function
#' @return A div containing the UI elements
#' @export
mod_keyvalue_ui <- function(value,
                            multiple,
                            submit,
                            key,
                            auto_complete_list = NULL,
                            ns = function(x) x) {
  names <- names(value)
  values <- unname(value)
  ids <- ns(paste0("pl_", seq(value)))

  core_ui <- tagList(Map(
    function(name, value, id) {
      exprs_ui(
        id,
        value_name = name,
        value_val = value,
        delete_button = multiple,
        key = key,
        auto_complete_list = auto_complete_list
      )
    },
    name = names,
    value = values,
    id = ids
  ))

  div(
    div(
      id = ns("pls"),
      core_ui
    ),
    div(
      class = "w-100 d-flex justify-content-end",
      div(
        class = "m-0 mb-5",
        if (multiple) {
          actionButton(
            ns("i_add"),
            label = NULL,
            icon = icon("plus"),
            class = "btn btn-success"
          )
        },
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

#' Run example app demonstrating key-value functionality
#'
#' This function launches a Shiny app that demonstrates the key-value module
#' functionality with a simple example.
#'
#' @examples
#' \dontrun{
#' run_keyvalue_example()
#' }
#' @export
run_keyvalue_example <- function() {
  shinyApp(
    ui = bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      mod_keyvalue_ui(
        value = list(newcol = "x + 1"),
        multiple = FALSE,
        submit = TRUE,
        key = "suggest",
        ns = NS("kv")
      )
    ),
    server = function(input, output, session) {
      r_ans <- mod_keyvalue_server(
        "kv",
        get_value = function() list(newcol = "x + 1"),
        get_cols = function() c("x", "y", "z")
      )

      observe({
        print(r_ans())
      })
    }
  )
}
