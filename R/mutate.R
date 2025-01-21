#' Mutate block constructor
#'
#' This block allows to add new variables and preserve existing ones
#' (see [dplyr::mutate()]). Changes are applied after clicking the submit button.
#'
#' @param string Reactive expression returning character vector of
#'   expressions
#' @param ... Additional arguments forwarded to [new_block()]
#'
#' @return A block object for mutate operations
#' @importFrom shiny req showNotification NS moduleServer reactive actionButton observeEvent icon
#' @importFrom glue glue
#' @seealso [new_transform_block()]
#' @examples
#' \dontrun{
#' # Basic usage with mtcars dataset
#' library(blockr.core)
#' serve(new_mutate_block(), list(data = mtcars))
#'
#' # With a custom dataset
#' df <- data.frame(x = 1:5, y = letters[1:5])
#' serve(new_mutate_block(), list(data = df))
#' }
#' @export
new_mutate_block <- function(string = character(), ...) {
  new_transform_block(
    function(data) {
      moduleServer(
        "expression",
        function(input, output, session) {
          r_choices <- reactive({
            colnames(data())
          })

          r_string <- mod_kvexpr_server(
            id = "kv",
            get_value = \() c(newcol = 'paste("my", "expression")'),
            get_cols = \() colnames(data())
          )

          # Store the validated expression
          r_expr_validated <- reactiveVal(parse_mutate())
          r_string_validated <- reactiveVal(string)

          # Validate and update on submit
          observeEvent(input$submit, {
            string <- r_string()

            # If empty or only whitespace, return simple mutate
            if (all(trimws(unname(string)) == "")) {
              expr <- parse_mutate(string)
              r_expr_validated(expr)
              return()
            }

            req(string)
            stopifnot(is.character(string), !is.null(names(string)))

            mutate_string <- glue::glue("{names(string)} = {unname(string)}")
            expr <- try(parse_mutate(mutate_string))
            # Validation
            if (inherits(expr, "try-error")) {
              showNotification(
                expr,
                type = "error",
                duration = 5
              )
              return()
            }
            print(expr)
            data <- data()
            ans <- try(eval(expr))
            if (inherits(ans, "try-error")) {
              showNotification(
                ans,
                type = "error",
                duration = 5
              )
              return()
            }
            r_expr_validated(expr)
            r_string_validated(r_string())
          })

          observe({
            print(r_expr_validated())
            print(r_string_validated())
          })

          list(
            expr = r_expr_validated,
            state = list(
              string = r_string_validated
            )
          )
        }
      )
    },
    function(ns) {
      div(
        mod_kvexpr_ui(ns("expression", "kv")),
        div(
          style = "text-align: right; margin-top: 10px;",
          actionButton(
            ns("expression", "submit"),
            "Submit",
            icon = icon("paper-plane"),
            class = "btn-primary"
          )
        )
      )
    },
    class = "mutate_block",
    ...
  )
}
# serve(new_mutate_block(), list(data = mtcars))

parse_mutate <- function(mutate_string = "") {
  text <- if (identical(mutate_string, "")) {
    "dplyr::mutate(data)"
  } else {
    glue::glue("dplyr::mutate(data, {mutate_string})")
  }
  parse(text = text)[1]
}
