#' Filter block constructor
#'
#' This block allows filtering rows in a data frame based on conditions
#' (see [dplyr::filter()]). Changes are applied after clicking the submit button.
#'
#' @param string Reactive expression returning character vector of
#'   filter conditions
#' @param ... Additional arguments forwarded to [new_block()]
#'
#' @return A block object for filter operations
#' @importFrom shiny req showNotification NS moduleServer reactive actionButton observeEvent icon
#' @importFrom glue glue
#' @seealso [new_transform_block()]
#' @examples
#' \dontrun{
#' # Basic usage with mtcars dataset
#' library(blockr.core)
#' serve(new_filter_block(), list(data = mtcars))
#'
#' # With a custom dataset
#' df <- data.frame(x = 1:5, y = letters[1:5])
#' serve(new_filter_block(), list(data = df))
#' }
#' @export
new_filter_block <- function(string = "TRUE", ...) {
  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          r_string <- mod_kvexpr_server(
            id = "kv",
            get_value = \() string,
            get_cols = \() colnames(data())
          )

          # Store the validated expression
          r_expr_validated <- reactiveVal(NULL)
          observe({
            r_string_validated <- reactiveVal("")
            r_expr_validated(parse_filter())
          })
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

            filter_string <- glue::glue("{string}")
            expr <- try(parse_filter(string))
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
    function(id) {
      div(
        class = "m-3",
        mod_kvexpr_ui(NS(id, "kv")),
        div(
          style = "text-align: right; margin-top: 10px;",
          actionButton(
            NS(id, "submit"),
            "Submit",
            icon = icon("paper-plane"),
            class = "btn-primary"
          )
        )
      )
    },
    class = "filter_block",
    ...
  )
}

parse_filter <- function(filter_string = "") {
  print(filter_string)
  text <- if (identical(unname(filter_string), "")) {
    "dplyr::filter(data)"
  } else {
    glue::glue("dplyr::filter(data, {filter_string})")
  }
  parse(text = text)[1]
}
