#' Mutate block constructor
#'
#' This block allows to add new variables and preserve existing ones
#' (see [dplyr::mutate()]). Changes are applied after clicking the submit button.
#'
#' @param r_strings Reactive expression returning character vector of
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
new_mutate_block <- function(r_strings, ...) {
  new_transform_block(
    function(data) {
      moduleServer(
        "expression",
        function(input, output, session) {
          r_choices <- reactive({
            colnames(data())
          })

          r_ans <- mod_kvexpr_server(
            id = "kv",
            get_value = \() c(newcol = 'paste("my", "expression")'),
            get_cols = \() colnames(data())
          )

          # Store the validated expression
          r_validated <- reactiveVal()

          # Validate and update on submit
          observeEvent(input$submit, {
            strings <- r_ans()
            req(strings)
            stopifnot(is.character(strings), !is.null(names(strings)))

            mutate_string <- glue::glue("{names(strings)} = {unname(strings)}")

            expr <- parse(text = glue::glue(
              "dplyr::mutate(
                data,
                {mutate_string}
              )"
            ))[1]

            # Validation
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
            r_validated(expr)
          })

          list(
            expr = r_validated,
            state = list(
              r_strings = r_ans
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
