#' Mutate block constructor
#'
#' This block allows to add new variables and preserve existing ones
#' (see [dplyr::mutate()]).
#'
#' @param r_strings Reactive expression returning character vector of
#'   expressions
#' @param ... Additional arguments forwarded to [new_block()]
#'
#' @return A block object for mutate operations
#' @importFrom shiny req showNotification NS moduleServer reactive
#' @importFrom glue glue
#' @seealso [new_transform_block()]
#' @examples
#' \dontrun{
#' serve(new_mutate_block(), list(data = mtcars))
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

          r_ans <- mod_keyvalue_server(
            id = "kv",
            get_value = \() c(newcol = 'paste("my", "expression")'),
            get_cols = \() colnames(data())
          )

          r_expr <- reactive({
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
            expr
          })

          list(
            expr = r_expr,
            state = list(
              r_strings = r_ans
            )
          )
        }
      )
    },
    function(ns) {
      mod_keyvalue_ui(
        value = list(newcol = ""),
        multiple = FALSE,
        submit = TRUE,
        key = "suggest",
        auto_complete_list = list(data = character()),
        ns = NS(ns("expression", "kv"))
      )
    },
    class = "mutate_block",
    ...
  )
}
# serve(new_mutate_block(), list(data = mtcars))
