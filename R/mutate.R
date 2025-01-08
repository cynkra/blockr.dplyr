#' Mutate block constructor
#'
#' This block allows to add new variables and preserve existing ones
#' (see [dplyr::mutate()]).
#'
#' @param strings Character vector of expressions
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_mutate_block <- function(strings = character(), ...) {

  new_transform_block(
    function(data) {
      moduleServer(
        "expression",
        function(input, output, session) {

          r_strings <-  mod_keyvalue_server(id = "kv")

          r_choices <- reactive({
            colnames(data())
          })





          r_expr <- reactive({
            strings <- r_strings()
            req(strings)
            stopifnot(is.character(strings), !is.null(names(strings)))

            mutate_string <- glue::glue("{names(strings)} = {unname(r_strings())}")

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
              strings = r_strings,
              choices = r_choices
              # choices = r_choices
            )
          )
        }
      )
    },
    function(ns, strings = list(newcol = "1"), choices = character()) {
      mod_keyvalue_ui(
        value = strings,
        multiple = FALSE,
        submit = TRUE,
        key = "suggest",
        auto_complete_list = list(data = choices),
        ns = NS(ns("expression", "kv"))
      )
    },
    class = "mutate_block",
    ...
  )
}
# serve(new_mutate_block(), list(data = mtcars))
