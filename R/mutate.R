#' Mutate block constructor
#'
#' This block allows to perform column subsetting on `data.frame` objects (see
#' [dplyr::select()]).
#'
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_mutate_block <- function(strings, ...) {

  new_transform_block(
    function(data) {
      moduleServer(
        "expression",
        function(input, output, session) {

          r_strings <-  mod_keyvalue_server(id = "kv")

          observe({
            message("hely")
            print(r_strings())
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
              strings = r_strings
            )
          )
        }
      )
    },
    function(ns, strings = list(newcol = "x + 1")) {
      mod_keyvalue_ui(
        value = strings,
        multiple = FALSE,
        submit = TRUE,
        key = "suggest",
        ns = NS(ns("expression", "kv"))
      )
    },
    class = "mutate_block",
    ...
  )
}
# serve(new_mutate_block(), list(data = mtcars))