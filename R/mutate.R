

#' Select block constructor
#'
#' This block allows to perform column subsetting on `data.frame` objects (see
#' [dplyr::select()]).
#'
#' @param columns Selected columns
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_mutate_block <- function(columns = character(), ...) {

  new_transform_block(
    function(data) {


      moduleServer(
        "expression",
        function(input, output, session) {

          sels <- reactiveVal(columns)
          cols <- reactive(colnames(data()))

          observeEvent(input$columns, sels(input$columns))

          observe(
            updateSelectInput(
              session,
              inputId = "columns",
              choices = cols(),
              selected = sels()
            )
          )

          list(
            expr = reactive(
              bquote(
                dplyr::select(data, ..(cols)),
                list(cols = lapply(sels(), as.name)),
                splice = TRUE
              )
            ),
            state = list(
              columns = reactive(sels()),
              choices = reactive(cols())
            )
          )
        }
      )
    },
    function(ns, value = list(newcol = "x + 1", another_col = "mean(y)")) {

      keyvalue_ui(
        value = value,
        multiple = TRUE,
        submit = TRUE,
        key = "suggest",
        ns = ns
      )
    },
    class = "mutate_block",
    ...
  )
}
# serve(new_mutate_block())