#' Select block constructor
#'
#' This block allows to perform column subsetting on `data.frame` objects (see
#' [dplyr::select()]).
#'
#' @param columns Selected columns
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_select_block <- function(columns = character(), ...) {

  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {

          sels <- reactiveVal(columns)
          cols <- reactive(colnames(data()))

          observeEvent(
            input$columns,
            sels(intersect(input$columns, cols()))
          )

          observe(
            {
              updateSelectInput(
                session,
                inputId = "columns",
                choices = cols(),
                selected = sels()
              )
            }
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
              columns = sels
            )
          )
        }
      )
    },
    function(id) {
      selectInput(
        inputId = NS(id, "columns"),
        label = "Columns",
        choices = list(),
        multiple = TRUE
      )
    },
    class = "select_block",
    ...
  )
}
