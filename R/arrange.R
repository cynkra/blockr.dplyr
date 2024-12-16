#' Arrange block constructor
#'
#' This block allows allows you to order the rows of a data frame by the values
#' of selected columns (see [dplyr::arrange()]).
#'
#' @param columns Columns to arrange by.
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_arrange_block <- function(columns = character(), ...) {
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
                dplyr::arrange(data, ..(vars)),
                list(vars = lapply(sels(), as.name)),
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
    function(ns, columns, choices = character()) {
      selectInput(
        ns("expression", "columns"),
        label = "Arrange by:",
        choices = choices,
        selected = columns,
        multiple = TRUE
      )
    },
    class = "arrange_block",
    ...
  )
}
