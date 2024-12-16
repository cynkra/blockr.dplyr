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
      server <- moduleServer(
        "arrange_expr",
        function(input, output, session) {
          arrange_by <- reactiveVal(columns)
          choices <- reactive(colnames(data()))

          arrange_by(input$columns) |>
            bindEvent(input$columns)

          observe(
            updateSelectInput(
              session,
              inputId = "columns",
              choices = choices(),
              selected = arrange_by()
            )
          )

          list(
            expr = reactive(
              bquote(
                dplyr::arrange(data, ..(vars)),
                list(vars = lapply(arrange_by(), as.name)),
                splice = TRUE
              )
            ),
            state = list(
              columns = reactive(arrange_by()),
              choices = reactive(choices())
            )
          )
        }
      )
    },
    ui = function(ns, columns, choices = character()) {
      selectInput(
        ns("arrange_expr", "dataset"),
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
