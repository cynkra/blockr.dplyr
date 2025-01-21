#' Arrange block constructor
#'
#' This block allows allows you to order the rows of a data frame by the values
#' of selected columns (see [dplyr::arrange()]).
#'
#' @param columns Columns to arrange by.
#' @param desc Should columns be sorted in descending order?
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_arrange_block <- function(columns = character(), desc = FALSE, ...) {

  new_transform_block(
    function(id, data) {
      moduleServer(
        id,
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

          observe(
            updateCheckboxInput(
              session,
              inputId = "desc",
              value = desc
            )
          )

          list(
            expr = reactive({
              expr_str <- if (input$desc) {
                glue::glue('dplyr::desc({glue::glue_collapse(sels(), sep = ", ")})')
              } else {
                glue::glue_collapse(sels(), sep = ", ")
              }
              parse(text = glue::glue(
                "dplyr::arrange(data, {expr_str})"
              ))[1]
            }),
            state = list(
              columns = reactive(sels()),
              desc = reactive(input$desc)
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        selectInput(
          NS(id, "columns"),
          label = "Arrange by:",
          choices = NULL,
          multiple = TRUE
        ),
        checkboxInput(
          NS(id, "desc"),
          label = "Descending order",
          value = FALSE
        )
      )
    },
    class = "arrange_block",
    ...
  )
}
