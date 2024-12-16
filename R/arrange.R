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
new_arrange_block <- function(columns = character(), desc = "False", ...) {

  desc_opts <- c("True", "False")

  desc <- match.arg(desc, desc_opts)

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
              if (input$desc == "True") {
                bquote(
                  dplyr::arrange(data, dplyr::desc(..(vars))),
                  list(vars = lapply(sels(), as.name)),
                  splice = TRUE
                )
              } else {
                bquote(
                  dplyr::arrange(data, ..(vars)),
                  list(vars = lapply(sels(), as.name)),
                  splice = TRUE
                )
              }
            ),
            state = list(
              columns = reactive(sels()),
              choices = reactive(cols()),
              desc = reactive(input$desc)
            )
          )
        }
      )
    },
    function(ns, columns, desc, choices = character()) {
      tagList(
        selectInput(
          ns("expression", "columns"),
          label = "Arrange by:",
          choices = choices,
          selected = columns,
          multiple = TRUE
        ),
        selectInput(
          ns("expression", "desc"),
          label = "Sort in descending order?",
          choices = desc_opts,
          selected = desc
        )
      )
    },
    class = "arrange_block",
    ...
  )
}
