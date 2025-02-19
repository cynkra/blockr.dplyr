#' Select block constructor
#'
#' This block allows for joining of two `data.frame` objects (see
#' [dplyr::left_join()]).
#'
#' @param type Join type
#' @param by Column(s) tp join on
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_join_block <- function(type = character(), by = character(), ...) {

  by_choices <- function(x, y) {
    intersect(colnames(x), colnames(y))
  }

  join_types <- paste0(
    c("left", "inner", "right", "full", "semi", "anti"),
    "_join"
  )

  if (length(type)) {
    type <- match.arg(type, join_types)
  } else {
    type <- join_types[1L]
  }

  new_transform_block(
    function(id, x, y) {
      moduleServer(
        id,
        function(input, output, session) {

          sels <- reactiveVal(by)
          type <- reactiveVal(type)

          observeEvent(input$by, sels(input$by))
          observeEvent(input$type, type(input$type))

          cols <- reactive(by_choices(x(), y()))

          observe(
            updateSelectInput(
              session,
              inputId = "by",
              choices = cols(),
              selected = sels()
            )
          )

          observe(
            updateSelectInput(
              session,
              inputId = "type",
              choices = join_types,
              selected = type()
            )
          )

          list(
            expr = reactive(
              bquote(
                .(func)(x, y, by = .(cols)),
                list(
                  func = eval(
                    bquote(
                      as.call(c(as.symbol("::"), quote(dplyr), quote(.(fun)))),
                      list(fun = as.name(type()))
                    )
                  ),
                  cols = sels()
                )
              )
            ),
            state = list(
              type = type,
              by = sels
            )
          )
        }
      )
    },
    function(id) {
      tagList(
        selectInput(
          inputId = NS(id, "type"),
          label = "Join type",
          choices = list()
        ),
        selectInput(
          inputId = NS(id, "by"),
          label = "By columns",
          choices = list(),
          multiple = TRUE
        )
      )
    },
    class = "join_block",
    ...
  )
}
