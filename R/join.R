#' Select block constructor
#'
#' This block allows for joining of two `data.frame` objects (see
#' [dplyr::left_join()]).
#'
#' @param type Join type
#' @param by Column(s) tp join on
#' @param ... Forwarded to [new_block()]
#'
#' @examples
#' \dontrun{
#' serve(
#'   new_board(
#'     blocks = c(
#'       c = new_join_block(),
#'       a = new_dataset_block(),
#'       b = new_dataset_block()
#'     ),
#'     links = links(
#'       from = c("a", "b"),
#'       to = c("c", "c"),
#'       input = c("x", "y")
#'     )
#'   )
#' )
#' }
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

          r_by <- reactiveVal(by)
          r_type <- reactiveVal(type)

          observe(
            updateSelectInput(
              session,
              inputId = "type",
              choices = join_types,
              selected = type
            )
          )

          observeEvent(input$type, r_type(input$type))

          r_choices <- reactive({
            by_choices(x(), y())
          })

          r_choices_expr <- reactive({
            union(colnames(x()), colnames(y()))
          })

          # Add flexpr server
          r_by_string <- mod_flexpr_server(
            "flexpr",
            get_value = function() r_by(),
            get_choices = function() r_choices(),
            get_choices_expr = function() r_choices_expr()
          )

          r_expr <- reactive({
            by_string <- r_by_string()
            req(by_string)
            req(r_type())

            by_expr <- try(parse(text = by_string)[[1]])
            if (inherits(by_expr, "try-error")) {
              showNotification(
                by_expr,
                type = "warning",
                duration = 2,
                closeButton = FALSE
              )
              return()
            }

            expr <- bquote(
              .(func)(x, y, by = dplyr::join_by(.(by_expr))),
              list(
                func = eval(
                  bquote(
                    as.call(c(as.symbol("::"), quote(dplyr), quote(.(fun)))),
                    list(fun = as.name(r_type()))
                  )
                ),
                by_expr = by_expr
              )
            )

            x <- x()
            y <- y()
            ans <- try(eval(expr))
            if (inherits(ans, "try-error")) {
              showNotification(
                ans,
                type = "error",
                duration = 5
              )
              return()
            }

            r_by(by_string)

            expr
          })

          observe({
            print(r_expr())
          })

          list(
            expr = r_expr,
            state = list(
              type = reactive(r_type()),
              by = reactive(r_by())
            )
          )
        }
      )
    },
    function(id) {
      div(
        class = "m-3",
        selectInput(
          inputId = NS(id, "type"),
          label = "Join type",
          choices = list()
        ),
        mod_flexpr_ui(id = NS(id, "flexpr"))
      )
    },
    class = "join_block",
    ...
  )
}
