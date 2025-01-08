#' Arrange block constructor
#'
#' This block allows for arranging data by columns (see
#' [dplyr::arrange()]).
#'
#' @param cols Columns to arrange by
#' @param desc Logical vector indicating which columns should be sorted in descending order
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_arrange_block <- function(cols = character(), desc = logical(), ...) {
  
  new_transform_block(
    function(data) {
      moduleServer(
        "expression",
        function(input, output, session) {
          sels <- reactiveVal(cols)
          desc_cols <- reactiveVal(character())
          available_cols <- reactive(colnames(data()))
          
          observeEvent(input$cols, {
            sels(input$cols)
            updateCheckboxGroupInput(
              session,
              inputId = "desc",
              choices = input$cols
            )
          })
          
          observeEvent(input$desc, desc_cols(input$desc))
          
          observe({
            updateSelectInput(
              session,
              inputId = "cols",
              choices = available_cols(),
              selected = sels()
            )
          })
          
          list(
            expr = reactive({
              selected_cols <- sels()
              if (length(selected_cols) == 0) return(quote(data))
              
              arrange_args <- lapply(selected_cols, function(col) {
                if (col %in% desc_cols()) {
                  bquote(dplyr::desc(.(as.symbol(col))))
                } else {
                  as.symbol(col)
                }
              })
              
              bquote(dplyr::arrange(data, !!!.(arrange_args)))
            }),
            state = list(
              cols = reactive(sels()),
              desc = reactive(desc_cols()),
              cols_opts = reactive(available_cols())
            )
          )
        }
      )
    },
    function(ns, cols, desc, cols_opts = character()) {
      tagList(
        selectInput(
          inputId = ns("expression", "cols"),
          label = "Arrange by columns",
          choices = cols_opts,
          selected = cols,
          multiple = TRUE
        ),
        checkboxGroupInput(
          inputId = ns("expression", "desc"),
          label = "Descending order",
          choices = cols,
          selected = if(length(desc) && any(desc)) cols[desc] else character()
        )
      )
    },
    class = "arrange_block",
    ...
  )
} 