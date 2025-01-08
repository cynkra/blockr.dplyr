#' Filter block constructor
#'
#' This block allows for filtering data using flexfilter's interface (see
#' [flexfilter::flexfilter_server()]).
#' 
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_filter_block <- function(...) {
  
  new_transform_block(
    function(data) {
      moduleServer(
        "expression",
        function(input, output, session) {
          # Initialize flexfilter
          values <- flexfilter::flexfilter_server(
            "filter",
            data
          )
          
          list(
            expr = reactive({
              req(values())
              # If no filter expressions, return data unchanged
              if (!length(values()$exprs)) {
                return(quote(data))
              }
              
              # Create filter expression
              bquote(
                dplyr::filter(data, !!!.(values()$exprs))
              )
            }),
            state = list(
              exprs = reactive({
                req(values())
                values()$exprs
              }),
              cols_opts = reactive({
                req(data())
                colnames(data())
              })
            )
          )
        }
      )
    },
    function(ns, cols_opts = character()) {
      flexfilter::flexfilterUI(ns("expression", "filter"))
    },
    class = "filter_block",
    ...
  )
} 