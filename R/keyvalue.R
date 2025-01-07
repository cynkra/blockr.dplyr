#' Key Value Field server
#'
#' Server module
#'
mod_keyvalue_server <- function(id, submit = TRUE, multiple = TRUE, key = "suggest") {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # submit <- isTRUE(x$submit)
      # multiple <- isTRUE(x$multiple)
      # key <- x$key

      r_value <- reactiveVal(c(newcol = ""))

      r_n_max <- reactiveVal(0L)
      # dynamically add aceAutocomplete, aceTooltip for each new row
      observe({
        n <- length(r_value())
        if (n > r_n_max()) {
          add <- (r_n_max() + 1):n
          for (i in add) {
            aceAutocomplete(paste0("pl_", i, "_val"))
            aceTooltip(paste0("pl_", i, "_val"))
          }
          r_n_max(n)
        }
      }) |>
        bindEvent(r_value())

      # using reactiveVal(), instead of reactive, reduces the number of updates
      r_value_user <- reactiveVal(NULL)
      observe({
        ans <- get_exprs("pl_", input)
        value <- isolate(r_value())

        # previously used ids are not removed from dom
        if (length(ans) > length(value)) {
          idx <- seq_along(value)
          if (length(idx) > 0) {
            ans <- ans[idx]
          }
        }
        r_value_user(ans)
      })



      # by user input
      observe({
        req(r_value_user())
        r_value(r_value_user())
      }) |>
        bindEvent(r_value_user(), ignoreInit = TRUE)

      # by add button
      observe({
        message("add")
        r_value(c(r_value(), newcol = ""))
      }) |>
        bindEvent(input$i_add)

      # by remove button
      r_to_be_removed <- reactive({
        rms <- get_rms("pl_", input)
        to_be_rm <- names(rms[rms > 0])
        if (identical(length(to_be_rm), 0L)) {
          return()
        }
        ans <- as.integer(gsub("_rm$", "", gsub("^pl_", "", to_be_rm)))
        ans
      })
      observe({
        req(r_to_be_removed())
        # keep one expression
        if (length(r_value()) <= 1) {
          return()
        }
        r_value(r_value()[-r_to_be_removed()])
      }) |>
        bindEvent(r_to_be_removed())

      r_auto_complete_list <- reactive({
        user_cols <- names(r_value())
        unique(c(user_cols))
      })

      observe({
        if (!identical(r_value(), r_value_user())) {
          output$kv <- renderUI({
            message("renderUI")
            # isolate here is needed, despite bindEvent(), for some reason
            mod_keyvalue_ui(
              value = isolate(r_value()),
              multiple = multiple,
              submit = submit,
              key = key,
              auto_complete_list = list(column = isolate(r_auto_complete_list())),
              ns = ns
            )
          })
        }
      }) |>
        bindEvent(r_value())

      if (submit) {
        r_result <- reactive({
          r_value()
        }) |>
          bindEvent(input$i_submit)
      } else {
        r_result <- reactive({
          r_value()
        })
      }

      r_result
    })
}

get_input_names <- function(prefix, input, regex) {
  input_names <- grep(paste0("^", prefix), names(input), value = TRUE)
  grep(regex, input_names, value = TRUE)
}

get_rms <- function(prefix, input) {
  input_names <- get_input_names(prefix, input, "_rm$")
  vapply(setNames(input_names, input_names), \(x) input[[x]], 0L)
}

get_exprs <- function(prefix, input) {
  input_names <- get_input_names(prefix, input, "_name$|_val$")
  ans <- lapply(setNames(input_names, input_names), \(x) input[[x]])
  vals <- unlist(ans[grepl("_val$", names(ans))])
  names <- unlist(ans[grepl("_name$", names(ans))])
  setNames(vals, names) # a named character vector
}



mod_keyvalue_ui <- function(value,
                        multiple,
                        submit,
                        key,
                        auto_complete_list = NULL,
                        ns = function(x) x) {
  names <- names(value)
  values <- unname(value)
  ids <- ns(paste0("pl_", seq(value)))

  core_ui <- tagList(Map(
    function(name, value, id) {
      exprs_ui(
        id,
        value_name = name,
        value_val = value,
        delete_button = multiple,
        key = key,
        auto_complete_list = auto_complete_list
      )
    },
    name = names,
    value = values,
    id = ids
  ))

  div(
    div(
      id = ns("pls"),
      core_ui
    ),
    div(
      class = "w-100 d-flex justify-content-end",
      div(
        class = "m-0 mb-5",
        if (multiple) {
          actionButton(
            ns("i_add"),
            label = NULL,
            icon = icon("plus"),
            class = "btn btn-success"
          )
        },
        if (submit) {
          actionButton(
            ns("i_submit"),
            label = "Submit",
            icon = icon("paper-plane"),
            class = "btn btn-primary"
          )
        }
      )
    )
  )
}



library(shiny)
library(shinyAce)
shinyApp(
  ui = bslib::page_fluid(
    theme = bslib::bs_theme(version = 5),  # Activate Bootstrap 5
    mod_keyvalue_ui(
      value = list(newcol = "x + 1"),
      multiple = FALSE,
      submit = TRUE,
      key = "suggest",
      ns = NS("kv")
    )
  ),
  server = function(input, output, session) {
    # riris <- reactive({ input$txt })

    r_ans <- mod_keyvalue_server(
      "kv"
    )

    observe({
      print(r_ans())
    })
  }
)
