#' Ask block constructor
#'
#' This block allows for data transformations using LLMs
#'
#' @param prompt prompt
#'
#' @export
new_ask_block <- function(prompt = character(), ...) {
  new_transform_block(
    function(x) {
      moduleServer(
        "expression",
        function(input, output, session) {
          rv_prompt <- reactiveVal(prompt)
          rv_code <- reactiveVal()
          rv_expr <- reactiveVal()
          observeEvent(input$ok, {
            req(input$prompt)
            rv_prompt(input$prompt)
            shinyalert::shinyalert(
              text = "Processing request",
              showConfirmButton = FALSE,
              closeOnEsc = FALSE,
              size = "xs"
            )
            code <- ask_transformation(x, rv_prompt())
            shinyalert::closeAlert()
            rv_code(code)
            # this might be several 
            expr <- rlang::call2("{", !!!rlang::parse_exprs(code))
            rv_expr(expr)
          })
          
          observe(
            updateTextInput(
              session,
              inputId = "prompt",
              value = rv_prompt()
            )
          )
          
          output$code <- renderText({ req(rv_code())})
          
          list(
            expr = reactive(rv_expr()),
            state = list(
              prompt = reactive(rv_prompt())
            )
          )
        }
      )
    },
    function(ns, prompt) {
      tagList(
        textInput(
          inputId = ns("expression", "prompt"),
          label = "Prompt",
          value = prompt,
          placeholder = "describe data transformation"
        ),
        actionButton(
          inputId = ns("expression", "ok"),
          label = "OK"
        ),
        textOutput(
          outputId = ns("expression", "code")
        ),
      )
    },
    class = "ask_block",
    ...
  )
}

ask_transformation <- function(x, prompt) {
  content <- c(
    "You are a helpful R assistant.",
    "You are provided a dataset x.",
    "We expect you to provide the code to transform the object `x` described hereafter",
    "following user instructions.",
    "If a non base package is used, such as dplyr or tidyr,",
    "use the pkg::fun() notation as in dplyr::mutate().",
    "This applies to the main function call and the possible nested calls.",
    "Extremely important: Provide only code without any commentary or code chunk markers"
  )
  context <- ask::context("Output format" = content, ask::context_objects(list(x = x())))
  convo <- ask::ask(prompt, context)
  # FIXME: unexported atm
  code <- ask:::extract_last_answer(convo)
  # need to cleanup chunk markers because LLMs don't always obey instructions
  code <- clean_up_ask_output(code)
  code
}

clean_up_ask_output <- function(code) {
  if (grepl("$```", code) || grepl("\n```", code)) {
    code <- strsplit(code, "\n")[[1]]
    code <- code[cumsum(startsWith(code, "```")) == 1][-1]
    code <- paste(code, collapse = "\n")
    # message("After cleanup:")
    # message(code)
  }
  code
}
