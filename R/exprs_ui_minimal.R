#' Create a minimal UI element for a single expression
#'
#' @param id Character string, an identifier for the UI element
#' @param value Default value for the expression
#' @return A div element containing the UI components
#' @importFrom shinyAce aceEditor
#' @importFrom shiny div
#' @importFrom htmltools tagList tags
#' @examples
#' \dontrun{
#' pkgload::load_all(); run_minimal_example()
#' }
exprs_ui_minimal <- function(
  id = "",
  value = ""
) {
  # Custom completer script
  custom_completer <- sprintf('
    var customCompleter = {
      getCompletions: function(editor, session, pos, prefix, callback) {
        // Define categories and their functions
        var categories = {
          arithmetic: ["abs", "sign", "ceiling", "floor", "round", "trunc",
                      "log", "log2", "log10", "exp", "sqrt"],
          aggregate: ["mean", "sum", "min", "max"],
          offset: ["lead", "lag", "cumsum", "cumprod", "cummin", "cummax"],
          logical: ["if_else", "case_when"],
          string: ["str_c", "paste", "paste0", "str_sub", "str_to_lower", "str_to_upper"],
          ranking: ["row_number", "min_rank", "dense_rank", "percent_rank", "ntile"]
        };

        var wordList = [];
        Object.keys(categories).forEach(function(category) {
          categories[category].forEach(function(fn) {
            wordList.push({
              caption: fn,
              snippet: fn + "()",
              value: fn,
              score: 1000,
              meta: category
            });
          });
        });

        // Sort by category and then by function name
        wordList.sort(function(a, b) {
          if (a.meta === b.meta) {
            return a.caption.localeCompare(b.caption);
          }
          return a.meta.localeCompare(b.meta);
        });
        callback(null, wordList);
      }
    };

    // Add custom key handler for tab
    document.addEventListener("DOMContentLoaded", function() {
      setTimeout(function() {
        var editor = ace.edit("expr");
        editor.commands.on("afterExec", function(e) {
          if (e.command.name == "insertstring" || e.command.name == "insertMatch") {
            var pos = editor.getCursorPosition();
            var line = editor.session.getLine(pos.row);
            var prefix = line.substring(0, pos.column);
            if (prefix.match(/\\w+\\($/)) {
              editor.moveCursorTo(pos.row, pos.column - 1);
            }
          }
        });
      }, 100);
    });

    ace.require("ace/ext/language_tools").addCompleter(customCompleter);
  ')

  tagList(
    tags$style(".mutate-expression .shiny-ace {
      border: none;
      margin: 7px;
      margin-bottom: 7.5px;   // to align with select box
    }

    .mutate-expression .mutate-code {
      width: 100%;
    }"),
    # Initialize completer immediately after Ace is loaded
    tags$script(HTML(custom_completer)),
    div(
      class = paste(
        "input-group mb-3",
        "mutate-expression border border-dark-subtle rounded"
      ),
      div(
        class = "mutate-code",
        shinyAce::aceEditor(
          outputId = id,
          value = value,
          mode = "r",
          autoComplete = "live",
          autoCompleters = c("custom"),
          height = "20px",
          showPrintMargin = FALSE,
          highlightActiveLine = FALSE,
          tabSize = 2,
          theme = "tomorrow",
          maxLines = 1,
          fontSize = 14,
          showLineNumbers = FALSE
        )
      )
    )
  )
}

#' Run minimal example app
#'
#' This function launches a minimal Shiny app that demonstrates the basic
#' expression UI functionality.
#'
#' @examples
#' \dontrun{
#' pkgload::load_all(); run_minimal_example()
#' }
#' @export
run_minimal_example <- function() {
  df <- data.frame(x = 1:10, y = 11:20, z = 21:30)

  shinyApp(
    ui = bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      shinyjs::useShinyjs(),
      div(
        class = "container mt-3",
        exprs_ui_minimal(
          id = "expr",
          value = ""
        ),
        verbatimTextOutput("value")
      )
    ),
    server = function(input, output, session) {
      # Initialize ace editor
      observe({
        shinyjs::runjs("
          if (typeof ace !== 'undefined') {
            var editor = ace.edit('expr');
            editor.setOptions({
              enableLiveAutocompletion: true,
              enableBasicAutocompletion: true
            });
          }
        ")
      })

      output$value <- renderPrint({
        input$expr
      })
    }
  )
}
