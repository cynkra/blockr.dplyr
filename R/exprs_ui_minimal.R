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
        // Categories will be updated dynamically from the server
        var categories = window.aceCategories || {
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
            var isColumn = category === "column";
            wordList.push({
              caption: fn,
              value: fn + (isColumn ? "" : "()"),
              meta: category,
              score: isColumn ? 1001 : 1000  // Columns appear first
            });
          });
        });

        // Sort by score (columns first), then category, then name
        wordList.sort(function(a, b) {
          if (a.score !== b.score) return b.score - a.score;
          if (a.meta === b.meta) return a.caption.localeCompare(b.caption);
          return a.meta.localeCompare(b.meta);
        });
        callback(null, wordList);
      },

      insertMatch: function(editor, data) {
        if (data.isColumn) {
          editor.insert(data.value);
        } else {
          editor.insert(data.caption + "()");
          editor.navigateLeft(1);
        }
      }
    };

    // Function to update categories from server
    window.updateAceCategories = function(newCategories) {
      window.aceCategories = newCategories;
    };

    // Add custom key handler for tab
    document.addEventListener("DOMContentLoaded", function() {
      setTimeout(function() {
        var editor = ace.edit("expr");
        editor.commands.on("afterExec", function(e) {
          var pos = editor.getCursorPosition();
          var line = editor.session.getLine(pos.row);
          if (line.substring(pos.column - 2, pos.column) === "()") {
            editor.moveCursorTo(pos.row, pos.column - 1);
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
  df <- data.frame(
    Sepal.Length = rnorm(10),
    Sepal.Width = rnorm(10),
    Species = sample(c("setosa", "versicolor"), 10, replace = TRUE)
  )

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
        # Update categories with current column names
        categories <- list(
          arithmetic = c("abs", "sign", "ceiling", "floor", "round", "trunc",
                        "log", "log2", "log10", "exp", "sqrt"),
          aggregate = c("mean", "sum", "min", "max"),
          offset = c("lead", "lag", "cumsum", "cumprod", "cummin", "cummax"),
          logical = c("if_else", "case_when"),
          string = c("str_c", "paste", "paste0", "str_sub", "str_to_lower", "str_to_upper"),
          ranking = c("row_number", "min_rank", "dense_rank", "percent_rank", "ntile"),
          column = colnames(df)  # Add column names as a category
        )

        # Convert to JSON and update in JavaScript
        js <- sprintf("window.updateAceCategories(%s);", jsonlite::toJSON(categories))
        shinyjs::runjs(js)

        # Initialize editor options
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
