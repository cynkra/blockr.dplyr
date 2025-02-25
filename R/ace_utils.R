#' Get default function categories for autocompletion
#' @noRd
get_default_categories <- function() {
  list(
    arithmetic = c("abs", "sign", "ceiling", "floor", "round", "trunc",
                  "log", "log2", "log10", "exp", "sqrt"),
    aggregate = c("mean", "sum", "min", "max"),
    offset = c("lead", "lag", "cumsum", "cumprod", "cummin", "cummax"),
    logical = c("if_else", "case_when"),
    string = c("str_c", "paste", "paste0", "str_sub", "str_to_lower", "str_to_upper"),
    ranking = c("row_number", "min_rank", "dense_rank", "percent_rank", "ntile")
  )
}

#' Setup Ace editor with custom completer
#'
#' @param id The editor ID
#' @param value Initial value (default: "")
#' @return A tagList containing the editor setup
#' @noRd
setup_ace_editor <- function(id, value = "") {
  # Custom completer script
  custom_completer <- sprintf('
    var customCompleter = {
      getCompletions: function(editor, session, pos, prefix, callback) {
        // Categories will be updated dynamically from the server
        var categories = window.aceCategories || %s;

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
      }
    };

    // Function to update categories from server
    window.updateAceCategories = function(newCategories) {
      window.aceCategories = newCategories;
    };

    // Add custom key handler for tab
    document.addEventListener("DOMContentLoaded", function() {
      setTimeout(function() {
        var editor = ace.edit("%s");
        editor.commands.on("afterExec", function(e) {
          if (e.command.name === "insertstring" || e.command.name === "Return") {
            var pos = editor.getCursorPosition();
            var line = editor.session.getLine(pos.row);
            if (line.substring(pos.column - 2, pos.column) === "()") {
              editor.moveCursorTo(pos.row, pos.column - 1);
            }
          }
        });
      }, 100);
    });

    ace.require("ace/ext/language_tools").addCompleter(customCompleter);
  ', jsonlite::toJSON(get_default_categories(), auto_unbox = TRUE), id)

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
          showLineNumbers = FALSE,
          placeholder = "type, e.g., log(col1), or paste(col1, col2)"
        )
      )
    )
  )
}

#' Initialize Ace editor in server
#'
#' @param session The Shiny session
#' @param editor_id The editor ID
#' @param column_names Vector of column names to add to completions
#' @noRd
initialize_ace_editor <- function(session, editor_id, column_names) {
  categories <- get_default_categories()
  categories$column <- column_names

  # Convert to JSON and update in JavaScript
  js <- sprintf("window.updateAceCategories(%s);", jsonlite::toJSON(categories))
  shinyjs::runjs(js)

  # Initialize editor options
  shinyjs::runjs(sprintf("
    if (typeof ace !== 'undefined') {
      var editor = ace.edit('%s');
      editor.setOptions({
        enableLiveAutocompletion: true,
        enableBasicAutocompletion: true
      });
    }
  ", editor_id))
}

#' Run example app demonstrating custom autocompletion
#'
#' This function launches a minimal Shiny app that demonstrates the custom
#' autocompletion functionality with a simple example.
#'
#' @examples
#' \dontrun{
#' pkgload::load_all(); run_ace_example()
#' }
#' @importFrom shiny NS
#' @export
run_ace_example <- function() {
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
        setup_ace_editor(NS("ace")("expr")),
        verbatimTextOutput(NS("ace")("value"))
      )
    ),
    server = function(input, output, session) {
      ns <- NS("ace")

      # Initialize ace editor with custom completions
      observe({
        initialize_ace_editor(session, ns("expr"), colnames(df))
      })

      output[[ns("value")]] <- renderPrint({
        input[[ns("expr")]]
      })
    }
  )
}
