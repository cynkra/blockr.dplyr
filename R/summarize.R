#' Summarize block constructor
#'
#' This block allows for grouping data and calculating summary statistics (see
#' [dplyr::summarise()]).
#'
#' @param group_cols Columns to group by
#' @param summary_cols Numeric columns to summarize
#' @param stats Summary statistics to calculate
#' @param ... Forwarded to [new_block()]
#'
#' @export
new_summarize_block <- function(group_cols = character(), 
                              summary_cols = character(),
                              stats = character(),
                              ...) {
  
  # Define available statistics with their expressions and labels
  stat_info <- list(
    n = list(
      expr = "dplyr::n()",
      label = "Count (n)"
    ),
    mean = list(
      expr = "mean(%col%, na.rm = TRUE)",
      label = "Mean"
    ),
    median = list(
      expr = "median(%col%, na.rm = TRUE)",
      label = "Median"
    ),
    sd = list(
      expr = "sd(%col%, na.rm = TRUE)",
      label = "Standard deviation"
    ),
    min = list(
      expr = "min(%col%, na.rm = TRUE)",
      label = "Minimum"
    ),
    max = list(
      expr = "max(%col%, na.rm = TRUE)",
      label = "Maximum"
    )
  )
  
  # Create named vectors for expressions and labels
  stat_exprs <- sapply(stat_info, `[[`, "expr")
  stat_labels <- sapply(stat_info, `[[`, "label")
  
  new_transform_block(
    function(data) {
      moduleServer(
        "expression",
        function(input, output, session) {
          group_sels <- reactiveVal(group_cols)
          summary_sels <- reactiveVal(summary_cols)
          stat_sels <- reactiveVal(stats)
          
          # Get all columns and numeric columns
          available_cols <- reactive(colnames(data()))
          
          # Get available summary columns (numeric and not in group_by)
          available_summary_cols <- reactive({
            cols <- colnames(data())
            numeric_cols <- cols[sapply(data(), is.numeric)]
            setdiff(numeric_cols, group_sels())
          })
          
          # Update group by columns
          observeEvent(input$group_cols, {
            old_groups <- group_sels()
            new_groups <- input$group_cols
            
            # Update group selections
            group_sels(new_groups)
            
            # Remove any newly grouped columns from summary selections
            current_summaries <- summary_sels()
            summary_sels(setdiff(current_summaries, new_groups))
          })
          
          # Update summary columns and stats
          observeEvent(input$summary_cols, {
            summary_sels(input$summary_cols)
          })
          
          observeEvent(input$stats, {
            stat_sels(names(stat_labels)[match(input$stats, stat_labels)])
          }, ignoreInit = TRUE)
          
          # Keep UI in sync
          observe({
            updateSelectInput(
              session,
              inputId = "group_cols",
              choices = available_cols(),
              selected = group_sels()
            )
            updateSelectInput(
              session,
              inputId = "summary_cols",
              choices = available_summary_cols(),
              selected = intersect(summary_sels(), available_summary_cols())
            )
          })
          
          # Separate observer for stats to avoid circular updates
          observe({
            updateCheckboxGroupInput(
              session,
              inputId = "stats",
              choices = stat_labels,
              selected = stat_labels[stat_sels()]
            )
          })
          
          list(
            expr = reactive({
              # If no selections, return data unchanged
              if (length(summary_sels()) == 0 || length(stat_sels()) == 0) {
                return(quote(data))
              }
              
              # Build group_by expression if needed
              group_expr <- if (length(group_sels()) > 0) {
                bquote(
                  dplyr::group_by(data, !!!.(lapply(group_sels(), as.symbol)))
                )
              } else {
                quote(data)
              }
              
              # Build summarize arguments
              summary_args <- list()
              for (col in summary_sels()) {
                for (stat in stat_sels()) {
                  # Create new column name
                  new_col <- paste(col, stat, sep = "_")
                  # Get stat expression and replace %col% placeholder
                  stat_expr <- gsub("%col%", col, stat_exprs[stat])
                  # Add to summary arguments
                  summary_args[[new_col]] <- parse(text = stat_expr)[[1]]
                }
              }
              
              # Combine into final expression
              bquote({
                .(group_expr) %>%
                  dplyr::summarise(!!!.(summary_args)) %>%
                  dplyr::ungroup()
              })
            }),
            state = list(
              group_cols = reactive(group_sels()),
              summary_cols = reactive(summary_sels()),
              stats = reactive(stat_sels()),
              cols_opts = reactive(available_cols())
            )
          )
        }
      )
    },
    function(ns, group_cols, summary_cols, stats, cols_opts = character()) {
      div(
        class = "summarize-block",
        tags$style(HTML("
          .summarize-block .section {
            background-color: #f8f9fa;
            padding: 10px;
            border-radius: 5px;
            margin-bottom: 10px;
          }
          .summarize-block .section-title {
            font-weight: bold;
            margin-bottom: 5px;
            color: #495057;
          }
          .summarize-block .help-text {
            font-size: 0.9em;
            color: #6c757d;
            margin-bottom: 8px;
          }
        ")),
        
        # Group By Section
        div(
          class = "section",
          div(class = "section-title", "1. Group By"),
          div(
            class = "help-text", 
            "Select columns to group the data by (optional)"
          ),
          selectInput(
            inputId = ns("expression", "group_cols"),
            label = NULL,
            choices = cols_opts,
            selected = group_cols,
            multiple = TRUE,
            width = "100%"
          )
        ),
        
        # Summary Columns Section
        div(
          class = "section",
          div(class = "section-title", "2. Select Numeric Columns"),
          div(
            class = "help-text", 
            "Choose numeric columns to calculate statistics for"
          ),
          selectInput(
            inputId = ns("expression", "summary_cols"),
            label = NULL,
            choices = cols_opts,
            selected = summary_cols,
            multiple = TRUE,
            width = "100%"
          )
        ),
        
        # Statistics Section
        div(
          class = "section",
          div(class = "section-title", "3. Choose Statistics"),
          div(
            class = "help-text", 
            "Select which statistics to calculate"
          ),
          checkboxGroupInput(
            inputId = ns("expression", "stats"),
            label = NULL,
            choices = stat_labels,
            selected = stats,
            inline = TRUE
          )
        )
      )
    },
    class = "summarize_block",
    ...
  )
} 