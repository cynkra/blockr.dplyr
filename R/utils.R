#'@export
extract_metadata <- function(data, domain, extract_codelist_vars = NULL, max_unique_values = 1e6) {
  # Determine the domain label
  domain_label <- ifelse(is.null(attr(data, "label")), domain, attr(data, "label"))

  # Extract variable names, labels, and types
  var_names <- names(data)
  var_labels <- sapply(data, function(col) attr(col, "label"))
  var_types <- sapply(data, class)
  all_vars <- names(data)

  # Default max unique values if not provided
  if (is.null(max_unique_values)) max_unique_values <- 1e6

  # Extract codelist variables based on provided patterns
  if (!is.null(extract_codelist_vars)) {
    positive_patterns <- extract_codelist_vars[!grepl("^-", extract_codelist_vars)]
    negative_patterns <- gsub("^-", "", extract_codelist_vars[grepl("^-", extract_codelist_vars)])

    selected_vars <- all_vars

    if (length(positive_patterns) > 0) {
      selected_vars <- unique(unlist(lapply(positive_patterns, function(pat) grep(pat, all_vars, value = TRUE))))
    }

    exclude_vars <- unique(unlist(lapply(negative_patterns, function(pat) grep(pat, all_vars, value = TRUE))))
    selected_vars <- setdiff(selected_vars, exclude_vars)
  } else {
    selected_vars <- all_vars
  }

  # Generate metadata for each variable
  variable_list <- lapply(seq_along(var_names), function(i) {
    var_meta <- list(
      name = var_names[i],
      label = var_labels[i],
      type = var_types[i]
    )

    if (var_names[i] %in% selected_vars) {
      if (is.character(data[[i]]) && (is.null(max_unique_values) || length(unique(data[[i]])) <= max_unique_values)) {
        var_meta$unique_values <- unique(data[[i]])
      } else if (is.factor(data[[i]])) {
        var_meta$levels <- levels(data[[i]])
      }
    }

    return(var_meta)
  })

  # Create the metadata entry
  metadata_entry <- list(
    description = domain_label,
    variables = variable_list
  )

  return(metadata_entry)
}

#'@export
make_metadata <- function(
    reactive_datasets,
    extract_codelist_vars = c("-.*DTC$", "-STUDYID", "-USUBJID", "-DOMAIN", "-SUBJID", "-SITEID", "-COUNTRY", "-.*ID$", "-.*NAM$"),
    max_unique_values = 130,
    tips = "") {
  metadata_list <- list()

  for (domain in names(reactive_datasets)) {
    # Read the data
    data <- reactive_datasets[[domain]]
    # Generate metadata
    metadata_list[[domain]] <- extract_metadata(data, domain, extract_codelist_vars, max_unique_values)
  }

  study_metadata <- list(
    context = paste("Treatment group information and population flags (sometimes called sets) are on DM and must be merged. Variables that end with FL are flag variables and are 'Y' when true. Visits should be displayed using VISIT, but ordered by VISITNUM. Unscheduled VISITs start with 'UNSCHEDULED'. ", tips),
    datasets = metadata_list
  )

  return(study_metadata)
}

#' Extract R code blocks from text using stringr
#' @param text A character string containing R code blocks between r code tags
#' @return A character vector containing the extracted code blocks
#' @importFrom stringr str_match_all str_trim str_replace_all regex
#' @export
extract_r_code <- function(text) {
  # Clean up any R/r variations
  text <- gsub("`R", "`r", text)
  
  # Try different patterns for code blocks
  patterns <- c(
    "```\\{?r\\}?\\s*(.*?)```",  # Standard R markdown
    "```r\\s*(.*?)```",          # Simple R code block
    "`r\\s*(.*?)`"               # Inline R code
  )
  
  for (pattern in patterns) {
    matches <- stringr::str_match_all(
      string = text,
      pattern = stringr::regex(pattern, dotall = TRUE)
    )[[1]]
    
    if (length(matches) > 0 && nrow(matches) > 0) {
      # Extract and clean the captured code
      code <- paste(matches[, 2], collapse = "\n")
      code <- stringr::str_trim(code)
      if (nchar(code) > 0) return(code)
    }
  }
  
  # If no matches found or all empty, return empty character vector
  return(character())
}
