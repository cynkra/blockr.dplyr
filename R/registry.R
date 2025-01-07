register_dplyr_blocks <- function() {
  register_blocks(
    c(
      "new_select_block",
      "new_join_block"
    ),
    name = c(
      "select block",
      "join block"
    ),
    description = c(
      "Subset columns in a data.frame",
      "Join together two data.frames"
    ),
    category = c(
      "transform",
      "transform"
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
