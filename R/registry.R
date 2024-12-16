register_dplyr_blocks <- function() {
  register_blocks(
    constructor = c(
      new_select_block,
      new_join_block,
      new_arrange_block
    ),
    name = c(
      "select block",
      "join block",
      "arrange block"
    ),
    description = c(
      "Subset columns in a data.frame",
      "Join together two data.frames",
      "Order to the rows of a data.frame"
    ),
    category = c(
      "transform",
      "transform",
      "transform"
    ),
    overwrite = TRUE
  )
}
