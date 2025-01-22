test_that("filter block constructor", {
  # Test basic constructor
  blk <- new_filter_block()
  expect_s3_class(blk, c("filter_block", "transform_block", "block"))

  # Test constructor with filter condition
  blk <- new_filter_block("mpg > 20")
  expect_s3_class(blk, c("filter_block", "transform_block", "block"))

  # Test constructor with multiple conditions
  blk <- new_filter_block("mpg > 20 & cyl == 6")
  expect_s3_class(blk, c("filter_block", "transform_block", "block"))
}) 