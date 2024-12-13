test_that("select block constructor", {
  blk <- new_select_block()
  expect_s3_class(blk, c("select_block", "transform_block", "block"))
})
