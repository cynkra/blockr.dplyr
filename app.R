pkgload::load_all()

blockr.core::serve(new_select_block(c("mpg", "cyl")), list(data = mtcars))

blockr.core::serve(new_arrange_block(c("mpg", "cyl")), list(data = mtcars))

blockr.core::serve(new_summarize_block(), list(data = mtcars))

blockr.core::serve(new_filter_block(), list(data = mtcars))

blockr.core::serve(new_llm_transform_block(), list(data1 = mtcars, data2 = iris))

# try as a question:
# "summarize data2 by species and provide, n, mean, min, max for all numeric variables"