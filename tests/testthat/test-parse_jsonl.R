context("parse_jsonl")
test_that("parse_jsonl works as expected", {
result <- {test_parse <- parse_jsonl("sample_data/test_jsonl_file.jsonl")
c(nrow(test_parse),ncol(test_parse))}
check <- c(520,99)
expect_equal(result,check)
})
