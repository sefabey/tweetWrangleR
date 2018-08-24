context("parse_jsonl")
test_that("parse_jsonl works as expected", {
result <- parse_jsonl("sample_data/test_jsonl_file.jsonl")
check <- read.csv("sample_data/test_jsonl_file_fromGH.csv", stringsAsFactors = F)
expect_equal(result,check)
})
