context("parse_jsonl")
test_that("parse_jsonl works as expected", {
result <- {test_parse <- parse_jsonl("sample_data/test_jsonl_file.jsonl", filter_term_regex = "muslim|islam", tweet_lang = "en")
c(nrow(test_parse),ncol(test_parse))}
check <- c(520,99) # expect 520 rows and 99 columns
expect_equal(result,check)
})
