context("extract_tweet_id_from_html")
test_that("extract_tweet_id_from_html works as expected", {
  result <- nrow(extract_tweet_id_from_html("sample_data/rstats_html_element.txt"))
  check <- 42 # expect 42 tweets in the smaple data provided
  expect_equal(result,check)
})
