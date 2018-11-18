#' Extract Tweet IDs From HTML Body
#'
#' This function is used to scrape tweets from Twitter's front end. The process to follow is as follows:
#' 1. In a web browser, search desired query on Twitter (e.g. https://twitter.com/search?f=tweets&vertical=default&q=rstats%20since%3A2017-10-01%20until%3A2017-11-29&src=typd&lang=en)
#' 2. Click the latest tab, then scroll down to the bottom using Page Down key.
#' 3. Right click the web page and select Inspect / Inspect Elements (command+shift+c in Chrome or F12 in firefox)
#' 4. Under elements (inspector in firefox), choose the top body element.
#' 5. Copy the body element (copy element in chrome, copy- inner HTML in firefox)
#' 6. Create an empty document, paste the copied body element and save as .txt file.
#'
#' Once above steps are completed, read in the .txt file into R and then extract Tweet IDs using this function.
#'
#' @param html_body Path of the a text document which is the HTML body class copy-pasted from web browser.
#'
#' @return Returns a single column tibble which includes distinct tweet IDs scraped from from `html_body`.
#' @export
#'
#' @examples
#'
#' # Extract tweet IDs from html element.
#' # extract_tweet_id_from_html("/tests/testthat/sample_data/rstats_html_element.txt")
#'
extract_tweet_id_from_html <- function(html_body) {
  if(str_detect(html_body, "$.txt")) {
    stop("Input is not a .txt file", call. = FALSE)
  }
  readLines(html_body) %>%
    as.character() %>%
    stringr::str_match_all(pattern = "data-tweet-id=\"(.*?)\"") %>%
    reshape2::melt() %>%
    reshape2::dcast( formula = value~Var2, drop = T) %>%
    tibble::as.tibble() %>%
    dplyr::select(tweet_id=`2`) %>%
    tidyr::drop_na() %>%
    dplyr::distinct(tweet_id)
}
