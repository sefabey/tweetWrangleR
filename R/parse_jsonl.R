#' Parse .jsonl files into a tibble
#'
#' Takes a .jsonl file, CONTINUE THE DOCUMENT
#'
#' @param jsonl_path Character. The path to the .jsonl file. It's a good idea to use the output of list.files() as an input.
#' @param tweet_colnames Character vector. The colnames you want in the output tibble. In the object tweetWrangleR::tweet_cols, 99 opinionated colnames provided as default columns. However, customisation is possible by providing a user defined column names vectors.
#' @param tweet_lang Character. Accepts regex. Not case sensitive. Filter tweets from selected languages. Default returns tweets from all languages. For English language tweets only, use "en". For multiple languages use regex or (e.g. "en|es|ar"). Users can use ISO 639-1 alpha-2 ('en'), ISO 639-3 alpha-3 ('msa'), or ISO 639-1 alpha-2 combined with an ISO 3166-1 alpha-2 localization ('zh-tw') formats.
#' @param filter_term_regex Character. Accepts regex. Not case sensitive. Default returns all tweets. You can pass the keyword(s) you've used quotation marks while filtering data. If you want to provide multiple keywords, put them between speech marks and separate by '|' (which means 'or').
#' @param export_as_csv Logical. If **TRUE**, it will export parsed data as a csv file within the same path as the .jsonl file. If **FALSE** (default), it will return a tibble.
#'
#' @return Always returns a tibble with the columns provided in the tweet_colnames argument. Default alwayns returns a tibble with 99 columns. If there are values that are missing in the source .jsonl file, fills NA.
#'
#' @export
#'
#' @examples parse_jsonl("user/desktop/tweets.jsonl")
#'
#' @examples parse_jsonl("user/desktop/tweets.jsonl",
#' filter_term_regex = "this|that", tweet_lang = "en"))
#'
#' @examples
#' **Parallel Example**
#' input_path <- "user/where/your/jsonl/files/are"
#' files <- list.files (input_path, full.names=TRUE, recursive=TRUE)
#' parallel::mclapply(files, safely(parse_jsonl), export_as_csv=TRUE,mc.cores=3)
#' files_csv <- list.files(input_path,full.names = TRUE, recursive = TRUE,pattern = ".csv$")
#' whole_data <- map_df(files_csv, .f =read_csv,  col_types= "cccciclc?ddcccciilcccciiiccclciilccilciccclddcccccciiiccciiillccciccclddccciiccciiicccilclccicicccc")
#'
#'
#'
parse_jsonl <- function (jsonl_path, tweet_colnames=tweet_cols, tweet_lang=".", filter_term_regex=".", export_as_csv=FALSE) {
  gc()
  stopifnot(stringr::str_detect(jsonl_path,"\\.jsonl$"))
  json <- ndjson::stream_in(jsonl_path, cls = "tbl") %>%
    filter(stringr::str_detect(string = lang, pattern = regex(tweet_lang,ignore_case = T))) %>%
    distinct(id_str,.keep_all = TRUE) %>%
    fncols(cname = tweet_colnames) %>%
    select_at(tweet_colnames) %>%
    mutate(text_long= coalesce(extended_tweet.full_text ,retweeted_status.extended_tweet.full_text,text)) %>%
    select(text_long, id_str, everything()) %>%
    filter(stringr::str_detect(string = text_long, pattern = regex(filter_term_regex,ignore_case = T))) %>%
    rename_all(longlat_rep)
  if (export_as_csv== TRUE) {
    write_csv(json, path = paste0(stringr::str_sub(jsonl_path,end = -7), ".csv"))
  } else {
    return(json)
  }
  gc()
}
