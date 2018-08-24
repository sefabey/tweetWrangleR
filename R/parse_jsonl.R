#' Parse .jsonl files into a tibble
#'
#' Takes a .jsonl file, CONTINUE THE DOCUMENT
#'
#' @param jsonl_path Character. The path to the .jsonl file. It's a good idea to use the output of list.files() as an input.
#' @param tweet_colnames Character vector. The colnames you want in the output tibble. In the object tweetWrangleR::tweet_cols, 99 opinionated colnames provided as default but users can provide a custom colname vector by add new ones or drop existing ones.
#' @param tweet_lang Character. Filter tweets only in one language. Default is "en" for English language. Users can provide other languages as well.
#' @param filter_term_regex Character. Accepts regex. Default is "muslim|islam". You can the keyword(s) you've used betwen quotation marks while filtering data. If you want to provide multiple keywords, put them between speech marks and separate by '|' (which means 'or').
#' @param export_as_csv Logical. If **TRUE**, it will export parsed data as a csv file within the same path as the .jsonl file. If **FALSE** (default), it will return a tibble.
#'
#' @return Always returns a tibble with the columns provided in the tweet_colnames argument. Default alwayns returns a tibble with 99 columns. If there are values that are missing in the source .jsonl file, fills NA.
#'
#' @export
#'
#' @examples parse_jsonl("user/desktop/tweets.jsonl")
#'
parse_jsonl <- function (jsonl_path, tweet_colnames=tweet_cols, tweet_lang='en', filter_term_regex="muslim|islam", export_as_csv=FALSE) {
  stopifnot(stringr::str_detect(jsonl_path,"\\.jsonl$"))
  json <- ndjson::stream_in(jsonl_path, cls = "tbl") %>%
    filter(lang==tweet_lang) %>%
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
}
