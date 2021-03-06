#' tweetWrangleR.
#'
#' As the name suggests, tweetWrangleR package wrangles Twitter data. Twitter data collected from Steaming API in .jsonl file format can be parsed and flattened into a 2D tibble using tweetWrangleR. tweetWrangleR also enables web scaping from Twitter. This is expecially useful to collect historical #' data because Twitter's search API only allows to search fro tweets sent in the last 7-9 days.
#' The package is constantly under development and suggestions, pull and feature requests are encouraged.
#' Features that will be added to the package in the future are:
#' * Clean tweet text
#' * Derive topic models
#' * Prepare RT data for social network analysis
#' * Calculate RT counts
#' * Calculate RT lags
#' * Derive user gender based on names
#' * Calculate sentiment scores
#' * Text classification using externally built classifiers.
#'
#'
#'
#' @import dplyr
#' @import ndjson
#' @import stringr
#' @import reshape2
#' @import tidyr
#' @import tibble
#' @name tweetWrangleR
#' @docType package
NULL


tweet_cols <- c("text_long", "text", "created_at", "id", "id_str", "lang", "possibly_sensitive", "source", "timestamp_ms", "coordinates.coordinates.0", "coordinates.coordinates.1", "coordinates.type", "place.name", "place.country", "place.country_code", "favorite_count", "retweet_count", "truncated","entities.urls.0.expanded_url", "withheld_in_countries.0", "user.created_at", "user.description", "user.followers_count", "user.friends_count", "user.id", "user.id_str", "user.location", "user.name", "user.protected", "user.screen_name", "user.statuses_count","user.favourites_count", "user.verified","user_created_at", "user.time_zone", "user.utc_offset", "is_quote_status", "quoted_status.created_at", "quoted_status.id", "quoted_status.id_str", "quoted_status.text", "quoted_status.lang", "quoted_status.possibly_sensitive", "quoted_status.coordinates.coordinates.0", "quoted_status.coordinates.coordinates.1", "quoted_status.place.name", "quoted_status.place.country", "quoted_status.place.country_code", "quoted_status.entities.urls.0.expanded_url", "quoted_status.user.created_at", "quoted_status.user.description", "quoted_status.user.friends_count", "quoted_status.user.followers_count", "quoted_status.user.id", "quoted_status.user.id_str", "quoted_status.user.screen_name", "quoted_status.user.name", "quoted_status.user.statuses_count", "quoted_status.favorite_count", "quoted_status.retweet_count", "quoted_status.user.verified", "quoted_status.user.protected", "quoted_status.user.location", "quoted_status.user.time_zone", "retweeted_status.created_at", "retweeted_status.id", "retweeted_status.id_str", "retweeted_status.text", "retweeted_status.lang", "retweeted_status.possibly_sensitive", "retweeted_status.coordinates.coordinates.0", "retweeted_status.coordinates.coordinates.1", "retweeted_status.place.name", "retweeted_status.place.country", "retweeted_status.place.country_code", "retweeted_status.favorite_count", "retweeted_status.retweet_count", "retweeted_status.entities.urls.0.expanded_url", "retweeted_status.user.created_at", "retweeted_status.user.description", "retweeted_status.user.followers_count", "retweeted_status.user.friends_count", "retweeted_status.user.id", "retweeted_status.user.id_str", "retweeted_status.user.screen_name", "retweeted_status.user.name", "retweeted_status.user.statuses_count", "retweeted_status.user.verified", "retweeted_status.user.location", "retweeted_status.user.protected", "retweeted_status.user.time_zone", "in_reply_to_screen_name", "in_reply_to_status_id", "in_reply_to_status_id_str", "in_reply_to_user_id", "in_reply_to_user_id_str", "quoted_status.extended_tweet.full_text", "retweeted_status.extended_tweet.full_text", "extended_tweet.full_text")
