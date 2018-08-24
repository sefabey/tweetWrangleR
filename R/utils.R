#' Create columns if they don't exist
#' 
#' This function is taken from [here](https://stackoverflow.com/a/45858044).
#' 
#' @param data Dataset you want to change the columns for.
#' @param cname Column names to check if exists.
#'
#' @return Returns a dataframe with the extra columns if they don't exist.
#' 
#'
#' @examples fncols(iris, "species")
fncols <- function(data, cname) {
  add <-cname[!cname%in%names(data)]
  
  if(length(add)!=0) data[add] <- NA
  data
}


#' Replace coordinate colnames
#'
#' @param x Colnames referring to long/lat infor if pattern matched.
#'
#' @return Vector with replaced strings if pattern found.
#'
#' @examples longlat_rep(c ("sample.coordinates.coordinates.0", "sample.coordinates.0"  ))
longlat_rep <- function(x) {
  str_replace(
    str_replace(x, fixed("coordinates.coordinates.0"), "coord_longitude"),
    fixed("coordinates.coordinates.1"),
    "coord_latitude")
}