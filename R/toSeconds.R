#' Convert Duration to Seconds
#'
#' This function converts a time duration 'x' formatted as HH:MM:SS
#' into seconds.
#' @param x a character string of a time duration formatted HH:MM:SS, MM:SS or SS
#' @return a character string of total seconds
#' @author BDW
#' @details
#' This function first tests that the input is a character string. Then it splits
#' the string depending on whether the input includes minutes and hours. Finally
#' it calculates the total number of seconds.
#' @export
#' @examples
#' x <- "35:24:20"
#' toSeconds(x)
#'

toSeconds <- function(x){
  if (!is.character(x)) stop("x must be a character string of the form H:M:S")
  if (length(x)<=0)return(x)

  unlist(
    lapply(x,
           function(i){
             i <- as.numeric(strsplit(i,':',fixed=TRUE)[[1]])
             if (length(i) == 3)
               i[1]*3600 + i[2]*60 + i[3]
             else if (length(i) == 2)
               i[1]*60 + i[2]
             else if (length(i) == 1)
               i[1]
           }
    )
  )
}
