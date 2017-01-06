# some toy functions for figgering out package dev workflow

#' a cute convert-to-string wrapper
#'
#' tha azChar phonk takes a obj + turns it into a character obj + gives it baqq
#' @param x an arbitrary object to be converted
#' @keywords character, string, convert
#' @export
#' @examples
#' azChar()
azChar <- function(x){
  x <- as.character(x)
  return(x)
}


#' a quik-round rounder
#'
#' tha choozeRound phonk takes a num + rounds it how u want + gives it baqq <3 
#' @param x a num or int (atomic or vector) or other obj
#' @param d a num, specifies how many diggies to round x to. Defaults to 2.
#' @keywords round, arithmetic, numbers
#' @export
#' @examples
#' choozeRound()
choozeRound <- function(x, d=2){
  x <- round(x, digits=d)
  return(x)
}

