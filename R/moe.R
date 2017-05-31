#' Calculate the margin of error for a derived sum
#'
#' @param ... The margins of error of the variables involved in the sum calculation
#'
#' @return A margin of error for a derived sum
#' @export
moe_sum <- function(...) {

  inputs <- c(...)

  squared <- map_dbl(inputs, function(x) x^2)

  result <- sqrt(sum(squared))

  return(result)

}

#' Calculate the margin of error for a derived proportion
#'
#' @param num The numerator involved in the proportion calculation (an estimate)
#' @param denom The denominator involved in the proportion calculation (an estimate)
#' @param moe_num The margin of error of the numerator
#' @param moe_denom The margin of error of the denominator
#'
#' @return A margin of error for a derived proportion
#' @export
moe_prop <- function(num, denom, moe_num, moe_denom) {

  prop <- num / denom

  x <- moe_num^2 - (prop^2 * moe_denom^2)

  result <- ifelse(x < 0, moe_ratio(num = num, denom = denom, moe_num = moe_num, moe_denom = moe_denom),
                   sqrt(x) / denom)

  return(result)
}


#' Calculate the margin of error for a derived ratio
#'
#' @param num The numerator involved in the ratio calculation (an estimate)
#' @param denom The denominator involved in the ratio calculation (an estimate)
#' @param moe_num The margin of error of the numerator
#' @param moe_denom The margin of error of the denominator
#'
#' @return A margin of error for a derived ratio
#' @export
moe_ratio <- function(num, denom, moe_num, moe_denom) {

  r2 <- (num / denom)^2

  mn2 <- moe_num^2

  md2 <- moe_denom^2

  result <- (sqrt(mn2 + (r2 * md2))) / denom

  return(result)

}

#' Calculate the margin of error for a derived product
#'
#' @param est1 The first factor in the multiplication equation (an estimate)
#' @param est2 The second factor in the multiplication equation (an estimate)
#' @param moe1 The margin of error of the first factor
#' @param moe2 The margin of error of the second factor
#'
#' @return A margin of error for a derived product
#' @export
moe_product <- function(est1, est2, moe1, moe2) {

  p1 <- (est1^2 * moe2^2)

  p2 <- (est2^2 * moe1^2)

  result <- sqrt(p1 + p2)

  return(result)

}











