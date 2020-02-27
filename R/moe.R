#' Calculate the margin of error for a derived sum
#'
#' Generates a margin of error for a derived sum.  The function requires a vector of margins of error involved in a sum calculation, and optionally a vector of estimates associated with the margins of error.  If the associated estimates are not specified, the user risks inflating the derived margin of error in the event of multiple zero estimates.  It is recommended to inspect your data for multiple zero estimates before using this function and setting the inputs accordingly.
#'
#' @param moe A vector of margins of error involved in the sum calculation
#' @param estimate A vector of estimates, the same length as \code{moe}, associated with the margins of error
#' @param na.rm A logical value indicating whether missing values (including NaN) should be removed
#' @seealso \url{https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/MultiyearACSAccuracyofData2015.pdf}
#'
#' @return A margin of error for a derived sum
#' @export
moe_sum <- function(moe, estimate = NULL, na.rm = FALSE) {

  if (!is.null(estimate)) {
    # ID those MOE values with 0 estimates
    zeros <- estimate == 0

    # Reduce the vector and keep the first one
    onezero <- unique(moe[zeros])

    # Combine with the non-zeros
    forcalc <- c(onezero, moe[!zeros])

  } else if (is.null(estimate)) {
    warning("You have not specified the estimates associated with the margins of error.  In the event that your calculation involves multiple zero estimates, this will unnaturally inflate the derived margin of error.", call. = FALSE)

    forcalc <- moe
  }

  squared <- map_dbl(forcalc, function(x) x^2)

  result <- sqrt(sum(squared, na.rm = na.rm))

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

  result <- moe_ratio(num = num, denom = denom, moe_num = moe_num, moe_denom = moe_denom)

  x <- moe_num^2 - (prop^2 * moe_denom^2)

  pos_x <- x >= 0

  result[pos_x] <- sqrt(x[pos_x]) / denom[pos_x]

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











