#' Title
#'
#' @param ... The margins of error of the variables involved in the sum calculation
#'
#' @return A derived margin of error for a sum
#' @export
#'
#' @examples
moe_sum <- function(...) {

  inputs <- c(...)

  squared <- map_dbl(inputs, function(x) x^2)

  result <- sqrt(sum(squared))

  return(result)

}



#' Calculate the margin of error for a derived proportion
#'
#' @export
moe_prop <- function(num, denom, moe_num, moe_denom) {

  prop <- num / denom

  x <- moe_num^2 - (prop^2 * moe_denom^2)

  result <- sqrt(x) / denom

  return(result)
}


#' Calculate the margin of error for a derived ratio
#'
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
#' @export
moe_product <- function(a, b, moe_a, moe_b) {

  p1 <- (a^2 * moe_b^2)

  p2 <- (b^2 * moe_a^2)

  result <- sqrt(p1 + p2)

  return(result)

}











