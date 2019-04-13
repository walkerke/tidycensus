#' Evaluate whether the difference in two estimates is statistically significant.
#'
#' @param est1 The first estimate.
#' @param est2 The second estimate
#' @param moe1 The margin of error of the first estimate
#' @param moe2 The margin of error of the second estimate
#' @param clevel The confidence level. May by 0.9, 0.95, or 0.99
#'
#' @seealso https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch07.pdf
#'
#' @return TRUE if the difference is statistically signifiance, FALSE otherwise.
#' @export
significance <- function(est1, est2, moe1, moe2, clevel = 0.9){
  # generate z score
  if(clevel == 0.9) z = 1.645
  else if(clevel == 0.95) z = 1.960
  else if(clevel == 0.99) z = 2.576
  else stop("clevel must be 0.9, 0.95, or 0.99")

  #ensure all variables are numeric
  if(any(lapply(c(est1, est2, moe1, moe2), class) != "numeric")){
    stop("All estimates and moes must be of class 'numeric'.")
  }

  #convert NA moes to 0.
  moe1[which(is.na(moe1))] <- 0
  moe2[which(is.na(moe2))] <- 0

  # 1. Calculate the SEs for the two ACS estimates.
  se1 <- moe1/z
  se2 <- moe2/z

  # 2. Square the resulting SE for each estimate.
  se1 <- se1^2
  se2 <- se2^2

  # 3. Sum the squared SEs.
  ses <- se1+se2

  # 4. Calculate the square root of the sum of the squared SEs.
  ses <- sqrt(ses)

  # 5. Divide the difference between the two ACS estimates by the square
  # root of the sum of the squared SEs.
  result <- (est1-est2)/ses

  # 6. Compare the absolute value of the result from Step 5 with the
  # critical value for the desired level of confidence (1.645 for 90 percent,
  # 1.960 for 95 percent, or 2.576 for 99 percent).
  result <- abs(result) > z

  # 7. If the absolute value of the result from Step 5 is greater than the
  # critical value, then the difference between the two estimates can be
  # considered statistically significant, at the level of confidence
  # corresponding to the critical value selected in Step 6.
  result

}
