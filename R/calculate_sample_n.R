#' Calculate Sample Number
#'
#' Calculate the sample number based on power, alpha, standard deviation(SD) and Minimum Detectable Effect(MDE)
#' @param power The probability of rejecting a false null hypothesis (domain: 0 ~ 1)
#' @param alpha The probability of a type I error (domain: 0 ~ 1)
#' @param sd The standard deviation of the outcome (domain: positive)
#' @param mde The difference between the mean of the control and treatment groups
#' @return The number of necessary samples calculated based on the arguments
#' @examples 
#' n <- cal_sample_n(0.8, 0.05, 500, 100);
#' @export
cal_sample_n <- function(power, alpha, sd, mde){
    n <- 2 * sd**2 * (qnorm(1-alpha/2) + qnorm(power))**2 / 100**2
    return(n)
}