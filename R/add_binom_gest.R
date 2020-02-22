#' Add binomial G-Estimate
#'
#' @description adds an estimated shinkage value obtained by G-Modeling
#'
#' @param tbl a table which *requires* two columns. A column with the number of
#' successes (x) and a column with totals (n).
#' @param x column with number of successes
#' @param n column with totals
#' @param cred_level level of credible interval to compute. For example
#' and interval of .80 will return a 20th percentile lower interval and a
#' 80th percentile upper interval
#' @param theta_from start of parameter space
#' @param theta_to end of parameter space
#' @param theta_by increment of parameter space
#'
#' @return The original table with the following columns
#'   \item{.gest_dist}{Full posterior distribution. Grain may be increased with the theta_* parameters}
#'   \item{.raw}{Estimate (success / total)}
#'   \item{.gest}{Posterior EB g-estimate}
#'   \item{.lo}{Lower bound of credible interval for EB estimate}
#'   \item{.hi}{Upper bound of credible interval for EB estimate}
#'
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom rlang expr !!
#' @importFrom deconvolveR deconv
#' @importFrom purrr pmap map2_dbl map_dbl
#' @importFrom stats dbinom
#'
#' @export
#'
#' @examples
#'
#' library(tibble)
#' set.seed(2017)
#'
#' # simulate 200 random examples from a beta-binomial
#' obs <- 200
#' dat <- tibble(prob = rbeta(obs, 10, 50),
#'                   n = round(rlnorm(obs, 4, 2)) + 1,
#'                   x = rbinom(obs, n, prob))
#'
#' result <- add_binom_gest(dat, x, n)
#' result
add_binom_gest <- function(tbl, x, n,
                           cred_level = .80,
                           theta_from = 0.01,
                           theta_to = 0.99,
                           theta_by = 0.01
                           ){

  n <- rlang::expr(n)
  x <- rlang::expr(x)
  tau <- seq(from = theta_from, to = theta_to, by = theta_by)
  data <- data.frame(n = tbl$n, x = tbl$x)
  result <- deconv(tau = tau, X = data, family = "Binomial", c0 = 1, pDegree = 6)
  d <- data.frame(result$stats)
  theta <- result$stats[, 'theta']
  gTheta <- result$stats[, 'g']

  # just to be clear
  n_k <- n
  x_k <- x

  f_alpha <- function(n_k, x_k) {
    ## .01 is the delta_theta in the Riemann sum
    sum(dbinom(x = x_k, size = n_k, prob = theta) * gTheta) * .01
  }

  g_theta_hat <- function(n_k, x_k) {
    ghat <- gTheta * dbinom(x = x_k, size = n_k, prob = theta) / f_alpha(n_k, x_k)
    list_distribution <- tibble(theta = theta, ghat = ghat )
    return(list_distribution)
  }

  # make alpha level
  alpha_lo <- ((1 - cred_level) * 100)
  alpha_hi <- 100 - alpha_lo

  get_mle <- function(.gest_dist){
    ghat_mle <- .gest_dist$theta[which(.gest_dist$ghat == max(.gest_dist$ghat))]
    return(ghat_mle)
  }

  get_lo <- function(.gest_dist, lo_val){
    ghat_lo <- .gest_dist$theta[which(cumsum(.gest_dist$ghat) >= lo_val)[1]]
    return(ghat_lo)
  }

  get_hi <- function(.gest_dist, hi_val){
    ghat_hi <- .gest_dist$theta[which(cumsum(.gest_dist$ghat) >= hi_val)[1]]
    return(ghat_hi)
  }

  df_density <- tbl %>%
    dplyr::mutate(.gest_dist = purrr::pmap(.l = list(tbl$n, tbl$x), .f = g_theta_hat)) %>%
    dplyr::mutate(.raw = purrr::map2_dbl(.x = !!x, .y = !!n, ~ .x/.y),
           .gest = purrr::map_dbl(.gest_dist, get_mle),
           .lo = purrr::map_dbl(.gest_dist, get_lo, alpha_lo),
           .hi = purrr::map_dbl(.gest_dist, get_hi, alpha_hi)
           )

  return(df_density)

}
