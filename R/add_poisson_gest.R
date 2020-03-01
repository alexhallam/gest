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
#'dontrun{}\
#' library(tidyverse)
#' library(ZIM)
#' data(syph)
#' df <- syph %>%
#'   pivot_longer(cols = starts_with("a"), names_to = "key", values_to = "value")
#'
#' df %>%
#'   add_poisson_gest(value)
#'   }
add_poisson_gest <- function(tbl, value,
                           cred_level = .80,
                           theta_from = -4,
                           theta_to = 5.0,
                           theta_by = 0.1

                           ){

  value <- rlang::expr(value)
  pre_counts  <- as.data.frame(table(tbl$value))
  colnames(pre_counts ) <- c("value","n")
  counts <- as.numeric(pre_counts$n)
  N <- length(pre_counts$n)

  lambda <- seq(theta_from, theta_to, theta_by)
  tau <- exp(lambda)
  result <- deconvolveR::deconv(tau = tau, y = counts, n = N, c0=1, pDegree = 6, ignoreZero = TRUE)
  stats <- result$stats
  g <- result$stats[,"g"]
  g <- g/sum(g)
  fE <- result$P %*% g
  fE <- counts[1] * fE/fE[1]
  log_counts <- log(counts)

  d <- data.frame(lambda = lambda, g = stats[, "g"], SE.g = stats[, "SE.g"], tg = stats[, "tg"])
  # get lookup table of densities for each mean. columns represent means column 1 is mean = 0
  gPost <- sapply(seq_len(nrow(result$P)), function(i) local({tg <- d$tg * result$P[i, ]; tg / sum(tg)}))

  get_dist <- function(value){
    df <- tibble(tau = tau, ghat = gPost[, (value + 1)]) # first columns is 0 which is position 1
    df
  }
  # make alpha level
  alpha_lo <- ((1 - cred_level) * 100)
  alpha_hi <- 100 - alpha_lo

  get_mle <- function(.gest_dist){
    ghat_mle <- .gest_dist$tau[which(.gest_dist$ghat == max(.gest_dist$ghat))]
    return(ghat_mle)
  }

  get_lo <- function(.gest_dist, lo_val){
    ghat_lo <- .gest_dist$tau[which(cumsum(.gest_dist$ghat) >= lo_val)[1]]
    return(ghat_lo)
  }

  get_hi <- function(.gest_dist, hi_val){
    ghat_hi <- .gest_dist$tau[which(cumsum(.gest_dist$ghat) >= hi_val)[1]]
    return(ghat_hi)
  }

  h <- tbl %>%
    group_by(key) %>%
    summarise(value = floor(mean(value))) %>%
    ungroup() %>%
    mutate(.gest_dist = purrr::map(.x = value, .f = get_dist)) %>%
    dplyr::mutate(
      .gest = purrr::map_dbl(.gest_dist, get_mle),
      .lo = purrr::map_dbl(.gest_dist, get_lo, alpha_lo/100),
      .hi = purrr::map_dbl(.gest_dist, get_hi, alpha_hi/100)
    )

  return(h)

}





