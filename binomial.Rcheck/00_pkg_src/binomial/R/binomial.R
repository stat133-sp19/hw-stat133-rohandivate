# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

check_prob <- function(prob) {
  if (length(prob) > 1) {
    stop("prob has to be length 1")
  }
  if (prob > 1 | prob < 0) {
    stop("p has to be a number betwen 0 and 1")
  }

  return(TRUE)
}

check_trials <- function(trials) {
  if (trials < 0) {
    stop("invalid trials value")
  }

  return(TRUE)
}

check_success <- function(success, trials) {
  if (success > trials) {
    stop("success cannot be greater than trials")
  }

  return(TRUE)
}

aux_mean <- function(trials, prob) {
  return(trials * prob)
}

aux_variance <- function(trials, prob) {
  return(trials * prob * (1 - prob))
}

aux_mode <- function(trials, prob) {
  return(floor(trials * prob + prob))
}

aux_skewness <- function(trials, prob) {
  num <- 1 - 2 * prob
  denom <- sqrt(aux_variance(trials, prob))
  return(num/denom)
}

aux_kurtosis <- function(trials, prob) {
  num <- 1 - 6 * prob * (1 - prob)
  denom <- aux_variance(trials, prob)
  return(num/denom)
}

bin_choose <- function(n, k) {
  if (k > n) {
    stop("k cannot be greater than n")
  }

  return(factorial(n) / (factorial(k) * factorial(n-k)) )
}

bin_probability <- function(success, trials, prob) {
  if (!check_trials(trials)) {
    stop("invalid trials value")
  }
  if (!check_prob(prob)) {
    stop("invalid prob value")
  }
  if(!check_success(success, trials)) {
    stop("invalid success value")
  }

  return(bin_choose(trials, success) * (prob^success) * (1 - prob)^(trials - success) )
}

bin_distribution <- function(trials, prob) {
  probability <- bin_probability(0:trials, trials, prob)
  success <- 0:trials
  df <- data.frame(success, probability)
  class(df) <- c("bindis", "data.frame")
  return(df)
}

plot.bindis <- function(dist_table) {
  barplot(height = dist_table$probability, names.arg = dist_table$success, xlab = "successes", ylab = "probability")
}

bin_cumulative <- function(trials, prob) {
  probability <- bin_probability(0:trials, trials, prob)
  success <- 0:trials
  cumulative <- cumsum(probability)
  df <- data.frame(success, probability, cumulative)
  class(df) <- c("bincum", "data.frame")
  return(df)
}


plot.bincum <- function(dist_table) {
  plot(dist_table$success, dist_table$cumulative, type = "o", xlab = "successes", ylab = "probability")
}


bin_variable <- function(trials, prob) {
  if (!check_trials(trials)) {
    stop("invalid trials value")
  }
  if (!check_prob(prob)) {
    stop("invalid prob value")
  }
  obj <- list(trials = trials, prob = prob)
  class(obj) <- "binvar"
  return(obj)
}

print.binvar <- function(x) {
  cat('"Binomial Variable"\n\n')
  cat('Parameters\n')
  cat('- number of trials: ')
  cat(x$trials)
  cat('\n- prob of success: ')
  cat(x$prob)
}

summary.binvar <- function(bin_var) {
  obj <- list(trials = bin_var$trials, prob = bin_var$prob, mean = aux_mean(bin_var$trials, bin_var$prob), variance = aux_variance(bin_var$trials, bin_var$prob), mode = aux_mode(bin_var$trials, bin_var$prob), skewness = aux_skewness(bin_var$trials, bin_var$prob), kurtosis = aux_kurtosis(bin_var$trials, bin_var$prob))
  class(obj) <- "summary.binvar"
  return(obj)
}

print.summary.binvar <- function(sum_binvar) {
  cat('"Summary Binomial"\n\n')
  cat('Parameters\n')
  cat('- number of trials: ')
  cat(sum_binvar$trials)
  cat('\n- prob of success: ')
  cat(sum_binvar$prob)
  cat('\n\nMeasures\n')
  cat('- mean: ')
  cat(sum_binvar$mean)
  cat('\n- variance: ')
  cat(sum_binvar$variance)
  cat('\n- mode: ')
  cat(sum_binvar$mode)
  cat('\n- skewness: ')
  cat(sum_binvar$skewness)
  cat('\n- kurtosis: ')
  cat(sum_binvar$kurtosis)
}


bin_mean <- function(trials, prob) {
  if (!check_trials(trials)) {
    stop("invalid trials value")
  }
  if (!check_prob(prob)) {
    stop("invalid prob value")
  }
  return(aux_mean(trials, prob))
}

bin_variance <- function(trials, prob) {
  if (!check_trials(trials)) {
    stop("invalid trials value")
  }
  if (!check_prob(prob)) {
    stop("invalid prob value")
  }
  return(aux_variance(trials, prob))
}

bin_mode <- function(trials, prob) {
  if (!check_trials(trials)) {
    stop("invalid trials value")
  }
  if (!check_prob(prob)) {
    stop("invalid prob value")
  }
  return(aux_mode(trials, prob))
}

bin_skewness <- function(trials, prob) {
  if (!check_trials(trials)) {
    stop("invalid trials value")
  }
  if (!check_prob(prob)) {
    stop("invalid prob value")
  }
  return(aux_skewness(trials, prob))
}

bin_kurtosis <- function(trials, prob) {
  if (!check_trials(trials)) {
    stop("invalid trials value")
  }
  if (!check_prob(prob)) {
    stop("invalid prob value")
  }
  return(aux_kurtosis(trials, prob))
}


