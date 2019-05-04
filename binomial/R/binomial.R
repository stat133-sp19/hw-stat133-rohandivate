# Checks if probability input is valid
check_prob <- function(prob) {
  if (length(prob) > 1) {
    stop("prob has to be length 1")
  }
  if (prob > 1 | prob < 0) {
    stop("p has to be a number betwen 0 and 1")
  }

  return(TRUE)
}

# Checks if the number of trials is valid
check_trials <- function(trials) {
  if (trials < 0) {
    stop("invalid trials value")
  }
  if (!(trials == round(trials))) {
    stop("invalid trials value")
  }

  return(TRUE)
}

# Check if the number of success and trials is valid
check_success <- function(success, trials) {
  if (length(success) > 1) {
    if (sum(success > trials) > 0) {
      stop("success cannot be greater than trials")
    }
    if (sum(success) < 0) {
      stop("success must be non-negative")
    }

    return(TRUE)
  }

  if (success < 0) {
    stop("success must be non-negative")
  }

  if (success > trials) {
    stop("success cannot be greater than trials")
  }

  return(TRUE)
}

# Computes the mean value given number of trials and probability of success
aux_mean <- function(trials, prob) {
  return(trials * prob)
}

# Computes the variance given number of trials and probability of success
aux_variance <- function(trials, prob) {
  return(trials * prob * (1 - prob))
}

# Compute sthe mode given the number of trials and probability of success
aux_mode <- function(trials, prob) {
  ret <- trials * prob + prob
  if (ret == round(ret)) {
    return(c(ret, ret - 1))
  }
  return(floor(trials * prob + prob))
}

# Computes the skewness of the distribution given number of trials and probability of success
aux_skewness <- function(trials, prob) {
  num <- 1 - 2 * prob
  denom <- sqrt(aux_variance(trials, prob))
  return(num/denom)
}

# Compute the kurtosis of the distribution given the number of trials and probability of success
aux_kurtosis <- function(trials, prob) {
  num <- 1 - 6 * prob * (1 - prob)
  denom <- aux_variance(trials, prob)
  return(num/denom)
}

#' @title Bin Choose
#' @description computes the number of combinations in which k successes can occur in n trials
#' @param n number of trials (numeric)
#' @param k number of successes (numeric)
#' @return number of combinations
#' @export
#' @examples
#' bin_choose(n = 5, k = 2)
#' bin_choose(5, 0)
#' bin_choose(5, 1:3)
bin_choose <- function(n, k) {
  if (length(k) > 1) {
    if (sum(k > n) > 0) {
      stop("success cannot be greater than trials")
    }
    return(factorial(n) / (factorial(k) * factorial(n-k)) )
  }

  if (k > n) {
    stop("k cannot be greater than n")
  }

  return(factorial(n) / (factorial(k) * factorial(n-k)) )
}

#' @title Bin Probability
#' @description computes the probability of getting a given number of successes in a given number of trials with a probability of success
#' @param success number of successes (numeric)
#' @param trials number of trials (numeric)
#' @param prob probability of success (numeric)
#' @return probability of getting that number of successes
#' @export
#' @examples
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#' bin_probability(success = 55, trials = 100, prob = 0.45)
bin_probability <- function(success, trials, prob) {
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)

  return(bin_choose(trials, success) * (prob^success) * (1 - prob)^(trials - success) )
}

#' @title Bin Distribution
#' @description computes a table of number of successes and their respective probabilities
#' @param trials number of trials (numeric)
#' @param prob probability of success (numeric)
#' @return data frame of number of successes and probabilities
#' @export
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
bin_distribution <- function(trials, prob) {
  probability <- bin_probability(0:trials, trials, prob)
  success <- 0:trials
  df <- data.frame(success, probability)
  class(df) <- c("bindis", "data.frame")
  return(df)
}

#' @export
plot.bindis <- function(dist_table) {
  barplot(height = dist_table$probability, names.arg = dist_table$success, xlab = "successes", ylab = "probability")
}

#' @title Bin Cumulative
#' @description computes a data frame with probability distribution and cumulative probabilities for each success value
#' @param trials number of trials (numeric)
#' @param prob probability of success (numeric)
#' @return data frame of number of successes, probabilities, and cumulative probabilities
#' @export
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative <- function(trials, prob) {
  probability <- bin_probability(0:trials, trials, prob)
  success <- 0:trials
  cumulative <- cumsum(probability)
  df <- data.frame(success, probability, cumulative)
  class(df) <- c("bincum", "data.frame")
  return(df)
}

#' @export
plot.bincum <- function(dist_table) {
  plot(dist_table$success, dist_table$cumulative, type = "o", xlab = "successes", ylab = "probability")
}

#' @title Bin Variable
#' @description takes in trials and probability values and returns a list with both of the values
#' @param trials number of trials (numeric)
#' @param prob probability of success (numeric)
#' @return list of class binvar that contains the trials and probability values
#' @export
#' @examples
#' bin_variable(trials = 10, p = 0.3)
bin_variable <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  obj <- list(trials = trials, prob = prob)
  class(obj) <- "binvar"
  return(obj)
}

#' @export
print.binvar <- function(x) {
  cat('"Binomial Variable"\n\n')
  cat('Parameters\n')
  cat('- number of trials: ')
  cat(x$trials)
  cat('\n- prob of success: ')
  cat(x$prob)
}

#' @export
summary.binvar <- function(bin_var) {
  obj <- list(trials = bin_var$trials, prob = bin_var$prob, mean = aux_mean(bin_var$trials, bin_var$prob), variance = aux_variance(bin_var$trials, bin_var$prob), mode = aux_mode(bin_var$trials, bin_var$prob), skewness = aux_skewness(bin_var$trials, bin_var$prob), kurtosis = aux_kurtosis(bin_var$trials, bin_var$prob))
  class(obj) <- "summary.binvar"
  return(obj)
}

#' @export
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

#' @export
bin_mean <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials, prob))
}

#' @export
bin_variance <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}

#' @export
bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials, prob))
}

#' @export
bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials, prob))
}

#' @export
bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials, prob))
}


