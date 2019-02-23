#' Discretise an ordered variable
#'
#' Discretise (bucket) an ordered variable into a set number of bins. Possible
#' binning schemes are equal width bins, equal weight bins and quantiles.
#'
#' @param x \code{numeric} vector to discretise.
#' @param w \code{numeric} vector of weights for each element of x. Default =
#' \code{NULL}, uses equal weights.
#' @param bins \code{numeric} number of bins to split x into.
#' @param type \code{character} type of bucketing scheme to use. Possible values
#' are; 'equal_weight', 'equal_width', 'quantiles'.
#' @param q \code{numeric} vector of quantiles to use as break points for bins
#' if \code{type} is \code{"quantiles"}.
#' @param ... other arguments passed down to cut for the equal width and
#' quantiles cases.
#'
#' @return
#' \code{ordered}, \code{factor} version of x, bucketed into the specified
#' number of bins.
#'
#' @details
#' Currently ordered factors not supported. The actual number of returns bins
#' may be different to that input depending on the distribution of data values.
#'
#' @examples
#' set.seed(1)
#' a = runif(200)
#' b = rpois(200, 3)
#' table(b)
#' sum(b) / 25
#' bins = discretise(x = a, w = b, bins = 25, type = 'equal_weight')
#' table(bins)
#' tapply(b, bins, sum)
#'
#' @export
discretise <- function(x, w = NULL, bins = 10, type = NULL, q = NULL, ...) {

  if (!is.vector(x)) {

    stop('x should be a vector')

  }

  # can support ordered factors in the future
  if (!is.numeric(x)) {

    stop('x should be numeric')

  }

  n <- length(x)

  if (is.null(w)) {

    w <- rep(1, length(x))

  } else {

    if (length(w) != n) {

      stop('w and x are different lengths')

    }

    if (any(is.null(w))) {

      stop('w has null values')

    }

    if (any(w < 0)) {

      stop('w have negative values')

    }

  }

  if (!is.numeric(bins)) {

    stop('bins must be numeric')

  }

  if (!bins > 1) {

    stop('bins must be > 1')

  }

  type_accepted_values <-
    c('equal_weight', 'equal_width', 'quantiles')

  if (!type %in% type_accepted_values) {

    stop('unexpected value for type')

  }

  if (type == 'quantiles' & is.null(q)) {

    stop('quantiles have not been supplied')

  }

  if (!is.null(q)) {

    if (!is.numeric(q)) {

      stop('q should be numeric')

    }

    if (!all(q >= 0 & q <= 1)) {

      stop('q values should be between 0 and 1')

    }

  }

  if (type == 'equal_width') {

    x_binned <- bin_equal_width(x = x, bins = bins)

  } else if (type == 'equal_weight') {

    x_binned <- bin_equal_weight(x = x, w = w, bins = bins)

  } else if (type == 'quantiles') {

    x_binned <- bin_quantiles(x = x, q = q)

  } else {

    stop('unexpected type')

  }

  return(x_binned)

}




bin_equal_width <- function(x, bins) {

  x_binned <- cut(x,
                  breaks = bins,
                  include.lowest = TRUE,
                  ordered_result = TRUE)

  x_binned_na <- add_character_NA(x_binned)

  return(x_binned_na)

}




bin_equal_weight <- function(x, w, bins) {

  q <- seq(0, bins, 1) / bins

  q_weighted <- weighted_percentile(x, w, q)

  x_binned <- cut(x,
                  breaks = q_weighted,
                  include.lowest = TRUE,
                  ordered_result = TRUE)

  x_binned_na <- add_character_NA(x_binned)

  return(x_binned_na)

}



bin_min_weight <- function() {


}



bin_quantiles <- function(x, q) {

  quantiles <- quantile(x, probs = q)

  x_binned <- cut(x,
                  breaks = quantiles,
                  include.lowest = TRUE,
                  ordered_result = TRUE)

  x_binned_na <- add_character_NA(x_binned)

  return(x_binned_na)

}



add_character_NA <- function(x) {

  if (!is.factor(x)) {

    stop('x should be a factor')

  }

  if (sum(is.na(x)) > 0) {

    x_na <- addNA(x)

    levels(x_na)[which(is.na(levels(x_na)))] <- "NA"

    return(x_na)

  } else {

    return(x)

  }

}





weighted_percentile <- function(x, w, q) {

  n <- length(x)

  x_order <- order(x)

  w <- w[x_order]

  x <- x[x_order]

  w_pct <- cumsum(w) / sum(w)

  w_q <- rep(0, length(q))

  for (i in 1:length(q)) {

    p <- q[i]

    gt <- which(w_pct >= p)[1]

    # if the quantile value has been found exactly return that value
    if (w_pct[gt] == p) {

      w_quantile <- x[gt]

    # otherwise interpolate between
    } else {

      # if the first value in x meets the condition don't interpolate
      if (gt == 1) {

        w_quantile <- x[gt]

      } else {

        lt <- gt - 1

        w_quantile <- x[lt] +
          (x[gt] - x[lt]) * ((p - w_pct[lt]) / (w_pct[gt] - w_pct[lt]))

      }

    }

    w_q[i] <- w_quantile

  }

  return(w_q)

}



