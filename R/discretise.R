
bin_ordered <- function(ordered, weights, bins = 30, binning = "equal_width") {

  if (binning == "equal_width") {

    ordered_binned <- cut(ordered, breaks = bins, include.lowest = T, ordered_result = T)

    ordered_binned <- addNA(ordered_binned)

    if (any(is.na(levels(ordered_binned)))) {

      levels(ordered_binned)[which(is.na(levels(ordered_binned)))] <- "NA"

    }

  } else if (binning == "equal_weight") {

    df <- data.frame(ordered = ordered,
                     weights = weights)

    total_weight <- sum(df$weights)

    if (sum(is.na(df$ordered)) > 0) {

      df <- df[!is.na(df$ordered), ]

    }

    total_weight_excl_NA <- sum(df$weight)

    df <- df[order(df$ordered, na.last = TRUE, decreasing = FALSE), ]

    df$cum_weight <- cumsum(df$weights)

    bucket_weight <- total_weight / bins

    buckets_cum_weight <- c(0,
                            (1:(bins-1)) * bucket_weight,
                            total_weight_excl_NA)

    bucket_indices <- sapply(buckets_cum_weight,
                             function(x) which(df$cum_weight >= x)[1])

    if (any(bucket_indices == 0, na.rm = TRUE)) {

      bucket_indices <- bucket_indices[-which(bucket_indices == 0)]

    }

    ordered_binned <- cut(ordered,
                          breaks = unique(df$ordered[bucket_indices]),
                          include.lowest = TRUE)

    ordered_binned <- addNA(ordered_binned)

    if (any(is.na(levels(ordered_binned)))) {

      levels(ordered_binned)[which(is.na(levels(ordered_binned)))] <- "NA"

    }

  } else {

    stop(gettextf("unexpected value for binning argument; (%s)",
                  sQuote(binning)))

  }

  return(ordered_binned)

}

