


summarise_column <- function(df,
                             col,
                             observed,
                             predictions,
                             weights,
                             ordered_bins = 20,
                             ordered_binning = "equal_width") {

  col_class <- class(df[[col]])

  nominal_classes <- c("character", "factor")

  ordered_classes <- c("integer", "numeric")

  if (any(col_class %in% nominal_classes)) {

    summarised_col <- summarise_col_dplyr(df = df,
                                          col = col,
                                          observed = observed,
                                          predictions = predictions,
                                          weights = weights)

  } else if (any(col_class %in% ordered_classes)) {

    df[["binned_ordered"]] <- discretise(x = df[[col]],
                                         w = df[[weights]],
                                         bins = ordered_bins,
                                         type = ordered_binning)

    summarised_col <- summarise_col_dplyr(df = df,
                                          col = "binned_ordered",
                                          observed = observed,
                                          predictions = predictions,
                                          weights = weights)

    colnames(summarised_col) <- gsub("binned_ordered",
                                     col,
                                     colnames(summarised_col))

    # remove the bucketed column - don't want to add to the data.frame input by
    # the user
    # but as a future enhancement... need to store bucket info and have a way to
    # create buckets from this
    df[["binned_ordered"]] <- NULL

  } else {

    stop(gettextf("unexpected class (%s) for %s",
                  sQuote(col_class),
                  sQuote(col)))

  }

  return(summarised_col)

}






summarise_col_dplyr <- function(df,
                                col,
                                observed,
                                predictions,
                                weights) {

  if (is.null(weights)) {

    stop("weights must be passed")

  }

  by_var <- parse_quosure(col)

  summary_results <- df %>%
    group_by(!!by_var) %>%
    summarise_at(c(observed, predictions, weights), sum)

  if (!is.null(observed)) {

    summary_results[[observed]] <- summary_results[[observed]] /
      summary_results[[weights]]

  }

  if (!is.null(predictions)) {

    for (p in predictions) {

      summary_results[[p]] <- summary_results[[p]] / summary_results[[weights]]

    }

  }

  return(summary_results)

}






