





summarise_column <- function(df,
                             col,
                             observed,
                             predictions1,
                             predictions2,
                             weights,
                             ordered_bins = 30,
                             ordered_binning = "equal_width") {

  col_class <- class(df[[col]])

  nominal_classes <- c("character", "factor")

  ordered_classes <- c("integer", "numeric")

  if (any(col_class %in% nominal_classes)) {

    col_type <- "nominal"

    summarised_col <- dplyr_summarise(df = df,
                                      col = col,
                                      observed = observed,
                                      predictions1 = predictions1,
                                      predictions2 = predictions2,
                                      weights = weights)

  } else if (any(col_class %in% ordered_classes)) {

    col_type <- "ordinal"

    df[["binned_ordered"]] <- bin_ordered(ordered = df[[col]],
                                          weights = df[[weights]],
                                          bins = ordered_bins,
                                          binning = ordered_binning)

    summarised_col <- dplyr_summarise(df = df,
                                      col = "binned_ordered",
                                      observed = observed,
                                      predictions1 = predictions1,
                                      predictions2 = predictions2,
                                      weights = weights)

    colnames(summarised_col) <- gsub("binned_ordered",
                                     col,
                                     colnames(summarised_col))

    # remove the bucketed column - don't want to add to the data.frame
    #   input by the user - but as a future enhancement...
    # need to store bucket info and have a way to create buckets from this
    df[["binned_ordered"]] <- NULL

  } else {

    stop(gettextf("unexpected class (%s) for %s",
                  sQuote(col_class),
                  sQuote(col)))

  }

  column_summary <- list()

  column_summary$col_class <- col_class

  column_summary$col_type <- col_type

  column_summary$summary <- summarised_col

  return(column_summary)

}






dplyr_summarise <- function(df,
                            col,
                            observed,
                            predictions1,
                            predictions2,
                            weights) {

  if (is.null(weights)) {

    stop("weights must be passed")

  }

  by_var <- parse_quosure(col)

  summary_results <- df %>%
    group_by(!!by_var) %>%
    summarise_at(c(observed,
                   predictions1,
                   predictions2,
                   weights),
                 sum)

  if (!is.null(observed)) {

    summary_results[[observed]] <- summary_results[[observed]] /
      summary_results[[weights]]

  }

  if (!is.null(predictions1)) {

    summary_results[[predictions1]] <- summary_results[[predictions1]] /
      summary_results[[weights]]

  }

  if (!is.null(predictions2)) {

    summary_results[[predictions2]] <- summary_results[[predictions2]] /
      summary_results[[weights]]

  }

  return(summary_results)

}







