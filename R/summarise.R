#' Summarise a column of a data.frame
#'
#' Take a particular variable (x) in a data.frame and summarise other variables
#' of interest by this variable (x). Useful if you want to summarise predictions
#' and observed values from a model, by certain explanatory variables.
#'
#' @param df \code{data.frame} containing variables of interest.
#' @param col \code{character} name of column to summarise by.
#' @param observed \code{character} name of observed column to summarise by col.
#' @param predictions \code{character} name of predictions column to summarise
#' by col, can be a vector of multiple columns.
#' @param weights \code{character} name of weights column to summarise by col.
#' @param ordered_bins \code{numeic} number of bins to discretise col into if it
#' is ordered. Default = \code{20}.
#' @param ordered_binning \code{character} type of discretisation to use if col
#' is ordered. Default = \code{"equal_width"}.
#'
#' @return
#' \code{tibble} containing weights, observed and predictions summarised by col.
#'
#' @details
#' For ordered columns (integer and numeric, ordered factors not yet supported)
#' the variable is discretised then used to summarise the observed, predictions
#' etc.
#'
#' @examples
#' set.seed(1)
#' data <- data.frame(a = c(runif(1000), rep(NA, 10)),
#'                    b = rnorm(1010),
#'                    c = rpois(1010, 3),
#'                    d = rnorm(1010),
#'                    e = runif(1010),
#'                    f = factor(sample(1010)),
#'                    g = as.character(sample(5, size = 1010, replace = T)))
#'
#' summarise_column(df = data,
#'                  col = "a",
#'                  observed = "b",
#'                  predictions = c("e","d"),
#'                  weights = "c")
#'
#' @export
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





#' Summarise a column of a data.frame using dplyr
#'
#' Take a particular variable (x) in a data.frame and summarise other variables
#' of interest by this variable (x). Useful if you want to summarise predictions
#' and observed values from a model, by certain explanatory variables.
#'
#' @param df \code{data.frame} containing variables of interest.
#' @param col \code{character} name of column to summarise by.
#' @param observed \code{character} name of observed column to summarise by col.
#' @param predictions \code{character} name of predictions column to summarise
#' by col, can be a vector of multiple columns.
#' @param weights \code{character} name of weights column to summarise by col.
#' @param ordered_bins \code{numeic} number of bins to discretise col into if it
#' is ordered. Default = \code{20}.
#' @param ordered_binning \code{character} type of discretisation to use if col
#' is ordered. Default = \code{"equal_width"}.
#'
#' @return
#' \code{tibble} containing weights, observed and predictions summarised by col.
#'
#' @examples
#' set.seed(1)
#' data <- data.frame(a = c(runif(1000), rep(NA, 10)),
#'                    b = rnorm(1010),
#'                    c = rpois(1010, 3),
#'                    d = rnorm(1010),
#'                    e = runif(1010),
#'                    f = factor(sample(1010)),
#'                    g = as.character(sample(5, size = 1010, replace = T)))
#'
#' summarise_col_dplyr(df = data,
#'                    col = "g",
#'                    observed = "b",
#'                    predictions = c("e","d"),
#'                    weights = "c")
#'
#' @export
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





#' Summary of data.frame values.
#'
#' For each requested variable return results from table() in a single
#' \code{data.frame}. If a given variable has more unique data values than
#' the cut off (\code{max_levels}), then the first, middle and last
#' \code{print_levels} will be returned instead. Results are returned in a
#' single \code{data.frame}.
#'
#' @param data \code{data.frame} containing columns to summarise.
#' @param cols \code{character} names of columns in data to summarise. Default =
#' \code{NULL} summarises all columns.
#' @param max_levels \code{numeric} maximum number of data values to report per
#' variable.
#' @param print_levels \code{numeric} for variables with more levels than
#' max_levels how many levels should be reported instead? Default value = 5
#' means than the first, middle and last 5 data values are output for these
#' variables.
#' @param csv_out \code{character} csv file to output results table to. Default
#' = \code{NULL} means no table is output.
#'
#' @return \code{data.frame} containing variable summaries and optionally csv
#' version too.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(a = rnorm(1000),
#'                  b = runif(1000),
#'                  c = sample(5),
#'                  d = as.factor(sample(5)))
#'
#' data_levels_summary(data = df,
#'                     cols = NULL,
#'                     max_levels = 20,
#'                     print_levels = 5,
#'                     csv_out = NULL)
#' @export
summarise_df_values <- function(data,
                                cols = NULL,
                                max_levels = 100,
                                print_levels = 5,
                                csv_out = NULL) {

  accepted_classes <- c('data.frame')

  if (!any(class(data) %in% accepted_classes)) {

    stop('data is not in expected classes; ', accepted_classes)

  }

  if (!is.null(cols)) {

    if (any(!cols %in% colnames(data))) {

      stop('the following columns are not in data; ',
           cols[which(!cols %in% colnames(data))])

    }

  } else {

    cols <- colnames(data)

  }

  cols_table <- lapply(data, function(x) data.frame(table(x, useNA = 'ifany')))

  na_row <- data.frame(x = NA, Freq = NA)

  for (column in cols) {

    n_levels <- nrow(cols_table[[column]])

    if (n_levels > max_levels) {

      bottom_indices <- 1:print_levels

      middle_start_index <- ceiling(n_levels / 2) - ceiling(print_levels / 2)

      middle_inidices <- middle_start_index:(middle_start_index + print_levels - 1)

      top_indices <- (n_levels - print_levels + 1):n_levels

      extra_rows <- max_levels - ((3 * print_levels) + 2)

      cols_table[[column]] <- rbind(cols_table[[column]][bottom_indices, ],
                                    na_row,
                                    cols_table[[column]][middle_inidices, ],
                                    na_row,
                                    cols_table[[column]][top_indices, ],
                                    na_row[rep(1, extra_rows), ])

    } else {

      extra_rows <- max_levels - n_levels

      cols_table[[column]] <- rbind(cols_table[[column]],
                                    na_row[rep(1, extra_rows), ])

    }

  }

  single_table <- do.call(cbind, cols_table)

  rownames(single_table) <- NULL

  colnames(single_table) <- as.vector(sapply(cols, function(x) paste0(x, c('.level', '.freq'))))

  if (!is.null(csv_out)) {

    write.table(x = single_table,
                file = csv_out,
                sep = ',',
                row.names = FALSE)

  }

  return(single_table)

}

