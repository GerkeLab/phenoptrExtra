#' Count Observations
#'
#' Counts observations by `count_by` column, respecting grouping variables.
#' When the input data frame is cell segmentation data and each row is an
#' individual cell, then the output is cell counts.
#'
#' @examples
#' \dontrun{
#' csd %>% group_by(Sample) %>% count_observations(Phenotype)
#' csd %>% group_by(Sample, `Tissue Category`) %>% count_cells(Phenotype)
#' }
#'
#' @return An ungrouped data frame with counts by grouping variables in addition
#'   to `count_by` column. Zero counts are included.
#'
#' @param x Input data frame
#' @param count_by Column to count by, unquoted
#' @param expected Expected values for the `count_by` columns. Providing these
#'   values ensures that 0 counts are included even if a value never appears
#'   in the input data.
#' @param exclude Excludes values in the `count_by` column entirely.
#' @param include_totals Adds a "{count_by} Total" value to the returned data.
#' @export
count_observations <- function(
  x,
  count_by = Phenotype,
  expected = NULL,
  exclude = NULL,
  include_totals = FALSE
) {
  count_by <- rlang::enquo(count_by)
  count_by_name <- rlang::quo_name(count_by)
  group_vars <- dplyr::group_vars(x)

  x <- dplyr::filter(x, !(!!count_by %in% exclude))

  expected_grouped_values <- unique_values(x, !!count_by, expected)

  if (include_totals) {
    total_cells <- x %>%
      dplyr::group_by(!!!rlang::syms(c(group_vars))) %>%
      dplyr::count() %>%
      dplyr::mutate(!!count_by_name := "Total")
  }

  ret <- x %>%
    dplyr::group_by(!!!rlang::syms(c(group_vars, count_by_name))) %>%
    dplyr::count() %>%
    # Ensure all expected group values and values show up
    dplyr::full_join(expected_grouped_values, by = c(group_vars, count_by_name))

  if (include_totals) ret <- dplyr::bind_rows(ret, total_cells)

  ret <- ret %>%
    dplyr::ungroup() %>%
    dplyr::mutate(n = ifelse(is.na(n), 0, n))

  if (!is.null(group_vars) && "Sample" %in% group_vars) {
    ret %>%
      tidyr::separate(Sample, c("Slide ID", "ROI"), sep = "_", remove = FALSE) %>%
      dplyr::arrange(`Slide ID`, ROI, !!count_by)
  } else {
    dplyr::arrange(ret, !!count_by)
  }
}

#' @describeIn count_observations Counts cells using `count_observations()` and
#'   returns a data frame with a `Cell Count` column in place of `n`.
#' @export
count_cells <- function(csd, count_by = Phenotype, expected = NULL, exclude = NULL, include_totals = FALSE) {
  count_by <- rlang::enquo(count_by)
  count_observations(csd, !!count_by, expected, exclude, include_totals) %>%
    rename(`Cell Count` = n)
}


#' Get Unique Values of a Column
#'
#' Gets all unique column values, where the expected values for `variable` can
#' be declared in advance, while respecting grouping variables. Thus, if the
#' input is a grouped data frame, the output is all unique combinations of the
#' grouping columns crossed with the `variable` (or `expected`) values.
#'
#' @param x Data frame (grouped or ungrouped)
#' @param variable Column of interest
#' @param expected Expected values for `variable`
#' @seealso [count_observations]
#' @export
unique_values <- function(x, variable, expected = NULL) {
  variable    <- rlang::enquo(variable)
  variable_nm <- rlang::quo_name(variable)
  group_vars <- dplyr::group_vars(x)

  unique_group_elements <- purrr::map(group_vars, ~ unique(x[[.]]))

  if (length(unique_group_elements)) names(unique_group_elements) <- group_vars

  if (is.null(expected)) {
    expected <- pull(x, !!variable) %>% unique() %>% paste()
  } else {
    var_unique <- pull(x, !!variable) %>% unique() %>% paste()
    not_in_variable <- setdiff(expected, var_unique)
    not_in_expected <- setdiff(var_unique, expected)
    if (length(not_in_variable)) {
      rlang::warn(paste0(
        "`expected` contains values not in `", variable_nm, "`: ",
        paste0('"', not_in_variable, '"', collapse = ", ")
      ))
    }
    if (length(not_in_expected)) {
      rlang::warn(paste0(
        "`", variable_nm, "` contains values not in `expected`: ",
        paste0('"', not_in_expected, '"', collapse = ", ")
      ))
    }
  }

  tidyr::crossing(!!!unique_group_elements, !!variable_nm := expected)
}
