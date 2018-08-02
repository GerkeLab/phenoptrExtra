#' Re-Order Phenotype
#'
#' Re-order the factor levels of the `.var` column (default is `Phenotype`) by
#' "alphabetical", "frequency", or "weighted". The "Other" and "Undefined"
#' levels are also automatically moved to the end of the ordering if present.
#'
#' @examples
#' \dontrun{
#' reorder_phenotype(csd, sort = "frequency")
#' }
#'
#' @param x Input data frame
#' @param sort One of "alphabetical", "frequencuy", or "weighted". The latter
#'   two use [forcats::fct_infreq] and [forcats::fct_reorder2] respectively and
#'   any additional parameters in `...` are passed to these functions.
#' @param .var Optional unquoted name for the Phenotype column, default is
#'   `Phenotype`.
#' @export
reorder_phenotype <- function(
  x,
  sort = c("alphabetical", "frequency", "weighted"),
  ...,
  .var = Phenotype
) {
  .var    <- rlang::enquo(.var)
  .var_nm <- rlang::quo_name(.var)
  sort <- match.arg(sort)

  extra_levels <- intersect(c("Other", "Undefined"), unique(pull(x, !!.var)))

  x %>%
    mutate(
      !!.var_nm := switch(
        sort,
        "alphabetical" = factor(!!.var, levels = sort(unique(!!.var))),
        "frequency" = forcats::fct_infreq(!!.var),
        "weighted" = forcats::fct_reorder2(!!.var, ...),
        !!.var),
      !!.var_nm := forcats::fct_relevel(!!.var, extra_levels, after = Inf)
    )
}
