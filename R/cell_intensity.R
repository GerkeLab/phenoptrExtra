#' Gathers Cell Intensity Readings
#'
#' Select IF intensity measures to associate with a phneotype.
#'
#' @seealso choose_measure
#' @param csd Cell Segmentation Data
#' @param ... Named variable pairs, e.g. `CD3 = choose_measure("Membrane", 520, "Mean")`
#'   or `CD3 = "Membrane opal 520 Mean"`
#' @param exclude Exclude intensity readings from cells with these `Phenotypes`.
#' @param summarize Should the output be summarized?
#' @return Summarized intensity measures with `group_var` column(s) and
#'   `Phenotype` (named in `...`), `measure`, and `intensity`.
#' @export
gather_cell_intensity <- function(
  csd,
  ...,
  exclude = "Undefined",
  summarize = length(group_vars(csd))
) {
  x <- csd %>%
    dplyr::filter(!Phenotype %in% exclude) %>%
    {
      if (!length(group_vars(csd))) {
        dplyr::select(., `Cell ID`, ...)
      } else {
        dplyr::select(., group_vars(csd), ...)
      }
    }

  if (!summarize) return(x)

  dplyr::summarize_all(x, dplyr::funs(min, mean, max, sd)) %>%
    tidyr::gather(Phenotype, intensity, -!!dplyr::group_vars(csd)) %>%
    tidyr::separate(Phenotype, c("Phenotype", "measure"), sep="_") %>%
    dplyr::arrange(!!!rlang::syms(group_vars(x)), measure)
}

#' Choose Measurement, Region and Frequency for Intensity Reading
#'
#' Helper function for selecting region, frequency, and measure for cell
#' intensity readings according to inForm column name output.
#'
#' @param region Region of interest, one of: "Entire Cell", "Membrane",
#'   "Nucleus"
#' @param freq Frequency of interest, integer or character, one of: 520, 540,
#'   570, 620, 650, 690.
#' @param measure Measurement of interest, one of: "Mean", "Min", "Max",
#'   "Std Dev", "Total".
#' @export
choose_measure <- function(
  region = c("Entire Cell", "Membrane", "Nucleus"),
  freq = c("520", "540", "570", "620", "650", "690"),
  measure = c("Mean", "Min", "Max", "Std Dev", "Total")
) {
  region <- match.arg(region)
  freq <- paste(freq)
  freq <- match.arg(freq)
  measure <- match.arg(measure)
  paste(region, "Opal", freq, measure)
}