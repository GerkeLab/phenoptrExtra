#' Gathers Cell Intensity Readings
#'
#' Select IF intensity measures to associate with a phneotype.
#'
#' @seealso choose_measure
#' @param csd Cell Segmentation Data
#' @param ... Named variable pairs, e.g. `CD3 = choose_measure("Membrane", 520, "Mean")`
#'   or `CD3 = "Membrane Opal 520 Mean"`
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

#' Pick Measurement, Region and Frequency for Intensity Reading
#'
#' Helper function for selecting region, frequency, and measure for cell
#' intensity readings according to inForm column name output.
#'
#' @examples
#' pick_measure("membrane", 520, "mean")
#' pick_measure(freq = 690, .sep = "_")
#'
#' @param region Region of interest, one of: "Entire Cell", "Membrane",
#'   "Nucleus"
#' @param freq Frequency of interest, integer or character, one of: 520, 540,
#'   570, 620, 650, 690.
#' @param measure Measurement of interest, one of: "Mean", "Min", "Max",
#'   "Std Dev", "Total".
#' @param .sep <chr> Separator between words in column names, default is space.
#' @export
pick_measure <- function(
  region = c("Entire Cell", "Membrane", "Nucleus"),
  freq = c("520", "540", "570", "620", "650", "690"),
  measure = c("Mean", "Min", "Max", "Std Dev", "Total"),
  .sep = " "
) {
  region  <- tools::toTitleCase(region)
  measure <- tools::toTitleCase(measure)
  region  <- match.arg(region)
  freq    <- paste(freq)
  freq    <- match.arg(freq)
  measure <- match.arg(measure)
  paste(region, "Opal", freq, measure, sep = .sep)
}

choose_measure <- function(...) {
  .Deprecated("pick_measure", msg = "choose_measure() is deprecated, use pick_measure() instead.")
  pick_measure(...)
}

#' Color Palette by Wavelength
#'
#' Uses colors chosen from <https://www.nature.com/articles/nmeth.1618> as
#' suggested in <https://petebankhead.gitbooks.io/imagej-intro/content/chapters/colors/colors.html>.
#'
#' @param wl Wavelength(s) or column name(s) containing wavelength.
#' @examples
#' color_measure()
#' color_measure("Entire Cell Opal 540 Mean")
#' color_measure(540)
#'
#' @export
color_measure <- function(wl = NULL) {
  colors <- c(
    "520" = rgb(54, 255, 0, max = 255),
    "540" = rgb(129, 255, 0, max = 255),
    "570" = rgb(255, 255, 0, max = 255),
    "620" = rgb(255, 119, 0, max = 255),
    "650" = rgb(255, 0, 0, max = 255),
    "690" = rgb(0.5, 0.5, 0.5)
  )
  colors <- c(
    "520" = rgb(230, 159,   0, max = 255), # orange
    "540" = rgb(  0, 158, 115, max = 255), # bluish green
    "570" = rgb(240, 228,  66, max = 255), # yellow
    "620" = rgb(  0, 114, 178, max = 255), # blue
    "650" = rgb(204, 121, 167, max = 255), # reddish purple
    "690" = rgb(213,  94,   0, max = 255) # vermillion
  )
  if (is.null(wl)) return(colors)
  wl_number <- gsub(".+([56][24579]0).+", "\\1", paste(wl))
  setNames(colors[paste(wl_number)], wl)
}
