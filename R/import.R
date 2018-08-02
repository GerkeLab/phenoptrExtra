#' Import All Cell Segmentation Data Files
#'
#' Import all cell segmentation files inside a folder or folders. Uses
#' [phenoptr::list_cell_seg_files] and [phenoptr::read_cell_seg_data] to import
#' the data sets, with additional but minor processing. All imported cell
#' segmentation files are row-bound upon import.
#'
#' @section Addtional Processing:
#'
#' - Empty `Phenotype`s are relabelled as `"Undefined"`.
#' - `Phenotype` values other than `"Undefined"` or `"Other"` are made uppercase for consistency.
#' - `Phenotype` levels are converted to factors, wtih `"Other"` and `"Undefined"` last.
#' - The file extension `.im3` is removed from the value in the `Sample Name` column.
#' - The `Sample Name` column is renamed `Sample` for convenience.
#'
#' @examples
#' \dontrun{
#' import_all_csd("base_csd_dir")
#' }
#'
#' @param sample_dir Directory or directories to search for inForm Cell Segmentation Files
#' @param recursive Should the directories be searched recursively?
#' @return A single tibble containing all of the imported cell segmentation data
#' @export
import_all_csd <- function(sample_dir, recursive = TRUE) {
  sample_files <- phenoptr::list_cell_seg_files(sample_dir, recursive = recursive)

  purrr::map_df(sample_files, phenoptr::read_cell_seg_data) %>%
    dplyr::mutate(
      Phenotype = dplyr::case_when(
        Phenotype == "" ~ "Undefined",  # ** I added
        Phenotype == "Other" ~ "Other", # inForm reports
        TRUE ~ toupper(Phenotype)),
      Phenotype = factor(Phenotype, levels = sort(unique(Phenotype))),
      Phenotype = forcats::fct_relevel(Phenotype, "Other", "Undefined", after = Inf),
      `Sample Name` = stringr::str_replace(`Sample Name`, "\\.im3$", "")
    ) %>%
    dplyr::rename(Sample = `Sample Name`)
}

#' Merge Sample Information
#'
#' Merge information about each sample into the CSD file. The sample info should
#' be a data frame containing at least the column `Sample`. Columns that
#' overlap with column names in the cell segmentation data are renamed with an
#' appended `.info`.
#'
#' @examples
#' \dontrun{
#' merge_sample_info(csd, sample_info)
#' }
#'
#' @param csd Cell segmenation data from [import_all_csd()]
#' @param sample_info A dataframe containing at least the column `Sample` and
#'   additional columns containin information about each sample.
#' @export
merge_sample_info <- function(csd, sample_info) {
  stopifnot("Sample" %in% names(sample_info))
  if (any(duplicated(sample_info$Sample))) {
    rlang::warn(deparse(substitute(sample_info)), "has duplicate values in the `Sample` column.")
  }

  left_join(csd, sample_info, by = "Sample", suffix = c("", ".info"))
}
