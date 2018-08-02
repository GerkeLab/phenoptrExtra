
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phenoptrExtra

The goal of phenoptrExtra is to extend the
[phenoptr](https://perkinelmer.github.io/phenoptr/) package with
additional functionality for working with immunofluorescence data tables
exported from the PerkinElmer inForm software.

## Installation

You can install the unreleased development version of phenoptrExtra from
GitHub with:

``` r
devtools::install_github("gerkelab/phenoptrExtra")
```

## This Package is in the Development Stage

``` r
library(phenoptrExtra)
```

### What works now

#### Importing

``` r
import_all_csd()
```

> Import all cell segmentation files inside a folder or folders. Uses
> phenoptr::list\_cell\_seg\_files and phenoptr::read\_cell\_seg\_data
> to import the data sets, with additional but minor processing. All
> imported cell segmentation files are row-bound upon import.

    ex> import_all_csd("base_csd_dir")

#### Tidying and Preparing

``` r
merge_sample_info()
```

> Merge information about each sample into the CSD file. The sample info
> should be a data frame containing at least the column ‘Sample’.
> Columns that overlap with column names in the cell segmentation data
> are renamed with an appended ‘.info’.

    ex> merge_sample_info(csd, sample_info)

``` r
reorder_phenotype()
```

> Re-order the factor levels of the ‘.var’ column (default is
> ‘Phenotype’) by “alphabetical”, “frequency”, or “weighted”. The
> “Other” and “Undefined” levels are also automatically moved to the
> end of the ordering if present.

    ex> reorder_phenotype(csd, sort = "frequency")

#### Counting Cell Types

``` r
count_observations()
count_cells()
```

> Counts observations by ‘count\_by’ column, respecting grouping
> variables. When the input data frame is cell segmentation data and
> each row is an individual cell, then the output is cell counts.

    ex> csd %>% group_by(Sample) %>% count_observations(Phenotype)
    ex> csd %>% group_by(Sample, `Tissue Category`) %>% count_cells(Phenotype)

#### Working with IF Intensity

``` r
gather_cell_intensity()
```

> Select IF intensity measures to associate with a phneotype.

``` r
choose_measure()
```

> Helper function for selecting region, frequency, and measure for cell
> intensity readings according to inForm column name output.

``` r
csd %>% 
  group_by(Sample) %>% 
  gather_cell_intensity(
    `CD3`   = choose_measure("Membrane",    520, "Mean"), 
    `CD8`   = choose_measure("Membrane",    540, "Mean"),
    `FOXP3` = choose_measure("Entire Cell", 570, "Mean"),
    `PD1`   = choose_measure("Membrane",    620, "Mean"),
    `PD-L1` = choose_measure("Entire Cell", 650, "Mean"),
    `PCK`   = choose_measure("Entire Cell", 690, "Mean")
  )
```
