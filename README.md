<img src="https://github.com/HedvigS/R_grambank_cookbook/assets/5327845/c666415a-4e8a-4be1-81f5-4ed29db62cb0" width="300" height="300">

# What this is

This repository contains a set of R functions that are useful for analysis of Grambank data, and other CLDF-datasets. Most of the functions are adapted from the code behind [the Grambank release paper of 2023](https://www.science.org/doi/10.1126/sciadv.adg6175). The code of the paper was also published as grambank-analysed on [Zenodo](https://zenodo.org/doi/10.5281/zenodo.7740821) and [GitHub](https://github.com/grambank/grambank-analysed/tree/v1.0/R_grambank). Part of that code has been been re-written here to produce more general functions that can easily be applied to future Grambank releases.

## Structure of content
Within this repository, functions are found in the directory called `functions` and examples in `example_scripts`. Each function-script comes with information on how arguments work etc, like functions in `R`-packages. The directory `example_scripts` contain `R`-scripts which illustrate sepcific functions. For example, the script `example_scripts/binarise.R` showcases the functions `functions/make_binary_ParameterTable`and `functions/make_binary_ValueTable`. In order to run the example scripts you need to set your working directory to `example_scripts`, as this is how the file-paths to `functions`and `fixed` are set up.

Detailed descriptions of the functions parameters and behaviour can be found in their respective scripts in the dir `functions`.

# What this is not

This is not a package, and can therefore not be installed as such. If you want to use the functions here, you can manually copy them over, clone the repos or fetch individual files as described below.

## Suggestion for how to fetch individual files

In `R` you can, if you'd like, fetch scripts from the internet to your local machine individually in this fashion:

```
# set up a folder where the scripts are stored
dir <- "R_grambank_cookbook"
if(!dir.exists(dir)){dir.create(dir)}

# specify the specific script you are interested in
script_url <- "https://raw.githubusercontent.com/HedvigS/R_grambank_cookbook/main/functions/add_family_name_column.R"

# check if the script has already been fetched, if not then download it.
if(!file.exists(
  paste0(dir, "/", basename(script_url)))){
script <- readLines(script_url, warn = F)

date_line <- paste0("# fetched: ", date(), "\n") #adding a line with the date of the download so that you know what version you've used in case there are changes.
script <- c(date_line, script)

writeLines(text = script, paste0(dir, "/", basename(script_url)))
}

```
# Who did what

The entire set of code of the Grambank release-paper and grambank-analysed was primarily written by Simon Greenhill, Sam Passmore, Hedvig Skirgård, Damián Blasi, Russell Dinnage, Hannah Haynie, Angela Chira and Luke Maurits. The functions here, in R_grambank_cookbook, are primarily written by Simon Greenhill and Hedvig Skirgård. Author(s) is/are specified for each function.

# Versioning

The content here will be continuously updated and periodically released with version tags. Git allows for accessing the state of the repos at a particular time via commit labels or tags. This can be used when cloning or accessing content via URLs. We _strongly_ encourage you to keep track of when you copied code, this makes it easier to identify issues later.

# Review
The functions in this repos are in the process of going through internal peer-review within the Department of Cultural and Linguistic Evolution at the Max Planck Insitute for Evolutionary Anthropology. The table below tracks which functions have been reviewed and by whom.

# Functions

|reviewed | Function | Short description | example scripts | Function author(s) |Review Pull Request | Reviewer | 
| -- | --| --| --| -- |-- |-- |
| ✅ |make_binary_ParameterTable.R| Takes the Grambank ParameterTable and adds binarised features for the multistate-features. | [example_scripts/binarise.R](https://github.com/HedvigS/R_grambank_cookbook/blob/main/example_scripts/binarise.R) |Hedvig Skirgård|[PR 7](https://github.com/dlce-eva/papers/pull/7)|Olena Shcherbakova|
|  ✅ |make_binary_ValueTable.R|Takes the GrambankValueTable and transforms mulistate feature values into binarised counter parts appropraitely.|[example_scripts/binarise.R](https://github.com/HedvigS/R_grambank_cookbook/blob/main/example_scripts/binarise.R)|Hedvig Skirgård|[PR 7](https://github.com/dlce-eva/papers/pull/7)|Olena Shcherbakova|
|  ✅ |make_theo_scores.R|Calculates metrics per language based on theoretical linguistics: fusion, informativity, gender/noun class, flexivity, locus of marking and word order. For more details, see supplementary material of the [Grambank release paper (2023)](https://www.science.org/doi/10.1126/sciadv.adg6175) |[example_scripts/theo_scores.R](https://github.com/HedvigS/R_grambank_cookbook/blob/main/example_scripts/theo_scores.R)|Hedvig Skirgård, Hannah Haynie and Olena Shcherbakova|[PR 7](https://github.com/dlce-eva/papers/pull/7)|Olena Shcherbakova|
| :x: |varcov.spatial.3D.R| Adjusted function based on geoR::varcov.spatial. If given Longitude and Latitude, it makes haversine distances that take into account curvature of the earth and handles the antimeridian correctly (unlike geoR::varcov.spatial|[example_scripts/example_varcov.spatial.3D.R](https://github.com/HedvigS/R_grambank_cookbook/blob/main/example_scripts/example_varcov.spatial.3D.R)|Original function: Paulo J. Ribeiro Jr. and Peter J. Diggle. Update: Hedvig Skirgård and Sam Passmore.|[PR 12](https://github.com/dlce-eva/papers/pull/12)||
| :x: |reduce_ValueTable_to_unique_glottocodes.R| Removes duplicate glottocodes in ValueTable for the same Parameter. Option for merging dialects into one entry. Read specification of method for merging closely.|[example_scripts/example_reduce_ValueTable_to_unique_glottocodes.R](https://github.com/HedvigS/R_grambank_cookbook/blob/main/example_scripts/example_reduce_ValueTable_to_unique_glottocodes.R)|Hedvig Skirgård| [PR 13](https://github.com/dlce-eva/papers/pull/13)|
| :x: |drop_duplicate_tips_random.R|Drops duplicate tips of a tree at random. Option for merging dialects to one tip (i.e. dropping all dialects but one).|[example_scripts/example_drop_duplicate_tips_random](https://github.com/HedvigS/R_grambank_cookbook/blob/main/example_scripts/example_drop_duplicate_tips_random.R)|Hedvig Skirgård|
| :x: |add_family_name_column.R|||Hedvig Skirgård| [PR 13](https://github.com/dlce-eva/papers/pull/13)|
| :x: |add_isolate_info.R|||Hedvig Skirgård|
| :x: |add_language_level_id_to_languages.R|||Hedvig Skirgård|
| :x: |crop_missing_data.R|||Hedvig Skirgård|
| :x: |as_grambank_wide.R|||Simon Greenhill|
| :x: |get_shared_features.R|||Simon Greenhill|
| :x: |get_values_for_clade.R|||Simon Greenhill|


# Differences between grambank/grambank-analysed and HedvigS/R_grambank_cookbok

The only difference of note between functions in [grambank/grambank-analysed](https://github.com/grambank/grambank-analysed/) (Grambank release paper of 2023) and here in HedvigS/R_grambank_cookbook concerns the treatment of missing data for the calculation of the theoretical scores. In grambank/grambank-analysed we did a subsetting of the entire dataset where we pruned away features and languages with large amounts of missing data. We then used this subset in several parts of the analysis, including PCA and calculation of theoretical scores. The function `make_theo_scores` in `R_scripts/make_theo_scores.R` instead prunes for missing data with respect to the specific features involved in each of the theoretical scores. Furthermore, the function allows the users to set a different cut-off (default = 0.75). The difference is very small in practice. Below are two scatterplot of two central theoretical scores, Fusion and Informativity. In each plot, the x-axis represents the newer way of computing the score (as in `HedvigS/R_grambank_cookbok`) and the y-axis the older (`grambank/grambank-analysed`).

For more details on the theoretical scores, see the supplmenetary material to [the Grambank release paper of 2023](https://www.science.org/doi/10.1126/sciadv.adg6175).

<img src="https://github.com/HedvigS/R_grambank_cookbook/assets/5327845/b187b78a-6175-4494-8ae5-499efa96887d" width="300" height="300">
<img src="https://github.com/HedvigS/R_grambank_cookbook/assets/5327845/9e9237d8-b7f5-4dc2-b82e-5d488e152965" width="300" height="300">
