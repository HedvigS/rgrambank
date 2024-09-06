<img src="https://github.com/HedvigS/R_grambank_cookbook/assets/5327845/c666415a-4e8a-4be1-81f5-4ed29db62cb0" width="300" height="300">

# What this is

This repository contains a set of R functions that are useful for analysis of Grambank data, and other CLDF-datasets. Most of the functions are adapted from the code behind [the Grambank release paper of 2023](https://www.science.org/doi/10.1126/sciadv.adg6175). The code of the paper was also published as grambank-analysed on [Zenodo](https://zenodo.org/doi/10.5281/zenodo.7740821) and [GitHub](https://github.com/grambank/grambank-analysed/tree/v1.0/R_grambank). Part of that code has been been re-written here to produce more general functions that can easily be applied to future Grambank releases and other CLDF-datasets.

# Installing package
The R-functions of this repository can be accessed as an R-package. The packages is not available via CRAN, instead you can install it directly here at GitHub.

```
library(remotes)
remotes::install_github("HedvigS/rgrambank")
library(rgrambank)
```

# Versioning

The content here will be continuously updated and periodically released with version tags. Git allows for accessing the state of the repos at a particular time via commit labels or tags. This can be used when cloning or accessing content via URLs and when installing the package withing R. We _strongly_ encourage you to keep track of versioning, this makes it easier to identify issues later.

```
library(remotes)
remotes::install_github("HedvigS/rgrambank", ref = "v1.0")
library(rgrambank)
```

## Structure of content
Within this repository, functions are found in the directory `R` and examples in `example_scripts`. The directory `example_scripts` contain `R`-scripts which illustrate sepcific functions. For example, the script `example_scripts/binarise.R` showcases the functions `rgrambank::make_binary_ParameterTable`and `rgrambank::make_binary_ValueTable`. This README contains a list of all the functions, linked to example scripts and with details on who wrote the function and who reviewed it. In order to run the example scripts you need to set your working directory to `example_scripts`, as this is how the file-paths to `R` and `fixed` are set up. The example scripts also rely on the package [rcldf by Simon Greenhill](https://github.com/SimonGreenhill/rcldf) for fetching Grambank and Glottobank-datasets.

Detailed descriptions of the functions parameters and behaviour can be found in their respective scripts in the dir `R` or accessed via the help-pages once the package is installed.

# Who did what

The entire set of code of the Grambank release-paper and grambank-analysed was primarily written by Simon Greenhill, Sam Passmore, Hedvig Skirgård, Damián Blasi, Russell Dinnage, Hannah Haynie, Angela Chira and Luke Maurits. The functions here, in rgrambank, are primarily written by Simon Greenhill and Hedvig Skirgård. Specific author(s) is/are specified for each function.


# Review
The functions in this repos go through internal peer-review within the Department of Cultural and Linguistic Evolution at the Max Planck Insitute for Evolutionary Anthropology. The table below tracks which functions have been reviewed and by whom.

# Functions

|reviewed | Function | Short description | example scripts | Function author(s) |Review Pull Request | Reviewer | 
| -- | --| --| --| -- |-- |-- |
| ✅ |make_binary_ParameterTable.R| Takes the Grambank ParameterTable and adds binarised features for the multistate-features. | [example_scripts/binarise.R](https://github.com/HedvigS/rgrambank/blob/main/example_scripts/binarise.R) |Hedvig Skirgård|[PR 7](https://github.com/dlce-eva/papers/pull/7)|Olena Shcherbakova|
|  ✅ |make_binary_ValueTable.R|Takes the GrambankValueTable and transforms mulistate feature values into binarised counter parts appropraitely.|[example_scripts/binarise.R](https://github.com/HedvigS/rgrambank/blob/main/example_scripts/binarise.R)|Hedvig Skirgård|[PR 7](https://github.com/dlce-eva/papers/pull/7)|Olena Shcherbakova|
|  ✅ |make_theo_scores.R|Calculates metrics per language based on theoretical linguistics: fusion, informativity, gender/noun class, flexivity, locus of marking and word order. For more details, see supplementary material of the [Grambank release paper (2023)](https://www.science.org/doi/10.1126/sciadv.adg6175) | [compare_new_old_theo_scores](https://github.com/HedvigS/rgrambank/blob/main/example_scripts/compare_new_old_theo_scores.R),  [example_make_theo_scores](https://github.com/HedvigS/rgrambank/blob/main/example_scripts/example_make_theo_scores.R)|Hedvig Skirgård, Hannah Haynie and Olena Shcherbakova|[PR 7](https://github.com/dlce-eva/papers/pull/7)|Olena Shcherbakova|
|  ✅ |varcov.spatial.3D.R| Adjusted function based on geoR::varcov.spatial. If given Longitude and Latitude, it makes haversine distances that take into account curvature of the earth and handles the antimeridian correctly (unlike geoR::varcov.spatial|[example_scripts/example_varcov.spatial.3D.R](https://github.com/HedvigS/rgrambank/blob/main/example_scripts/example_varcov.spatial.3D.R)|Original function: Paulo J. Ribeiro Jr. and Peter J. Diggle. Update: Hedvig Skirgård and Sam Passmore.|[PR 12](https://github.com/dlce-eva/papers/pull/12)|Angela Chira| 
|  ✅ |reduce_ValueTable_to_unique_glottocodes.R| Removes duplicate glottocodes in ValueTable for the same Parameter. Option for merging dialects into one entry. Read specification of method for merging closely.|[example_scripts/example_reduce_ValueTable_to_unique_glottocodes.R](https://github.com/HedvigS/rgrambank/blob/main/example_scripts/example_reduce_ValueTable_to_unique_glottocodes.R), [example_make_theo_scores](https://github.com/HedvigS/rgrambank/blob/main/example_scripts/example_make_theo_scores.R)|Hedvig Skirgård| [PR 13](https://github.com/dlce-eva/papers/pull/13)|Stephen Mann| 
|  ✅  |drop_duplicate_glottocode_tips.R|Drops tips which are mapped to the same glottocode of a tree at random. Option for merging dialects to one tip (i.e. dropping all dialects but one).|[example_scripts/example_drop_duplicate_glottocode_tips](https://github.com/HedvigS/rgrambank/blob/main/example_scripts/drop_duplicate_glottocode_tips.R)|Hedvig Skirgård| [PR 13](https://github.com/dlce-eva/papers/pull/13)|Stephen Mann| 
|  ✅  |crop_missing_data.R| Takes a CLDF ValueTable and removes parameters and languages with lots of missing data. The cut-offs are defined by the missing data in the full dataset and can be set to any value between 0 and 1. The pruning is not stepwise, i.e. it is not the case that parameters are pruned first and then languages based on the missingness after the first pruning. This can be a practical step before imputation as it reduces missing data to be imputed. For more advanced approaches, please see [annagraff/densify](https://github.com/annagraff/densify). |   [example_script_worldmap_rgb.R](https://github.com/HedvigS/rgrambank/blob/main/example_scripts/example_script_worldmap_rgb.R)  |Hedvig Skirgård|[PR 17](https://github.com/dlce-eva/papers/pull/17)| Enock Appiah Tieku|
|  ✅ |match_to_rgb | Takes a data-frame or matrix and maps three numeric columns to colors using RGB (RedGreenBlue).  |   [example_script_worldmap_rgb.R](https://github.com/HedvigS/rgrambank/blob/main/example_scripts/example_script_worldmap_rgb.R)  |Hedvig Skirgård and Damián Blasi|[PR 17](https://github.com/dlce-eva/papers/pull/17)| Enock Appiah Tieku|
|  ✅  | basemap_pacific_center |  Function that generates a base-layer in ggplot with a Pacific-centered worldmap with a van der Grinten project. The function outputs both a basemap ggplot layer and a data-frame which is the combination of the two input data-frames (LongLatTable and DataTable) with adjusted longitude to match the basemap layer.  |   [example_script_worldmap_rgb.R](https://github.com/HedvigS/rgrambank/blob/main/example_scripts/example_script_worldmap_rgb.R)  |Hedvig Skirgård|[PR 17](https://github.com/dlce-eva/papers/pull/17)| Enock Appiah Tieku|
|  ✅ |combine_ValueTable_LanguageTable.R|Combines CLDF ValueTable and LanguageTable in a practical manner useful for many Grambank analysis purposes| [example_enrich_language_table.R](https://github.com/HedvigS/rgrambank/blob/main/example_scripts/example_enrich_language_table.R) |Hedvig Skirgård| [PR 17](https://github.com/dlce-eva/papers/pull/17)| Enock Appiah Tieku|
|  ✅  |add_family_name_column.R| Adds the column "Family_name" to a LanguageTable, using Family_ID and Name. | [example_enrich_language_table.R](https://github.com/HedvigS/rgrambank/blob/main/example_scripts/example_enrich_language_table.R) |Hedvig Skirgård| [PR 17](https://github.com/dlce-eva/papers/pull/17)| Enock Appiah Tieku|
|  ✅ |add_isolate_info.R|Marks dialects of isolates as isolates as well in the column "Is_Isolate" and fills in Family_ID.| [example_enrich_language_table.R](https://github.com/HedvigS/rgrambank/blob/main/example_scripts/example_enrich_language_table.R)|Hedvig Skirgård|[PR 17](https://github.com/dlce-eva/papers/pull/17)| Enock Appiah Tieku|

# Differences between grambank/grambank-analysed and HedvigS/rgrambank

There are a few minor differences between the code in [grambank/grambank-analysed](https://github.com/grambank/grambank-analysed/) (Grambank release paper of 2023) and the functions here in HedvigS/rgrambank. They are all listed here:

## Theoretical scores
This difference concerns the treatment of missing data for the calculation of the theoretical scores. In grambank/grambank-analysed we did a subsetting of the entire dataset where we pruned away features and languages with large amounts of missing data considering all features and languages at once. We then used this subset in several parts of the analysis, including PCA and calculation of theoretical scores. The function here in  `make_theo_scores` in `R_scripts/make_theo_scores.R` instead prunes for missing data with respect to the specific features involved in each of the theoretical scores. Furthermore, the function allows the users to set a different cut-off (default = 0.75). The difference is very small in practice. Below are two scatterplots of two central theoretical scores, Fusion and Informativity. In each plot, the x-axis represents the newer way of computing the score (as in `HedvigS/rgrambank`) and the y-axis the older (`grambank/grambank-analysed`).

For more details on the theoretical scores, see the supplementary material to [the Grambank release paper of 2023](https://www.science.org/doi/10.1126/sciadv.adg6175).

<img src="https://github.com/HedvigS/R_grambank_cookbook/assets/5327845/b187b78a-6175-4494-8ae5-499efa96887d" width="300" height="300">
<img src="https://github.com/HedvigS/R_grambank_cookbook/assets/5327845/9e9237d8-b7f5-4dc2-b82e-5d488e152965" width="300" height="300">


## Spatial Variance-Co-Variance calculations
In order to model spatial auto-correlation in regression models it is necessary to compute a variance-co-variance matrix (vcv) of the data points. This can be done using a Matérn decay function with the function `varcov.spatial` in the R-package geoR. However, the package `geoR` can be difficult to install and in addition, the function uses `stats::dist` to compute distances which is inappropriate for geographic points (see reasoning [here](https://hedvigsr.tumblr.com/post/730257310587453440/dont-use-statsdist-for-geographic-distances-it)). The code at [grambank/grambank-analysed](https://github.com/grambank/grambank-analysed/) uses the `geoR::varcov.spatial` as is, copying over the source code  over into a separate script in order to avoid the installation problems. The issue with the underlying distances was discovered later. The impact on the analysis was negligent, but all the same we have created an updated version of `geoR::varcov.spatial` in this repository which uses `fields::rdist.earth` for computing the distances given longitude and latitude. Unlike `stats:.dist`, `fields::rdist.earth` takes into account the antimeridian correctly and the curvature of the earth. The new function is called `varcov.spatial.3D` and is almost identical to the original function created for `geoR` by Paulo J. Ribeiro Jr. and Peter J. Diggle which was used in the Grambank release paper, with the difference of `stats::dist` -> `fields::rdist.earth`.

> [!TIP]
> Good to know: you can give `geoR:varcov.spatial` or `varcov.spatial.3D` distances directly, instead of coordinates. You can for example calculate distances over cost-surfaces and create a spatial vcv of that. The difference discussed above only concerns when you give the function spatial coordinates and it computes haversine distances for you.
