#install.packages("remotes")
library(remotes)

#install_github("SimonGreenhill/rcldf", dependencies = TRUE, ref = "v1.2.0")
library(rcldf)

#remotes::install_version("tidyverse", version = "2.0.0", repos = "http://cran.us.r-project.org")
library(tidyverse)

#devtools::install_version(package = "spam", version = "2.10.0")
library(spam)

#remotes::install_version("fields", version = "14.1", repos = "http://cran.us.r-project.org",  dependencies = F)
library(fields, attach.required = F)

#devtools::install_version("reshape2", version = "1.4.4", repos = "http://cran.us.r-project.org")
library(reshape2)

#devtools::install_github("HedvigS/rgrambank", ref = "v1.0")
library(rgrambank)

glottolog_cldf_object <- rcldf::cldf("https://zenodo.org/records/10804582/files/glottolog/glottolog-cldf-v5.0.zip", load_bib = F)

coords <- glottolog_cldf_object$tables$LanguageTable %>% 
  filter(ID == "fiji1242" |
           ID == "bisl1239"|
           ID == "samo1305") %>% 
  column_to_rownames("ID") %>% 
  dplyr::select(Longitude, Latitude) %>% 
  as.matrix()

#illustrating the differences in base::dist and fields::rdist.earth with just plain distance calculations, without vcvs as in varcov.spatial
dists_2D <- stats::dist(coords) %>% as.matrix() 
dists_3D <- fields::rdist.earth(x1 = coords, x2 = coords, miles = F) 

# we would expect the distance between fiji1242 and bisl1239 to be similar to that of fiji1242 and samo1305. See the following tumblr post for visual illustration of why: https://hedvigsr.tumblr.com/post/730257310587453440/dont-use-statsdist-for-geographic-distances-it

fiji_dists_2D <- abs(dists_2D["fiji1242", "bisl1239"] - dists_2D["fiji1242", "samo1305"])

fiji_dists_3D <- abs(dists_3D["fiji1242", "bisl1239"] - dists_3D["fiji1242", "samo1305"]) / 100 # we need to divide by 100 to make the scale more similar to that of the state::dist

if(!(fiji_dists_2D < 100) &
fiji_dists_3D < 100){
  cat(paste0("The distances between fiji1242, bisl1239 and samo1305 are much different in the 2D dist compared to the 3D dists.\n"))
  }

#sigma and kappa settings from grambank release paper and Dinnage et al (2020)
kappa = 2 # smoothness parameter as recommended by Dinnage et al. (2020)
sigma = c(1, 1.15) # Sigma parameter. First value is not used. 

#computing the co-variance with the new varcov.spatial function but giving the dists directly as from stats::dist, i.e. the 2D approach
vcv_2D <- rgrambank::varcov.spatial.3D(dists.lowertri = as.vector(dist(coords)), 
                            cov.pars  = sigma, kappa = kappa)$varcov

colnames(vcv_2D) <- rownames(vcv_2D) <- rownames(coords)

#computing the co-variance with the new function giving it the coordinates directly, which it will pass to fields::rdist.earth.
vcv_3D <- rgrambank::varcov.spatial.3D(coords = coords, 
                            cov.pars  = sigma, kappa = kappa)$varcov

colnames(vcv_3D) <- rownames(vcv_3D) <- rownames(coords)

fiji_vcv_2D <- abs(vcv_2D["fiji1242", "bisl1239"] - vcv_2D["fiji1242", "samo1305"])

fiji_vcv_3D <- abs(vcv_3D["fiji1242", "bisl1239"] - vcv_3D["fiji1242", "samo1305"])  

  cat(paste0("the co-variation betweenfiji1242, bisl1239 and samo1305 are different in the 2D dist compared to the 3D dists.\n", 
           "2D: ",   format(fiji_vcv_2D, scientific = F), "\n",
           "3D: ",   format(fiji_vcv_3D, scientific = F), "\n"
  ))
  

