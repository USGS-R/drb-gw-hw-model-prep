library(targets)

options(tidyverse.quiet = TRUE, timeout = 300)
tar_option_set(packages = c("raster","sbtools","sf","nhdplusTools", "terra", 
                            "tidyverse", "arrow", "tidync", "ncdf4", "reticulate")) 

# create sub-directories for selected input datasets
dir.create("1_fetch/out/nhdv2_attr", showWarnings = FALSE)
dir.create("1_fetch/out/mcmanamay2018", showWarnings = FALSE)
dir.create("1_fetch/out/drb_facet", showWarnings = FALSE)

source("1_fetch.R")
source("2a_process_nhd_downscaling.R")
source("2b_process_nhm_groundwater.R")

# Define crs
crs <- 5070

# Define the url for the NHGFv1 to NHDv2 crosswalk 
# Note that there are different versions of the crosswalk table that might be best suited
# to different use cases (e.g. retain divergences? retain zero-area flowlines?)
# see https://github.com/USGS-R/drb-network-prep/commit/3637931f5a17469a4234eaed3d20ed44ba45958d
GFv1_NHDv2_xwalk_url <- paste0("https://raw.githubusercontent.com/USGS-R/drb-network-prep/",
                               "3637931f5a17469a4234eaed3d20ed44ba45958d",
                               "/2_process/out/GFv1_NHDv2_xwalk.csv")
GFv1_NHDv2_xwalk_dendritic_url <- paste0("https://raw.githubusercontent.com/USGS-R/drb-network-prep/",
                                         "3637931f5a17469a4234eaed3d20ed44ba45958d",
                                         "/2_process/out/GFv1_NHDv2_xwalk_dendritic.csv")

## nhd parent id 
# pulled from https://www.sciencebase.gov/catalog/item/5728d6ace4b0b13d3918a992
nhd_statsgo_parent_sbid <- '5728d6ace4b0b13d3918a992'

## Depth to bedrock data source (required form submission to obtain)
## original source: http://globalchange.bnu.edu.cn/research/dtb.jsp#download 
## To successfully run this pipeline, this data must be manually copied locally to 
## 1_fetch/in. Clipped drb version already stored in 1_fetch/in on caldera. 
Shangguan_dtb_cm_250m_clip_path <- '1_fetch/in/Shangguan_dtb_cm_250m_clip/w001001.adf'

# Return the complete list of targets
c(p1_targets_list, p2a_targets_list, p2b_targets_list)


