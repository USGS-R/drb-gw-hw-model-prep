# drb-gw-hw-model-prep
Code repo to prepare groundwater and headwater-related datasets for modeling river temperature in the Delaware River Basin

**NOTE: This repository has been moved to an internal GitLab repository. The 
repository at https://github.com/USGS-R/drb-gw-hw-model-prep is considered archived 
and will no longer be actively monitored or maintained.**


This repo contains a targets pipeline for compiling datasets and a snakemake workflow for extracting simulated groundwater discharge from a MODFLOW groundwater model

## Extracting the catchment / reach attributes
The scripts to compile the catchment attributes use an R targets pipeline that is initialized with the "_targets.R" file in the main directory. The targets pipeline is divided into three phases that divide the workflow:

1. **`1_fetch`**: Download raw or pre-processed datasets that will be used to compile catchment/reach attributes for both sets of model experiments represented in this pipeline (nhd-downscaling and nhm-groundwater).
2. **`2a_process_nhd_downscaling`**: Process the catchment/reach attributes to the [NHDPlusv2](https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data#Download) scale. These attributes are compiled for those NHDPlusv2 flowlines that overlap the river network used by the [National Hydrologic Model (NHM)](https://www.sciencebase.gov/catalog/item/4f4e4773e4b07f02db47e234) and therefore many smaller streams in the DRB are excluded. The compiled NHDPlusv2-scale attributes are used to evaluate a neural network model that is pre-trained at the NHM-resolution but makes predictions on the more highly-resolved NHDPlusv2 network. 
3. **`2b_process_nhm_groundwater`**: Process the catchment/reach attributes to the NHM scale. The compiled dataset includes geomorphic, hydrologic, geologic, and land cover attributes hypothesized to influence groundwater-surface water interactions and/or the influence of groundwater discharge on surface water temperatures. The NHM groundwater attributes are used as input features to a neural network model that predicts daily water temperature for 456 NHM river segments in the DRB. 

Note that the pipeline depends on temperature data compiled as part of the DRB temperature forecasting project. Temperature data are contained in a [data release](https://www.sciencebase.gov/catalog/item/623e4418d34e915b67d7dd78) on ScienceBase.


## Extracting the MODFLOW outputs
The scripts to run the MODFLOW extraction are contained within the directory **`MODFLOW_extraction`**. They are python scripts and utilize a Snakemake workflow.

### To run the Snakemake workflow locally:

1. Install the dependencies in the `environment_MODFLOW_CONUS.yaml` file. With conda you can do this with `conda env create -f environment_MODFLOW_CONUS.yaml`
2. Activate your conda environment `source activate mf_data_extraction`
3. Edit the run configuration (including paths for I/O data) in the appropriate `config.yml` (either `config_MODFLOW_DRB.yml` or `config_MODFLOW_CONUS.yml`)
4. Run Snakemake with `snakemake --configfile config_MODFLOW_CONUS.yml -s Snakefile_MODFLOW_CONUS -j1`

### To run the Snakemake Workflow on TallGrass
1. Request a GPU allocation and start an interactive shell

        salloc -N 1 -t 4:00:00 -A <account>
        srun -A <account> --pty bash

2. Follow steps 1-4 above as you would to run the workflow locally. 

___

### Disclaimer
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided “AS IS.”
