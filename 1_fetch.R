source("1_fetch/src/download_nhdplus_flowlines.R")
source("1_fetch/src/download_nhdplus_catchments.R")
source("1_fetch/src/fetch_nhdv2_attributes_from_sb.R")
source("1_fetch/src/download_file.R")
source("1_fetch/src/read_netcdf.R")

p1_targets_list <- list(
  
  # Important! This pipeline uses two versions of the NHM-NHDv2 crosswalk table
  # for different purposes. All targets pertaining to the "dendritic" version
  # of the crosswalk table will include the string "dendritic" in the name.
  # Crosswalk #1: Read in the NHM - NHDv2 crosswalk file that contains all 
  # NHDPlusv2 COMIDs in the DRB (including divergent reaches and reaches 
  # without catchments)
  tar_target(
    p1_GFv1_NHDv2_xwalk,
    read_csv(GFv1_NHDv2_xwalk_url,col_types = cols(.default = "c"))
  ),
  
  # Reshape crosswalk table to return all COMIDs that drain to each NHM segment.
  tar_target(
    p1_drb_comids_all_tribs, 
    p1_GFv1_NHDv2_xwalk %>%
      select(PRMS_segid, segidnat, comid_cat) %>% 
      tidyr::separate_rows(comid_cat,sep=";") %>% 
      rename(COMID = comid_cat,
             seg_id_nat = segidnat)
  ),
  
  # Reshape crosswalk table to return all COMIDs that overlap each NHM segment.
  tar_target(
    p1_drb_comids_segs, 
    p1_GFv1_NHDv2_xwalk %>%
      select(PRMS_segid, segidnat, comid_seg) %>% 
      tidyr::separate_rows(comid_seg,sep=";") %>% 
      rename(COMID = comid_seg,
             seg_id_nat = segidnat)
  ),
  
  # Reshape crosswalk table to return all COMIDs that represent the downstream
  # end of each NHM segment.
  tar_target(
    p1_drb_comids_down,
    p1_GFv1_NHDv2_xwalk %>%
      select(PRMS_segid, segidnat, comid_down) %>% 
      tidyr::separate_rows(comid_down,sep=";") %>% 
      rename(COMID = comid_down,
             seg_id_nat = segidnat)    
  ),
  
  # Use crosswalk table to fetch all NHDv2 reaches in the DRB. These COMIDs 
  # should be used for preparing feature data, including aggregating feature 
  # values from the NHD-scale to the NHM-scale and/or for deriving feature 
  # values from raster data.
  tar_target(
    p1_nhd_reaches,
    download_nhdplus_flowlines(p1_drb_comids_all_tribs$COMID,
                               crs = crs)
  ),
  
  # Use crosswalk table to fetch just the NHDv2 reaches that overlap the NHM network.
  tar_target(
    p1_nhd_reaches_along_NHM,
    download_nhdplus_flowlines(p1_drb_comids_segs$COMID,
                               crs = crs)
  ),
  
  # Download all NHDPlusv2 catchments in the DRB (may take awhile to run)
  tar_target(
    p1_nhd_catchments,
    get_nhdplusv2_catchments(comid = p1_nhd_reaches$comid,
                             crs = crs)
  ),
  
  # Dissolve NHDPlusv2 catchments to create a single catchment polygon for
  # each NHM segment (analogous to HRU). Note that we are aggregating by
  # seg_id_nat, so split subsegid's (e.g. 3_1, 3_2; 8_1, 8_2; 152_1, 152_2)
  # will be combined. We end up with 455 catchments instead of 456 because
  # one very short segment, subsegid 287_1/seg_id_nat 1721, does not have an 
  # NHDPlusv2 catchment.
  tar_target(
    p1_nhm_catchments_dissolved,
    left_join(x = mutate(p1_nhd_catchments, COMID = as.character(COMID)),
              y = p1_drb_comids_all_tribs,
              by = 'COMID') %>%
      group_by(seg_id_nat) %>%
      dplyr::summarize(geometry = sf::st_union(geometry))
  ),
  
  # Crosswalk #2: Read in the NHM - NHDv2 crosswalk file that corresponds to 
  # the *dendritic* network only (i.e., divergent reaches have been omitted). 
  # This version of the crosswalk table was used to build the network distance
  # matrix in drb-network-prep, and so includes the COMIDs that we will make 
  # predictions on in the NHD-downscaling set of experiments. 
  tar_target(
    p1_GFv1_NHDv2_xwalk_dendritic,
    read_csv(GFv1_NHDv2_xwalk_dendritic_url,col_types = cols(.default = "c"))
  ),
  
  # Reshape dendritic crosswalk table to return all COMIDs that overlap each 
  # NHM segment
  tar_target(
    p1_drb_comids_dendritic_segs, 
    p1_GFv1_NHDv2_xwalk_dendritic %>%
      select(PRMS_segid, segidnat, comid_seg) %>% 
      tidyr::separate_rows(comid_seg,sep=";") %>% 
      rename(COMID = comid_seg,
             seg_id_nat = segidnat)
  ),
  
  # Use crosswalk table to fetch just the dendritic NHDv2 reaches that overlap
  # the NHM network. For now, crs is set to 4326. Note if crs is set to a value
  # other than 4326, an error will get thrown when the `estimate_mean_width()` 
  # function is called in 2_process.R. For more information, see:
  # https://github.com/USGS-R/drb-gw-hw-model-prep/pull/35#discussion_r966072518
  tar_target(
    p1_dendritic_nhd_reaches_along_NHM,
    download_nhdplus_flowlines(p1_drb_comids_dendritic_segs$COMID, 
                               crs = 4326)
  ),
  
  # Download temperature site locations from ScienceBase:
  # https://www.sciencebase.gov/catalog/item/623e54c4d34e915b67d83580
  # Note that we've experienced sporadic issues with downloading this file from
  # ScienceBase, where the file will appear to download successfully but the zip
  # file cannot be unzipped when building downstream targets. If this happens, 
  # the following warning message will appear "In unzip(zipfile = 
  # p1_drb_temp_sites_zip, exdir = "1_fetch/out",: error 1 in extracting from zip
  # file" and p1_drb_temp_sites_shp will not build successfully. This problem has 
  # been resolved by waiting a waiting a few hours and trying again. 
  tar_target(
    p1_drb_temp_sites_zip,
    download_sb_file(sb_id = "623e54c4d34e915b67d83580",
                     file_name = "study_monitoring_sites.zip",
                     out_dir = "1_fetch/out"),
    format = "file"
  ),

  # Unzip downloaded temperature site locations and save shp file in 1_fetch/out
  tar_target(
    p1_drb_temp_sites_shp,
    {
      file_names <- unzip(zipfile = p1_drb_temp_sites_zip, 
                          exdir = "1_fetch/out", 
                          overwrite = TRUE)
      grep(".shp",file_names, value = TRUE, ignore.case = TRUE)
    },
    format = "file"
  ),
  
  # Read in temperature site locations
  tar_target(
    p1_drb_temp_sites_sf,
    sf::st_read(p1_drb_temp_sites_shp)
  ),
  
  # Download unaggregated temperature observations from ScienceBase:
  # https://www.sciencebase.gov/catalog/item/623e550ad34e915b67d8366e
  tar_target(
    p1_drb_temp_obs_zip,
    download_sb_file(sb_id = "623e550ad34e915b67d8366e",
                     file_name = "unaggregated_temperature_observations_drb.zip",
                     out_dir = "1_fetch/out"),
    format = "file"
  ),

  # Unzip unaggregated temperature observations and save csv file in 1_fetch/out
  tar_target(
    p1_drb_temp_obs_csv,
    unzip(zipfile = p1_drb_temp_obs_zip, 
          overwrite = TRUE, 
          exdir = "1_fetch/out"),
    format = "file"
  ),
  
  # Read in temperature observations
  tar_target(
    p1_drb_temp_obs,
    read_csv(p1_drb_temp_obs_csv, col_types = list(seg_id_nat = "c"))
  ),
  
  # Download DRB distance matrix from ScienceBase:
  # https://www.sciencebase.gov/catalog/item/623e5587d34e915b67d83806
  tar_target(
    p1_nhm_distance_matrix_csv,
    download_sb_file(sb_id = "623e5587d34e915b67d83806",
                     file_name = "distance_matrix_drb.csv",
                     out_dir = "1_fetch/out"),
    format = "file"
  ),
  
  # Read in DRB distance matrix:
  tar_target(
    p1_nhm_distance_matrix,
    read_csv(p1_nhm_distance_matrix_csv, show_col_types = FALSE)
  ),
  
  # Download PRMS-SNTemp model driver data from ScienceBase:
  # https://www.sciencebase.gov/catalog/item/623e5587d34e915b67d83806
  tar_target(
    p1_sntemp_input_output_zip,
    download_sb_file(sb_id = "623e5587d34e915b67d83806",
                     file_name = "uncal_sntemp_input_output.zip",
                     out_dir = "1_fetch/out"),
    format = "file"
  ),
  
  # Unzip PRMS-SNTemp model driver data and save netcdf to 1_fetch/out
  tar_target(
    p1_sntemp_input_output_nc,
    unzip(zipfile = p1_sntemp_input_output_zip, 
          overwrite = TRUE, 
          exdir = "1_fetch/out"),
    format = "file"
  ),
  
  # Read in PRMS-SNTemp model driver data
  tar_target(
    p1_sntemp_input_output,
    read_netcdf(p1_sntemp_input_output_nc)
  ),
  
  # Read in meteorological data aggregated to NHDPlusV2 catchments for the DRB
  # (prepped in https://github.com/USGS-R/drb_gridmet_tools). Note that the DRB
  # met data file must be stored in 1_fetch/in. If working outside of tallgrass/
  # caldera, this file will need to be downloaded from the PGDL-DO project's S3
  # bucket and manually placed in 1_fetch/in.
  tar_target(
    p1_drb_nhd_gridmet,
    "1_fetch/in/drb_climate_2022_06_14.nc",
    format = "file"
  ),
  
  # Download ref-gages v0.6 to help QC matching NWIS sites to NHDv2 flowlines
  tar_target(
    p1_ref_gages_geojson,
    download_file(url = 'https://github.com/internetofwater/ref_gages/releases/download/v0.6/usgs_nldi_gages.geojson',
                  fileout = "1_fetch/out/ref_gages.geojson",
                  mode = "wb", quiet = TRUE)
  ),
  
  # Read in ref-gages v0.6 as an sf object
  tar_target(
    p1_ref_gages_sf,
    sf::st_read(p1_ref_gages_geojson, quiet = TRUE) %>%
      mutate(COMID_refgages = as.character(nhdpv2_COMID))
  ),
  
  tar_target(
    p1_sb_attributes_csv,
    '1_fetch/in/nhdv2_attributes_from_sciencebase.csv',
    format = 'file'
  ),
  
  # Read in and format segment/catchment attribute datasets from ScienceBase 
  # note: use tar_group to define row groups based on ScienceBase ID; 
  # row groups facilitate branching over subsets of the sb_attributes 
  # table in downstream targets
  tar_target(
    p1_sb_attributes,
    read_csv(p1_sb_attributes_csv, show_col_types = FALSE) %>%
      # parse sb_id from https link 
      mutate(sb_id = str_extract(SB_link,"[^/]*$")) %>%
      group_by(sb_id) %>%
      tar_group(),
    iteration = "group"
  ),
  
  # Map over desired attribute datasets to download NHDv2 attribute data
  # Note that a txt file within the BASIN_CHAR group returns a warning that
  # it's skipped over. That is OK because the data is also included in a zip
  # file on ScienceBase. The zip file is the version of the data that's being
  # downloaded and saved here.
  tar_target(
    p1_sb_attributes_downloaded_csvs,
    fetch_nhdv2_attributes_from_sb(vars_item = p1_sb_attributes, 
                                   save_dir = "1_fetch/out/nhdv2_attr", 
                                   comids = p1_nhd_reaches$comid, 
                                   delete_local_copies = TRUE),
    pattern = map(p1_sb_attributes),
    format = "file"
  ),

  # Track depth to bedrock raster dataset in 1_fetch/in.
  # Note: If you do not have the data in Shangguan_drb_cm_250m_clip_path, 
  # you must grab it from the project folder. The dataset on caldera has 
  # already been clipped to the DRB. Original data source: 
  # http://globalchange.bnu.edu.cn/research/dtb.jsp. 
  tar_target(
    p1_depth_to_bedrock_tif,
    Shangguan_dtb_cm_250m_clip_path,
    format = "file"
  ),
  
  # Track crosswalk file created by J. Barclay that translates surficial 
  # material categorizations from Soller et al. 2009 (https://pubs.usgs.gov/ds/425/)
  # into a binary variable that indicates whether each classification
  # corresponds with coarse sediments. 
  tar_target(
    p1_soller_coarse_sediment_xwalk_csv,
    "1_fetch/in/SollerEtAl_SurficialMat_CoarseSed_UnitNames.csv",
    format = "file"
  ),
  
  # Dataset build in consultation with GW subject matter expert. 
  tar_target(  
    p1_soller_coarse_sediment_xwalk,
    read_csv(p1_soller_coarse_sediment_xwalk_csv,
             col_types = 'c'
    )
  ),
  
  # load in Soller et al. 2009's surficial material dataset
  tar_target(
    p1_soller_surficial_mat_zip,
             download_file("https://pubs.usgs.gov/ds/425/USGS_DS_425_SHAPES.zip",
                          fileout = "1_fetch/out/USGS_DS_425_SHAPES.zip", 
                          mode = "wb", quiet = TRUE),
             format = "file"
  ),
  
  # Unzip downloaded soller et al surficial material shps saved in 1_fetch/out
  tar_target(
    p1_soller_surficial_mat_shp,
    {
      file_names <- unzip(zipfile = p1_soller_surficial_mat_zip, 
                          exdir = "1_fetch/out", 
                          overwrite = TRUE)
      # grep-ing the exact file because there are multiple .shp files
      # in this compressed downloaded folder
      grep("Surficial_materials.shp$",file_names, value = TRUE, ignore.case = TRUE)
    },
    format = "file"
  ),
  
  # Read in soller shp file as sf object + filtering using coarse sediment xwalk
  # dataset + clipping to drb bbox
  tar_target(
    p1_soller_coarse_sediment_drb_sf,
    st_read(p1_soller_surficial_mat_shp,
            quiet = TRUE) %>% 
      st_transform(crs = crs) %>% 
      left_join(.,
                p1_soller_coarse_sediment_xwalk,
                by = c('UNIT_NAME' = 'Surficial Material Name')) %>% 
      filter(CoarseSediments == 1) %>% 
      st_crop(., p1_nhd_reaches_along_NHM %>%
                st_bbox()
      )
  ),
  
  # Download McManamay and DeRolph stream classification dataset that contains 
  # channel confinement data for each NHDPlusV2 flowline within CONUS:
  # https://doi.org/10.6084/m9.figshare.c.4233740.v1 (accompanying paper in
  # Scientific Data, https://doi.org/10.1038/sdata.2019.17).
  tar_target(
    p1_confinement_mcmanamay_zip,
    download_file(url = "https://springernature.figshare.com/ndownloader/files/13089668",
                  fileout = "1_fetch/out/mcmanamay2018/mcmanamay_confinement.zip",
                  mode = "wb", quiet = TRUE),
    format = "file"
  ),
  
  # Subset McManamay and DeRolph stream classification dataset to csv file
  # containing channel confinement categories for the Eastern U.S. region.
  tar_target(
    p1_confinement_mcmanamay_csv,
    {
      file_names <- unzip(zipfile = p1_confinement_mcmanamay_zip, 
                          exdir = "1_fetch/out/mcmanamay2018", 
                          overwrite = TRUE)
      # select "East" region which includes the Delaware River Basin
      grep("Valley_Confinement/East_VC.csv",file_names, value = TRUE, ignore.case = TRUE)
    }, 
    format = "file"
  ),
  
  # Read in McManamay and DeRolph valley confinement data for the Eastern U.S. region.
  tar_target(
    p1_confinement_mcmanamay,
    read_csv(p1_confinement_mcmanamay_csv, show_col_types = FALSE)
  ),
  
  # Download DRB geomorphometry dataset, collected using the FACET model 
  # (Hopkins et al. 2020, Chesapeake and Delaware Combined Files; 
  # https://doi.org/10.5066/P9RQJPT1).
  tar_target(
    p1_facet_zip,
    download_sb_file(sb_id = "5e4d6d68e4b0ff554f6db504",
                     file_name = "ChesapeakeDelaware_FACEToutput.zip",
                     out_dir = "1_fetch/out/drb_facet"),
    format = "file"
  ),
  
  # Unzip DRB geomorphometry dataset and identify geomorphic metrics file.
  tar_target(
    p1_facet_files,
    unzip(zipfile = p1_facet_zip, 
          exdir = "1_fetch/out/drb_facet", 
          overwrite = TRUE),
    format = "file"
  ),
  
  # Identify and read in DRB geomorphic metrics file from FACET.
  tar_target(
    p1_facet_geomorph_metrics,
    {
      metrics_file <- grep("CDW_FACET_MetricsAll_Reach.csv", p1_facet_files, 
                           value = TRUE, ignore.case = TRUE)
      read_csv(metrics_file, show_col_types = FALSE)
    }
  ),
  
  # Read in combined DRB and Chesapeake Bay watershed (CBW) FACET stream network,
  # transform to preferred coordinate reference system, subset to only include 
  # the DRB, and join with geomorphic metrics table.
  tar_target(
    p1_facet_network,
    {
      network_file <- grep("CDW_FACET_Network.shp$", p1_facet_files, value = TRUE)
      sf::st_read(network_file, quiet = TRUE) %>%
        filter(!sf::st_is_empty(.)) %>%
        sf::st_transform(., crs = crs) %>%
        mutate(HUC4 = stringr::str_sub(HUCID, 1, 4)) %>%
        filter(HUC4 == "0204") %>%
        select(UniqueID, HUC4, geometry) %>%
        left_join(p1_facet_geomorph_metrics, by = "UniqueID")
    }
  ),
  
  # Track outputs from MODFLOW_extraction workflow
  tar_target(
    p1_modflow_params_feather,
    "1_fetch/in/data_zell2020_wrr/MODFLOW_params_NHM_zell2020_wrr_02.feather"
  ),
  tar_target(
    p1_modflow_discharge_feather,
    "1_fetch/in/data_zell2020_wrr/MODFLOW_Discharge_NHM_zell2020_wrr_02.feather"
  ),
  
  # Read in outputs from MODFLOW_extraction workflow
  tar_target(
    p1_modflow_params,
    arrow::read_feather(p1_modflow_params_feather)
  ),
  tar_target(
    p1_modflow_discharge,
    arrow::read_feather(p1_modflow_discharge_feather) |> 
      select(!c(q_std,q_std_per,nDown))
  )
  
)
