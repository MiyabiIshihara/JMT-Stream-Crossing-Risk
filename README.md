# JMT-Stream-Crossing-Risk

DS421 Partner Project working with John Ladd, founder of the JMT Hiker Survey, on modeling, predicting, and vizualizing risks to hikers along the John Muir Trail (JMT). Currently most data is housed on googledrive and various functions to read data from googledrive into R are used because many of the layers are too large to push to github (hendering reproducibility).  

## scripts  
All code for the project  

### scripts/googledrive_read_write_functions  
Contains various functions to read and write data from/to the shared googledrive folder (shared access required)  

### scripts/dem_hydrology.R  
Script used to download Digital Elevation Model (DEM) covering the extent of the JMT and use TauDEM to hydrologically process the DEM and get key nwe layers including watersheds upstream of outlet points, flow direction and slope rasters, and stream order  

### scripts/Risk_model  
`Rmd` file and associated outputs for the risk model which incorporates data from the 2015 JMT hiker survey and processed SNODAS data on snow melt. Outcome variable is hikers reported challenge posed by stream corssings on their trip on a scale of 0-5. Their trip duration and starting/ending points are incorporated into a network model which was used to determine their approximate location (and stream crossings encountered) on a given date. This location/time information was then matched with snow melt data in watersheds associated with the crossings experienced to generate the peak snow melt experienced coefficient  

### scripts/precipitation  
Contains scripts to download and process PRISM precipitation and temperature data  

### scripts/snow  
Contains scripts to download and process [SNODAS](https://nsidc.org/data/g02158) data which contains daily modeled rasters of estimated snow depth and snow water equivalent (SWE) across the contiguous US. 
* Scripts to obtain the data and upload it to googledrive (`get_snodas...R`) have already been run and don't need to be run again.  
* `extract_snowdepth_to_watersheds.R` was used to summarize snodas variables in the watersheds along the JMT associated with stream crossings which are ultimately used in the risk model  
* `snodas_swe_viz.R` creates visualizations that are placed in the Plots_Viz folder below to show time series of SWE, SWE melt, and estimated change in risk score associated with snow melt over time  

### scripts/Shiny_App  
Home for the vizualization arm of the project where most work is currently being done. Right now contains a leaflet object used to vizualize JMT-related data including the trail itself and access trails, a couple app attempts to try different ways of pulling in and vizualizing large raster data, a special icon to represent river crossings in the leaflet object, some attempts at data preprocessing, etc.  

## Plots_Viz  
Contains output graphs from snodas data processing and risk model  