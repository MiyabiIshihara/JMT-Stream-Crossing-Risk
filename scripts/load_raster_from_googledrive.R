require(readr)
require(googledrive)
require(rgdal)

load_geotiff_from_googledrive <- function(id){

#Create folder to place spatial files in    
  temp <- tempfile(fileext = ".tif")
  
#download file to temp folder  
  dl<-drive_download(as_id(id), path = temp, overwrite = TRUE)
  
#load as raster object  
  out <- raster(temp)

  return(out)
}