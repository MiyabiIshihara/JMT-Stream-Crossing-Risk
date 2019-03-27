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

load_rgdal_from_googledrive <- function(id){

#Get all googledrive ids in the folder containing the shapefile
  file_info <- drive_ls(as_id(id))
  file_ids <- file_info$id
  file_names <- file_info$name

#Create folder to place spatial files in    
  temp <- tempdir()
  
#Get all files in the google drive folder and put them in the temp folder  
  for(i in 1:length(file_ids)){
    drive_download(as_id(file_ids[i]), path = paste0(temp, "\\", file_names[i]), overwrite = TRUE)
  }
  
#double assignment operator stores the object in your global environment  
  out <- readOGR(temp, layer = strsplit(file_names[which(grepl(".shp", file_names))], ".shp")[[1]])
  
  unlink(temp)
  
  return(out)
}