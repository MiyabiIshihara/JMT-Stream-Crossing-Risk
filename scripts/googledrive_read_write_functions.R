require(readr)
require(googledrive)
require(rgdal)
require(dplyr)

load_geotiff_from_googledrive <- function(id){

#Create folder to place spatial files in    
  temp <- tempfile(fileext = ".tif")
  
#download file to temp folder  
  dl<-drive_download(as_id(id), path = temp, overwrite = TRUE)
  
#load as raster object  
  out <- raster(temp)

  return(out)
}

load_rgdal_from_googledrive <- function(id, layer_name = NA){
  
  if(is.na(layer_name)){
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

  } else {
    #Get all googledrive ids in the folder containing the shapefile corresponding to LAYER_NAME
      file_info <- drive_ls(as_id(id)) %>% 
        filter(grepl(layer_name, name))
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

  }

  unlink(temp)
  
  return(out)
}

load_Rdata_from_googledrive <- function(id){
  temp <- tempfile(fileext = ".Rdata")

  dl <- drive_download(as_id(id), path = temp, overwrite = TRUE)
  
  load(temp)
  
}


write_csv_to_googledrive <- function(df, csv_name, folder_id){
  
  temp_file<-tempfile(pattern=csv_name, fileext = ".csv")
  write.csv(df, file=temp_file, row.names=F)
  
  
  #determining if the file you're writing is already in the folder on drive that you're writing to. If it is, you just update the current file so the id stays the same. Else, you create a new file
  if(paste(csv_name,".csv", sep="") %in% data.frame(drive_ls(path =as_id(folder_id)))[,1]){
    
    file_id<-drive_ls(path =as_id(folder_id))[drive_ls(path =as_id(folder_id))[,1]==paste(df_name,".csv", sep=""),]$id
    drive_update(file=as_id(file_id), media=temp_file)
    
  } else{
  drive_upload(media=temp_file, name=paste(csv_name,".csv", sep=""),path = as_id(folder_id))
  }
}

load_csv_from_googledrive <- function(id){
  
temp <- tempfile(fileext = ".csv")

  dl <- drive_download(as_id(id), path = temp, overwrite = TRUE)
  
  out <- read_csv(temp)
  
  unlink(temp)
  
  return(out)
}