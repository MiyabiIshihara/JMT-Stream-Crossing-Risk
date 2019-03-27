get_snodas <- function(date, var_name, temp_directory){ #var_name = "snowDepth" for snowDepth, "SWE" for snow water equivalent
#Get file from ftp  
  snodas_got <- GetSnodasDepthSweDate(as.POSIXct(date), outputDir = temp_directory)
  if(snodas_got){
    snodas_list <- ReadSnodasDepthSweDate(as.POSIXct(date), outputDir = temp_directory)
    snodasNC <- PutSnodasNcdf(snodas_list, outputDir = temp_directory)
    
    snodas <- raster(snodasNC, varname = var_name)
    
#correct extent of raster
      extent(snodas) <- c(-124.729166666662, -66.9416666666642,
                       24.949999999999, 52.8749999999978)
      
      projection(snodas) <- CRS('+proj=longlat +datum=WGS84')
      
#Return projected raster  
      out <- snodas
    
  } else {
    out <- NA
  }
  
  unlink(temp_directory, recursive = F)
  
  return(out)
}  

write_snodas_to_GoogleDrive <- function(date, var_name, temp_directory, GDfolder_id){ #var = "snowDepth" for snowDepth, "SWE" for snow water equivalent
  
#Get snodas raster  
  snodas <- get_snodas(date, var_name, temp_directory)
  
  if(class(snodas) == "RasterLayer"){
  
#Write projected raster to tempdirectory 
 writeRaster(snodas,
             filename = paste0(temp_directory, "\\", var_name, "_", date, ".tif"),
             format = "GTiff")
    
 drive_upload(media=paste0(temp_directory, "\\", var_name, "_", date, ".tif"), 
              name=paste0(var_name, "_", date, ".tif"),
              path = as_id(GDfolder_id))
 
  unlink(temp_directory, recursive = F)
  
  } else {
    
    return(paste0("Retrieval of ", var_name, " on ", date, " was unsuccessful"))
    
  }
  
}  
