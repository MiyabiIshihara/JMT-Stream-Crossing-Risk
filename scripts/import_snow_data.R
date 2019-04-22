# Import Snow Data


#Create folder to place spatial files in    
temp <- tempdir()

id <- "1bvrY-Be43gJOSkNNGhVjGhHX8AXFahzV"

#download file to temp folder  
dl<-drive_download(as_id(id), path = temp, overwrite = TRUE)

#load as raster object  
out <- raster(temp)

return(out)