# download precipitation data from Google Drive


library(googledrive)

########### 
file_info <- drive_ls(as_id("1yr5xYEkd3ON6eD6pw3CQgEf7ciJRXnXM"))
file_ids <- file_info$id
file_names <- file_info$name

temp <- tempdir()
#Get all files in the google drive folder and put them in the temp folder  
for(i in 1:length(file_ids)){
  drive_download(as_id(file_ids[i]), path = paste0(temp, "/", file_names[i]), overwrite = TRUE)
}
temp
out <- raster(paste0(temp, "/", file_names[2])) 
plot(out)
########### 




########### 
file_info <- drive_ls(as_id("1yr5xYEkd3ON6eD6pw3CQgEf7ciJRXnXM"))
file_ids <- file_info$id
file_names <- file_info$name


ind <- which(str_detect(file_names, "_bil.bil"))[1]
temp <- tempdir()
drive_download(as_id(file_ids[ind]), path = paste0(temp, "/", file_names[ind]), overwrite = T)
raster(paste0(temp, "/", file_names[ind]))
#  "/var/folders/93/z0nvkmfx5c5c4whzmfq8pqt00000gn/T//Rtmp9blp5v/PRISM_ppt_stable_4kmD2_20150628_bil.bil"
########### 




########### 
# obtain folder ids 
folder_info <- drive_ls(as_id("16TsvJGV4YNxG6EER2RokJTOZMFurhsX4"))
folder_ids <- folder_info$id
folder_names <- folder_info$name
folder_names

# obtain file ids in each folder
file_info <- drive_ls(as_id(folder_ids[1]))
file_ids <- file_info$id
file_names <- file_info$name
file_names

ind <- which(str_detect(file_names, "_bil.bil"))[1]
temp <- tempdir()
drive_download(as_id(file_ids[ind]), path = paste0(temp, "/", file_names[ind]), overwrite = T)
raster(paste0(temp, "/", file_names[ind]))
######### 


######### 
length_folder <- length(folder_names)

download_raster <- function(folder_index){
  # obtain file ids in each folder
  file_info <- drive_ls(as_id(folder_ids[folder_index]))
  file_ids <- file_info$id
  file_names <- file_info$name
  
  ind <- which(str_detect(file_names, "_bil.bil"))[1]
  temp <- tempdir()
  drive_download(as_id(file_ids[ind]), path = paste0(temp, "/", file_names[ind]), overwrite = T)
  raster_data <- raster(paste0(temp, "/", file_names[ind]))
  crs(raster_data) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
}


raster_data <- list()
for(i in 1:2){
  # obtain file ids in each folder
  file_info <- drive_ls(as_id(folder_ids[i]))
  file_ids <- file_info$id
  file_names <- file_info$name
  
  ind <- which(str_detect(file_names, "_bil.bil"))[1]
  temp <- tempdir()
  drive_download(as_id(file_ids[ind]), path = paste0(temp, "/", file_names[ind]), overwrite = T)
  raster_data[[i]] <- raster(paste0(temp, "/", file_names[ind]))
  crs(raster_data[[i]]) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  
}

plot(raster_data[[1]])

map(1:2, download_raster)


######### 



######### 
drive_download(as_id(file_ids[i]), path = paste0(temp, "/", file_names[i], "/", file_names[i], ".bil"), overwrite = TRUE)

# length(folder_ids)
for(i in 1:2){
  file_info <- drive_ls(as_id(folder_ids[i]))
  file_ids <- file_info$id
  file_names <- file_info$name
  file_names
  
  temp <- tempdir()
  for(j in 1:8){
    drive_download(as_id(file_ids[j]), path = paste0(temp, "/", file_names[j], "/", file_names[j], ".bil"), overwrite = TRUE)
  }
}


# END HERE
########### 