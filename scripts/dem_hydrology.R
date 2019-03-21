#  R script to run TauDEM

# This also assumes that MPICH2 is properly installed on your machine and that TauDEM command line executables exist
# MPICH2.  Obtain from http://www.mcs.anl.gov/research/projects/mpich2/
# Install following instructions at http://hydrology.usu.edu/taudem/taudem5.0/downloads.html.  
# It is important that you install this from THE ADMINISTRATOR ACCOUNT.
 
# TauDEM command line executables.  
# If on a PC download from http://hydrology.usu.edu/taudem/taudem5.0/downloads.html
# The install package will install to c:\program files\taudem or c:\program files (x86)\taudem set a 
# system path.  If you want to do this manually you can download the command line executables and place where you wish.
# If on a different system, download the source code and compile for your system.

library(raster)
library(shapefiles)
library(rgdal)
library(mapview)
library(sf)
library(tidyverse)

# Get data #####################

# Function to download DEM rasters from USGS 3DEP server
  get_dem <- function(dem_coords){
  temp <- tempfile()
  temp2 <- tempfile()

  download.file(paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/1/IMG/",
                dem_coords,
                ".zip"),
                temp)
  
  unzip(zipfile = temp, exdir = temp2)
  
  out <- raster(file.path(temp2, paste0("img", dem_coords, "_1.img")))
  
  unlink(c(temp, temp2))

  return(out)
}

# Download tiles that overlap JMT
  n37w119 <- get_dem("n37w119")
  n38w119 <- get_dem("n38w119")
  n37w120 <- get_dem("n37w120")
  n38w120 <- get_dem("n38w120")
  n39w120 <- get_dem("n39w120")

# Mosaic rasters together   
  jmt_dem <- mosaic(n37w119, n37w120,
                    n38w119, n38w120,
                    n39w120, fun = mean)
  
  writeRaster(jmt_dem, "C:/Users/chris_hoover/Desktop/Me/Other/JMT_analyze/Data/DEM_Hydrology/jmt_dem.tif")

# Process DEM to produce relevant hydrological layers #################  
  setwd("C:/Users/chris_hoover/Desktop/Me/Other/JMT_analyze/Data/DEM_Hydrology")
  
  z=raster("jmt_dem.tif")
#plot(z)

# Pitremove
  system("mpiexec -n 8 pitremove -z jmt_dem.tif -fel jmt_demfel.tif")
  fel=raster("jmt_demfel.tif")
  #plot(fel)


# D8 flow directions
  system("mpiexec -n 8 D8Flowdir -p jmt_demp.tif -sd8 jmt_demsd8.tif -fel jmt_demfel.tif",
         show.output.on.console=F,invisible=F)
  p=raster("jmt_demp.tif")
  #plot(p)
  sd8=raster("jmt_demsd8.tif")
  #plot(sd8)

# Contributing area
  system("mpiexec -n 8 AreaD8 -p jmt_demp.tif -ad8 jmt_demad8.tif")
  ad8=raster("jmt_demad8.tif")
  #plot(log(ad8))
  #zoom(log(ad8))


# Grid Network 
  system("mpiexec -n 8 Gridnet -p jmt_demp.tif -gord jmt_demgord.tif -plen jmt_demplen.tif -tlen jmt_demtlen.tif")
  gord=raster("jmt_demgord.tif")
  
#Convert stream order from grid to shapefile
  #max_order = cellStats(gord, max)
  
  #gord12 <- rasterToPolygons(gord, fun = function(x){x==max_order})
  #gord11 <- rasterToPolygons(gord, fun = function(x){x==(max_order - 1)})
  #gord10 <- rasterToPolygons(gord, fun = function(x){x==(max_order - 2)})
  #gord9 <- rasterToPolygons(gord, fun = function(x){x==(max_order - 3)})
  #gord8 <- rasterToPolygons(gord, fun = function(x){x==(max_order - 4)})
  #gord7 <- rasterToPolygons(gord, fun = function(x){x==(max_order - 5)})
  #gord6 <- rasterToPolygons(gord, fun = function(x){x==(max_order - 6)})
  #gord5 <- rasterToPolygons(gord, fun = function(x){x==(max_order - 7)})

  #stream_grid <- bind(gord12, gord11, gord10, gord9, gord8, gord7, gord6, gord5)
  
# DInf flow directions
  system("mpiexec -n 8 DinfFlowdir -ang jmt_demang.tif -slp jmt_demslp.tif -fel jmt_demfel.tif", show.output.on.console=F,invisible=F)
    ang=raster("jmt_demang.tif")
    #plot(ang)
    slp=raster("jmt_demslp.tif")
    #plot(slp)


# Dinf contributing area
  system("mpiexec -n 8 AreaDinf -ang jmt_demang.tif -sca jmt_demsca.tif")
  sca=raster("jmt_demsca.tif")
  #plot(log(sca))
  #zoom(log(sca))

# Threshold
  system("mpiexec -n 8 Threshold -ssa jmt_demad8.tif -src jmt_demsrc.tif -thresh 100")
  src=raster("jmt_demsrc.tif")
  #plot(src)
  #zoom(src)

# import stream crossing shapefile for outlets
  #Needed to be transformed to same coordinate system as rasters
    #crossings <- readOGR("JMT_Main_Crossings.shp")
      #crossings2 <- spTransform(crossings, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
        #crossings3 <- spTransform(crossings2, crs(z))
          #writeOGR(crossings3, ".", "JMT_Stream_Crossings_gcs", driver="ESRI Shapefile")

# Move Outlets
  #system("mpiexec -n 8 moveoutletstostreams -p jmt_demp.tif -src jmt_demsrc.tif -o JMT_Stream_Crossings_gcs.shp -om JMT_Stream_crossings_snap.shp")
  
# some outlets were moved manually in ArcGIS afterwards to make sure they were lying on a source pixel corresponding to the right stream          
  outpt=readOGR("JMT_Stream_crossings_snap.shp")
  approxpt=readOGR("JMT_Main_Crossings.shp")

#plot(src)
#points(outpt$shp[2],outpt$shp[3],pch=19,col=2)
#points(approxpt$shp[2],approxpt$shp[3],pch=19,col=4)

#zoom(src)

#Contributing area of all outlets
  system("mpiexec -n 8 Aread8 -p jmt_demp.tif -o JMT_Stream_crossings_snap.shp -ad8 jmt_demssa.tif")
  ssa=raster("jmt_demssa.tif")
  plot(ssa) 
  
# Contributing area upstream of each outlet

  for(i in 1:nrow(outpt)){  
    shp_name <- gsub(" ", "_", outpt[i,]@data$JMT_Cross)
    
    # write shapefile of target outlet to same directory as flow direction tif
      writeOGR(outpt[i,], ".", paste0(shp_name, "_outlet"), driver="ESRI Shapefile")
    
    # Run tauDEM contributing area exe
      system(paste0("mpiexec -n 8 Aread8 -p jmt_demp.tif -o ", 
                    shp_name, "_outlet.shp -ad8 ", 
                    shp_name, "_ssa.tif"))
      
    # convert resulting watershed raster into shapefile
      wtrshd <- raster(paste0(shp_name, "_ssa.tif"))
      
      wtrshd1 <- wtrshd > -Inf
      
      wtrshd_trim <- trim(wtrshd1)
      
      wtrshd_shp <- rasterToPolygons(wtrshd_trim, dissolve = T)

    # write shapefile to watersheds directory
      writeOGR(wtrshd_shp, ".", paste0("watersheds/", shp_name, "_watershed"), driver = "ESRI Shapefile")
      
      print(i)  
  }
  
#Combine all individual watersheds into one shapefile
  get_wtrshd <- function(file){
    name <- strsplit(file, ".shp")[[1]]
    
    read_sf(paste0("watersheds/", file)) %>% 
      mutate(crossing = gsub("_", " ", (strsplit(file, "_watershed.shp"))))
  }
  
  wtrshd_files <- list.files("watersheds/")[grep(".shp", list.files("watersheds/"))]
  
  all_wtrshds <- lapply(wtrshd_files, get_wtrshd)
  
  wtrshds_merged <- do.call(rbind, all_wtrshds)

#Project and calulate area in square meters  
  wtrshds_utm11N <- st_transform(wtrshds_merged, crs = "+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") %>% 
    mutate(area = as.numeric(st_area(.)))

#Write final shapefile
  st_write(wtrshds_utm11N, "watersheds/all_watersheds_UTM11N.shp")
  