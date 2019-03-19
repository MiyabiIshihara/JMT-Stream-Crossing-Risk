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

# Process DEM to produce relevant hydrological layers #################  
  setwd("")
z=raster("JMT_mosaic1.tif")
#plot(z)

# Pitremove
  system("mpiexec -n 8 pitremove -z JMT_mosaic1.tif -fel JMT_mosaic1fel.tif")
  fel=raster("JMT_mosaic1fel.tif")
  #plot(fel)


# D8 flow directions
  system("mpiexec -n 8 D8Flowdir -p JMT_mosaic1p.tif -sd8 JMT_mosaic1sd8.tif -fel JMT_mosaic1fel.tif",
         show.output.on.console=F,invisible=F)
  p=raster("JMT_mosaic1p.tif")
  #plot(p)
  sd8=raster("JMT_mosaic1sd8.tif")
  #plot(sd8)

# Contributing area
  system("mpiexec -n 8 AreaD8 -p JMT_mosaic1p.tif -ad8 JMT_mosaic1ad8.tif")
  ad8=raster("JMT_mosaic1ad8.tif")
  #plot(log(ad8))
  #zoom(log(ad8))


# Grid Network 
  system("mpiexec -n 8 Gridnet -p JMT_mosaic1p.tif -gord JMT_mosaic1gord.tif -plen JMT_mosaic1plen.tif -tlen JMT_mosaic1tlen.tif")
  gord=raster("JMT_mosaic1gord.tif")
  
#Convert stream order from grid to shapefile
  gord13 <- rasterToPolygons(gord, fun = function(x){x==13})
  gord12 <- rasterToPolygons(gord, fun = function(x){x==12})
  gord11 <- rasterToPolygons(gord, fun = function(x){x==11})
  gord10 <- rasterToPolygons(gord, fun = function(x){x==10})
  gord9 <- rasterToPolygons(gord, fun = function(x){x==9})
  gord8 <- rasterToPolygons(gord, fun = function(x){x==8})
  gord7 <- rasterToPolygons(gord, fun = function(x){x==7})
  gord6 <- rasterToPolygons(gord, fun = function(x){x==6})
  gord5 <- rasterToPolygons(gord, fun = function(x){x==5})

# DInf flow directions
  system("mpiexec -n 8 DinfFlowdir -ang JMT_mosaic1ang.tif -slp JMT_mosaic1slp.tif -fel JMT_mosaic1fel.tif", show.output.on.console=F,invisible=F)
    ang=raster("JMT_mosaic1ang.tif")
    #plot(ang)
    slp=raster("JMT_mosaic1slp.tif")
    #plot(slp)


# Dinf contributing area
  system("mpiexec -n 8 AreaDinf -ang JMT_mosaic1ang.tif -sca JMT_mosaic1sca.tif")
  sca=raster("JMT_mosaic1sca.tif")
  #plot(log(sca))
  #zoom(log(sca))

# Threshold
  system("mpiexec -n 8 Threshold -ssa JMT_mosaic1ad8.tif -src JMT_mosaic1src.tif -thresh 100")
  src=raster("JMT_mosaic1src.tif")
  #plot(src)
  #zoom(src)

# import stream crossing shapefile for outlets
  #Needed to be transformed to same coordinate system as rasters
    #crossings <- readOGR("JMT_Stream_Crossings.shp")
      #crossings2 <- spTransform(crossings, crs(z))
        #writeOGR(crossings2, ".", "JMT_Stream_Crossings_NAD83", driver="ESRI Shapefile")

# Move Outlets
  system("mpiexec -n 8 moveoutletstostreams -p JMT_mosaic1p.tif -src JMT_mosaic1src.tif -o JMT_Stream_Crossings_NAD83.shp -om JMT_Stream_crossings_snap.shp")
  
  outpt=readOGR("JMT_Stream_crossings_snap.shp")
  approxpt=readOGR("JMT_Stream_Crossings_NAD83.shp")

#plot(src)
#points(outpt$shp[2],outpt$shp[3],pch=19,col=2)
#points(approxpt$shp[2],approxpt$shp[3],pch=19,col=4)

#zoom(src)

# Contributing area upstream of outlet
system("mpiexec -n 8 Aread8 -p JMT_mosaic1p.tif -o JMT_Stream_crossings_snap.shp -ad8 JMT_mosaic1ssa.tif")
  ssa=raster("JMT_mosaic1ssa.tif")
  plot(ssa) 


# Threshold
system("mpiexec -n 8 threshold -ssa JMT_mosaic1ssa.tif -src JMT_mosaic1src1.tif -thresh 2000")
src1=raster("JMT_mosaic1src1.tif")
plot(src1)
zoom(src1)

# Stream Reach and Watershed
system("mpiexec -n 8 Streamnet -fel JMT_mosaic1fel.tif -p JMT_mosaic1p.tif -ad8 JMT_mosaic1ad8.tif -src JMT_mosaic1src1.tif -o outlet.shp -ord JMT_mosaic1ord.tif -tree JMT_mosaic1tree.txt -coord JMT_mosaic1coord.txt -net JMT_mosaic1net.shp -w JMT_mosaic1w.tif")
plot(raster("JMT_mosaic1ord.tif"))
zoom(raster("JMT_mosaic1ord.tif"))
plot(raster("JMT_mosaic1w.tif"))

# Plot streams using stream order as width
snet=read.shapefile("JMT_mosaic1net")
ns=length(snet$shp$shp)
for(i in 1:ns)
{
  lines(snet$shp$shp[[i]]$points,lwd=snet$dbf$dbf$Order[i])
}

# Peuker Douglas stream definition
system("mpiexec -n 8 PeukerDouglas -fel JMT_mosaic1fel.tif -ss JMT_mosaic1ss.tif")
ss=raster("JMT_mosaic1ss.tif")
plot(ss)
zoom(ss)

#  Accumulating candidate stream source cells
system("mpiexec -n 8 Aread8 -p JMT_mosaic1p.tif -o outlet.shp -ad8 JMT_mosaic1ssa.tif -wg JMT_mosaic1ss.tif")
ssa=raster("JMT_mosaic1ssa.tif")
plot(ssa)

#  Drop Analysis
system("mpiexec -n 8 Dropanalysis -p JMT_mosaic1p.tif -fel JMT_mosaic1fel.tif -ad8 JMT_mosaic1ad8.tif -ssa JMT_mosaic1ssa.tif -drp JMT_mosaic1drp.txt -o outlet.shp -par 5 500 10 0")

# Deduce that the optimal threshold is 300 
# Stream raster by threshold
system("mpiexec -n 8 Threshold -ssa JMT_mosaic1ssa.tif -src JMT_mosaic1src2.tif -thresh 300")
plot(raster("JMT_mosaic1src2.tif"))

# Stream network
system("mpiexec -n 8 Streamnet -fel JMT_mosaic1fel.tif -p JMT_mosaic1p.tif -ad8 JMT_mosaic1ad8.tif -src JMT_mosaic1src2.tif -ord JMT_mosaic1ord2.tif -tree JMT_mosaic1tree2.dat -coord JMT_mosaic1coord2.dat -net JMT_mosaic1net2.shp -w JMT_mosaic1w2.tif -o Outlet.shp",show.output.on.console=F,invisible=F)

plot(raster("JMT_mosaic1w2.tif"))
snet=read.shapefile("JMT_mosaic1net2")
ns=length(snet$shp$shp)
for(i in 1:ns)
{
  lines(snet$shp$shp[[i]]$points,lwd=snet$dbf$dbf$Order[i])
}

# Wetness Index
system("mpiexec -n 8 SlopeAreaRatio -slp JMT_mosaic1slp.tif -sca JMT_mosaic1sca.tif -sar JMT_mosaic1sar.tif", show.output.on.console=F, invisible=F)
sar=raster("JMT_mosaic1sar.tif")
wi=sar
wi[,]=-log(sar[,])
plot(wi)

# Distance Down
system("mpiexec -n 8 DinfDistDown -ang JMT_mosaic1ang.tif -fel JMT_mosaic1fel.tif -src JMT_mosaic1src2.tif -m ave v -dd JMT_mosaic1dd.tif",show.output.on.console=F,invisible=F)
plot(raster("JMT_mosaic1dd.tif"))