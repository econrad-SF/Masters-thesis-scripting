#!/bin/bash

#Projection
#gdalwarp -s_srs EPSG:4326 -t_srs '+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +no_defs +ellps=GRS80 +a=6378137 +rf=298.257222101 +towgs84=0,0,0 +to_meter=1' -r near -dstnodata -9999 -of GTiff biomes_current_climate.img vegetation_proj

#Clip
#gdalwarp -cutline Step3b_StudyBoundary_shp85North.shp -r near -dstnodata -9999 -crop_to_cutline vegetation_proj -of GTiff vegetation_clip

# Import into GRASS
#r.in.gdal input=vegetation_clip output=vegetation


# Set the Extent of Vegetation to be that of the WorldClim layers. Do this by setting region and exporting file
#g.region rast=bio6_avg
#r.out.gdal input=vegetation format=GTiff type=Float32 nodata=-9999 output=vegetation_extend.tif


#Burn outer Aleutians (Commander Islands) into existing vegetation map with value 45. *Couldn't make this work from commandline; works alright using QGIS 
#gdal_rasterize -burn 45 -a ID_0 -l Aleutians_for_Biomes.shp src_datasource vegetation_extend.tif dst_filename vegetation_extend.tif 

# Import back into GRASS in order to do Reclassify Step 
#r.in.gdal input=vegetation_extend.tif output=vegetation_extend

g.region rast=vegetation_extend
r.reclass input=vegetation_extend output=vegetation_reclass rules=biome.rules.txt  #biome.rule.txt is a text file I created where all alpine/tundra categories are now labeled 1 and everything else is labeled 0.

r.out.gdal input=vegetation_reclass format=GTiff type=Int16 nodata=-9999 output=vegetation.tif


