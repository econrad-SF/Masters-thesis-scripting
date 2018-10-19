#!/bin/bash

#Set Region
#g.region n=90 s=20 e=180 w=-180 res=00:00:07.5 

#Import
#r.in.gdal input=piece1.tif output=piece1
#r.in.gdal input=piece2.tif output=piece2
#r.in.gdal input=piece3.tif output=piece3
#r.in.gdal input=piece4.tif output=piece4
#r.in.gdal input=piece5.tif output=piece5
#r.in.gdal input=piece6.tif output=piece6
#r.in.gdal input=piece7.tif output=piece7
#r.in.gdal input=piece8.tif output=piece8
#r.in.gdal input=piece9.tif output=piece9

#Merge
#r.patch input=piece1,piece2,piece3,piece4,piece5,piece6,piece7,piece8,piece9 output=elevation
#r.out.gdal input=elevation format=GTiff type=Float32 nodata=-9999 output=elevation


#Projection
#gdalwarp -s_srs EPSG:4326 -t_srs '+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +no_defs +ellps=GRS80 +a=6378137 +rf=298.257222101 +towgs84=0,0,0 +to_meter=1' -r bilinear -dstnodata -9999 -of GTiff elevation elevation_proj

#Clip
#gdalwarp -cutline Step3b_StudyBoundary_shp85North.shp -dstnodata -9999 -crop_to_cutline elevation_proj -of GTiff elevation_clip

