#!/bin/bash


# Import Merged, Projected, Clipped Elevation File
#r.in.gdal input=elevation_clip output=elevation


# Assign Null to Values in Ocean
#g.region rast=elevation
#r.mapcalc "elevation_fix=if(elevation==0,null(),elevation)"


# Calculate Slope #Algorithm uses a 3x3 neighborhood around each cell in the elevation file
#r.slope.aspect elevation=elevation_fix slope=cliffs format=degrees prec=float


# Reclassify Slopes >= 30 degrees
#r.mapcalc "reclass_cliffs=if(cliffs>=30,1,null())"   # "if the user wishes to calculate distances from only selected input map layer category values, the user should run (for example) r.reclass prior to r.buffer, to reclass all categories from which distance zones are not desired to be calculated into category NULL.


# Create 2km buffer 
#r.buffer input=reclass_cliffs output=cliffs_2kmbuffer dist=2000 units=meters

# Create Mask to exlude ocean
#r.out.gdal input=vegetation_extend format=GTiff type=Int16 nodata=-9999 output=vegetation_extend.tif
#r.in.gdal input=vegetation_extend.tif output=veg_mask

r.mask input=veg_mask maskcats="6 thru 50"
r.mapcalc "cliffs_buffer=if(isnull(cliffs_2kmbuffer),0,1)"


# Spatial Aggregation to Same Resolution as WorldClim (must be done using the Projected Location in GRASS)
g.region rast=bio1_set_region
r.resamp.stats input=cliffs_buffer output=cliffs_aggregated method=maximum -w  
# -w flag: "the aggregate uses the values from all input cell which intersect the output cell, weighted according to the proportion of the source cell which lies inside the output cell. This is slower, but produces a more accurate result."


# Write File for Modeling 
r.out.gdal input=cliffs_aggregated format=GTiff type=Float32 nodata=-9999 output=cliffs.tif

