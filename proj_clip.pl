#!/usr/bin/perl

#system "r.out.gdal input=bio1_merged format=GTiff type=Float32 output=bio1_merged nodata=-9999";
#system "r.out.gdal input=bio2_merged format=GTiff type=Float32 output=bio2_merged nodata=-9999";
#system "r.out.gdal input=bio3_merged format=GTiff type=Float32 output=bio3_merged nodata=-9999";
#system "r.out.gdal input=bio4_merged format=GTiff type=Float32 output=bio4_merged nodata=-9999";
system "r.out.gdal input=bio5_merged format=GTiff type=Float32 output=bio5_merged nodata=-9999";
system "r.out.gdal input=bio6_merged format=GTiff type=Float32 output=bio6_merged nodata=-9999";
#system "r.out.gdal input=bio7_merged format=GTiff type=Float32 output=bio7_merged nodata=-9999";
#system "r.out.gdal input=bio8_merged format=GTiff type=Float32 output=bio8_merged nodata=-9999";
#system "r.out.gdal input=bio9_merged format=GTiff type=Float32 output=bio9_merged nodata=-9999";
#system "r.out.gdal input=bio10_merged format=GTiff type=Float32 output=bio10_merged nodata=-9999";
#system "r.out.gdal input=bio11_merged format=GTiff type=Float32 output=bio11_merged nodata=-9999";
#system "r.out.gdal input=bio12_merged format=GTiff type=Float32 output=bio12_merged nodata=-9999";
#system "r.out.gdal input=bio13_merged format=GTiff type=Float32 output=bio13_merged nodata=-9999";
#system "r.out.gdal input=bio14_merged format=GTiff type=Float32 output=bio14_merged nodata=-9999";
#system "r.out.gdal input=bio15_merged format=GTiff type=Float32 output=bio15_merged nodata=-9999";
#system "r.out.gdal input=bio16_merged format=GTiff type=Float32 output=bio16_merged nodata=-9999";
#system "r.out.gdal input=bio17_merged format=GTiff type=Float32 output=bio17_merged nodata=-9999";
#system "r.out.gdal input=bio18_merged format=GTiff type=Float32 output=bio18_merged nodata=-9999";
#system "r.out.gdal input=bio19_merged format=GTiff type=Float32 output=bio19_merged nodata=-9999";


@files=qw(bio5_merged bio6_merged);   #bio1_merged bio2_merged bio3_merged bio4_merged bio5_merged bio6_merged.tif bio7_merged bio8_merged bio9_merged bio10_merged bio11_merged bio12_merged bio13_merged bio14_merged bio15_merged bio16_merged bio17_merged bio18_merged bio19_merged);

foreach (@files) {
	$file=$_;
	system "gdalwarp -s_srs EPSG:4326 -t_srs '+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +no_defs +ellps=GRS80 +a=6378137 +rf=298.257222101 +towgs84=0,0,0 +to_meter=1' -r bilinear -dstnodata -9999 -of GTiff $file $file.\"proj\"";
	print "$file.proj is now projected!";
	system "gdalwarp -cutline Step3b_StudyBoundary_shp85North.shp -dstnodata -9999 -crop_to_cutline $file.\"proj\" -of GTiff $file.\"clip\"";
}
