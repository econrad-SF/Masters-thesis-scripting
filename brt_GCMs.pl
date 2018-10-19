#!/bin/bash

# 15 GCMs for RCP2.6_2070

r.mapcalc "
$name=qw(RCP26_2070_brt_);
@GCMs = qw(BC CC); #CN GD GF GS HD HE IP MC MG MI MP MR NO);

foreach ($name) {
	$nm=$_;
	foreach (@GCMs) {
		$gcm=$_;
		$filename=$nm.$gcm.".tif";
		$grassname=$nm.$gcm;
		system "g.region rast=bio1_set_region";
		system "r.in.gdal input=$filename output=$grassname";
        }
}