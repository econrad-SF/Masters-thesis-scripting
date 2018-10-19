#!/usr/bin/perl

@variable=qw(bio1_ bio2_ bio3_ bio4_ bio5_ bio6_ bio7_ bio8_ bio9_ bio10_ bio11_ bio12_ bio13_ bio14_ bio15_ bio16_ bio17_ bio18_ bio19_);
@region=qw(00 01 02 10 11 12 111);

foreach (@variable) {
	$var=$_;
	foreach (@region) {
		$rg=$_;
		$filename=$var.$rg.".tif";
		print "$filename\n";
		system "g.region n=90 s=-90 e=180 w=-180 res=00:00:30";
		system "r.in.gdal input=$filename output=$filename";
		print "$filename has been imported into GRASS!\n";
	}
}
