#!/usr/bin/perl

@variable = qw(bio1_ bio2_ bio3_ bio4_ bio5_ bio7_ bio15_ bio18_);
@region = qw(00 01 02 10 11 12 111);

foreach (@variable) {
	$var=$_;
	foreach(@region) {
		$rg=$_;
		$filename=$var.$rg.".tif";
		print "$filename\n";
		system "rm $filename";
	}
}
