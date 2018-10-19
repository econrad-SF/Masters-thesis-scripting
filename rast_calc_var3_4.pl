#!/usr/bin/perl

system "g.region rast=bio1_set_region";

@correct=qw(bio3_merged.clip bio4_merged.clip);

foreach (@correct) {
        $cor=$_;
        system "r.in.gdal input=$cor output=$cor";
        print "$cor has been imported into GRASS!\n";
        system "r.mapcalc \"$cor_\"FINAL\"=float($cor)/float(100)\"";
        system "r.out.gdal input=$cor_\"FINAL\" format=GTiff type=Float32 nodata=-9999 output=$cor.\"FINAL\"";
}
