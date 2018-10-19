#!/usr/bin/perl

system "r.in.gdal input=bio1_merged.clip output=bio1_set_region";
system "g.region rast=bio1_set_region";

@correct=qw(bio5_merged.clip bio6_merged.clip); #bio1_merged.clip bio2_merged.clip bio5_merged.clip bio6_merged.clip bio7_merged.clip bio8_merged.clip bio9_merged.clip bio10_merged.clip bio11_merged.clip);

foreach (@correct) {
        $cor=$_;
        system "r.in.gdal input=$cor output=$cor";
        print "$cor has been imported into GRASS!\n";
        system "r.mapcalc \"$cor_\"FINAL\"=float($cor)/float(10)\"";
        system "r.out.gdal input=$cor_\"FINAL\" format=GTiff type=Float32 nodata=-9999 output=$cor.\"FINAL\"";
}
