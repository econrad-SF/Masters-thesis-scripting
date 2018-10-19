#!/bin/bash

g.region n=90 s=-90 e=180 w=-180 res=00:00:30 

r.patch input=bio1_00.tif,bio1_01.tif,bio1_02.tif,bio1_10.tif,bio1_11.tif,bio1_12.tif,bio1_111.tif output=bio1_merged
r.patch input=bio2_00.tif,bio2_01.tif,bio2_02.tif,bio2_10.tif,bio2_11.tif,bio2_12.tif,bio2_111.tif output=bio2_merged
r.patch input=bio3_00.tif,bio3_01.tif,bio3_02.tif,bio3_10.tif,bio3_11.tif,bio3_12.tif,bio3_111.tif output=bio3_merged
r.patch input=bio4_00.tif,bio4_01.tif,bio4_02.tif,bio4_10.tif,bio4_11.tif,bio4_12.tif,bio4_111.tif output=bio4_merged
r.patch input=bio5_00.tif,bio5_01.tif,bio5_02.tif,bio5_10.tif,bio5_11.tif,bio5_12.tif,bio5_111.tif output=bio5_merged
r.patch input=bio6_00.tif,bio6_01.tif,bio6_02.tif,bio6_10.tif,bio6_11.tif,bio6_12.tif,bio6_111.tif output=bio6_merged
r.patch input=bio8_00.tif,bio8_01.tif,bio8_02.tif,bio8_10.tif,bio8_11.tif,bio8_12.tif,bio8_111.tif output=bio8_merged
r.patch input=bio9_00.tif,bio9_01.tif,bio9_02.tif,bio9_10.tif,bio9_11.tif,bio9_12.tif,bio9_111.tif output=bio9_merged
r.patch input=bio11_00.tif,bio11_01.tif,bio11_02.tif,bio11_10.tif,bio11_11.tif,bio11_12.tif,bio11_111.tif output=bio11_merged
r.patch input=bio12_00.tif,bio12_01.tif,bio12_02.tif,bio12_10.tif,bio12_11.tif,bio12_12.tif,bio12_111.tif output=bio12_merged
r.patch input=bio13_00.tif,bio13_01.tif,bio13_02.tif,bio13_10.tif,bio13_11.tif,bio13_12.tif,bio13_111.tif output=bio13_merged
r.patch input=bio14_00.tif,bio14_01.tif,bio14_02.tif,bio14_10.tif,bio14_11.tif,bio14_12.tif,bio14_111.tif output=bio14_merged
r.patch input=bio15_00.tif,bio15_01.tif,bio15_02.tif,bio15_10.tif,bio15_11.tif,bio15_12.tif,bio15_111.tif output=bio15_merged
r.patch input=bio16_00.tif,bio16_01.tif,bio16_02.tif,bio16_10.tif,bio16_11.tif,bio16_12.tif,bio16_111.tif output=bio16_merged
r.patch input=bio17_00.tif,bio17_01.tif,bio17_02.tif,bio17_10.tif,bio17_11.tif,bio17_12.tif,bio17_111.tif output=bio17_merged
r.patch input=bio18_00.tif,bio18_01.tif,bio18_02.tif,bio18_10.tif,bio18_11.tif,bio18_12.tif,bio18_111.tif output=bio18_merged
r.patch input=bio19_00.tif,bio19_01.tif,bio19_02.tif,bio19_10.tif,bio19_11.tif,bio19_12.tif,bio19_111.tif output=bio19_merged
