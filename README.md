# suma_mozart

Sistema for adding US emissions from NEI and Mexico 2014 to
a single file.

Input files:
	wrfchemi_00z_d01_us	US emissions starting at 00Z
	wrfchemi_00z_d01_mx	MX
	wrfchemi_12z_d01_us	US emissions starting at 12Z
	wrfchemi_12z_d01_mx

output files:
	wrfchemi_00z_d01
	wrfchemi_12z_d01
	
All the files are netcdf, with the same grid size the number of
RADM species can be different for each file. The RADM  mechanism 
considers 49 emissions categories.

