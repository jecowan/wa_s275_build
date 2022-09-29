# wa_s275_build

The S-275 is a statewide personnel reporting system for school districts in 
Washington State. Districts report all personnel as of October 1 each
school year. Personnel are reported with a unique identification code
that permits matching across years and districts. 

The original S-275 files are available 
[here](https://k12.wa.us/safs-database-files) for recent school years. The
codebooks are available 
[here](https://www.k12.wa.us/policy-funding/school-apportionment/instructions-and-tools/personnel-reporting).

This script builds stacked S-275 datasets for academic years 1996-2022. 
The files are hosted on Dropbox and have been downloaded from the OSPI website 
and converted to .csv files. The codebooks are included with the downloaded
data. 

Outputs three files:

* s275_stacked.RData: A stacked version of the S-275 with minimal data cleaning.

* s275_district.RData: Employee-district level dataset with demographics, salary, and teacher characteristics.

* s275_assignment.RData: Employee-assignment level dataset with information on individual assignments.

The script requires a FRED account to construct deflated salary information.
See the [```fredr```](https://cran.r-project.org/web/packages/fredr/vignettes/fredr.html)
package for more information.
