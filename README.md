# SLR_VegChange_EF
Coastal Vegetation Inventory in Eastern North Carolina, USA
Written and compiled by: Paul Taillie
Shared: 12/19/18


### Project Overview and Script Format
This project was originally designed and implemented by Ben Poulter in 2003/2004 to investigate vegetation composition across a gradient of saltwater exposure.  The study was then replicated by Paul Taillie in 2016/2017 to investigate vegetation change. The files are grouped in an R project and thus are relatively linked.  This means that you do not have to change your working directory to get scripts to run on your machine.  Simply unzip the folder, open R, select file>open project>browse to the unzipped folder>click the Rproject file called “APPveg_data.”  Each script starts with a preamble that will clear your workspace and load the required packages.  You may have to install packages first if they are not yet installed on your machine.  Otherwise, the script should run from start to finish.

### Vegetation Plot Locations

Location of the vegetation plots are included in ESRI shapefile format.  The points mark the center of the 12-m radius vegetation plot, which was delineated in the field.  

### Woody Vegetation Structure
File names: basal_2004 and basal_2016

These datasets include a row for every woody stem taller than breast height with a DBH greater than 2.5cm.  In 2004, the height of broken dead trees was not recorded.  

### Groundcover composition
File names: cover_2004, cover_2016, cover_2017_LS

Because the data collected by two observers, over ten years apart, with minimal calibration, we were quite conservative with how we treated species.  Many species are lumped together as a genus.  For specific details, see the script files for formatting these data.  These scripts also includes many fixes to human errors during data entry.  

All of the 98 plots were surveyed in April-June 2016.  However, the Long Shoal (LS) plots were recently burned that year, and were resampled for groundcover (<1m in height) vegetation only in May 2017.  As currently written, all analyses use 2017 data for these plots, not 2016.

### Soils
Soil was collected in February in both 2004 and 2017.  Soil data is straight from reports from soils lab that conducted the analyses.  The lab reports include a second unit for cation concentrations, but we use mg/kg as currently written.

### For additional information regarding study design and methods, see the following references:

Poulter, B. (2005). Interactions between landscape disturbance and gradual environmental change: Plant community migration in response to fire and sea-level rise. Nicholas School of the Environment, Duke University.

Poulter, B., Qian, S. S., & Christensen, N. L. (2009). Determinants of coastal treeline and the role of abiotic and biotic interactions. Plant Ecology, 202(1), 55–66. https://doi.org/10.1007/s11258-008-9465-3

Taillie, P. J. (2018). Coastal bird responses to the salinization-induced transition from forest to marsh as sea level rises. North Carolina State University. Retrieved from https://repository.lib.ncsu.edu/handle/1840.20/35455

Taillie, P. J., C. E. Moorman, B. Poulter, M. Ardon, R. E. Emanuel. (2019). Decadal-scale vegetation change driven by salinity at the leading edge of rising sea level. Ecosystems.










