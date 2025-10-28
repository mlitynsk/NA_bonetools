# NA_bonetools (Thermoregulation predicts needle and awl use in North America: an ethnographic meta-analysis )

## table1
The csv file contains all the information needed to successfully run statistical models that account for spatial autocorrelation. This includes the tool type, tool presence in the ethnographic literature, and minimum temperature of the coldest month (MTCM) information based on Bioclimatic Variable 6 within the WorldClim database asssociated with 59 Indigenous groups across North America. The csv also contains latitude and longitude coordinates for all groups considered. Including lat and long information ensures that we account for potential spatial autocorrelation. Ethnographic information was compiled from the eHRAF World Cultures database (https://ehrafworldcultures.yale.edu/).                        

## ANALYSIS
The R-script "ANALYSIS.R" demonstrates all analyses needed to conduct linear mixed-effect models that take into account spatial autocorrelation and summary stats of these data files.
