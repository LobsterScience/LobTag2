
Installation

Enter these lines into the R console to install the package:

Install.packages(“devtools”)
devtools::install_github(“jakeelement/LobTag2”)

After installation, find the location of your user guide by entering:
system.file("data", "user_guide.pptx", package = "LobTag2")
Read the user guide for more info.

Likewise, the location of your template csv files for data entry is found with:
system.file("extdata", package = "LobTag2")

Getting a Mapbox token:
You will need an account with Mapbox to use the mapping functionality. Setting up an account is quick and easy, just go to: 
https://account.mapbox.com/auth/signup/
and create an account. Once you have an account you’ll get a free a public token. Use map_token = “your token” when running the generate_maps() function. 

Other Included data files (for mapping) found in system.file("data", package = "LobTag2"):
NS_extent   ## default basemap to use for reference inset map when making maps. You can choose to draw your own when running generate_maps().
depthraster2.tif   ## depth raster used for drawing probable paths
