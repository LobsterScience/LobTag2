
Installation

Enter these lines into the R console to install the package:

install.packages("devtools")

devtools::install_github("yutannihilation/ggsflabel")

devtools::install_github('LobsterScience/LobTag2')

You may need to update some of your packages while installing if you have older versions of dependencies installed.
After installation, find the location of your user guide by entering:

When you load the package you should get a message telling you where to find the user guide. Read the user guide for more info.
Loading the package creates a folder called LOBTAG on your C: drive, all of your files (user guide, data upload templates, extra data, etc. can be found here).

Getting a Mapbox token:
You will need an account with Mapbox to use the mapping functionality. Setting up an account is quick and easy, just go to: 
https://account.mapbox.com/auth/signup/
and create an account. Once you have an account you’ll get a free a public token. Use map_token = “your token” when running the generate_maps() function. 

Other included data files found in system.file("data", package = "LobTag2"): 


gebco_2024.tif ## depth raster that functions use to identify land


knit_rewards.Rmd ## RMarkdown file used to knit lobster letters


new_land_coords.rds ## polygons that define additional land segments to the initial depth raster for mapping. User can add more land to this file with the draw_land() function.



Other possible dependency issues:
vctrs
