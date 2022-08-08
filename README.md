# ***itineRis*** <img src="https://raw.githubusercontent.com/anr-itineris/itineris/main/www/logo.png" width='200px' align="right"/>
> R package of the ANR-Itineris


# Install and load package

Install the R package

```
devtools::install_github("zoometh/itineRis")
```

And load the package

```
library(itineRis)
```

By default, output will be saved in the `results/` folder. You can change the output folder by changing the `dirOut` option in the various functions.

# 3D interactive plot of LIA

Read the LIA dataframe and create a Plotly plot (3D interactive)

```
library(dplyr)

df.isotop <- isotop_dataframe(
  df.path = paste0("C:/Rprojects/itineRis/inst/extdata/isotop_results.tsv"),
  color.column = "object"
)

isotop_3d(df.isotop,  
          vars = c("Pb206_Pb204", "Pb207_Pb204", "Pb208_Pb204"),
          export.plot = T,
          out.plot = "isotop_3d.html",
          dirOut = "C:/Rprojects/itineRis/results/")
```

Gives:

<img src="https://raw.githubusercontent.com/zoometh/itineRis/main/results/isotop_3d.png" width='500px'>

# Interactive map of mines

Create an interactive leaflet map of mines

```
d <- hash::hash()
d <- read_mines(d = d,
                df.path = "C:/Rprojects/itineRis/results/Coordinates-mines_Thomas Huet.xlsx")
map_leaflet(d = d,
            d.coords = "mines.coords",
            export.plot = T,
            out.plot = "map_sites.html",
            dirOut = "C:/Rdev-itineris/itineris/data/")
```

Gives:

<img src="https://raw.githubusercontent.com/zoometh/itineRis/main/results/map_sites.png" width='500px'>
