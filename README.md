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

Create an hash object to store the results

```
d <- hash::hash()
```

Read the LIA measures for objects and mines

```
d <- read_lia(d = d,
              d.tag = "lia.objects",
              df.path = "C:/Rprojects/itineRis/results/LIA data objects.xlsx",
              header.line = 2)
d <- read_lia(d = d,
              d.tag = "lia.mines",
              pattern.objects.num = "Locality/.Mine",
              pattern.objects.Pb206_Pb204 = "206Pb/204Pb",
              pattern.objects.Pb207_Pb204 = "207Pb/204Pb",
              pattern.objects.Pb208_Pb204 = "208Pb/204Pb",
              df.path = "C:/Rprojects/itineRis/results/Coordinates-mines_Thomas Huet.xlsx",
              header.line = 1)
```
Group the datasets and assign colors

```
library(dplyr)

mydf <- rbind(d$lia.mines, d$lia.objects)
df.isotop <- isotop_dataframe(mydf)
df.isotop[df.isotop$object == "golasecca", "color.object"] <- "#0000FF"
df.isotop[df.isotop$object == "hochdorf", "color.object"] <- "#00FFFF"
df.isotop[df.isotop$object == "France", "color.object"] <- "#FF0000"
df.isotop[df.isotop$object == "France", "symbol"] <- "triangle"
df.isotop[df.isotop$object == "Iberian Peninsula", "color.object"] <- "#FFA500"
df.isotop[df.isotop$object == "Iberian Peninsula", "symbol"] <- "triangle"
df.isotop[df.isotop$object == "Switzerland", "color.object"] <- "#964B00"
df.isotop[df.isotop$object == "Switzerland", "symbol"] <- "triangle"
```

Create a Plotly plot (3D interactive) where LIA measurements of objects and mines are displayed

```
isotop_3d(df.isotop,
          vars = c("Pb206_Pb204", "Pb207_Pb204", "Pb208_Pb204"),
          marker.size = 5,
          export.plot = T,
          out.plot = "isotop_3d.html",
          dirOut = "C:/Rprojects/itineRis/results/")
```

## A simplier example...


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
