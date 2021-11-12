## hexagons yellowstone nat'l park

library(sf)
library(dplyr)
library(ggplot2)
library(elevatr)
library(terra)
library(twriTemplates)


parks <- st_read("https://raw.githubusercontent.com/imran-5/National_Parks.Geo.JSON/master/US/Polygons/national_parks_polygons.geojson")


yellowstone <- parks %>%
  filter(parkname == "Yellowstone") %>%
  st_transform(crs = 32612)

ggplot(yellowstone) +
  geom_sf()


elevation <- get_elev_raster(yellowstone, z = 12)

elevation <- rast(elevation)
elevation <- project(elevation, y = "epsg:32612")

## create and extent polygon
elev_ext <- as.polygons(ext(elevation), crs=crs(elevation))
elev_ext <- st_as_sf(elev_ext)

## create a hexagon grid in the extent polygon
hexagons <- st_make_grid(elev_ext,
                         n = c(100,100),
                         square = FALSE,
                         crs = crs(elev_ext))

hexagons <- vect(hexagons)  
crs(hexagons) <- crs(elevation)

## calculate the mean raster values in each polygon
elev_summary <- extract(x = elevation,
                         y = hexagons,
                         fun = median,
                         na.rm = TRUE)

## extract returns a matrix, need to get the data back
## into hexagons 
values(hexagons) <- data.frame(ID = 1:nrow(hexagons))
hexagons <- merge(hexagons, data.frame(elev_summary))
values(hexagons) <- data.frame(elev_summary)

names(hexagons) <- c("ID", "Elevation")

plot(hexagons, "Elevation")

## erase parts of hexagons outside of the park
yellowstone <- terra::vect(yellowstone)

clipped_hexagons <- terra::intersect(hexagons, yellowstone)
plot(clipped_hexagons, "Elevation")


clipped_hexagons <- st_as_sf(clipped_hexagons)

library(scico)
ggplot(clipped_hexagons) +
  geom_sf(aes(fill = Elevation), color = NA) +
  scale_fill_scico("Median Elevation [m]", palette = "tofino") +
  guides(fill = guide_colorbar(barheight = 10)) +
  theme_TWRI_print() +
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.direction = "vertical") -> p1
library(rayshader)  
## make 3D ggplot
plot_gg(p1, 
        multicore = TRUE, 
        width = 4*1.777, 
        height = 4,
        solidcolor = "white",
        theta = 0,
        phi = 80,
        fov = 0,
        zoom = .5,
        background = "grey80",
        windowsize = c(1920,1080))
render_highquality(filename = "Day4/Day4.png",
                   lightdirection = 45, 
                   lightaltitude = 60,
                   lightintensity = 1000,
                   samples = 1000, #lower this to get faster rendering
                   sample_method = "sobol",
                   parallel = TRUE,
                   width = 1920,
                   height = 1080,
                   ground_material = rayrender::diffuse(color = "grey40"),
                   clear = TRUE)
