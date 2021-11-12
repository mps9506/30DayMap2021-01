library(archive)
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(twriTemplates)
library(terra)
library(rnaturalearth)
temp_fle <- tempfile()
download.file("https://echo.epa.gov/files/echodownloads/echo_exporter.zip", temp_fle)
archive::archive(temp_fle)



df <- read_csv(archive::archive_read(temp_fle))

us <- ne_countries(returnclass = "sf")
us <- us %>%
  filter(geounit == "United States of America")

#points to raster to contours

df %>%
  filter(!is.na(NPDES_IDS),
         !is.na(FAC_LAT),
         !is.na(FAC_LONG)) %>%
  filter(NPDES_FLAG == "Y") %>%
  filter(CWA_PERMIT_TYPES == "Major") %>%
  st_as_sf(coords = c("FAC_LONG", "FAC_LAT")) %>%
  st_set_crs(4326) -> df


df %>%
  st_filter(us) %>%
  lwgeom::st_transform_proj("+proj=moll +lon_0=-96.4247 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") -> df

df.spat <- terra::vect(df)
ras.temp <- terra::rast(ext(df.spat), resolution = 80000, crs = st_crs(df)$wkt)

ras.df <- terra::rasterize(df.spat, ras.temp, touches = TRUE, fun = length, background = 0)
plot(ras.df)


cl <- as.contour(ras.df, nlevels = 25)

lines(cl)


lines.sf <- st_as_sf(cl)

lines.sf <- lines.sf %>%
  lwgeom::st_transform_proj("+proj=moll +lon_0=-96.4247 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

us <- us %>%
  lwgeom::st_transform_proj("+proj=moll +lon_0=-96.4247 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") 

ggplot() +
  geom_sf(data = us, fill = "grey90", color = NA) +
  geom_sf(data = lines.sf, size = 0.25, color = "dodgerblue") +
  theme_void() +
  labs(caption = "Contour lines of WWTP location density\nSource: EPA Environmental Compliance and History Online Database") +
  theme(plot.caption = element_text(family = "OpenSansCondensed_TWRI",
                                    size = 8)) -> p1


p1


ragg::agg_png("Day2/day2.png", width = 1200, height = 900, res = 144)
p1
dev.off()