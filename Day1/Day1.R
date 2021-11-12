library(archive)
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(twriTemplates)
temp_fle <- tempfile()
download.file("https://echo.epa.gov/files/echodownloads/echo_exporter.zip", temp_fle)
archive::archive(temp_fle)



df <- read_csv(archive::archive_read(temp_fle))

df %>%
  filter(!is.na(NPDES_IDS),
         !is.na(FAC_LAT),
         !is.na(FAC_LONG)) %>%
  filter(NPDES_FLAG == "Y") %>%
  filter(CWA_PERMIT_TYPES == "Major") %>%
  st_as_sf(coords = c("FAC_LONG", "FAC_LAT")) %>%
  st_set_crs(4326) %>%
  lwgeom::st_transform_proj("+proj=moll +lon_0=-96.4247 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>%
  ggplot() +
  geom_sf(color = "dodgerblue", size = 0.25, alpha = 0.15) +
  theme_void() +
  labs(caption = "Major Permitted US Wastewater Dischargers\nSource: EPA Environmental Compliance and History Online Database") +
  theme(plot.caption = element_text(family = "OpenSansCondensed_TWRI",
                                    size = 8))-> p1

p1


ragg::agg_png("Day1/day1.png", width = 1200, height = 1200, res = 144)
p1
dev.off()
