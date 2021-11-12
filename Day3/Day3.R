## 2020 results 

## load dplyr
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(sf)
library(tigris)
library(twriTemplates)
library(stringr)

nms <- counties()

nms <- nms %>% filter(STATEFP != "02")

ak_voting <- state_legislative_districts(state = "Alaska", house = "lower")
ak_voting ## this is correct but the codes are weird. 02027 should be 02927

ak_voting <- ak_voting %>%
  mutate(COUNTYFP = str_replace(SLDLST, str_sub(SLDLST, 1, 1), "9")) %>%
  mutate(GEOID = paste0(STATEFP, COUNTYFP))

nms <- bind_rows(nms, ak_voting)


nms %>% 
  mutate(fips5 = paste0(STATEFP, COUNTYFP)) %>%
  mutate(AREA = st_area(nms)) %>%
  mutate(AREA = units::set_units(AREA, "mi^2")) -> nms

## read data from github
d <- readr::read_csv("https://github.com/kjhealy/us_elections_2020_csv/blob/master/results_x2020_11_18_15_02_23.csv?raw=true")




d %>%
  filter(race == "President",
         id != "0") %>%
  select(id, fips_char, fips5, place, votes, party) %>%
  group_by(fips5, party) %>%
  summarise(votes = sum(votes, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = party,
              values_from = votes) %>%
  mutate(Total = DEM + REP,
         P.Dem = DEM/Total*100,
         P.Rep = REP/Total*100) %>%
  select(fips5, DEM, REP, Total, P.Dem, P.Rep) %>%
  ## margin of victory
  mutate(MOV = P.Rep - P.Dem) ->d


nms %>%
  left_join(d, by = c("fips5" = "fips5")) %>%
  mutate(vote_density = Total/as.numeric(AREA),
         log_vote_density = log10(vote_density)) %>%
  mutate(alpha = log_vote_density/max(log_vote_density, na.rm =TRUE)) %>%
  arrange(desc(alpha)) %>%
  mutate(alpha = case_when(
    !is.na(alpha) ~ alpha,
    is.na(alpha) ~ 0
  )) %>%
  filter(!is.na(Total)) %>%
  lwgeom::st_transform_proj("+proj=moll +lon_0=-96.4247 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") -> df
  
## make a bivariate type legend
bv_data <- tibble(MOV = seq(-1, 1, length.out = 50),
                  vote_density = seq(1, 
                                     max(df$vote_density, na.rm = TRUE),
                                     length.out = 50),
                  log_vote_density = log10(vote_density),
                  alpha = vote_density/max(vote_density, na.rm = TRUE))
bivariate_scale <- bv_data %>%
  tidyr::expand(MOV, alpha)

fn_ecdf <- ecdf(bv_data$vote_density)
vote_density_labs <- c(1000,5000,10000,13000)
y_tick_pos <- fn_ecdf(vote_density_labs)

legend <- ggplot(bivariate_scale) +
  geom_raster(aes(x = MOV,
                y = alpha,
                fill = MOV,
                alpha = alpha),
            interpolate = TRUE) +
  scale_fill_gradient(name = "", limits = c(-1, 1), low = "#2222dd", high = "#dd2222") +
  scale_alpha_continuous(name = "", range = c(0, 1)) +
  scale_x_continuous(breaks = c(-1, -0.5, 0, .5, 1),
                     labels = c("100", "50", "0", "50", "100")) +
  scale_y_continuous(breaks = y_tick_pos, labels = vote_density_labs) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none",
        axis.title = element_text(size = 8, family = "OpenSansCondensed_TWRI"),
        axis.title.y = element_text(angle = 90),
        axis.text = element_text(size = 8, family = "OpenSansCondensed_TWRI"),
        axis.ticks.y = element_line(color = "grey50", size = 0.25),
        axis.ticks.length.y = unit(6, "points")) +
  labs(x = "<- Democratic   Republican ->\nMargin of Victory [%]",
       y = "Votes Per Sq Mi") 
  
ggplot(df) +
  geom_sf(aes(fill = MOV, alpha = alpha), color = NA) +
  scale_fill_gradient(name = "", limits = c(-100, 100), low = "#2222dd", high = "#dd2222") +
  theme_minimal() +
  labs(caption = "2020 Presedential Election Vote Share\nSource: https://github.com/kjhealy/us_elections_2020_csv") +
  theme(plot.caption = element_text(family = "OpenSansCondensed_TWRI",
                                    size = 6),
        legend.position = "none",
        axis.text = element_blank()) -> map

library(patchwork)
layout <- c(
  area(t = 1, l = 1, b = 8, r = 10),
  area(t = 1, l = 8, b = 2, r = 10)
)

map + legend + plot_layout(design = layout) -> p1
p1


ragg::agg_png("Day3/day3.png", width = 1200, height = 900, res = 200)
p1
dev.off()