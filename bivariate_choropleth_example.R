# Bivariate Map in R
# Author: Lance R. Owen 
# Date: 7 June 2022


pacman::p_load(biscale, tidycensus, geojsonR, leaflet, rgdal, geojsonio, sf, ggplot2, cowplot)


# Import TopoJSON of SVI and set CRS
SVI_2018 <- topojson_read("https://raw.githubusercontent.com/lancerowen23/bivariate_choropleth/main/SVI2018_US_county.json") 
SVI_2018 <-  st_set_crs(SVI_2018, "EPSG:4326")
SVI_2018_Albers <-SVI_2018 %>% 
  st_transform(5070) %>% 
  select(FIPS, RPL_THEMES, E_DISABL) %>% 
  st_set_geometry(NULL) #This step removes the geometry prior to joining data with the new topojson below, which has AK and HI positioned below U.S. 


# Census Boundary file counties 20m with Alaksa and Hawaii positioned below U.S. 
counties_2018 <- topojson_read("https://raw.githubusercontent.com/lancerowen23/US_topojson/main/cb_2018_us_county_20m_AK_HI.json") 
counties_2018 <-  st_set_crs(counties_2018, "EPSG:4326")
counties_2018_Albers <-counties_2018 %>% st_transform(5070)

# Complete join
SVI_2018_Albers <- merge(counties_2018_Albers, SVI_2018_Albers, by.x = "GEOID", by.y = "FIPS")

# Import states version for borders and project into Albers
states_2018 <- topojson_read("https://raw.githubusercontent.com/lancerowen23/US_topojson/main/cb_2018_us_state_20m_AK_HI.json") 
states_2018 <-  st_set_crs(states_2018, "EPSG:4326")
states_2018_Albers <-states_2018 %>% st_transform(5070)

# Use bi_class function to set quantiles for bivariate mapping and choose two variables
us_data <- bi_class(SVI_2018_Albers, 
                    x = E_DISABL, 
                    y = RPL_THEMES, 
                    style = "quantile", 
                    dim = 3)

# Create map
map <- ggplot() +
  geom_sf(data = us_data, 
          mapping = aes(fill = bi_class), 
          color = "grey70", 
          size = 0.1,
          show.legend = FALSE) +
  geom_sf(data = states_2018_Albers,
          fill = NA,
          color = "white", 
          size = 1) +
  bi_scale_fill(pal = "DkCyan", 
                dim = 3) +
  labs(
    title = "UNITED STATES | SVI and Disability",
    subtitle = "CDC 2018 Social Vulnerability Index"
  ) +
  bi_theme()
 
# Create bivariate legend
legend <- bi_legend(pal = "DkCyan",
                    dim = 3,
                    xlab = "Higher SVI",
                    ylab = "Higher Disability",
                    size = 10)

# Compile final plot
finalPlot <- cowplot::ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.1, 0.6, 0.2, 0.2)

finalPlot
