# Bivariate Map in R
# Author: Lance R. Owen 
# Date: 7 June 2022


pacman::p_load(biscale, tidycensus, geojsonR, leaflet, rgdal, geojsonio, sf, ggplot2, cowplot)


# Import TopoJSON and set CRS
SVI_2018 <- topojson_read("https://raw.githubusercontent.com/lancerowen23/bivariate_choropleth/main/SVI2018_US_county.json") 
SVI_2018 <-  st_set_crs(SVI_2018, "EPSG:4326")
SVI_2018_Albers <-SVI_2018 %>% 
  st_transform(5070) %>% 
  select(FIPS, RPL_THEMES, E_DISABL) %>% 
  st_set_geometry(NULL)


# Census Boundary file counties 20m
counties_2018 <- topojson_read("https://raw.githubusercontent.com/lancerowen23/US_topojson/main/cb_2018_us_county_20m_AK_HI.json") 
counties_2018 <-  st_set_crs(counties_2018, "EPSG:4326")
counties_2018_Albers <-counties_2018 %>% st_transform(5070)

SVI_2018_Albers <- merge(counties_2018_Albers, SVI_2018_Albers, by.x = "GEOID", by.y = "FIPS")

# States for borders
states_2018 <- topojson_read("https://raw.githubusercontent.com/lancerowen23/US_topojson/main/cb_2018_us_state_20m_AK_HI.json") 
states_2018 <-  st_set_crs(states_2018, "EPSG:4326")
states_2018_Albers <-states_2018 %>% st_transform(5070)


# Look at extent of variables in sf dataframe
colnames(SVI_2018)
length(unique(SVI_2018$STATE))

us_data <- bi_class(SVI_2018_Albers, 
                    x = E_DISABL, 
                    y = RPL_THEMES, 
                    style = "quantile", 
                    dim = 3)

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
  
legend <- bi_legend(pal = "DkCyan",
                    dim = 3,
                    xlab = "Higher SVI",
                    ylab = "Higher Disability",
                    size = 10)

finalPlot <- cowplot::ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.1, 0.6, 0.2, 0.2)

finalPlot

#get correlations 
library(corrplot)

#subset selected variables
list <- as.list(colnames(SVI_2018))
SVI_2018_vars <- SVI_2018 %>% 
  select(E_DISABL, E_SNGPNT, E_MINRTY, E_LIMENG, E_MUNIT) %>% 
  st_drop_geometry()
  
str(SVI_2018_vars)
pairs(SVI_2018_vars)

m <- cor(SVI_2018_vars)
corrplot(m, method = 'number', order = 'AOE')
