library(raster)  # load before dplyr (against select conflict)
library(tidyverse)
library(httr)
library(sf)
library(btb)

fr <- dat %>%
      st_cast("POLYGON")

comm <- fr %>% 
        st_make_valid() %>% 
        st_point_on_surface()
        
        
comm %>% 
  lissage("total", 625, 62.5, fr, 32736) %>%
  raster::writeRaster(here("figures/pop.tif"))
