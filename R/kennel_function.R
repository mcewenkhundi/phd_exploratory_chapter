#' Kernel weighted smoothing with arbitrary bounding area
#'
#' @param df sf object (points)
#' @param field weight field in the df
#' @param bandwidth kernel bandwidth (map units)
#' @param resolution output grid resolution (map units)
#' @param zone sf study zone (polygon)
#' @param out_crs EPSG (should be an equal-area projection)
#'
#' @return a raster object
#' @import btb, raster, fasterize, dplyr, plyr, sf
lissage <- function(df, field, bandwidth, resolution, zone, out_crs = 3035) {
  
  if (st_crs(zone)$epsg != out_crs) {
    message("reprojecting data...")
    zone <- st_transform(zone, out_crs)
  }
  
  if (st_crs(df)$epsg != out_crs) {
    message("reprojecting study zone...")
    df <- st_transform(df, out_crs)
  }
  
  zone_bbox <- st_bbox(zone)
  
  # grid generation
  message("generating reference grid...")
  zone_xy <- zone %>% 
    dplyr::select(geometry) %>% 
    st_make_grid(cellsize = resolution,
                 offset = c(plyr::round_any(zone_bbox[1] - bandwidth, resolution, f = floor),
                            plyr::round_any(zone_bbox[2] - bandwidth, resolution, f = floor)),
                 what = "centers") %>%
    st_sf() %>% 
    st_join(zone, join = st_intersects, left = FALSE) %>% 
    st_coordinates() %>% 
    as_tibble() %>% 
    dplyr::select(x = X, y = Y)
  
  # kernel
  message("computing kernel...")
  kernel <- df %>% 
    cbind(., st_coordinates(.)) %>%
    st_set_geometry(NULL) %>% 
    dplyr::select(x = X, y = Y, field) %>% 
    btb::kernelSmoothing(dfObservations = .,
                         sEPSG = out_crs,
                         iCellSize = resolution,
                         iBandwidth = bandwidth,
                         vQuantiles = NULL,
                         dfCentroids = zone_xy)
  
  # rasterization
  message("\nrasterizing...")
  raster::raster(xmn = plyr::round_any(zone_bbox[1] - bandwidth, resolution, f = floor),
                 ymn = plyr::round_any(zone_bbox[2] - bandwidth, resolution, f = floor),
                 xmx = plyr::round_any(zone_bbox[3] + bandwidth, resolution, f = ceiling),
                 ymx = plyr::round_any(zone_bbox[4] + bandwidth, resolution, f = ceiling),
                 resolution = resolution) %>% 
    fasterize::fasterize(kernel, ., field = field)
}