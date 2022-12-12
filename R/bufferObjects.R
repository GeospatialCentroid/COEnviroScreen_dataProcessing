###
# buffer spatial features and summarize all weighted intersections
# carverd@colostat.edu
# 20211001
###

### The distance and weight values need to be ranked relative to each other
### as they are assigned based on index.
# ex.
# weight <- c(1,0.5,0.25,0.1)
# dist <- c(250,500,750,1000)
# all distances are in meters


bufferObjects <- function(bufferFeature, g2, dist, weight){
  ### Preforms and intersect across various distances and returns all intersections
  # bufferFeatures : the spatial object you are looking to buffer
  # geometry : the goemetry object to test buffer intersections agains
  # dist : vector of number values for distance in meters.
  # weight : vector of numeric values to apply at various distances
  ###
  # test for correct CRS
  if(sf::st_crs(bufferFeature) != st_crs(5070) |sf::st_crs(g2) != st_crs(5070)){
    return(print("Please ensure that both spatial objects are project in ESPG 5070 before attempting this function"))
  }else{

    # create list to hold output results from buffering process
    intersect <- vector("list", length = length(dist))

    #test for single intersection at largest buffer
    b <- st_buffer(bufferFeature, 1000)
    # crop the geomentry layer to the extent of the maximun buffer object
    g3 <- st_intersection(g2, st_as_sfc(st_bbox(b)))
    if(nrow(g3)==1){
      # set weight to direct intersect value
      bufferFeature$weight <- 1
      bufferFeature$dist <- 250
      bufferFeature$GEOID <- g3$GEOID
      intersect_all <- st_drop_geometry(bufferFeature)%>% select(GEOID, everything())
    }else{
      # itorate over other scales
      for(i in 1:length(dist)) {
        # generate the buffer and asign relative weight
        b <- st_buffer(bufferFeature, dist[i])%>%
          mutate(dist = dist[i], weight = weight[i])

        # test for the intersection between buffered object and geometry feature
        b2 <- st_intersection(g3, b)
        # store result
        intersect[[i]] <- b2
      }
      #now combine into one df
      intersect_all <- bind_rows(intersect) %>% st_drop_geometry()
    }

    return(intersect_all)
  }
}
