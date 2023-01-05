# creating new Justice40 layer
# updated by federal gov 11/22/22
#
# Updating in EnviroScreen 12/12/22


filePath <- "data/input/spatialLayers/justice40/1.0-communities.csv"

getJustice40(filePath,
             removeNativeLand = TRUE,
             overwrite = TRUE)
