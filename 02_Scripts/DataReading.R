### Data reading script
library(haven)
library(dplyr)
library(readxl)

#DATA

### Main dataframe with sensor data
datos <- haven::read_dta("01_Data/Sensors_PM2.5_Long.dta")

### School coordinates file
coordenadas_escuela <- haven::read_dta("01_Data/sedes_cor.dta")

### Ground truth with all schools that had sensores installed
gt <- read_excel("01_Data/Groundtruth.xlsx")
gt <- gt %>% select(nombreSede,Localidad,cdigosede,installed_filter,installed_monitor,Sensor1,
                    Sensor2,Sensor_ext) %>% pivot_longer(cols=c("Sensor1","Sensor2","Sensor_ext"))

### Shapefile with polygons for maps
shapefile <- st_read("01_Data/Shapefiles/loca/")
shapefile <- st_transform(shapefile, 4326)
shapefile$LocNombre[shapefile$LocNombre == "LOS MARTIRES"] <- "MARTIRES"


#OTHER GLOBAL VARIABLES
WHO_THRESHOLD <- 15
DAILY_THRESHOLD <- 5
