#### R script to get Lat and Lon from CETESB Stations of automatic,
#### continuous monitoring of air quality in Sao Paulo State, Brazil
#### Otavio Ranzani, 21/07/2017

#### Geocoding Sao Paulo State stations for 
#### Open Online, Air Quality Data - openaq

rm(list=ls())

require(dplyr)
require(ggmap)
require(readxl)
require(sp)
require(rgdal)

### Sources of data:
### online, from http://ar.cetesb.sp.gov.br/configuracao-da-rede-automatica/
### for street address, automatic monitoring network

### If you log in at the QUALAR (http://qualar.cetesb.sp.gov.br/qualar/home.do), 
### you can get data about stations GIS:
### at the left menu,"CONSULTAS" -> "Configurações das Estações" -> "Automático"

### You can also gather data (stations, UTM for geocoding) from the 2016 report, PDF file:
### http://ar.cetesb.sp.gov.br/wp-content/uploads/sites/37/2013/12/relatorio-ar-2016.pdf

### I used the data from QUALAR, copied and paste in a Excell sheet, exported 21/07/2017

### The exact Latitute and Longitude data are in UTM format, zones 22k and 23k. 
### I mailed CETESB and got the datum used: SIRGAS 2000
### SIRGAS 2000 is considered similar to the WGS84, but I generated the information
### from the Proj4String referece: http://www.spatialreference.org/ref/epsg/4674/proj4js/

## -> "+proj=utm +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
## I added +zone +south

## importing data frame from a Excel file instead of csv (to maintain Brazilian Portuguese for google)

db <- read_excel("~/Dropbox/AirPollution/Brazil/CETESB/automatic_stations.xlsx", 
                 sheet = "toimport", na = "empty")

### Transforming Lat/Lon from UTM, datum SIRGAS 2000 to Lat/Lon in degrees

### The stations are located in the zones 22k and 23k. To transform them
### in the same projection, need some cooding I am not expert in
### So, I decided to split the db in those at 22k and 23k

db_twthree <- subset(db, zone=="23k"|zone=="23K")
db_twtwo <- subset(db, zone=="22k")

### zone 23k, 62 stations
### transforming data.frame
coordinates(db_twthree) <- ~long_utm+lat_utm
proj4string(db_twthree) <- CRS("+proj=utm +south +zone=23k +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
### transforming to longlat
res_twthree <- spTransform(db_twthree, CRS("+proj=longlat"))

### zone 22k, 8 stations
### transforming data.frame
coordinates(db_twtwo) <- ~long_utm+lat_utm
proj4string(db_twtwo) <- CRS("+proj=utm +south +zone=22k +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
### transforming to longlat
res_twtwo <- spTransform(db_twtwo, CRS("+proj=longlat"))

### checking the plot with Sao Paulo State "map"
plot(res_twthree, 
     axes=T, col="black", pch=19, cex=0.25,
     xlim=c(-52,-44),
     ylim=c(-24,-20))
plot(res_twtwo,col="red", pch=19, cex=0.25, add=T)

## OK

### creating a new csv
db_final23 <- data.frame(res_twthree@data$station_name, 
                         res_twthree@coords)
db_final22 <- data.frame(res_twtwo@data$station_name, 
                         res_twtwo@coords)
names <- c("station_name", "lon", "lat")

colnames(db_final23) <- names
colnames(db_final22) <- names

db_final <- rbind(db_final23, db_final22)

write.csv(db_final, file = "~/Dropbox/AirPollution/Brazil/CETESB/saopaulo_stationsgeo.csv")

###### Just confirming and testing, using the raw addressess from the web or PDF file
## initial tests

a <- geocode("Rua Espanha, 386, Jd das Nações, Taubaté,
             Brasil", output="more", source="google")
a$country
a$lon
a

## OK

## I confirmed Lat and Lon based on street adresses and geocode function
## nb -> sometimes geocode, based on googlemaps, found 16 or 18 problematic directions
## I corrected it manually, bellow.

result <- geocode(db$adress, output = "more", source = "google")
db <- cbind(db, result)

## Filtering 18 stations with problems

# problems come with "s/n", meaning without street number; or
# "Esquina com", meaning corner with...
# Some place reference, like, "Library XYZ",

# all adressess below were manually checked in googlemaps, etc
# these indexes (rows order) may change depending on the problem googlpmaps return
# off course, there are better ways to deal with it

db_solve$adress
db_solve$adress[1] <- "Rua Suécia, 465, Vila Santa Maria, Americana"
db_solve$adress[2] <- "Avenida Doutor Heitor Penteado, Portão 5, Jd N S Auxiliadora, Campinas"
db_solve$adress[3] <- "Praça Vereador Pietrobom, Vila Bressani, Paulínia"
db_solve$adress[4] <- "Rua Angelo Pigatto Ferro, Santa Terezinha, Paulínia"
db_solve$adress[5] <- "Avenida Inocêncio Seráfico, 1051, Centro, Carapicuíba"
db_solve$adress[6] <- "Rua da Consolação, 94, Consolação, São Paulo"
db_solve$adress[7] <- "Rua Benjamin Constant, 3, Vila Diadema, Diadema" 
db_solve$adress[8] <- "Avenida Embaixador Macedo Soares, 7995, Lapa, Vila Anastácio, São Paulo"
db_solve$adress[9] <- "Rua Bresser, 2341, Moóca, São Paulo"
# here I looked at the number in google, because "Avenida dos Autonomistas is a long road"
db_solve$adress[10] <- "Avenida dos Autonomistas, 4769, Vila Quitaúna, Osasco"
db_solve$adress[11] <- "Estrada Turística do Jaraguá, 9000, Vila Jaraguá, São Paulo"
db_solve$adress[12] <- "Rua Manágua, 02, Parque Capuava, Santo André"  
db_solve$adress[13] <- "Praça Quarto Centenário, Centro, Santo André"
db_solve$adress[14] <- "Rua Padre José Maria, 555, Santo Amaro, São Paulo"  
db_solve$adress[15] <- "Rua Castro Alves, Vila Souto, Bauru" #not much exact, short street
db_solve$adress[16] <- "Rua Sete de Setembro, 1319, Vila Nova, Jaú" #number manually
db_solve$adress[17] <- "Rua Fortaleza, 1310, Vila Rodrigues, Catanduva" 
db_solve$adress[18] <- "Rua Roberto Simonsen, 464, Vila Santa Helena, Presidente Prudente" 