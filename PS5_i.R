#1 
library(sp)
library(rgdal)
library(sf)
library(raster)

wind_Jan <- raster("F://R/ESE5023/wind/wc2.1_2.5m_wind_01.tif")
wind_Feb <- raster("F://R/ESE5023/wind/wc2.1_2.5m_wind_02.tif")
wind_Mar <- raster("F://R/ESE5023/wind/wc2.1_2.5m_wind_03.tif")
wind_Apr <- raster("F://R/ESE5023/wind/wc2.1_2.5m_wind_04.tif")
wind_May <- raster("F://R/ESE5023/wind/wc2.1_2.5m_wind_05.tif")
wind_Jun <- raster("F://R/ESE5023/wind/wc2.1_2.5m_wind_06.tif")
wind_Jul <- raster("F://R/ESE5023/wind/wc2.1_2.5m_wind_07.tif")
wind_Aug <- raster("F://R/ESE5023/wind/wc2.1_2.5m_wind_08.tif")
wind_Sep <- raster("F://R/ESE5023/wind/wc2.1_2.5m_wind_09.tif")
wind_Oct <- raster("F://R/ESE5023/wind/wc2.1_2.5m_wind_10.tif")
wind_Nov <- raster("F://R/ESE5023/wind/wc2.1_2.5m_wind_11.tif")
wind_Dec <- raster("F://R/ESE5023/wind/wc2.1_2.5m_wind_12.tif")
prec_Jan <- raster("F://R/ESE5023/prec/wc2.1_2.5m_prec_01.tif")
prec_Feb <- raster("F://R/ESE5023/prec/wc2.1_2.5m_prec_02.tif")
prec_Mar <- raster("F://R/ESE5023/prec/wc2.1_2.5m_prec_03.tif")
prec_Apr <- raster("F://R/ESE5023/prec/wc2.1_2.5m_prec_04.tif")
prec_May <- raster("F://R/ESE5023/prec/wc2.1_2.5m_prec_05.tif")
prec_Jun <- raster("F://R/ESE5023/prec/wc2.1_2.5m_prec_06.tif")
prec_Jul <- raster("F://R/ESE5023/prec/wc2.1_2.5m_prec_07.tif")
prec_Aug <- raster("F://R/ESE5023/prec/wc2.1_2.5m_prec_08.tif")
prec_Sep <- raster("F://R/ESE5023/prec/wc2.1_2.5m_prec_09.tif")
prec_Oct <- raster("F://R/ESE5023/prec/wc2.1_2.5m_prec_10.tif")
prec_Nov <- raster("F://R/ESE5023/prec/wc2.1_2.5m_prec_11.tif")
prec_Dec <- raster("F://R/ESE5023/prec/wc2.1_2.5m_prec_12.tif")
srad_Jan <- raster("F://R/ESE5023/srad/wc2.1_2.5m_srad_01.tif")
srad_Feb <- raster("F://R/ESE5023/srad/wc2.1_2.5m_srad_02.tif")
srad_Mar <- raster("F://R/ESE5023/srad/wc2.1_2.5m_srad_03.tif")
srad_Apr <- raster("F://R/ESE5023/srad/wc2.1_2.5m_srad_04.tif")
srad_May <- raster("F://R/ESE5023/srad/wc2.1_2.5m_srad_05.tif")
srad_Jun <- raster("F://R/ESE5023/srad/wc2.1_2.5m_srad_06.tif")
srad_Jul <- raster("F://R/ESE5023/srad/wc2.1_2.5m_srad_07.tif")
srad_Aug <- raster("F://R/ESE5023/srad/wc2.1_2.5m_srad_08.tif")
srad_Sep <- raster("F://R/ESE5023/srad/wc2.1_2.5m_srad_09.tif")
srad_Oct <- raster("F://R/ESE5023/srad/wc2.1_2.5m_srad_10.tif")
srad_Nov <- raster("F://R/ESE5023/srad/wc2.1_2.5m_srad_11.tif")
srad_Dec <- raster("F://R/ESE5023/srad/wc2.1_2.5m_srad_12.tif")

#1.2 - 1.4

wind_year<- mean(wind_Jan+wind_Feb+wind_Mar+wind_Apr+wind_May+wind_Jun+wind_Jul+
              wind_Aug+wind_Sep+wind_Oct+wind_Nov+wind_Dec)
prec_year <- mean(prec_Jan+prec_Feb+prec_Mar+prec_Apr+prec_May+prec_Jun+prec_Jul+
                prec_Aug+prec_Sep+prec_Oct+prec_Nov+prec_Dec)
srad_year <- mean(srad_Jan+srad_Feb+srad_Mar+srad_Apr+srad_May+srad_Jun+srad_Jul+
                srad_Aug+srad_Sep+srad_Oct+srad_Nov+srad_Dec)

China_map <- readOGR("F://R/ESE5023/China_map", "bou2_4p")

wind_china<-crop(wind_year,China_map)
wind_china <- mask(wind_china,China,na.rm=T)
plot(wind_china, main="wind in china", col=col)
contour(wind_china, add=T, levels=4,col="red")


prec_china<-crop(prec_year,China_map)
prec_china <- mask(prec_china,China,na.rm=T)
plot(prec_china,  main="prec in china", col=col)
contour(prec_china, add=T,levels=100, col="red")

srad_china<-crop(srad_year,China_map)
srad_china <- mask(srad_china,China,na.rm=T)
plot(srad_china,  main="srad in china", col=col)
contour(srad_china, add=T,levels=16000, col="red")
# MingYANG noticed:
# 1.3 & 1.4 missing
# the end

#2.1
cd ~
  ln -s data_demo data_demo_link
ll
#2.2
cd data_demo
cd data
touch planets.txt_1st
ll
#2.3
echo ~
  #2.4
  cd pdb
find
#2.5
grep -c "$c" tnt.pdb
#2.6
diff ethane.pdb ethanol.pdb 
#2.7
cd ~/data_demo
du
#2.8
zip -r data_demo_new.zip data_demo_new
unzip data_demo_new.zip
A #all replace
#2.9
chmod 710 data_demo_new
#2.10
history 10

