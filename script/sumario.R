
### carregar pacotes
library(dplyr)
library(plyr)
library(reshape2)
library(ggplot2)
library(openxlsx)
library(vegan)
library(leaflet)
library(leaflet.minicharts)


### carregar dados
dados <- read.xlsx("~/Google Drive/LECAR/projetos/Workshop temporal/DADOS/dados temporais_19Fev.xlsx", sheet=3)
# dados <- read.xlsx("data/dados temporais_19Fev.xlsx", sheet=3) # Codigo JPQ
head(dados)

forms <- read.xlsx("~/Google Drive/LECAR/projetos/Workshop temporal/DADOS/Respostas workshop temporais.xlsx")
names(forms)

####
# FREQUENCIA DE AMOSTRAGEM
teste2 <- dcast(dados, local+lat+lon~frequencia, value.var="habitat")
dados$local %>% as.factor() %>% levels()
dados$habitat %>% as.factor() %>% levels()
dados$grupos_biologicos %>% as.factor() %>% levels()


teste3 <- cbind(teste2[,1:3], decostand(teste2[,4:ncol(teste2)], method = 'pa'))

leaflet() %>% 
  addTiles() %>%
  addMinicharts(
    teste3$lon, teste3$lat,
    type = "pie",
    chartdata = teste3[, 4:ncol(teste3)],
    colorPalette = topo.colors(12)
  )

####
# HABITAT
habitat <- dcast(dados, local+lat+lon~habitat, value.var="habitat")
habitat1 <- cbind(habitat[,1:3], decostand(habitat[,4:ncol(habitat)], method = 'pa'))

leaflet() %>% 
  addTiles() %>%
  addMinicharts(
    habitat1$lon, habitat1$lat,
    type = "pie",
    chartdata = habitat1[, 5:11],
    #width = 20, transitionTime = 0 #60 * sqrt(prod2016$total) / sqrt(max(prod2016$total))
  )

####
# GRUPOS MONITORADOS
teste <- dcast(dados, local+lat+lon~grupos_biologicos, value.var="habitat")
teste1 <- cbind(teste[,1:3], decostand(teste[,4:ncol(teste)], method = 'pa'))

leaflet() %>% 
  addTiles() %>%
  addMinicharts(
    teste1$lon, teste1$lat,
    type = "pie",
    chartdata = teste1[, 4:ncol(teste1)],
    colorPalette = topo.colors(12)
  )
  
####
# SITES
sites <- aggregate(sites~local+lat+lon+habitat, dados, max)
sites[sites == 326] <- 1

leaflet() %>%  addTiles() %>%
  addMinicharts(
    sites$lon, sites$lat,
    chartdata = sites$sites,
    showLabels = TRUE,
    width = 50
  )


#################
# FORM INTERNET
lapply(forms, unique)

# 
peld_ativo <- forms[,c(4:10)] %>% 
  filter("O.programa.é,.ou.já.foi,.associado.ao.programa.Pesquisa.Ecológica.de.Longa.Duração.(PELD).do.CNPq?" == "Sim")

# 
financiador <- forms[,c(11:16)]
financiamento <- forms[,c(17)]

# 
produtos <- forms[,c(18:23)]

# 
divulgacao <- forms[,c(24:33)]
divulgacao1 <- forms[,c(34)]

# MPA
divulgacao <- forms[,c(37,38)]

# # # END # # #
########################################################
########################################################
########################################################
# Conversao de coordenadas

library(rgdal)

# dados copiados da planilha
bts_estuario <- read.table(pipe("pbpaste"), sep="\t", header=T)

utms <- SpatialPoints(bts_estuario[, c("Longitude", "Latitude")], proj4string=CRS("+proj=utm +zone=24 +init=epsg:32724 +datum=WGS84")) #create UTM matrix

longlats <- spTransform(utms, CRS("+proj=longlat")) #transform

bts_estuario$lon <- coordinates(longlats)[,1]
bts_estuario$lat <- coordinates(longlats)[,2]

bts_estuario$sites <- paste(bts_estuario$site, bts_estuario$estacao)



# dados adquiridos no site (https://peld.furg.br/metadados)
sul <- read.table(pipe("pbpaste"), sep="\t", header=T)

chd = substr(sul$lat_deg, 3, 3)[1]
chm = substr(sul$lat_deg, 6, 6)[1]
chs = substr(sul$lat_deg, 9, 9)[1]

cd_lat = char2dms(as.vector(sul$lat_deg),chd=chd,chm=chm,chs=chs)
cd_lon = char2dms(as.vector(sul$lon_deg),chd=chd,chm=chm,chs=chs)

as.numeric(cd_lon)

sul <- data.frame(sul, lat=as.numeric(cd_lat), lon=as.numeric(cd_lon))

coords <- cbind(rbind(sul[,c(1,4,5)], bts_estuario[,c(7,5,6)]), region=c(rep('RS',15), rep('BTS',31)))

write.csv(coords, '/Users/cesarcordeiro/Google Drive/LECAR/projetos/Workshop temporal/coord_converted.csv', row.names = F)

###
leaflet(coords) %>%  
  addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(sites), label = ~as.character(sites), clusterOptions = markerClusterOptions())

char2dms(c('31d54.865mS', '52d09.138mW'), chd=chd,chm=chm,chs=chs) %>% as.numeric()
