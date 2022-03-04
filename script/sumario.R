
### carregar pacotes
library(tidyverse)
library(reshape2)
library(openxlsx)
library(vegan)
library(leaflet)
library(leaflet.minicharts)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggrepel)
library(igraph)
library(rgdal)
library(patchwork)
library(splitstackshape)
library(scatterpie)
library(rgdal)



# setwd("~/github/workshop_temporal")

### carregar dados
dados <- read.xlsx("data/dados temporais_19Fev.xlsx", sheet=3)
dados <- dados %>% 
  aggregate(cbind(lat, lon) ~ local + nome_programa, ., mean) %>%
  rename(lat_mean = lat,
         lon_mean = lon) %>%
  inner_join(dados)

formulario <- read.xlsx("data/Respostas_WS_temporais final.xlsx", skipEmptyRows = TRUE)

# central coordinates
pontos <- dados %>% 
  filter(!nome_programa %in% c("IEAPM", "PELD-ILOC", "Projeto Coral-Sol", "Sem nome")) %>% 
  aggregate(cbind(lat, lon) ~ nome_programa + peld, ., mean) %>% 
  filter(nome_programa != "IEAPM") %>% 
  bind_rows(dados %>% select(nome_programa, peld, lat, lon) %>% 
              filter(nome_programa %in% c("PELD-ILOC", "Projeto Coral-Sol")) %>% 
              distinct()) %>% 
  mutate(nome_programa = plyr::mapvalues(nome_programa, from = c("Ecologia dos Ambientes Estuarinos da BTS", "Igor_UFBA", "SeagrassNet Cabo Frio", "Projeto Coral-Sol", "Projeto Costão Rochoso"), 
                                         to = c("LEB", "LEB", "LEMBUERJ", "LEMBUERJ", "LECAR"))) %>% 
  distinct()

# world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# areas prioritarias (Magris et al 2021)
priori_ll <- readOGR("output/Priorities_.shp") %>% 
  spTransform(., CRS("+proj=longlat +datum=WGS84"))

##########################
# FREQUENCIA DE AMOSTRAGEM
# teste2 <- dcast(dados, local + lat_mean + lon_mean ~ frequencia, value.var="habitat")
# dados$local %>% as.factor() %>% levels()
# dados$habitat %>% as.factor() %>% levels()
# dados$grupos_biologicos %>% as.factor() %>% levels()
# 
# 
# teste3 <- cbind(teste2[,1:3], decostand(teste2[,4:ncol(teste2)], method = 'pa'))
# 
# leaflet() %>%
#   addTiles() %>%
#   addMinicharts(
#     teste3$lon, teste3$lat,
#     type = "pie",
#     chartdata = teste3[, 4:ncol(teste3)],
#     colorPalette = topo.colors(12)
#   )

####
# HABITAT
habitat <- dcast(dados, local + lat_mean + lon_mean ~ habitat, value.var = "habitat")
habitat1 <- cbind(habitat[,1:3], decostand(habitat[,4:ncol(habitat)], method = 'pa'))

# leaflet() %>%
#   addTiles() %>%
#   addMinicharts(
#     habitat1$lon, habitat1$lat,
#     type = "polar-radius",
#     chartdata = habitat1[, 5:11],
#     #width = 20, transitionTime = 0 #60 * sqrt(prod2016$total) / sqrt(max(prod2016$total))
#   )

####
# GRUPOS MONITORADOS
# teste <- dcast(dados, local+lat+lon~grupos_biologicos, value.var="habitat")
# teste1 <- cbind(teste[,1:3], decostand(teste[,4:ncol(teste)], method = 'pa'))
# 
# leaflet() %>%
#   addTiles() %>%
#   addMinicharts(
#     teste1$lon, teste1$lat,
#     type = "pie",
#     chartdata = teste1[, 4:ncol(teste1)],
#     colorPalette = topo.colors(12)
#   )
# 
####
# SITES
# sites <- aggregate(sites~local+lat+lon+habitat, dados, max)
# sites[sites == 326] <- 1
# 
# leaflet() %>%  
#   addTiles() %>%
#   addMinicharts(
#     sites$lon, sites$lat,
#     showLabels = TRUE,
#     width = 50
#   )
# 
# 
#################
# FORM INTERNET

# lapply(formulario, unique)
# names(formulario)
# 
# 
# formulario %>% 
#   dplyr::filter(PELD == "Sim")

# continuidade (anos e interrupcoes)


### financiamento 
# old
# financiador <- formulario[,c(11:16)] # aporte_rel:teses_etc
# financiamento <- formulario[,c(17)] # producao

# new
financiamento <- formulario %>% 
  filter(PELD == "Sim") %>% 
  select(nome_programa, fontes, aporte_rel, aporte_valor)

producao <- formulario %>% 
  filter(PELD == "Sim") %>% 
  select(nome_programa, producao_n)

#plot(net1, edge.arrow.size=0, vertex.label=NA)
#plot(net1, edge.arrow.size=0)
#plot(net, vertex.size=10, vertex.label.family="Arial Black" )

#l <- layout.circle(net1)
#l1 <- layout.sphere(net1)
#plot(net1, layout=l1)
#plot(net1, layout=layout.fruchterman.reingold)


# # # END # # #
########################################################
########################################################
########################################################
# Conversao de coordenadas



# dados copiados da planilha
# bts_estuario <- read.table(pipe("pbpaste"), sep="\t", header=T)
# 
# utms <- SpatialPoints(bts_estuario[, c("Longitude", "Latitude")], proj4string=CRS("+proj=utm +zone=24 +init=epsg:32724 +datum=WGS84")) #create UTM matrix
# 
# longlats <- spTransform(utms, CRS("+proj=longlat")) #transform
# 
# bts_estuario$lon <- coordinates(longlats)[,1]
# bts_estuario$lat <- coordinates(longlats)[,2]
# 
# bts_estuario$sites <- paste(bts_estuario$site, bts_estuario$estacao)
# 
# 
# 
# # dados adquiridos no site (https://peld.furg.br/metadados)
# sul <- read.table(pipe("pbpaste"), sep="\t", header=T)
# 
# chd = substr(sul$lat_deg, 3, 3)[1]
# chm = substr(sul$lat_deg, 6, 6)[1]
# chs = substr(sul$lat_deg, 9, 9)[1]
# 
# cd_lat = char2dms(as.vector(sul$lat_deg),chd=chd,chm=chm,chs=chs)
# cd_lon = char2dms(as.vector(sul$lon_deg),chd=chd,chm=chm,chs=chs)
# 
# as.numeric(cd_lon)
# 
# sul <- data.frame(sul, lat=as.numeric(cd_lat), lon=as.numeric(cd_lon))
# 
# coords <- cbind(rbind(sul[,c(1,4,5)], bts_estuario[,c(7,5,6)]), region=c(rep('RS',15), rep('BTS',31)))
# 
# write.csv(coords, '/Users/cesarcordeiro/Google Drive/LECAR/projetos/Workshop temporal/coord_converted.csv', row.names = F)
# 
# ###
# leaflet(coords) %>%  
#   addTiles() %>%
#   addMarkers(~lon, ~lat, popup = ~as.character(sites), label = ~as.character(sites), clusterOptions = markerClusterOptions())
# 
# char2dms(c('31d54.865mS', '52d09.138mW'), chd=chd,chm=chm,chs=chs) %>% as.numeric()


# https://www.wikiparques.org/wiki/%C3%81rea_de_Prote%C3%A7%C3%A3o_Ambiental_de_Fernando_de_Noronha,_Rocas,_S%C3%A3o_Pedro_e_S%C3%A3o_Paulo


formulario %>% 
  filter(PELD == "Sim") %>%  
  dplyr::select(nome_programa, nome_uc) %>% 
  separate(nome_uc, ";", into = LETTERS[1:5]) %>% 
  pivot_longer(cols = A:E, values_to = "MPA") %>% 
  mutate(MPA = str_trim(MPA, side = "both"),
         MPA = gsub("APA", "Área de Proteção Ambiental", MPA),
         MPA = gsub("PARNAM", "Parque Nacional Marinho", MPA),
         MPA = gsub("REVIS", "Refúgio da Vida Silvestre", MPA)) %>% 
  select(-name) %>% 
  na.omit() %>% 
  distinct() #%>% 
# mutate(MPA_start = c(1988, 1979, 1986),
#        Management_plan = c(, 2007, 2017),
#        area_ha = c(, , 154409.03))

Parque Nacional Marinho de Fernando de Noronha -> 10929.47, 1988, 1990, "federal"
Reserva Biológica Atol das Rocas -> 35186.41, 1979, 2007, "federal"                                                 
Área de Proteção Ambiental de Fernando de Noronha - Rocas - São Pedro e São Paulo -> 38450193.81, 2018, NA, "federal"
Monumento Natural de Trindade e Martim Vaz -> 6769671,75, 2018, NA, "federal"                                       
Monumento Natural do Arquipélago de São Pedro e São Paulo -> 4726317.84, 2018, NA, "federal" 
Refúgio da Vida Silvestre do Molhe do Leste -> 30, 1996, NA, "estadual"                                 
Área de Proteção Ambiental Costa das Algas -> 115001.92, 2010, NA, "federal"                                       
Refúgio da Vida Silvestre de Santa Cruz -> 17709.39, 2010, NA, "federal"                                        
Parque Nacional Marinho dos Abrolhos -> 87943.14, 1983, 1991, "federal"
Resex Marinha do Arraial do Cabo -> 51601.46, 1997, 2020, "federal"
Área de Proteção Ambiental Costa dos Corais -> 406085.93, 1997, 2021, "federal"                                 
Parque Nacional Marinho Abrolhos -> 87943,14, 1983, 1991, "federal"                                                
Área de Proteção Ambiental Ponta da Baleia Abrolhos -> 346535, 1993, NA, "estadual"                              
Área de Proteção Ambiental Costa das Algas -> 115001.92, 2010, NA, "federal"                                       
Refúgio da Vida Silvestre Santa Cruz -> 17.709,39, 2010, NA, "federal"                                            
Área de Proteção Ambiental Costa dos Corais  -> 406.085,93, 1997, 2021, "federal"                                      
Área de Proteção Ambiental Guadalupe -> 44799, 1997, NA, "estadual"                                             
Parque Municipal Marinho do Forte de Tamandaré -> 349, 2003, NA, "municipal"                                 
Área de Proteção Ambiental de Guapi-Mirim -> 13890.54, 1984, 2004, "federal"                                        
Estação Ecológica da Guanabara -> 1936.25, 2006, 2012, "federal"                                                   

APA do Arquipélago de Trindade e Martim Vaz -> 40385419.59, 2018, NA, "federal"
Parna da Restinga de Jurubatiba - > 14919.46, 1998, 2007, "federal"



####
# variables and sampling frequency

dados %>% 
  select(nome_programa, peld, habitat, grupos_biologicos, frequencia) %>% 
  filter(peld == "PELD") %>% 
  distinct()

