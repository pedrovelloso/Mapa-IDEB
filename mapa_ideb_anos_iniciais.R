#Limpa Workspace
rm(list = ls())

#Instalado Pacotes necessários####
# install.packages("sf")
# install.packages("sp")
# install.packages("ggplot2")
# install.packages("rgdal")
# install.packages("ggpubr")
# install.packages("RColorBrewer")
# install.packages("dplyr")
# install.packages("readxls")
# install.packages("maptools")
# install.packages("rgeos")


#Pacotes Usados #####
library(sf)
library(sp)
library(ggplot2)
library(rgdal)
library(ggpubr)
library(RColorBrewer)
library(dplyr)
library(readxl)
library(rgeos)
library(maptools)


############################################# MAPAS ################################################################

#Shape Brasil
brasil_mun <- readOGR(dsn = "D:/Pedro/Mapas/shapes_2019", layer = "BR_Municipios_2019")


#Criando o Shape dos estados e só do Ceará usando library(maptools)
uf_id <- brasil_mun$SIGLA_UF
brasil_uf <- unionSpatialPolygons(brasil_mun, uf_id)
brasil_uf <- st_as_sf(brasil_uf)
ceara <- subset(brasil_mun, SIGLA_UF == "CE")
ce_id <- ceara$SIGLA_UF
ceara <- unionSpatialPolygons(ceara, ce_id)
ceara <- st_as_sf(ceara)


############################################### BASE IDEB ##########################################################

#Base do Ideb 
ideb <- read_excel("D:/Pedro/Tese Pedro/divulgacao_anos_iniciais_municipios2017-atualizado-Jun_2019/ideb 05-17.xlsx")

########################################### Subset dos IDEB #######################################################

#IDEB 2005
ideb_2005 <- ideb[, c(1:5)]
ideb_2005 <- ideb_2005 %>% rename(CD_MUN = cod_mun)

#IDEB 2007
ideb_2007 <- ideb[,c(1:4,6)]
ideb_2007 <- ideb_2007 %>% rename(CD_MUN =cod_mun)

#IDEB 2009
ideb_2009 <- ideb[,c(1:4,7)]
ideb_2009 <- ideb_2009 %>% rename(CD_MUN =cod_mun)

#IDEB 2011
ideb_2011 <- ideb[,c(1:4,8)]
ideb_2011 <- ideb_2011 %>% rename(CD_MUN =cod_mun)

#IDEB 2013
ideb_2013 <- ideb[,c(1:4,9)]
ideb_2013 <- ideb_2013 %>% rename(CD_MUN =cod_mun)

#IDEB 2015
ideb_2015 <- ideb[,c(1:4,10)]
ideb_2015 <- ideb_2015 %>% rename(CD_MUN =cod_mun)

#IDEB 2017
ideb_2017 <- ideb[,c(1:4,11)]
ideb_2017 <- ideb_2017 %>% rename(CD_MUN =cod_mun)

#IDEB 2019
ideb_2019 <- ideb[,c(1:4,12)]
ideb_2019 <- ideb_2019 %>% rename(CD_MUN =cod_mun)



######################################### MERGE MAPA COM BASE IDEB ################################################

## A escolha desse intervalo é para seguir o padrão utilizado pelo inep em seu resumo tecnico
#http://download.inep.gov.br/educacao_basica/portal_ideb/planilhas_para_download/2017/ResumoTecnico_Ideb_2005-2017.pdf


#Merge Brasil com Ideb 2005
brasil_ideb_2005 <- merge(brasil_mun, ideb_2005, by = "CD_MUN")
brasil_ideb_2005 <- st_as_sf(brasil_ideb_2005)
brasil_ideb_2005 <- brasil_ideb_2005 %>% rename(ideb2005 = `IDEB 2005`) %>%
  mutate(ideb_2005_disc = cut(ideb2005,
                              breaks =  c(-Inf, 3.7, 4.9, 5.9, Inf),
                              labels = c("Até 3,7", "De 3,8 a 4,9", "5,0 a 5,9", "6,0 ou mais")))


#Merge Brasil com Ideb 2007 
brasil_ideb_2007 <- merge(brasil_mun, ideb_2007, by = "CD_MUN")
brasil_ideb_2007 <- st_as_sf(brasil_ideb_2007)
brasil_ideb_2007 <- brasil_ideb_2007 %>% rename(ideb2007 = `IDEB 2007`) %>%
  mutate(ideb_2007_disc = cut(ideb2007,
                              breaks =  c(-Inf, 3.7, 4.9, 5.9, Inf),
                              labels = c("Até 3,7", "De 3,8 a 4,9", "5,0 a 5,9", "6,0 ou mais")))



#Merge Brasil com Ideb 2009 
brasil_ideb_2009 <- merge(brasil_mun, ideb_2009, by = "CD_MUN")
brasil_ideb_2009 <- st_as_sf(brasil_ideb_2009)
brasil_ideb_2009 <- brasil_ideb_2009 %>% rename(ideb2009 = `IDEB 2009`) %>%
  mutate(ideb_2009_disc = cut(ideb2009,
                              breaks =  c(-Inf, 3.7, 4.9, 5.9, Inf),
                              labels = c("Até 3,7", "De 3,8 a 4,9", "5,0 a 5,9", "6,0 ou mais")))


#Merge Brasil com Ideb 2011 
brasil_ideb_2011 <- merge(brasil_mun, ideb_2011, by = "CD_MUN")
brasil_ideb_2011 <- st_as_sf(brasil_ideb_2011)
brasil_ideb_2011 <- brasil_ideb_2011 %>% rename(ideb2011 = `IDEB 2011`) %>%
  mutate(ideb_2011_disc = cut(ideb2011,
                              breaks =  c(-Inf, 3.7, 4.9, 5.9, Inf),
                              labels = c("Até 3,7", "De 3,8 a 4,9", "5,0 a 5,9", "6,0 ou mais")))


#Merge Brasil com Ideb 2013 
brasil_ideb_2013 <- merge(brasil_mun, ideb_2013, by = "CD_MUN")
brasil_ideb_2013 <- st_as_sf(brasil_ideb_2013)
brasil_ideb_2013 <- brasil_ideb_2013 %>% rename(ideb2013 = `IDEB 2013`) %>%
  mutate(ideb_2013_disc = cut(ideb2013,
                              breaks =  c(-Inf, 3.7, 4.9, 5.9, Inf),
                              labels = c("Até 3,7", "De 3,8 a 4,9", "5,0 a 5,9", "6,0 ou mais")))



#Merge Brasil com Ideb 2015
brasil_ideb_2015 <- merge(brasil_mun, ideb_2015, by = "CD_MUN")
brasil_ideb_2015 <- st_as_sf(brasil_ideb_2015)
brasil_ideb_2015 <- brasil_ideb_2015 %>% rename(ideb2015 = `IDEB 2015`) %>%
  mutate(ideb_2015_disc = cut(ideb2015,
                              breaks =  c(-Inf, 3.7, 4.9, 5.9, Inf),
                              labels = c("Até 3,7", "De 3,8 a 4,9", "5,0 a 5,9", "6,0 ou mais")))


#Merge Brasil com Ideb 2017
brasil_ideb_2017 <- merge(brasil_mun, ideb_2017, by = "CD_MUN")
brasil_ideb_2017 <- st_as_sf(brasil_ideb_2017)
brasil_ideb_2017 <- brasil_ideb_2017 %>% rename(ideb2017 = `IDEB 2017`) %>%
  mutate(ideb_2017_disc = cut(ideb2017,
                              breaks =  c(-Inf, 3.7, 4.9, 5.9, Inf),
                              labels = c("Até 3,7", "De 3,8 a 4,9", "5,0 a 5,9", "6,0 ou mais")))


#Merge Brasil com Ideb 2019
brasil_ideb_2019 <- merge(brasil_mun, ideb_2019, by = "CD_MUN")
brasil_ideb_2019 <- st_as_sf(brasil_ideb_2019)
brasil_ideb_2019 <- brasil_ideb_2019 %>% rename(ideb2019 = `IDEB 2019`) %>%
  mutate(ideb_2019_disc = cut(ideb2019,
                              breaks =  c(-Inf, 3.7, 4.9, 5.9, Inf),
                              labels = c("Até 3,7", "De 3,8 a 4,9", "5,0 a 5,9", "6,0 ou mais")))


###################################### FAZENDO MAPAS DO IDEB ######################################################

#Mapa 2005
mapa05 <- ggplot()+
  geom_sf(data = brasil_ideb_2005, aes(fill = ideb_2005_disc), color = NA)+ #Color = NA retira as linhas do mapa
  geom_sf(data = brasil_uf, color = "gray", fill = NA, size = 0.5)+ 
  geom_sf(data = ceara, color = "black", fill = NA, size = 1)+ #fill = NA torna o contorno do CE transparente
  theme(panel.grid.major = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank())+
  scale_fill_brewer(palette ="Spectral")+
  labs(fill ="IDEB 2005")



#Mapa 2007
mapa07< -ggplot()+
  geom_sf(data = brasil_ideb_2007, aes(fill = ideb_2007_disc), color = NA)+ #Color = NA retira as linhas do mapa
  geom_sf(data = brasil_uf, color = "gray", fill = NA, size = 0.5)+ 
  geom_sf(data = ceara, color = "black", fill = NA, size = 1)+ #fill = NA torna o contorno do CE transparente
  theme(panel.grid.major = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank())+
  scale_fill_brewer(palette ="Spectral")+
  labs(fill ="IDEB 2007")


#Mapa 2009
mapa09 <- ggplot()+
  geom_sf(data = brasil_ideb_2009, aes(fill = ideb_2009_disc), color = NA)+ #Color = NA retira as linhas do mapa
  geom_sf(data = brasil_uf, color = "gray", fill = NA, size = 0.5)+ 
  geom_sf(data = ceara, color = "black", fill = NA, size = 1)+ #fill = NA torna o contorno do CE transparente
  theme(panel.grid.major = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank())+
  scale_fill_brewer(palette ="Spectral")+
  labs(fill ="IDEB 2009")



#Mapa 2011
mapa11 <- ggplot()+
  geom_sf(data = brasil_ideb_2011, aes(fill = ideb_2011_disc), color = NA)+ #Color = NA retira as linhas do mapa
  geom_sf(data = brasil_uf, color = "gray", fill = NA, size = 0.5)+ 
  geom_sf(data = ceara, color = "black", fill = NA, size = 1)+ #fill = NA torna o contorno do CE transparente
  theme(panel.grid.major = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank())+
  scale_fill_brewer(palette ="Spectral")+
  labs(fill ="IDEB 2011")


#Mapa 2013
mapa13 <- ggplot()+
  geom_sf(data = brasil_ideb_2013, aes(fill = ideb_2013_disc), color = NA)+ #Color = NA retira as linhas do mapa
  geom_sf(data = brasil_uf, color = "gray", fill = NA, size = 0.5)+ 
  geom_sf(data = ceara, color = "black", fill = NA, size = 1)+ #fill = NA torna o contorno do CE transparente
  theme(panel.grid.major = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank())+
  scale_fill_brewer(palette ="Spectral")+
  labs(fill ="IDEB 2013")


#Mapa 2015
mapa15 <- ggplot()+
  geom_sf(data = brasil_ideb_2015, aes(fill = ideb_2015_disc), color = NA)+ #Color = NA retira as linhas do mapa
  geom_sf(data = brasil_uf, color = "gray", fill = NA, size = 0.5)+ 
  geom_sf(data = ceara, color = "black", fill = NA, size = 1)+ #fill = NA torna o contorno do CE transparente
  theme(panel.grid.major = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank())+
  scale_fill_brewer(palette ="Spectral")+
  labs(fill ="IDEB 2015")


#Mapa 2017
mapa17 <- ggplot()+
  geom_sf(data = brasil_ideb_2017, aes(fill = ideb_2017_disc), color = NA)+ #Color = NA retira as linhas do mapa
  geom_sf(data = brasil_uf, color = "gray", fill = NA, size = 0.5)+ 
  geom_sf(data = ceara, color = "black", fill = NA, size = 1)+ #fill = NA torna o contorno do CE transparente
  theme(panel.grid.major = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank())+
  scale_fill_brewer(palette ="Spectral")+
  labs(fill ="IDEB 2017")


#Mapa 2019
mapa19 <- ggplot()+
  geom_sf(data = brasil_ideb_2019, aes(fill = ideb_2019_disc), color = NA)+ #Color = NA retira as linhas do mapa
  geom_sf(data = brasil_uf, color = "gray", fill = NA, size = 0.5)+ 
  geom_sf(data = ceara, color = "black", fill = NA, size = 1)+ #fill = NA torna o contorno do CE transparente
  theme(panel.grid.major = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank())+
  scale_fill_brewer(palette ="Spectral")+
  labs(fill ="IDEB 2019")

mapa19


mapa <- ggarrange (mapa05, mapa11, mapa19,
                   ncol=3,
                   nrow=1)

mapa
###################################### Salvando os Mapas  ########################################################

