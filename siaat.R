### Preparacion de datos de accidentes de trabajo SIAAT para TPR
#Fuente: http://siel.stps.gob.mx:303/ibmcognos/cgi-bin/cognos.cgi?b_action=xts.run&m=portal/launch.xts&ui.gateway=XSSSTART*2fibmcognos*2fcgi-bin*2fcognos.cgiXSSEND&ui.tool=AnalysisStudio&ui.action=edit&launch.openJSStudioInFrame=true&ui.object=XSSSTART*2fcontent*2ffolder*5b*40name*3d*27SIAAT*27*5d*2fpackage*5b*40name*3d*27Cubo_5fSIAAT*27*5d*2fanalysis*5b*40name*3d*27Vista*20cubo*20del*20SIAAT*27*5dXSSEND&ui.drillThroughTargetParameterValues=k
# dar click en "ver más" y ajustar por mes por año según la información requerida
# o ir a reportes SIAAT STPS -> home -> launch ibm cognos connection -> siaat -> cubo siaat -> 
# plantilla -> ver más
#setwd('C:/Users/baldenebro/Documents/Datos_TPR')
agem <- read.csv('AGEEML.csv') # descarga desde https://www.inegi.org.mx/app/ageeml/#
agem <- subset(agem,select = c('CVE_ENT','NOM_ENT','NOM_ABR','CVE_MUN','NOM_MUN'))
agem <- unique(agem)

setwd(paste0(getwd(),'/SIAAT'))

paquetes <- c('magrittr','readxl','stringr','tidyr','dplyr','rjson','ggplot2',
              'geojsonio','plotly')

for (paq in paquetes) {
  if (!require(paq,character.only = T)) {
    install.packages(paq,dependencies = T)
    library(paq, character.only = T)
  }
}
# Accidentes de trabajo totales por EF ------------------------------------
 # Revisar esta parte para correr reporte como xls en cognos
tot_22 <- read.csv(paste0('22/230327 siaat_total_mes_ef_2022.csv'))

names(tot_22)[1] <- 'estado'
for (i in 2:length(names(tot_22))) {
  names(tot_22)[i] <- str_sub(names(tot_22)[i]
                              ,start = str_length(names(tot_22)[i])-2
                              ,str_length(names(tot_22)[i])
                              )
}
tot_22 <- pivot_longer(tot_22,names_to = 'mes',cols = !estado, values_to = 'acc')
tot_22$year <- 2022

tot_23 <- read.csv(paste0('2309/',"231024 siaat_total_mes_ef_2023.csv"))

names(tot_23)[1] <- 'estado'
for (i in 2:length(names(tot_23))) {
  names(tot_23)[i] <- str_sub(names(tot_23)[i]
                              ,start = str_length(names(tot_23)[i])-2
                              ,str_length(names(tot_23)[i])
  )
}
tot_23 <- pivot_longer(tot_23,names_to = 'mes',cols = !estado, values_to = 'acc')
tot_23$year <- 2023

#tot <- rbind(tot_22,tot_23)
tot <- tot_23
tot$acc <- replace_na(tot$acc,0)
# Incluir claves geoestadisticas
tot <- left_join(tot
                 ,unique(subset(agem,select = c('CVE_ENT','NOM_ENT','NOM_ABR')))
                 ,by = c('estado'='NOM_ENT')
                 )

### Totales del periodo a reportar en TPR
tot$fecha <- paste(tot$year,str_to_lower(tot$mes),'01',sep = '/') %>% 
  as.Date(.,format='%Y/%b/%d')

# Totales del periodo a reportar
inic <- "2023-04-01"
fin <- "2023-10-01"
tot_tpr <- aggregate(subset(tot,fecha>=inic&fecha<fin)
                     ,acc~estado+CVE_ENT, FUN=sum)
tot_tpr[order(tot_tpr$acc, decreasing = T),]
sum(tot_tpr$acc)
# Total agricultura por ef ------------------------------------------------
agr_22 <- read.csv("22/230327 siaat_ef_agr_mes_2022.csv") # Revisar esta parte para correr reporte como xls en cognos

names(agr_22)[1] <- 'estado'
for (i in 2:length(names(agr_22))) {
  names(agr_22)[i] <- str_sub(names(agr_22)[i]
                              ,start = str_length(names(agr_22)[i])-2
                              ,str_length(names(agr_22)[i])
  )
}
agr_22 <- pivot_longer(agr_22,names_to = 'mes',cols = !estado, values_to = 'acc')
agr_22$year <- 2022


agr_23 <- read.csv("2309/231024 siaat_ef_agr_mes_2023.csv") # Revisar esta parte para correr reporte como xls en cognos

names(agr_23)[1] <- 'estado'
for (i in 2:length(names(agr_23))) {
  names(agr_23)[i] <- str_sub(names(agr_23)[i]
                              ,start = str_length(names(agr_23)[i])-2
                              ,str_length(names(agr_23)[i])
  )
}
agr_23 <- pivot_longer(agr_23,names_to = 'mes',cols = !estado, values_to = 'acc')
agr_23$year <- 2023

#agr <- rbind(agr_22,agr_23)
agr <- agr_23

agr$acc <- replace_na(agr$acc,0)
# Incluir claves geoestadisticas
agr <- left_join(agr
                 ,unique(subset(agem,select = c('CVE_ENT','NOM_ENT','NOM_ABR')))
                 ,by = c('estado'='NOM_ENT')
)

### Totales del periodo a reportar en TPR
agr$fecha <- paste(agr$year,str_to_lower(agr$mes),'01',sep = '/') %>% 
  as.Date(.,format='%Y/%b/%d')

agr_tpr <- aggregate(subset(agr,fecha>=inic&fecha<fin)
                     ,acc~estado+CVE_ENT, FUN=sum)
agr_tpr[order(agr_tpr$acc, decreasing = T),]
sum(agr_tpr$acc)
# Jalisco agricultura -----------------------------------------------------

jal_22 <- read.csv("22/230237 siaat_jal_mun_agr_gen_2022.csv",na.strings = "")
names(jal_22)[1:3] <- c('mun','tipo_agr','gen')
for (i in 4:length(names(jal_22))) {
  names(jal_22)[i] <- str_sub(names(jal_22)[i]
                              ,start = str_length(names(jal_22)[i])-2
                              ,str_length(names(jal_22)[i])
  )
}
jal_22 <- tidyr::fill(jal_22,1:2)
jal_22 <- pivot_longer(jal_22,names_to = 'mes',cols = 4:length(names(jal_22))
                       , values_to = 'acc')
jal_22$year <- 2022


jal_23 <- read.csv("2303/230327 siaat_jal_mun_agr_gen_2023.csv",na.strings = "")
names(jal_23)[1:3] <- c('mun','tipo_agr','gen')
for (i in 4:length(names(jal_23))) {
  names(jal_23)[i] <- str_sub(names(jal_23)[i]
                              ,start = str_length(names(jal_23)[i])-2
                              ,str_length(names(jal_23)[i])
  )
}
jal_23 <- tidyr::fill(jal_23,1:2)
jal_23 <- pivot_longer(jal_23,names_to = 'mes',cols = 4:length(names(jal_23))
                       , values_to = 'acc')
jal_23$year <- 2023

jal_tot <- rbind(jal_22,jal_23)


# Incluir claves geoestadisticas
jal_tot <- left_join(jal_tot
                 ,unique(subset(agem,NOM_ENT=='Jalisco'
                                ,select = c('CVE_ENT','NOM_ENT','NOM_ABR'
                                            ,'CVE_MUN','NOM_MUN')))
                 ,by = c('mun'='NOM_MUN')
)
jal_tot$acc <- replace_na(jal_tot$acc,0)

### Totales de Jalisco del periodo a reportar en TPR
jal_tot$fecha <- paste(jal_tot$year,str_to_lower(jal_tot$mes),'01',sep = '/') %>% 
  as.Date(.,format='%Y/%b/%d')

jal_tot_tpr <- aggregate(subset(jal_tot,fecha>=inic&fecha<fin)
                     ,acc~mun+CVE_MUN, FUN=sum)

# Mapas -------------------------------------------------------------------
## Por Entidad Federativa
### Leer datos creados
carpeta_json <- "D:/OIT/Stats/Mapa/json"
ent_json <- rjson::fromJSON(file=paste0(carpeta_json,"/ent.geojson"))
ent_json_b <- geojson_read(paste0(carpeta_json,"/ent.geojson"),  what = "sp")

for (i in 1:32) {
  ent_json$features[[i]]$properties$CVEGEO <- ent_json$features[[i]]$properties$CVEGEO %>% 
    as.numeric() %>% as.character()
}

#### Mapa de total de accidentes por EF
df <- data.frame(region=as.character(formatC(tot_tpr$CVE_ENT,width = 2,flag = "0")),
                 value=tot_tpr$acc)

ent_json_c <- fortify(ent_json_b, region = 'CVEGEO') %>% 
  inner_join(.,df,by=c('id'='region'))
p <- ggplot(ent_json_c) +  
  geom_polygon(aes(x=long, y=lat, 
                   group=group,      #El argumento group=group arma grupos tanto para para los polígonos como para `fill=`. 
                   fill=value,
                   text = id)
  ) +
  labs(title = "Occupational Accidents",
       fill = 'Number of reported accidents (SIAAT)'
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
        ) 
ggplotly(p)


#### Mapa de total de accidentes en Agricultura por EF
df <- data.frame(region=as.character(formatC(agr_tpr$CVE_ENT,width = 2,flag = "0")),
                 value=agr_tpr$acc)

ent_json_d <- fortify(ent_json_b, region = 'CVEGEO') %>% 
  inner_join(.,df,by=c('id'='region'))
p <- ggplot(ent_json_d) +  
  geom_polygon(aes(x=long, y=lat, 
                   group=group,      #El argumento group=group arma grupos tanto para para los polígonos como para `fill=`. 
                   fill=value,
                   text = id)
  ) +
  labs(title = "Occupational Accidents - Agro-livestock",
       fill = 'Number of reported accidents (SIAAT)'
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
  ) +
  scale_fill_gradientn(colours = terrain.colors(5))
ggplotly(p)
