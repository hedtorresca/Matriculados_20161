##############################
#Matriculados a PREGRADO 2016-I##
###############################

########***** Por lugar de residencia

#Se requieren las siguientes librerías#

library(rgdal)
library(leaflet)
library(htmlwidgets)
library(tidyverse)
library(rjson)
library(readxl)
library(extrafont)


#Lectura de base de datos DIVIPOLA#

divipola.R <- read.table("DIVIPOLA_20160930.csv", sep=";", header=T)
municipios <-divipola.R[,5]
departamentos <- divipola.R[,4]
latitud <- divipola.R[,9]
longitud <- divipola.R[,8]
tipo_centro <- divipola.R[,7]
code_mun <- divipola.R[,2]
code_dept <- divipola.R[,1]
poblados <- data.frame(municipios, code_mun, code_dept, departamentos, longitud, latitud, tipo_centro)

#Base de datos con solo cabeceras municipales #

cabeceras <- poblados[tipo_centro=="CABECERA MUNICIPAL (CM)",]



#Lectura de datos de matriculados por lugar de nacimiento#

procedencia.R <- read.table("2016_matriculados.csv", sep=";", header=T, encoding = "UTF-8") 
nivel <- procedencia.R[,1]
depart_asp <- procedencia.R[,5]
ciudad_asp <- procedencia.R[,6]
codept_asp<-procedencia.R[,13]
codept_asp <-as.integer(as.character(codept_asp))

codecity_asp <-procedencia.R[,15]
codecity_asp <-as.integer(as.character(codecity_asp))
long_asp <- procedencia.R[,16]
lat_asp <- procedencia.R[,17]
indi <- procedencia.R[,20]<-1
matriculados<-data.frame(nivel,depart_asp,codecity_asp,codept_asp, ciudad_asp,long_asp, lat_asp, indi)

#Omitir matriculados del extranjero

matriculados <- matriculados%>%filter(depart_asp!="DEPARTAMENTO EXTRANJERO")
matriculados <- matriculados%>%filter(nivel!="POS")
matriculados <- matriculados%>%filter(codept_asp!=666)
matriculados <- matriculados%>%filter(codept_asp!=9999)
matriculados <- matriculados%>%filter(codept_asp!="SIN INFORMACIÓN")
matriculados <- matriculados%>%filter(codecity_asp!=91)



#Contar número de matriculados por departamento#

cant_asp <- table(matriculados[matriculados$indi==1,]$codept_asp)
cant_asp

#Contar número de matriculados por ciudad#

cantasp_city <- table(matriculados[matriculados$indi==1,]$codecity_asp)
cantasp_city

#Chequear cuáles son las capitales y crear data.frame solo con estas capitales#

check.integer <- function(x) {
  x == round(x)
}

#Localizar las capitales de cada departamento

capitales <- cabeceras[check.integer((cabeceras$code_mun-1)/1000)==T,]

#Se omite Agua de Dios

capitales <- capitales%>%filter(code_mun!="25001")
capitales

asp_capitales <- matriculados[check.integer((matriculados$codecity_asp-1)/1000)==T,]
asp_capitales

#Contar número de matriculados por capital

cantasp_cap <- table(asp_capitales[asp_capitales$indi==1,]$codecity_asp)
cantasp_cap


#Extraer lista de códigos de los municipios con la respectiva cantidad de matriculados

json_file <- "mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS=as.integer(json_data$features[[i]]$properties$MPIOS)
}

codigos <- matrix(0, nrow=1122,ncol=2)

for(i in 1:1122){
  codigos[i,1]=json_data$features[[i]]$properties$MPIOS
}

cant_cities=as.data.frame(cantasp_city)

cant_cities$Var1=as.integer(as.character(cant_cities$Var1))

for(i in cant_cities$Var1){
  codigos[codigos[,1]==i,2]=cant_cities[cant_cities$Var1==i,2]
}

codigos <-data.frame(codigos) 



#Lectura de JSON de Colombia por departamentos

colombia.R<- rgdal::readOGR("depto4.json", use_iconv = T, encoding= "UTF-8")

colombia.R@data<-colombia.R@data[-c(1,3,4,5)] #Se omite información complementaria

colombia.R@data$CANT_ASP=cant_asp

# Buscar el centroide de cada departamento en la base


x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}


#Centroides de los departamentos
centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

#Contar número de municipios

no_muni<-table(cabeceras[cabeceras$tipo_centro=="CABECERA MUNICIPAL (CM)",]$code_dept)
no_muni <- data.frame(x, no_muni)
colnames(no_muni) <- c("dept", "cod", "No")
no_muni


######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("mpio5.json",use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[-c(1,2,3,4,5,7,9,10,11,12,13,14,15)]



#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI=codigos[,1]
cities_col.R@data$CANT_ASP=codigos[,2]

#Abreviar nombre de Bogotá

#cities_col.R@data$NOMBRE_MPI <- gsub("SANTAFÉ DE BOGOTÁ D.C", "BOGOTÁ D. C", cities_col.R@data$NOMBRE_MPI )

#Leer en formato excel la base de datos de DIVIPOLA

divipola2.R <- read_excel("DIVIPOLA_20160930.xlsx")
cabeceras2 <- divipola2.R %>% filter(divipola2.R$`Tipo Centro Poblado` == "CABECERA MUNICIPAL (CM)")


#ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Sedes de Presencia Nacional

capitaless <- cabeceras2[check.integer((cabeceras2$`Código Municipio`-1)/1000)==T,]

#Suprimir Agua de Dios
capitaless <- capitaless%>%filter(`Código Municipio`!="25001")

#Filtrar sedes de la Universidad Nacional de Colombia

sedes <- rbind(cabeceras2[cabeceras2$`Código Municipio`== "5001",],
               cabeceras2[cabeceras2$`Código Municipio`== "11001",], 
               cabeceras2[cabeceras2$`Código Municipio`== "76520",], 
               cabeceras2[cabeceras2$`Código Municipio`== "52835",],
               cabeceras2[cabeceras2$`Código Municipio`== "17001",],
               cabeceras2[cabeceras2$`Código Municipio`== "88001",],
               cabeceras2[cabeceras2$`Código Municipio`== "81001",],
               cabeceras2[cabeceras2$`Código Municipio`== "91001",])

Sede <- c("Medellín", "Bogotá", "Palmira", "Tumaco", "Manizales",   "Caribe", "Orinoquia", "Amazonas" )

sedes <- cbind(sedes, Sede)
sedes

#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")


label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

#Función para mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


depto <- as.vector(no_muni$dept)
reducido <- c("ANT", "ATL", "BDC", "BOL", "BOY", "CAL", "CAQ", "CAU",  "CES", "COR", "CUN", "CHO", "HUI", "LAG", "MAG", "MET", "NAR", "NSA", "QUI", "RIS", "SAN", "SUC", "TOL", "VAC", "ARA", "CAS", "PUT", "AMA", "GUA", "GUV", "VAU","VID", "SAP")

list_kurz <- cbind.data.frame(depto, reducido)
list_kurz

#Se omiten departamentos

list_kurz1 <- list_kurz[c(1,5, 13, 14, 17, 21, 22, 23, 25, 16, 27),] 
list_kurz1

list_kurz2 <- list_kurz[c(2, 6, 19, 20),]
list_kurz2


list_kurz3 <- list_kurz[c(7, 16),]
list_kurz3


list_kurz4 <- list_kurz[c( 9,12, 15),]
list_kurz4

