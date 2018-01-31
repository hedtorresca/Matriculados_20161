#################################################
###Mapas según el número de matriculados###########



source("resi_mat.R" ,  encoding = "UTF-8")


labels_mun <- sprintf(
  "<strong> %s </strong> (%s) <br/> %g  matriculados" , 
  cities_col.R@data$NOMBRE_MPI ,  cities_col.R@data$NOMBRE_DPT ,  cities_col.R@data$CANT_ASP
) %>% lapply(htmltools::HTML)


centroidecol <- centro_dept%>% filter(dept  == "CUNDINAMARCA")
centroidecol


#### MAPA PARA CERO O MÄS DE UN ASPIRANTE####

pal_uno <- colorBin(palette = c("#fdae61" ,  "#a6d96a") , bins = c(0 , 1 ,  35000))

municipios2 <- leaflet(cities_col.R) %>% setView(lat = centroidecol$lat , lng =  centroidecol$lon , zoom = 9)




for (k in c(1 , 2 , 3)) {
  municipios2 <- municipios2 %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 5))
}

municipios2 <- municipios2 %>%
  addLayersControl(baseGroups = names.esri ,  overlayGroups = c( "Mostrar <br> sedes UNAL") , 
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -69.79854267 ,  lat = 4.444487622 ,  zoom = 6)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~pal_uno(CANT_ASP) , 
              label = labels_mun ,  labelOptions = labelOptions(     style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addPolylines(data =  colombia.R ,  stroke = T ,  smoothFactor = 0.05 ,  color = "white" ,  weight = 2)%>%
  addLegend("bottomright" ,  values = ~CANT_ASP ,  bins = c(0 , 1 ,  34750) , 
            title = "Matriculados" , labels = c("0 matriculados" , "1 o más matriculados") ,  colors = c("#fdae61" ,  "#a6d96a") , opacity = 1)%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  hideGroup("Mostrar <br> sedes UNAL")%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(4.796276 , -74.11589) ,  6); }")))%>%
  addLabelOnlyMarkers(data =  colombia.R ,  lat = centro_dept$lat ,  lng = centro_dept$lon , label =  ~paste0(sapply(tolower(NOMBRE_DPT) , simpleCap)) ,   labelOptions = labelOptions(zoomAnimation = T ,  noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "9px") )%>%
  addCircleMarkers(data =  colombia.R ,  radius = 2 ,  fillOpacity =  0.9 ,  stroke = T ,  color =  "#377eb8" , fill =  T ,  fillColor = "purplelight"  , lat = capitaless$Latitud ,  lng = capitaless$Longitud)%>%
  addAwesomeMarkers(group = "Mostrar <br> sedes UNAL" ,  lat = sedes$Latitud ,  lng = sedes$Longitud ,  icon = sedeIcon ,  label =  label_sede ,  labelOptions = labelOptions(     style = list("font-weight" = "large" ,  padding = "3px 8px") ,   textsize = "15px" , direction = "auto"   ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))


municipios2

saveWidget(municipios2 , file = file.path(getwd() ,  "Residencia" ,  "J1_G2_I1_ COL1_F15.html")  ,  selfcontained = F , libdir = "libraryjs")

