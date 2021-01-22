############################################################################################################
# Librairy des packages
############################################################################################################

library(shiny)       # Quick start for shiny : file -> new file -> shiny web app ; ?BOOtsrap Navbar
library(leaflet)
library(rgdal)       # readOGR(): importer des shapefils
library(raster)
library(maptools)    # readshapepoints(): importer des multiples points ; multiple points of shapefile
library(RColorBrewer)
library(sf)
library(shinythemes)


###########################################################################################################
# Importer les bassint versants (shapefiles)
###########################################################################################################
# Dans le packageS de Leaflet , on utilse le systèmes de coordonnées géographiques WGS 84
# (a.k.a. EPSG:4326). et le systèmes de coordonnées géographiques projetées EPSG:3857
# Pour ça ,on doit transformer le Systèmes de Coordonnées de Référence,

Dir_bv      ="DATA/BV_shp/"
Brague_bv   = readOGR(paste0(Dir_bv,"BV_Brague.shp"))
Brague      = spTransform(Brague_bv, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
Brague      = Brague[-2,]  #Dans ce couche , il y a deux polygones identiques, on enlève la 2ème
proj4string = Brague_bv@proj4string  # Cet SCR est Lambert-93 EPSG:2154
# Celui de Brague_bv2 est Lambert zone II EPSG:27572
# Mais celui-ci n'affiche pas bon

Brague_bv2  = readOGR(paste0(Dir_bv,"BV_Valmasque_PontRD353.shp"))
Brague_bv2@proj4string = proj4string # On utilse Lambert-93 pour Brague_bv2
Brague2     = spTransform(Brague_bv2, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

Loup_bv     = readOGR(paste0(Dir_bv,"BV_Loup.shp"))
Loup        = spTransform(Loup_bv , CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

Siagne_bv   = readOGR(paste0(Dir_bv,"BV_Siagne.shp"))
Siagn       = spTransform(Siagne_bv , CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

Var_bv      = readOGR(paste0(Dir_bv,"Contour_Var.shp"))
Var         = spTransform(Var_bv, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

########################################################################################################
# Ajouter des attributes dans les shapefiles de bassin versants
########################################################################################################

# temps de réponse
temps_r = c(2, 0.5, 5 , 5.3,8)

Brague$temp_reponse  = temps_r[1]
Brague2$temp_reponse = temps_r[2]
Loup$temp_reponse    = temps_r[3]
Siagn$temp_reponse   = temps_r[4]
Var$temp_reponse     = temps_r[5]

#Gradientmax
Gradientmax = c(140,50,70,140,1600)

Brague$Gradientmax   = Gradientmax[1]
Brague2$Gradientmax  = Gradientmax[2]
Loup$Gradientmax     = Gradientmax[3]
Siagn$Gradientmax    = Gradientmax[4]
Var$Gradientmax      = Gradientmax[5]

#Popup: pour les pop-ups des bassint versants
noms_courdeau = c("Brague", "Valmasque" ,"Loup","Siagn","Var")

Brague$Popup         = noms_courdeau[1]
Brague2$Popup        = noms_courdeau[2]
Loup$Popup           = noms_courdeau[3]
Siagn$Popup          = noms_courdeau[4]
Var$Popup            = noms_courdeau[5]


ALL_BV = c(Brague,Brague2,Loup,Siagn,Var) # Mettre les Bvs dans un list

#########################################################################################################
# Importer les stations et les cours d'eau (shapefile )
#########################################################################################################

Dir_shp    = "DATA/couches_shp/"
Cours_deau = readOGR(paste0(Dir_shp,"Cours_deau.shp"))
stream     = spTransform(Cours_deau, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
Station    = readShapePoints(paste0(Dir_shp,"Station_priorisation_jaugeage.shp"))  # Du package maptools

########################################################################################################
# Une Fonction qui définit les échelles de risque
#######################################################################################################
# **Les donnés qu'on a : Le Gradientmax , Le temps de réponse , Les pluies pour chaque pas de temps **

# Pour définir les échelles de risques , On ne prend en compte que le gradient de monté. On va créer un
# L'index
#                           Index = Gradient/ Gradientmax.
#
# Quand l'index est plus élevé , la priorité de jaugeage est plus urgent
#
# Et on assume , le Gradient[m3/s/h] égale à débit[m3/h] divisé par le temps de réponse.
# Le débit pour chaque bassin versant est en fonction de la localisation du pluies et la surfaces de
# Bassin versant. Le débit = résolutions_X fois résolutions_Y fois Somme de lame d'eau [mm] / une heure.

inters_grille_bv = function(rain, ALL_BV) {
    rain_bv = c()
    resolution = res(rain)[1]*res(rain)[2]             # La surface de chaque pixel
    for (i in 1:length(ALL_BV))
    {
        bv          = ALL_BV[[i]]
        tmp         = extract(rain,bv,df=TRUE)
        Pluie_mm_h  = sum(tmp[,2],na.rm = TRUE)          # Son unité est : mm/h
        Pluie_m3_h  = (Pluie_mm_h*resolution/1000)*0.1   # Son unité est : m3/h et on pense le coefficient
        # de ruissellement est de 0.1

        Gradient    = Pluie_m3_h/(bv$temp_reponse*60*60) # Son unité est : m3/s/h
        Gradientmax = bv$Gradientmax                     # l'index est plus élevé , la priorité de jaugeage
        # est plus urgent
        Index       = Gradient/ Gradientmax
        rain_bv     = c(rain_bv, Index)
    }
    return(rain_bv)
}




##############################################################################################################################
#                                    Définir le server
#############################################################################################################################

shinyServer(function(input, output) {

    # Définit la palette pour les Bassin versants
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    col_vector    = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    pal_bv        = sample(col_vector, 6)
    pal_BV        = colorFactor(palette = pal_bv,
                                domain = c("Brague","Brague2","Loup","Siagn","Var"))

    # Définit la palette pour le légend des échelles de risques
    pal1          = colorFactor(c("Red","darkorange", "gold", "limegreen"),
                                domain = c("échelle 1","échelle 2","échelle 3","échelle 4"))


    map <- leaflet(Station) %>%
        fitBounds(lng1     = 6.91,
                  lat1     = 43.56,
                  lng2     = 7.14,
                  lat2     = 43.70
        ) %>%
        addTiles(options   = tileOptions(opacity = 1)) %>%
        addPolygons(data   = Brague,                          # Ajouter les Bassin versants
                    color  = pal_bv[1],
                    weight = 2,
                    fillOpacity = 0.6,
                    group  = "BASSIN VERSANT",
                    popup  = ~paste("Cour d'eau :",Popup)
        ) %>%
        addPolygons(data   = Brague2,
                    color  = pal_bv[2],
                    weight = 2,
                    fillOpacity = 0.6,
                    group  = "BASSIN VERSANT",
                    popup  = ~paste("Cour d'eau :",Popup)
        ) %>%
        addPolygons(data   = Loup,
                    color  = pal_bv[3],
                    weight = 2,
                    fillOpacity = 0.6,
                    group  = "BASSIN VERSANT",
                    popup  = ~paste("Cour d'eau :",Popup)
        ) %>%
        addPolygons(data   = Siagn,
                    color  = pal_bv[4],
                    weight = 2,
                    fillOpacity = 0.6,
                    group  = "BASSIN VERSANT",
                    popup  = ~paste("Cour d'eau :",Popup)
        ) %>%
        addPolygons(data   = Var,
                    color  = pal_bv[5],
                    weight = 2,
                    fillOpacity = 0.6,
                    group  = "BASSIN VERSANT",
                    popup  = ~paste("Cour d'eau :",Popup)
        ) %>%
        addPolylines(data  = stream ,
                     color = "navy",
                     weight  = 2,
                     opacity = 0.4,
                     group = "COUR D'EAU",
        ) %>%
        addCircleMarkers(
            radius = 6,
            color  = pal_bv[6],
            stroke = TRUE,
            fillOpacity = 1,
            group  = "STATION",
            popup  = ~LOCALISATI
        ) %>%
        addPopups(data  = Station,
                  popup = ~LOCALISATI
        ) %>%
        addLegend(pal      = pal1,
                  values   = c("échelle 1","échelle 2","échelle 3","échelle 4"),
                  title    = "ÉCHELLE DE RISQUE",
                  opacity  = 1,
                  position =  "bottomright",
        ) %>%
        addLegend(pal      = pal_BV,
                  values   = c("Brague","Brague2","Loup","Siagn","Var"),
                  title    = "BASSIN VERSANT",
                  opacity  = 1,
                  position = "bottomright",
        ) %>%
        addLayersControl(
            # baseGroups  = c("COUR D'EAU", "Toner", "Toner Lite"), 最基本的不能动
            overlayGroups = c("STATION", "COUR D'EAU","BASSIN VERSANT"),
            position      = "bottomright",
            options       = layersControlOptions(collapsed = TRUE) # 选择是否折叠
        )

    output$myMap = renderLeaflet(map)

    #########################################################################################################
    # Importer les rasters des pluies
    #########################################################################################################

    observeEvent(input$DATA_pluie, {
        list_files = as.data.frame(input$DATA_pluie[4])
        # 显示slider
        output$ui <- renderUI({
            sliderInput(inputId = "dynamic",
                        label   = "Les pluies",
                        min     = 1,
                        max     = nrow(list_files),
                        value   = 1,                 # La valeur initiale
                        step    = 1,
                        animate = animationOptions(interval = 2500, loop = FALSE)
            )
        })
    })

    #根据slider显示raster
    observeEvent(input$dynamic,{
        # index de la pluie
        Pluie_ind <- reactive({
            input$dynamic
        })
        list_files = as.data.frame(input$DATA_pluie[4])
        rain       = raster(list_files[Pluie_ind(),])
        rain@crs   = CRS("+init=epsg:27572")


        # 定risque
        all           = inters_grille_bv(rain,ALL_BV)
        noms_courdeau = c("Brague", "Valmasque" ,"Loup","Siagn","Var")
        noms_index    = c(1,4,2,3,5)
        df_risque     = data.frame(Bv      = c("Brague","Brague2","Loup","Siagn","Var"),
                                   courdeau   = noms_courdeau,
                                   noms_index = noms_index,
                                   Index      = all )
        df_ordered      = df_risque[order(df_risque$Index,decreasing = TRUE),]
        df_ordered$rang = 1:5
        df_reordered    = df_ordered[order(df_ordered$noms_index),]
        Station$risque  = df_reordered$rang


        # Définir la palette pour les rasters des pluies
        pal2 = colorNumeric(palette  = "YlGnBu", values(rain),
                            na.color = "transparent")
        #
        pal  = colorNumeric(c("Red","darkorange", "darkorange","gold", "limegreen"),
                            domain = c(1:5))


        # 修改存在的map
        leafletProxy("myMap") %>%
            clearControls()%>%
            clearMarkers()%>%
            addCircleMarkers(
                data    = Station,
                radius  = 6,
                color   = ~pal(risque),
                stroke  = TRUE,
                fillOpacity = 1,
                group   = "STATION",
                popup   = ~LOCALISATI,
                options = popupOptions(closeButton = FALSE)
            ) %>%
            addRasterImage(rain,
                           group  = "PLUIES",
                           colors = pal2) %>%
            addLegend(pal      = pal1,
                      values   = c("échelle 1","échelle 2","échelle 3","échelle 4"),
                      title    = "ÉCHELLE DE RISQUE",
                      opacity  = 1,
                      position =  "bottomright",
            ) %>%
            addLegend(pal      = pal_BV,
                      values   = c("Brague","Brague2","Loup","Siagn","Var"),
                      title    = "BASSIN VERSANT",
                      opacity  = 1,
                      position = "bottomright",
            ) %>%
            addLegend(pal      = pal2,
                      values   = values(rain),
                      title    = "La pluie",
                      position = "topleft"
            )%>%
            addLayersControl(
                # baseGroups  = c("COUR D'EAU", "Toner", "Toner Lite"), 最基本的不能动
                overlayGroups = c("STATION", "COUR D'EAU","BASSIN VERSANT","PLUIES"),
                position      = "bottomright",
                options       = layersControlOptions(collapsed = TRUE) # 折叠
            )
    })
})
