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


##########################################################################################################
#                                Définit l'interface du logiciel
##########################################################################################################

shinyUI(navbarPage("PROJET R",
                   tabPanel("Component 1",
                            leafletOutput('myMap',
                                          width = "100%",
                                          height= "90vh"),
                            absolutePanel(top   = "10vh",
                                          right = "3vw",
                                          fileInput(
                                              inputId     = "DATA_pluie",
                                              label       = "Ex_dossier_pluie",
                                              multiple    = TRUE,   # Affiche plusieurs fichers dans une
                                              accept      = NULL,   # seule fois
                                              width       = NULL ,
                                              buttonLabel = "Browse...",
                                              placeholder = "No file selected"),
                                          uiOutput("ui")            # Après le chargements des raster ,il y aura
                            )                                       # un slider
                   ),
                   tabPanel("Component 2"),
                   tabPanel("Component 3")
)
)


