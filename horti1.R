library(tidyverse)
library("googledrive")

drive_download(
    "https://docs.google.com/spreadsheets/d/1mr8NAuNCDRg20TFOT69bobscBcu8RhfolD9qZOWsNFQ/edit#gid=579148914",
  path = "data/muestra.xlsx",
  overwrite = TRUE
)



muestra <- readxl::read_excel("data/muestra.xlsx")
#censo <- 
#fao <- 
  
## Modelo productividad bruta
  ## kg anuales para RACC
  ## kg/anuales*m2
  
  ## Productividad (kg/m2*mes)

  funcion_fran <- function(i){
    x <- muestra[i,] 
    string <- x$Nombre_de_Cultivo
    return(as.numeric(select(x,string)))
  }

  muestra$recurr_esp <- apply(muestra,1,funcion_fran())
  
  muestra$recurr_esp <- funcion_fran(1:nrow(muestra))

  muestra_agregado <- 
  muestra %>% group_by(Productor_a , Nombre_de_Cultivo,.drop= TRUE) %>% 
  summarise("Prod_media"=mean(Peso_kg_m2_mensual, na.rm=T),
            "Prod_sd"=sd(Peso_kg_m2_mensual, na.rm=T),
         "n"=n(),
         "recurr"= unique(recurr_esp)
           )

 resumen <-  muestra_anual %>% group_by(Nombre_de_Cultivo) %>% 
    summarise("Media"=mean(Prod_anual),
              "SD"=sd(Prod_anual),
              "n"=n())

  ## Superficie asignada 
