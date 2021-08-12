library(tidyverse)
library("googledrive")
library(ggpubr)

drive_download(
    "https://docs.google.com/spreadsheets/d/1mr8NAuNCDRg20TFOT69bobscBcu8RhfolD9qZOWsNFQ/edit#gid=579148914",
  path = "data/muestra.xlsx",
  overwrite = TRUE
)

as.vector(unique(muestra$Nombre_de_Cultivo))

muestra <- readxl::read_excel("data/muestra.xlsx")

muestra_cut <- muestra %>% select('Nombre_de_Cultivo', 
                                  'Productor_a', 
                                  'Peso_kg_m2_mensual',
                                   as.vector(unique(muestra$Nombre_de_Cultivo)))


## Modelo productividad bruta
  ## kg anuales para RACC
  ## kg/anuales*m2
  
  ## Productividad (kg/m2*mes)

  # funcion_fran <- function(i){
  #   x <- muestra[i,] 
  #   string <- x$Nombre_de_Cultivo
  #   rec <- x %>% select(string)
  #   #print(rec)
  #   as.numeric(rec[1,paste(string),drop=T])
  # }

  #a <- funcion_fran(1:nrow(muestra))
  
  #muestra$recurr_esp <-a
  
  #muestra$recurr_esp==muestra$recurrencia...273

midf %>%
  group_by(NombreDeCultivo, Productor) %>%
  summarize(recurrencia=
              unique(.[,unique(NombreDeCultivo) %in% colnames(.), drop=T])) 

  muestra_agregado <- 
  muestra %>% group_by(Productor_a , Nombre_de_Cultivo,.drop= TRUE) %>% 
  summarise("Prod_media"=mean(Peso_kg_m2_mensual, na.rm=T),
            "Prod_sd"=sd(Peso_kg_m2_mensual, na.rm=T),
         "n"=n(),
         "recurr"= unique(recurrencia...273)
           )

 #resumen_agregado <-  
   muestra_anual %>% group_by(Nombre_de_Cultivo) %>% 
    summarise("Media"=mean(Prod_anual),
              "SD"=sd(Prod_anual),
              "n"=n())
   
 #FAO  
   
   drive_download(
     "https://drive.google.com/file/d/1wtpHKqKBdw0stfnGcx_Dastq2FUApvZj/view?ts=6115312d",
     path = "data/fao.xlsx",
     overwrite = TRUE
   )
   
   fao <- readxl::read_excel("data/fao.xlsx", sheet = "conosur")

  fao_tabla <- fao %>% group_by(Producto) %>% 
    #filter(Producto) %>%
    dplyr::summarise(
      media=mean(`Valor (kg/m2)`),
      sd=sd(`Valor (kg/m2)`),
      n=n(),
      ci=mean_ci(`Valor (kg/m2)`,ci=0.95)
      #IC95_inf=media-qt(1 - (alpha / 2))* sd/sqrt(n),
     # IC95_sup=media+qt(1 - (alpha / 2))* sd/sqrt(n)
    
  )
   

  #Censo 
   
    drive_download(
     "https://docs.google.com/spreadsheets/d/1wtpHKqKBdw0stfnGcx_Dastq2FUApvZj/edit#gid=562798443",
     path = "data/fao.xlsx",
     overwrite = TRUE
   )
   
   censo <-  readxl::read_excel("data/censo.xlsx")

  ## Superficie asignada 


   