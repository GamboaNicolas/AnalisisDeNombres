# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library(readxl)
library(tidyverse)
library(openxlsx)


hello <- function(datos) {

  datos$nombre <- toupper(datos$nombre)

  # FILTRADO Y TRABAJO DE LA BASE----

  ## seleccionamos columnas de inter?s-----

  datos_filtrados <- datos %>% select(
    "nombre", "apellido", "Fecha nacimiento.", "Ocurrio en.", "Edad de la madre.","Lugar de residencia.",
    "regcivil", "acta","tiporegistro" , "Sexo.", "departamento", "localidad", "Distrito municipal.","Seccional policial."
  )

  ## Guardo los nombres originales en una columna----
  datos_filtrados$nombre_tipeado <- datos_filtrados$nombre

  names(datos_filtrados)

  datos_filtrados <- datos_filtrados[,c(15,1:14)]


  ## Filtro los registros con dato faltante en el nombre
  datos1 <- datos_filtrados %>% filter(
    is.na(nombre) == F,
    nombre != "SIN DATOS",
    nombre != "SIN DATO",
    nombre != "DESCONOCIDO"
  )

  datosFaltantes <- datos_filtrados %>% filter(
    is.na(nombre) == T |
      nombre == "SIN DATOS" |
      nombre == "SIN DATO" |
      nombre == "DESCONOCIDO"
  )

  # Separo los nombres


  lista_nombres <- str_split(datos1$nombre, " ")


  #
  ## Elimino las palabras vacias y articuos/enlaces para que no se consideren como un nombre mas-------
  #

  descartes <- toupper(
    c("","EL", "LOS", "LA", "LAS", "LO", "A", "AL", "DEL",
      "UN", "UNA", "UNOS", "UNAS",
      "a", "ante", "bajo", "cabe", "con", "contra", "de", "desde",
      "durante", "en", "entre", "hacia", "hasta", "mediante",
      "para", "por", "segun", "sin", "so", "sobre", "tras", "versus", "via")
  )

  cuenta <- 0
  for(i in 1:length(lista_nombres)){
    descarte <- NULL
    for(j in 1:length(lista_nombres[[i]])){
      if(lista_nombres[[i]][j] %in% descartes){
        descarte <- c(descarte,-j)
        cuenta <- cuenta+1
      }
    }
    if(!is.null(descarte)){
      lista_nombres[[i]] <- lista_nombres[[i]][descarte]
    }
  }



  #
  ## Buscamos cuantos nombres tiene una persona como maximo-----
  #

  maximo <- 0

  for (i in 1:(length(lista_nombres))){
    if(length(lista_nombres[[i]])>maximo){
      maximo <- length(lista_nombres[[i]])
    }
  }

  if(length(lista_nombres)!=nrow(datos1)){
    stop("Hubo un error en el filtrado de los nombres.\nLa cantidad de nombres no coincide con la cantidad de\nnacidos que en teoría tienen al menos un nombre.\n")
  }

  cant_nombres <- c("Primero", "Segundo")

  if (maximo>=3) {cant_nombres <- c(cant_nombres, "Tercero")}
  if (maximo>=4) {cant_nombres <- c(cant_nombres, "Cuarto")}
  if (maximo>=5) {cant_nombres <- c(cant_nombres, "Quinto")}
  if (maximo>=5) {cant_nombres <- c(cant_nombres, "Sexto")}

  datos2$cantidad_nombres <- rep(NA,length(lista_nombres))
  # datos2$Primero <- rep(NA,length(lista_nombres))
  # datos2$Segundo <- rep(NA,length(lista_nombres))
  # if (maximo>=3) {datos2$Tercero <- rep(NA,length(lista_nombres))}
  # if (maximo>=4) {datos2$Cuarto <- rep(NA,length(lista_nombres))}
  # if (maximo>=5) {datos2$Quinto <- rep(NA,length(lista_nombres))}

  for (i in 1:maximo) {
    datos2 <- datos2 %>%
      mutate(a = NA)
    datos2 <- datos2 %>%
      rename(cant_nombres[i] = a)
  }

  # p <- 0
  for(i in 1:length(lista_nombres)){
    for (j in 1:maximo) {
      datos2[i,cant_nombres[j]] <- lista_nombres[[i]][j]
    }
    datos2$cantidad_nombres[i] <- length(lista_nombres[[i]])
    # if(round(i/length(lista_nombres)*100)>p){
    #   p <- round(i/length(lista_nombres)*100)
    #   print(paste0(p,"%"))
    # }
  }



    # CORRECIONES DE NOMBRES


  reemplazos_hombres <- read_xlsx("./data/correciones_hombres_R.xlsx")

  reemplazos_mujeres <- read_xlsx("./data/correciones_mujeres_R.xlsx")



  datos2_hombres <- datos2 %>%
    filter(Sexo. == "1 - Masculino")

  for (i in 1:nrow(datos2_hombres)) {
    for (j in cant_nombres) {
      if(is.na(datos2_hombres[i,j]) || !(datos2_hombres[i,j] %in% reemplazos_hombres$Nombre)){
        next
      }
      indice <- match(datos2_hombres[i,j],reemplazos_hombres$Nombre)
      datos2_hombres[i,j] <- reemplazos_hombres$Corregido[indice]
    }

  }



  datos2_mujeres <- datos2 %>%
    filter(Sexo. != "1 - Masculino")

  for (i in 1:nrow(datos2_mujeres)) {
    for (j in cant_nombres) {
      if(is.na(datos2_mujeres[i,j]) || !(datos2_mujeres[i,j] %in% reemplazos_mujeres$Nombre)){
        next
      }
      indice <- match(datos2_mujeres[i,j],reemplazos_mujeres$Nombre)
      datos2_mujeres[i,j] <- reemplazos_mujeres$Corregido[indice]
    }
  }


}

