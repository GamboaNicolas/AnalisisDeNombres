
crearDiccionariosDeNombres <- function(datos) {

  datos$nombre <- toupper(datos$nombre)

  # FILTRADO Y TRABAJO DE LA BASE----

  ## seleccionamos columnas de inter?s-----

  datos_filtrados <- datos %>% select(
    "nombre", "apellido", "Fecha nacimiento.", "Ocurrio en.", "Edad de la madre.","Lugar de residencia.",
    "regcivil", "acta","tiporegistro" , "Sexo.", "departamento", "localidad", "Distrito municipal.","Seccional policial."
  )


  ## Guardo los nombres originales en una columna----
  datos_filtrados$nombre_tipeado <- datos_filtrados$nombre

  # names(datos_filtrados)

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
    c(""," ","EL", "LOS", "LA", "LAS", "LO", "A", "AL", "DEL",
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


  datos2 <- datos1

  cant_nombres <- c("Primero", "Segundo")

  if (maximo>=3) {cant_nombres <- c(cant_nombres, "Tercero")}
  if (maximo>=4) {cant_nombres <- c(cant_nombres, "Cuarto")}
  if (maximo>=5) {cant_nombres <- c(cant_nombres, "Quinto")}
  if (maximo>=6) {cant_nombres <- c(cant_nombres, "Sexto")}

  # a
  # datos2$Primero <- rep(NA,length(lista_nombres))
  # datos2$Segundo <- rep(NA,length(lista_nombres))
  # if (maximo>=3) {datos2$Tercero <- rep(NA,length(lista_nombres))}
  # if (maximo>=4) {datos2$Cuarto <- rep(NA,length(lista_nombres))}
  # if (maximo>=5) {datos2$Quinto <- rep(NA,length(lista_nombres))}

  # creamos las columnas de cada posicion de nombre

  for (i in cant_nombres) {
    datos2 <- datos2 %>%
      mutate(nomb = NA)
    names(datos2)[length(names(datos2))] <- i
  }

  datos2$cantidad_nombres <- rep(NA,length(lista_nombres))

  # Llenamos la columna de los nombres y la de cantidad de nombres

  # p <- 0
  for(i in 1:length(lista_nombres)){
    for (j in 1:maximo) {
      datos2[i,cant_nombres[j]] <- lista_nombres[[i]][j]
    }
    datos2$cantidad_nombres[i] <- length(lista_nombres[[i]])
  }



  # CORRECIONES DE NOMBRES





  datos3 <- datos2


  if(nrow(datos3) != nrow(datos2)){
    stop("Los tamaños de las bases antes y después de la correccion no coinciden")
  }

  for (i in cant_nombres) {
    datosFaltantes <- datosFaltantes %>%
      mutate(nomb = NA)
    names(datosFaltantes)[length(names(datosFaltantes))] <- i
  }

  datosFaltantes$cantidad_nombres <- rep("No disponible",nrow(datosFaltantes))

  datos_print <- rbind(datos3,datosFaltantes)
#
#   View(datos_print)

  # en caso de querer ver la base con las columnas de nombres agregadas
  # write.xlsx(datos_print, "./Bases_creadas/base_nombres_arreglada.xlsx)


  # CREAMOS LA BASE FINAL PARA EL AN?LISIS-----
  # names(datos_print)

  data_long <- gather(datos_print[,c(-3:-1)],
                      posicion,
                      nombre_ind,
                      "Primero":cant_nombres[length(cant_nombres)],
                      na.rm = T
  )

  # rm(datos1,datos2,datos2_hombres,datos2_mujeres,datos3,datosFaltantes)
  # rm(datos_filtrados)


  # write.xlsx(,file = "nombres_individuales1*asdsad.xlsx")


  hombres <- data_long %>% filter(Sexo. == "1 - Masculino")

  tabla_hombres <- as.data.frame(table(hombres$nombre_ind),stringsAsFactors = F) %>% arrange(desc(Freq))

  tabla_hombres$dist <- 0

  tabla_hombres$dist_norm <- 0

  tabla_hombres$Correccion <- NA

  names(tabla_hombres)[1] <- c("nombre")


  cat("\nSistema de corrrecion de nombres.\n\nNombres de nacidos Varones:\n")

  for (i in 1:nrow(tabla_hombres)) {
    tabla_hombres <- tabla_hombres %>% arrange(desc(Freq))
    compara <- tabla_hombres[i,1]
    frecuencia <- tabla_hombres[i,2]

    tabla_hombres$dist <- stringdist(tabla_hombres$nombre, compara , method = "lv")

    tabla_hombres <- tabla_hombres %>% mutate(
      dist_norm = (1 - (dist/max(nchar(nombre),nchar(compara))))
    )

    tabla_hombres <- tabla_hombres %>% arrange(desc(dist_norm))

    cat("\nEstá por comparar nombres contra",paste0(compara,","),"el nombre número",i,"con frecuencia: ",frecuencia,"\n1: Continuar\n2: Saltear nombre y pasar al siguiente\n3: Finalizar\n\n")

    n <- scan(n=1,what = character(), quiet = T)

    while (!(n %in% c("1","2","3"))) {

      cat("\nIngrese una opcion válida. 1, 2 o 3")
      n <- scan(n=1,what = character(), quiet = T)

    }

    if(n == 2){next}

    if(n == 3){break}

    a <- 0
    for(j in 2:nrow(tabla_hombres)) {

      if(!is.na(tabla_hombres[j,5])){next}
      a <- a+1
      revision <- tabla_hombres[j,1]
      frecuencia2 <- tabla_hombres[j,2]

      cat("Comparación número:",a,"\n")
      cat("\nEstá comparando contra",compara,"con freq:",frecuencia,
          "\n\nEl nombre en revisión es",revision,"con freq:",frecuencia2,"\nLa similitud entre los nombres es del",
          paste0(round(tabla_hombres[j,4]*100,2),"%"))


      cat("\nOpciones:\n\n  1: Dejarlo como está\n  2: Reemplazarlo con",compara,"\n  3: Ingresar un nombre a mano",
          "\n  4: Colocar INCONSISTENTE para posterior revisión\n  5: Terminar comparaciones contra",compara)


      n <- scan(n=1,what = character(), quiet = T)

      while (!(n %in% c("1","2","3","4","5"))) {

        cat("\nIngrese una opcion válida. 1, 2, 3, 4 o 5:\n")
        n <- scan(n=1,what = character(), quiet = T)

      }

      if(n == 1){next}
      if(n == 2) {tabla_hombres[j,5] <- compara}
      if(n == 3){
        cat("\nIngrese el nombre sin tildes ni espacios.\nAsegúrese de ingresarlo correctamente.")
        tabla_hombres[j,5] <- toupper(scan(n=1,what = character(), quiet = T))
      }
      if(n == 4) {tabla_hombres[j,5] <- "INCONSISTENTE"}
      if(n == 5){break}

    }

  }

  correciones_nombres_hombres <- tabla_hombres

  inconsistentes_hombres <- tabla_hombres %>% filter(
    Correccion == "INCONSISTENTE"
  ) %>% select(c(1,2))


  mujeres <- data_long %>% filter(Sexo. != "1 - Masculino")

  tabla_mujeres <- as.data.frame(table(mujeres$nombre_ind),stringsAsFactors = F) %>% arrange(desc(Freq))

  tabla_mujeres$dist <- 0

  tabla_mujeres$dist_norm <- 0

  tabla_mujeres$Correccion <- NA

  names(tabla_mujeres)[1] <- c("nombre")


  cat("\nSistema de corrrecion de nombres.\n\nNombres de nacidos Mujeres:\n")

  for (i in 1:nrow(tabla_mujeres)) {
    tabla_mujeres <- tabla_mujeres %>% arrange(desc(Freq))
    compara <- tabla_mujeres[i,1]
    frecuencia <- tabla_mujeres[i,2]

    tabla_mujeres$dist <- stringdist(tabla_mujeres$nombre, compara , method = "lv")

    tabla_mujeres <- tabla_mujeres %>% mutate(
      dist_norm = (1 - (dist/max(nchar(nombre),nchar(compara))))
    )

    tabla_mujeres <- tabla_mujeres %>% arrange(desc(dist_norm))

    cat("\nEstá por comparar nombres contra",paste0(compara,","),"el nombre número",i,"con frecuencia: ",frecuencia,"\n1: Continuar\n2: Saltear nombre y pasar al siguiente\n3: Finalizar\n\n")

    n <- scan(n=1,what = character(), quiet = T)

    while (!(n %in% c("1","2","3"))) {

      cat("\nIngrese una opcion válida. 1, 2 o 3")
      n <- scan(n=1,what = character(), quiet = T)

    }

    if(n == 2){next}

    if(n == 3){break}

    a <- 0
    for(j in 2:nrow(tabla_mujeres)) {

      if(!is.na(tabla_mujeres[j,5])){next}
      a <- a+1
      revision <- tabla_mujeres[j,1]
      frecuencia2 <- tabla_mujeres[j,2]

      cat("Comparación número:",a,"\n")
      cat("\nEstá comparando contra",compara,"con freq:",frecuencia,
          "\n\nEl nombre en revisión es",revision,"con freq:",frecuencia2,"\nLa similitud entre los nombres es del",
          paste0(round(tabla_mujeres[j,4]*100,2),"%"))


      cat("\nOpciones:\n\n  1: Dejarlo como está\n  2: Reemplazarlo con",compara,"\n  3: Ingresar un nombre a mano",
          "\n  4: Colocar INCONSISTENTE para posterior revisión\n  5: Terminar comparaciones contra",compara)


      n <- scan(n=1,what = character(), quiet = T)

      while (!(n %in% c("1","2","3","4","5"))) {

        cat("\nIngrese una opcion válida. 1, 2, 3, 4 o 5:\n")
        n <- scan(n=1,what = character(), quiet = T)

      }

      if(n == 1){next}
      if(n == 2) {tabla_mujeres[j,5] <- compara}
      if(n == 3){
        cat("\nIngrese el nombre sin tildes ni espacios.\nAsegúrese de ingresarlo correctamente.")
        tabla_mujeres[j,5] <- toupper(scan(n=1,what = character(), quiet = T))
      }
      if(n == 4) {tabla_mujeres[j,5] <- "INCONSISTENTE"}
      if(n == 5){break}

    }

  }

  correciones_nombres_mujeres <- tabla_mujeres

  inconsistentes_mujeres <- tabla_mujeres %>% filter(
    Correccion == "INCONSISTENTE"
  ) %>% select(c(1,2))

  lista_return <- list(
    correciones_hombres = correciones_nombres_hombres,
    correciones_mujeres = correciones_nombres_mujeres,
    inconsistencias_hombres = inconsistentes_hombres,
    inconsistencias_mujeres = inconsistentes_mujeres
  )

  return(lista_return)

}

crearBaseNombresInd <- function(datos, correciones_hombres, correciones_mujeres){

  datos$nombre <- toupper(datos$nombre)

  correciones_hombres <- correciones_hombres %>%
    select(nombre, Correccion)

  correciones_mujeres <- correciones_mujeres %>%
    select(nombre, Correccion)


  # FILTRADO Y TRABAJO DE LA BASE----

  ## seleccionamos columnas de inter?s-----

  datos_filtrados <- datos %>% select(
    "nombre", "apellido", "Fecha nacimiento.", "Ocurrio en.", "Edad de la madre.","Lugar de residencia.",
    "regcivil", "acta","tiporegistro" , "Sexo.", "departamento", "localidad", "Distrito municipal.","Seccional policial."
  )

  # INTENTO DE CHEQUEAR COLUMNAS, NO FUNCIONA

  # if (names(datos_filtrados) == c("nombre","apellido","Fecha nacimiento.",
  #                                 "Ocurrio en.","Edad de la madre." ,"Lugar de residencia.",
  #                                 "regcivil","acta","tiporegistro",
  #                                 "Sexo.", "departamento","localidad",
  #                                 "Distrito municipal.","Seccional policial." )) {
  #
  # }
  #
  # c("nombre","apellido","Fecha nacimiento.",
  #  "Ocurrio en.","Edad de la madre." ,"Lugar de residencia.",
  #  "regcivil","acta","tiporegistro",
  #  "Sexo.", "departamento","localidad",
  #  "Distrito municipal.","Seccional policial." )


  ## Guardo los nombres originales en una columna----
  datos_filtrados$nombre_tipeado <- datos_filtrados$nombre

  # names(datos_filtrados)

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
    c(""," ","EL", "LOS", "LA", "LAS", "LO", "A", "AL", "DEL",
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


  datos2 <- datos1

  cant_nombres <- c("Primero", "Segundo")

  if (maximo>=3) {cant_nombres <- c(cant_nombres, "Tercero")}
  if (maximo>=4) {cant_nombres <- c(cant_nombres, "Cuarto")}
  if (maximo>=5) {cant_nombres <- c(cant_nombres, "Quinto")}
  if (maximo>=6) {cant_nombres <- c(cant_nombres, "Sexto")}

  # a
  # datos2$Primero <- rep(NA,length(lista_nombres))
  # datos2$Segundo <- rep(NA,length(lista_nombres))
  # if (maximo>=3) {datos2$Tercero <- rep(NA,length(lista_nombres))}
  # if (maximo>=4) {datos2$Cuarto <- rep(NA,length(lista_nombres))}
  # if (maximo>=5) {datos2$Quinto <- rep(NA,length(lista_nombres))}

  # creamos las columnas de cada posicion de nombre

  for (i in cant_nombres) {
    datos2 <- datos2 %>%
      mutate(nomb = NA)
    names(datos2)[length(names(datos2))] <- i
  }

  datos2$cantidad_nombres <- rep(NA,length(lista_nombres))

  # Llenamos la columna de los nombres y la de cantidad de nombres

  # p <- 0
  for(i in 1:length(lista_nombres)){
    for (j in 1:maximo) {
      datos2[i,cant_nombres[j]] <- lista_nombres[[i]][j]
    }
    datos2$cantidad_nombres[i] <- length(lista_nombres[[i]])
  }



  # CORRECIONES DE NOMBRES

  #
  #   reemplazos_hombres <- read_xlsx("./data/correciones_hombres_R.xlsx")
  #
  #   reemplazos_mujeres <- read_xlsx("./data/correciones_mujeres_R.xlsx")

  correciones_hombres <- correciones_hombres[!is.na(correciones_hombres$Correcion),]
  correciones_nom
  datos2_hombres <- datos2 %>%
    filter(Sexo. == "1 - Masculino")

  for (i in 1:nrow(datos2_hombres)) {
    for (j in cant_nombres) {
      if(is.na(datos2_hombres[i,j]) || !(datos2_hombres[i,j] %in% correciones_hombres$nombre)){
        next
      }
      datos2_hombres[i,j] <- correciones_hombres$Correcion[match(datos2_hombres[i,j],correciones_hombres$nombre)]
    }

  }



  datos2_mujeres <- datos2 %>%
    filter(Sexo. != "1 - Masculino")

  for (i in 1:nrow(datos2_mujeres)) {
    for (j in cant_nombres) {
      if(is.na(datos2_mujeres[i,j]) || !(datos2_mujeres[i,j] %in% correciones_mujeres$nombre)){
        next
      }
      datos2_mujeres[i,j] <- correciones_mujeres$Correcion[match(datos2_mujeres[i,j],correciones_mujeres$nombre)]
    }
  }

  datos3 <- rbind(datos2_hombres,datos2_mujeres)

  if(nrow(datos3) != nrow(datos2)){
    stop("Los tamaños de las bases antes y después de la correccion no coinciden")
  }

  for (i in cant_nombres) {
    datosFaltantes <- datosFaltantes %>%
      mutate(nomb = NA)
    names(datosFaltantes)[length(names(datosFaltantes))] <- i
  }

  datosFaltantes$cantidad_nombres <- rep("No disponible",nrow(datosFaltantes))

  datos_print <- rbind(datos3,datosFaltantes)
  #
  #   View(datos_print)

  # en caso de querer ver la base con las columnas de nombres agregadas
  # write.xlsx(datos_print, "./Bases_creadas/base_nombres_arreglada.xlsx)


  # CREAMOS LA BASE FINAL PARA EL AN?LISIS-----
  # names(datos_print)

  data_long <- gather(datos_print[,c(-3:-1)],
                      posicion,
                      nombre_ind,
                      "Primero":cant_nombres[length(cant_nombres)],
                      na.rm = T
  )

  # rm(datos1,datos2,datos2_hombres,datos2_mujeres,datos3,datosFaltantes)
  # rm(datos_filtrados)



  faltantes_final <- datos_print %>%
    filter(is.na(Primero)) %>%
    select(
      names(datos_print[
        c(-3:-1,(-ncol(datos_print)+1):(-ncol(datos_print)+length(cant_nombres)))
      ])
    )
  faltantes_final$posicion <- NA
  faltantes_final$nombre_ind <- NA


  nombres_final <- rbind(data_long, faltantes_final)
  nombres_final$ID <- c(1:nrow(nombres_final))

  # write.xlsx(,file = "nombres_individuales1*asdsad.xlsx")

  return(nombres_final) #si

}
