#' @title Carga archivo de resultados fenotipicos (AST)
#' y selecciona columnas por Categoria RAM
#'
#' @description Carga el archivo de ASTs.
#' Proporciona las hojas existentes en el archivo de excel al ususario.
#' Proporciona una lista de antimicrobianos disponibles y pide al ususario sellecionar un antimicrobiano.
#' Selecciona la columna de identificadores de aislados (columna ID).
#' Selecciona la columna de resultados por AST del antimicrobiano seleccionado.
#'
#' @param archivo_fenotipo Ruta del archivo de entrada de datos fenotipicos (AST)
#' @return Data Frame con los IDs y la columna de resultados de AST del antimicrobiano seleccionado
#' @export

FenotipoCategorias <- function(archivo_fenotipo){

  ####################### seleccionar hojas ##########################

  ## NOTA: si no carga el archivo verificar que exista la hoja ##
  # ver hojas de excel
  cat("\nEl arhivo elegido tiene las siguientes hojas: ",
      excel_sheets(archivo_fenotipo),
      sep = "\n")

  repeat {
    hoja <- readline(prompt="Elige la hoja que deseas cargar a R para Perfiles, patrones y comparaciones (AST): ")
    if (hoja == "AST"){
      break
      }
  }

  repeat {
    hoja2 <- readline(prompt="Elige la hoja que deseas cargar a R para multiresistencia (bacteria): ")
    if (hoja2 == "bacteria"){
      break
      }
  }

  ########################### abrir hojas ################################

  # abrir hoja de AST para sensibilidad, especificidad y perfiles
  ASTs <<- read_excel(archivo_fenotipo,
                      sheet = hoja,
                      col_names = TRUE)

  # abrir hoja de AST para patrones de antimicrobianos
  Multi <<- read_excel(archivo_fenotipo,
                       sheet = hoja,
                       col_names = TRUE)

  # abrir hoja de bacteria para multiresistencias (tabla y grafica)
  serotipos <<- read_excel(archivo_fenotipo,
                       sheet = hoja2,
                       col_names = TRUE)

  ################# mostrar antimicrobianos disponibles ##################
  # guardar vector de nombres
  Pares <- vector()
  for (i in 1:length(names(ASTs))){
    if (i %% 2 == 0) {
      Pares[i] <- names(ASTs[,i])
      Pares <- na.omit(Pares)
    }
  }

  Pares <<- Pares

  # mostrar lista
  cat("\nLista de antimicrobianos disponibles: ", Pares, sep = "\n")

  # seleccionar antimicrobiano a filtrar del df AST
  Antimicrobiano <<- readline(prompt="Elige el antimicrobiano que deseas comparar: ") %>%
    str_to_title()

  # conservar solo columnas que contengan la palabra Betalactamicos
  ASTs_corto <- ASTs %>% select(
    "ID", contains(Antimicrobiano)
  )

  # remover NAs
  ASTs_corto <- na.omit(ASTs_corto)

  ## obtener antimicrobianos correspondiente a Antiimicrobiano seleccionado ###
  Categoria <- ""
  Categoria <- case_when(
    Antimicrobiano == "Amikacina" ~ "Aminoglucosidos",
    Antimicrobiano == "Gentamicina" ~ "Aminoglucosidos",
    Antimicrobiano == "Ampicilina" ~ "Betalactamicos",
    Antimicrobiano == "Ceftazidima" ~ "Betalactamicos",
    Antimicrobiano == "Ertapenem" ~ "Betalactamicos",
    Antimicrobiano == "Meropenem" ~ "Betalactamicos",
    Antimicrobiano == "Imipenem" ~ "Betalactamicos",
    Antimicrobiano == "Colistina" ~ "Polimixina",
    Antimicrobiano == "Cloranfenicol" ~ "Fenicoles",
    Antimicrobiano == "Tigeciclina" ~ "Tetraciclinas",
    Antimicrobiano == "Tetraciclina" ~ "Tetraciclinas",
    Antimicrobiano == "Ciprofloxacino" ~ "Quinolonas",
    Antimicrobiano == "Levofloxacino" ~ "Quinolonas",
    Antimicrobiano == "Azitromicina" ~ "Macrolidos",
    Antimicrobiano == "Trimetoprima/sulfametoxazol" ~ "Sulfonamidas",
    TRUE ~ "")

  # categoria
  Categoria <<- Categoria

  # devolver la columna seleccionada
  cat("\nSe seleccionanó la columna: ",colnames(ASTs_corto)[2])

  # devolver categoria seleccionada
  cat("\nEl Antimicrobiano seleccionado pertenece a la clase: ", Categoria)

  ########################### a nivel e categoria ##############################

  Antimicrobiano_Categoria <- c("Amikacina_Aminoglucosidos",
                                "Ampicilina_Betalactamicos",
                                "Azitromicina_Macrolidos",
                                "Ceftazidima_Betalactamicos",
                                "Cloranfenicol_Fenicoles",
                                "Ciprofloxacino_Quinolonas",
                                "Colistina_Polimixina",
                                "Ertapenem_Betalactamicos",
                                "Gentamicina_Aminoglucosidos",
                                "Imipenem_Betalactamicos",
                                "Levofloxacino_Quinolonas",
                                "Meropenem_Betalactamicos",
                                "Tetraciclina_Tetraciclinas",
                                "Tigeciclina_Tetraciclinas",
                                "Trimetoprima/sulfametoxazol")

  # obtener columnas que tengan la categoria identificada
  cols <- str_subset(Antimicrobiano_Categoria, Categoria)

  # obtener antimicrobianos dentro del objeto ASTs (crear cols2)
  cols2 <- ""

  # obtener antimicrobianos dentro del objeto ASTs (poblar cols2)
  for (i in 1:length(cols)){
    cols2[i] <- str_split(cols, "_")[[i]][1]
  }

  # obtener objeto de ASts por categorias,
  # incluye todos los antimicrobianos que tienen la categoria
  ASTs_categoria <- ASTs %>% select(
    "ID", contains(cols2)
  )

  # crear nueva columna de Tipo (Resistente o Susceptible) en archivo de ASTs categoria
  # conjunta para todos los antimicrobianos de la misma categoria
  ASTs_categoria <- ASTs_categoria %>% rowwise() %>%
    mutate(Global = ifelse(any(
      str_detect(c_across(cols = 2:ncol(ASTs_categoria)), "Resistente")),
      "Resistente", "Susceptible"))

  # quitar formato raro por usar rowwise
  ASTs_categoria <- ungroup(ASTs_categoria)

  # remover columnas y NAs
  ASTs_categoria <- na.omit(ASTs_categoria[,c("ID", "Global")])

  # volver global
  ASTs_categoria <<- as_tibble(ASTs_categoria)

  ############## obtener tablka de patrones de antimicrobianos #################

  # subset de columnas con nombre de antimicrobiano (removiendo columnas de diluciones)
  Multi_corto <- Multi[,!str_detect(colnames(Multi),"(mg/L)")]

  # convertir a titulo todo, menos columna de IDs
  Multi_corto <- as.data.frame(lapply(Multi_corto,str_to_title))

  # volver primera columna mayusculas
  Multi_corto$ID <- str_to_upper(Multi_corto$ID)

  # convertir a titulo nombres de columnas, menos columna de IDs
  colnames(Multi_corto) <- str_to_title(colnames(Multi_corto))

  # convertir amicacina a amikacina
  colnames(Multi_corto)[colnames(Multi_corto) == "Amicacina"] <- "Amikacina"

  # remover filas vacias
  Multi_corto <- Multi_corto[rowSums(is.na(Multi_corto)) ==  0,]

  # remover columnas vacias
  Multi_corto <- Multi_corto[,!nrow(Multi_corto) == colSums(is.na(Multi_corto))]

  # nombres de columnas
  Antimicrobiano_patrones <- colnames(Multi_corto)[-1]

  # cambiar la palabra "resistente" por el nombre de columna y "Susceptible" por character vacio
  for (numfila in 1:nrow(Multi_corto)) {
    for (numcol in 2:ncol(Multi_corto)) {
      if (Multi_corto[numfila, numcol] == "Resistente"){
        Multi_corto[numfila, numcol] <- colnames(Multi_corto)[numcol]
      } else {
        Multi_corto[numfila,numcol] <- NA
      }
    }
  }

  # eliminar columnas vacias
  Multi_corto <- Multi_corto[,!nrow(Multi_corto) == colSums(is.na(Multi_corto))]

  # crear vector vacio de nombres de columnas de Multi_corto en categorias
  columnas_categorias <- ""

  # cambiar nombre de columnas de antimicrobianos a categrorias
  columnas_categorias <- case_when(
    colnames(Multi_corto)[-1] == "Amikacina" ~ "Aminoglucosidos",
    colnames(Multi_corto)[-1] == "Ampicilina" ~ "Betalactamicos",
    colnames(Multi_corto)[-1] == "Azitromicina" ~ "Macrolidos",
    colnames(Multi_corto)[-1] == "Ceftazidima" ~ "Betalactamicos",
    colnames(Multi_corto)[-1] == "Ciprofloxacino" ~ "Quinolonas",
    colnames(Multi_corto)[-1] == "Cloranfenicol" ~ "Fenicoles",
    colnames(Multi_corto)[-1] == "Colistina" ~ "Polimixinas",
    colnames(Multi_corto)[-1] == "Ertapenem" ~ "Betalactamicos",
    colnames(Multi_corto)[-1] == "Gentamicina" ~ "Aminoglucosidos",
    colnames(Multi_corto)[-1] == "Imipenem" ~ "Betalactamicos",
    colnames(Multi_corto)[-1] == "Meropenem" ~ "Betalactamicos",
    colnames(Multi_corto)[-1] == "Levofloxacino" ~ "Quinolonas",
    colnames(Multi_corto)[-1] == "Tetraciclina" ~ "Tetraciclinas",
    colnames(Multi_corto)[-1] == "Tigeciclina" ~ "Tetraciclinas",
    colnames(Multi_corto)[-1] == "Trimetoprima/sulfametoxazol" ~ "Sulfonamidas",
    TRUE ~ "")

  # asignar nombre de columnas de antimicrobianos a categorias
  colnames(Multi_corto) <- c("ID", columnas_categorias)

  ################## obtener filas con multiresistencia #####################

  # crear y poblar vector de categorias duplicadas
  duplicados <- colnames(Multi_corto[ , duplicated(colnames(Multi_corto))])

  # tratamiento de duplicados
  if (!length(duplicados) == 0){
    # crear lista para guardar categorias duplicadas
    lista_duplicados <- vector(mode = "list", length = length(duplicados))

    # nombrar elementos de la lista
    names(lista_duplicados) <- duplicados

    # numerar columnas de categorias para distinguirlas y poblar lista
    for (i in 1:length(columnas_categorias)){
      colnames(Multi_corto)[i + 1] <- paste(columnas_categorias[i], i, sep = "_")
    }

    # remover columnas vacias
    Multi_corto <- Multi_corto[ , !nrow(Multi_corto) == colSums(is.na(Multi_corto))]

    # poblar lista de categorias duplicadas
    for (i in 1:length(duplicados)){
      # poblar
      lista_duplicados[[i]] <- Multi_corto[, str_detect(colnames(Multi_corto),duplicados[i])]
      # unir
      lista_duplicados[[i]] <- unite(lista_duplicados[[i]],
                                   Global,
                                   colnames(lista_duplicados[[i]]),
                                   sep = "-"
                                   )
    }

    # volver la lista de antimicrobianos duplicados unidos un data frame (o tibble)
    antimicrobianos_duplicados <- bind_cols(lista_duplicados)

    # cambiar nombres de columnas
    colnames(antimicrobianos_duplicados) <- duplicados

    # coercionar de tibble a data frame
    antimicrobianos_duplicados <- as.data.frame(antimicrobianos_duplicados)

    # remover NAs como caracteres
    for (i in duplicados){
      antimicrobianos_duplicados[ , i] <- str_replace(antimicrobianos_duplicados[ , i], "NA-|-NA", "")
      antimicrobianos_duplicados[antimicrobianos_duplicados[ , i] == "NA", i] <- NA
    }

    # eliminar columnas de categorias duplicadas de Multi corto (archivo de AST)
    for (i in 1:length(duplicados)){
      Multi_corto <- Multi_corto[ , !str_detect(colnames(Multi_corto), duplicados[i])]
    }

    # quitar numeración a nombres de columnas
    for (i in 2:ncol(Multi_corto)){
      colnames(Multi_corto)[i] <- str_split(colnames(Multi_corto), "_")[[i]][1]
    }

    # unir categorias antes duplicadas con el resto de categorias
    Multi_corto <- cbind(Multi_corto, antimicrobianos_duplicados)
  }

  # obtener numero de categorias en df
  num_categorias <- ncol(Multi_corto) - 1

  # numero minimo de categorias para ser considerado un aislado multiresistente
  multiresistencia <- 3

  # numero de NAs por fila (islados)
  rowSums(is.na(Multi_corto))

  # limite de NAs permitidos
  limite <- num_categorias - multiresistencia

  # establecer si el aislado es multiresistente o no
  Multi_corto$Multiresistente <- ifelse(
    rowSums(is.na(Multi_corto)) > limite, NA, "Multiresistente")

  # subset de aislados multiresistentes
  Multi_corto <- Multi_corto[Multi_corto$Multiresistente == "Multiresistente",]

  # remover filas vacias
  Multi_corto <- Multi_corto[!rowSums(is.na(Multi_corto)) == ncol(Multi_corto),]

  # volver global (para multiresistencia)
  Multi_corto2 <<- Multi_corto

  # obtener columna de antimicrobianos
  Antimicrobianos_2 <- unite(Multi_corto,
                             sep = "-",
                             col = "Antimicrobianos",
                             2:(ncol(Multi_corto) - 1)
  )

  # remover NAs
  Antimicrobianos_2$Antimicrobianos <- str_replace_all(
    Antimicrobianos_2$Antimicrobianos, "NA-|-NA", ""
  )

  # convertir a mayusculas
  Antimicrobianos_2$Antimicrobianos <- str_to_upper(
    Antimicrobianos_2$Antimicrobianos
  )

  # convertir antimicrobiano a mayusculas
  Antimicrobiano_patrones <- str_to_upper(Antimicrobiano_patrones)

  # crear vector de abrreviaciones
  Abreviacion <- vector(mode = "character", length = length(Antimicrobiano_patrones))

  Abreviacion <- case_when(
    Antimicrobiano_patrones == "AMIKACINA" ~ "AMI",
    Antimicrobiano_patrones == "GENTAMICINA" ~ "GEN",
    Antimicrobiano_patrones == "AMPICILINA" ~ "AMP",
    Antimicrobiano_patrones == "CEFTAZIDIMA" ~ "TAZ",
    Antimicrobiano_patrones == "IMIPENEM" ~ "IMI",
    Antimicrobiano_patrones == "MEROPENEM" ~ "MEM",
    Antimicrobiano_patrones == "ERTAPENEM" ~ "ERT",
    Antimicrobiano_patrones == "CLORANFENICOL" ~ "CHL",
    Antimicrobiano_patrones == "TIGECICLINA" ~ "TGC",
    Antimicrobiano_patrones == "TETRACICLINA" ~ "TET",
    Antimicrobiano_patrones == "CIPROFLOXACINO" ~ "CIP",
    Antimicrobiano_patrones == "LEVOFLOXACINO" ~ "LVX",
    Antimicrobiano_patrones == "AZITROMICINA" ~ "AZI",
    Antimicrobiano_patrones == "TRIMETOPRIMA/SULFAMETOXAZOL" ~ "TMP/SMX",
    TRUE ~ "")

  # cambiar nombre de columnas de antimicrobianos a categrorias
  for (i in 1:length(Antimicrobianos_2$Antimicrobianos)){
    for (j in 1:length(Antimicrobiano_patrones)){
      Antimicrobianos_2$Antimicrobianos[i] <- str_replace_all(
        Antimicrobianos_2$Antimicrobianos[i], Antimicrobiano_patrones[j], Abreviacion[j])
    }
  }

  # remover columna de "Multiresistente"
  Antimicrobianos_2$Multiresistente <- NULL

  # obtener data frames de conteos
  Antimicrobianos_conteo <- count(
    Antimicrobianos_2, Antimicrobianos_2$Antimicrobianos,
    sort = T)

  # asignar columnas a data frame de conteos
  colnames(Antimicrobianos_conteo) <- c("Patron_Antimicrobianos",
                                        "Conteo")

  ############################## agregar IDs #####################################

  # convertir columna de patrones a factor
  Antimicrobianos_conteo$Patron_Antimicrobianos <- as.factor(
    Antimicrobianos_conteo$Patron_Antimicrobianos)

  # asignar niveles
  niveles_patrones <- levels(
    Antimicrobianos_conteo$Patron_Antimicrobianos)[-nrow(Antimicrobianos_conteo)]

  # crear lsta que guarde data frames de patrones (IDs de cada patron)
  Patrones_ID <- vector(mode = "list", length = length(niveles_patrones))

  # poblar lsta que guarde data frames de patrones (IDs de cada patron)
  for (i in 1:length(Patrones_ID)){
    Patrones_ID[[i]] <- assign(
      paste("Patron_", niveles_patrones[i], sep = ""),
      na.omit(Antimicrobianos_2[Antimicrobianos_2$Antimicrobianos == niveles_patrones[i],])
    )
  }

  # asignar nombres de columnas a df guardados en listas
  for (i in 1:length(niveles_patrones)){
    names(Patrones_ID[[i]]) <- niveles_patrones[i]
  }

  # crear un solo df en base a df guardados en listas
  Patrones_ID_0 <- bind_rows(Patrones_ID)

  # eliminar coluna sin NAs
  Patrones_ID_0 <- Patrones_ID_0[, !(0 == colSums(is.na(Patrones_ID_0)))]

  # transponer
  Patrones_ID_1 <- t(Patrones_ID_0)

  # insertar fila
  Patrones_ID_2 <- cbind(rownames(Patrones_ID_1), Patrones_ID_1) %>% as_tibble()

  # aisgnar nombres a columnas
  colnames(Patrones_ID_2) <- c("Patron_Antimicrobianos",
                               paste("V", 2:ncol(Patrones_ID_2), sep = "")
  )

  # agregar columna unica de IDs
  Patrones_ID_3 <- unite(Patrones_ID_2,
                         col = "ID",
                         paste("V", 2:ncol(Patrones_ID_2), sep = ""),
                         sep = ",")

  # remover NAs
  Patrones_ID_3$ID <- str_replace_all(Patrones_ID_3$ID, ",NA|NA,", "")

  ########### unir tablas de Patrones con IDs y Patrones con conteos #############

  # quitar totales de df Perfiles
  Antimicrobianos_conteo <- Antimicrobianos_conteo[-nrow(Antimicrobianos_conteo),]

  # unir conteos con IDs a traves de patrones de antimicrobianos
  Patrones_Final <- merge(Antimicrobianos_conteo, Patrones_ID_3)

  ############## agregar fila de conteos a datos de conteos #################

  # ordenar de mayor a menor
  Patrones_Final <- Patrones_Final[order(Patrones_Final$Conteo[-nrow(Patrones_Final)],
                                         decreasing = T),]

  # agregar totales
  Patrones_Final[nrow(Patrones_Final) + 1, "Conteo"] <- sum(
    Patrones_Final$Conteo)

  # volver character de vuelta
  Patrones_Final$Patron_Antimicrobianos <- as.character(
    Patrones_Final$Patron_Antimicrobianos)

  # agregar Totales
  Patrones_Final[nrow(Patrones_Final), 1] <- "TOTAL"

  Patrones_Final <<- Patrones_Final

  ######################### multiresistencia ###############################

  # definir solo filas importantes
  serotipos <- serotipos[,c("ID", "Serotipo")]

  # remover filas con NA en serotipo
  serotipos <- serotipos[!grepl("N[AD]", serotipos$Serotipo),]

  # unir serotipos con ASTs
  AM_Ser <- merge(serotipos, ASTs, by = "ID")

  # conservar solo columnas de interes
  AM_Ser <- AM_Ser[,!grepl("(mg/L)", names(AM_Ser))]

  # eliminar filas vacias
  AM_Ser <- AM_Ser[!( (ncol(AM_Ser) - 2) == rowSums(is.na(AM_Ser)) ),]

  # Volver todo a tipo titulo
  for (i in 2:ncol(AM_Ser)){
    AM_Ser[,i] <- str_to_title(AM_Ser[,i])
  }

  # cambiar resistente por antimicrobiano y susceptible por NA
  for (i in 1:nrow(AM_Ser)){
    for (j in 3:ncol(AM_Ser)){
      if (AM_Ser[i,j] == "Resistente"){
        AM_Ser[i,j] <- "Resistente"
      } else {
        AM_Ser[i,j] <- NA
      }
    }
  }

  # remover columnas vacias
  AM_Ser <- AM_Ser[!(nrow(AM_Ser) == colSums(is.na(AM_Ser)))]

  ################# obtener filas con multiresistencia ########################

  Categoria <- ""

  # cambiar nombre de antimicrobiano a columna
  Categoria <- case_when(
    colnames(AM_Ser)[-c(1,2)] == "Amikacina" ~ "Aminoglucosidos",
    colnames(AM_Ser)[-c(1,2)] == "Ampicilina" ~ "Betalactamicos",
    colnames(AM_Ser)[-c(1,2)] == "Azitromicina" ~ "Macrolidos",
    colnames(AM_Ser)[-c(1,2)] == "Ceftazidima" ~ "Betalactamicos",
    colnames(AM_Ser)[-c(1,2)] == "Ciprofloxacino" ~ "Quinolonas",
    colnames(AM_Ser)[-c(1,2)] == "Cloranfenicol" ~ "Fenicoles",
    colnames(AM_Ser)[-c(1,2)] == "Colistina" ~ "Polimixinas",
    colnames(AM_Ser)[-c(1,2)] == "Colistina" ~ "Polimixinas",
    colnames(AM_Ser)[-c(1,2)] == "Ertapenem" ~ "Betalactamicos",
    colnames(AM_Ser)[-c(1,2)] == "Gentamicina" ~ "Aminoglucosidos",
    colnames(AM_Ser)[-c(1,2)] == "Imipenem" ~ "Betalactamicos",
    colnames(AM_Ser)[-c(1,2)] == "Ertapenem" ~ "Betalactamicos",
    colnames(AM_Ser)[-c(1,2)] == "Meropenem" ~ "Betalactamicos",
    colnames(AM_Ser)[-c(1,2)] == "Levofloxacino" ~ "Quinolonas",
    colnames(AM_Ser)[-c(1,2)] == "Tetraciclina" ~ "Tetraciclinas",
    colnames(AM_Ser)[-c(1,2)] == "Tigeciclina" ~ "Tetraciclinas",
    colnames(AM_Ser)[-c(1,2)] == "Trimetoprima/sulfametoxazol" ~ "Sulfonamidas",
    TRUE ~ "")

  # sustituir AM por Categoria en nombres de columnas
  colnames(AM_Ser)[-c(1,2)] <- Categoria

  # nombres de columnas de categorias
  columnas_categorias <- colnames(AM_Ser)[-c(1,2)]

  # crear y poblar vector de categorias duplicadas
  duplicados <- colnames(AM_Ser[ , duplicated(colnames(AM_Ser))])

  # tratamiento de duplicados
  if (!length(duplicados) == 0){
    # crear lista para guardar categorias duplicadas
    lista_duplicados <- vector(mode = "list", length = length(duplicados))

    # nombrar elementos de la lista
    names(lista_duplicados) <- duplicados

    # numerar columnas de categorias para distinguirlas y poblar lista
    for (i in 1:length(columnas_categorias)){
      colnames(AM_Ser)[i + 2] <- paste(columnas_categorias[i], i, sep = "_")
    }

    # remover columnas vacias
    AM_Ser <-AM_Ser[ , !nrow(AM_Ser) == colSums(is.na(AM_Ser))]

    # poblar lista de categorias duplicadas
    for (i in 1:length(duplicados)){
      # poblar
      lista_duplicados[[i]] <- AM_Ser[, str_detect(colnames(AM_Ser), duplicados[i])]
      # hacer columna de resistentes
      lista_duplicados[[i]] <- lista_duplicados[[i]] %>% rowwise() %>%
        mutate(Global = ifelse(any(
          str_detect(c_across(cols = 1:ncol(lista_duplicados[[i]])), "Resistente")),
          "Resistente", NA))
    }

    # obtener columnas de Globales
    cat_multir_dup <- bind_cols(lista_duplicados)

    # seleccionar solo columnas "Globales"
    cat_multir_dup <- cat_multir_dup[,str_detect(names(cat_multir_dup), "Global")]

    # asinar nombres a columnas Globales
    colnames(cat_multir_dup) <- duplicados

    # quitar categorias duplicadas de AM_Ser
    for (i in duplicados){
      print(i)
      AM_Ser <- AM_Ser[,!str_detect(colnames(AM_Ser), i)]
    }

    # quitar numeración a nombres de columnas
    for (i in 3:ncol(AM_Ser)){
      colnames(AM_Ser)[i] <- str_split(colnames(AM_Ser), "_")[[i]][1]
    }

    # unir categorias antes duplicadas con el resto de categorias
    AM_Ser <- cbind(AM_Ser, cat_multir_dup)

    # volver global
    AM_Ser <<- AM_Ser
  }

  # volver global
  AM_Ser <<- AM_Ser

  # contar numero de antimicrobianos totales
  num_AM_Tot <- ncol(AM_Ser) - 2

  # contar numero de antimicrobianos por aislado
  for (i in 1:nrow(AM_Ser)){
    AM_Ser$Num_AM[i] <- num_AM_Tot - rowSums(is.na(AM_Ser[i,]))
  }

  #### obtener data frames por serotipos totales y divididas por R1 y R3 ####

  # iodentificar numero de serotipos
  niveles_ser <<- levels(as.factor(AM_Ser$Serotipo))

  # crear lista para guardar data frames de nieveles de serotipos
  Serotipos_ID <- vector(mode = "list", length = length(AM_Ser))
  Serotipos_ID_R3 <- vector(mode = "list", length = length(AM_Ser))
  Serotipos_ID_R1 <- vector(mode = "list", length = length(AM_Ser))

  # poblar lista data de frames divididos por serotipos totales
  for (i in 1:length(niveles_ser)){
    Serotipos_ID[[i]] <- assign(
      paste("Serotipos_ID", niveles_ser[i], sep = "_"),
      AM_Ser[AM_Ser$Serotipo == niveles_ser[i],]
    )
  }

  # poblar lista dividiendo por R1 y R3
  for (i in 1:length(niveles_ser)){
    Serotipos_ID_R1[[i]] <- assign(
      paste("Serotipos_ID_R1", niveles_ser[i], sep = "_"),
      AM_Ser[AM_Ser$Serotipo == niveles_ser[i] & AM_Ser$Num_AM < 3,]
    )
    Serotipos_ID_R3[[i]] <- assign(
      paste("Serotipos_ID_R3", niveles_ser[i], sep = "_"),
      AM_Ser[AM_Ser$Serotipo == niveles_ser[i] & AM_Ser$Num_AM >= 3,]
    )
  }

  ############# hacer conteos ################
  # crear vectores vacios para guardar conteos
  R1 <- vector(length = length(niveles_ser))
  R3 <- vector(length = length(niveles_ser))
  Total <- vector(length = length(niveles_ser))

  # poblar veectores de conteos que formaran al data frame final
  for (i in 1:length(niveles_ser)){
    R1[i] <- nrow(Serotipos_ID_R1[[i]])/nrow(Serotipos_ID[[i]]) * 100
    R3[i] <- nrow(Serotipos_ID_R3[[i]])/nrow(Serotipos_ID[[i]]) * 100
    Total[i] <- nrow(Serotipos_ID[[i]])
  }

  # volver globales
  R1 <<- R1
  R3 <<- R3
  Total <<- Total

  # R1 + R3 > 99.9 # CONTROL

  ##### si R1 y R3 suman 100 % entoces continua, obten tabla final sin IDs ######

  ################ obtener IDs ############################

  # asignar nombres de columnas a df guardados en listas
  for (i in 1:length(niveles_ser)){
    names(Serotipos_ID_R1[[i]]) <- niveles_ser[i]
    names(Serotipos_ID_R3[[i]]) <- niveles_ser[i]
  }

  #################### R3 ###########################

  # crear un solo df en base a df guardados en lista
  Serotipos_ID_R3_0 <- bind_rows(Serotipos_ID_R3)

  # conservar solo columnas de interes
  Serotipos_ID_R3_0 <- Serotipos_ID_R3_0[,colnames(Serotipos_ID_R3_0) %in% niveles_ser]

  # transponer
  Serotipos_ID_R3_0 <- t(Serotipos_ID_R3_0)

  # aisgnar nombres a columnas
  colnames(Serotipos_ID_R3_0) <- paste("V", 1:ncol(Serotipos_ID_R3_0), sep = "")

  # agregar columna de Serotipos
  serovars <- rownames(Serotipos_ID_R3_0) # crear columna de serotipos
  rownames(Serotipos_ID_R3_0) <- NULL # eliminar nombres de filas

  # unir y convertir en tibble
  Serotipos_ID_R3_1 <- as_tibble(cbind(serovars, Serotipos_ID_R3_0))

  # agregar columna unica con todos los IDs
  Serotipos_ID_R3_2 <- unite(Serotipos_ID_R3_1,
                             col = "ID",
                             paste("V", 1:(ncol(Serotipos_ID_R3_1) - 1), sep = ""),
                             sep = ",")

  # remover NAs
  Serotipos_ID_R3_2$ID <- str_replace_all(Serotipos_ID_R3_2$ID, ",NA|NA,|NA", "")

  #################### R1 ###########################

  # crear un solo df en base a df guardados en lista
  Serotipos_ID_R1_0 <- bind_rows(Serotipos_ID_R1)

  # conservar solo columnas de interes
  Serotipos_ID_R1_0 <- Serotipos_ID_R1_0[,colnames(Serotipos_ID_R1_0) %in% niveles_ser]

  # transponer
  Serotipos_ID_R1_0 <- t(Serotipos_ID_R1_0)

  # aisgnar nombres a columnas
  colnames(Serotipos_ID_R1_0) <- paste("V", 1:ncol(Serotipos_ID_R1_0), sep = "")

  # agregar columna de Serotipos
  serovars <- rownames(Serotipos_ID_R1_0) # crear columna de serotipos
  rownames(Serotipos_ID_R1_0) <- NULL # eliminar nombres de filas

  # unir y convertir en tibble
  Serotipos_ID_R1_1 <- as_tibble(cbind(serovars, Serotipos_ID_R1_0))

  # agregar columna unica con todos los IDs
  Serotipos_ID_R1_2 <- unite(Serotipos_ID_R1_1,
                             col = "ID",
                             paste("V", 1:(ncol(Serotipos_ID_R1_1) - 1), sep = ""),
                             sep = ",")

  # remover NAs
  Serotipos_ID_R1_2$ID <- str_replace_all(Serotipos_ID_R1_2$ID, ",NA|NA,|NA", "")

  ############ Obtener tabla final ##############

  # Tabla final sin IDs
  tablaR_final <- data.frame("Serotipo" = niveles_ser,
                             "R1" = R1,
                             "R3" = R3,
                             "Total" = Total)

  # unir Tabla Final con IDs
  Serotipos_IDs <- merge(Serotipos_ID_R1_2, Serotipos_ID_R3_2, by = "serovars")

  # asiganr nombres a Serotipos_IDs
  colnames(Serotipos_IDs) <- c("Serotipo", "ID_R1", "ID_R3")

  # unir Tabla Final
  TablaR_Final <- merge(tablaR_final, Serotipos_IDs, by = "Serotipo")

  # ordenar tabla Final
  TablaR_Final <- TablaR_Final[order(TablaR_Final$R3, decreasing = T),]

  ##################### Graficar ##########################

  # str(TablaR_Final) # CONTROL
  TablaR_Final$Serotipo <- as.factor(TablaR_Final$Serotipo)
  # str(TablaR_Final) # CONTROL

  # ordenar
  TablaR_Final <- TablaR_Final %>%
    mutate(Serotipo = reorder(Serotipo, R3, method = "length"))

  # graficar
  Multiresistencia <<- ggplot(data = TablaR_Final,
                             mapping = aes(x = Serotipo, y = R3, fill = Serotipo, group = Serotipo)) +
    geom_bar(stat = "identity", position = "stack") +
    theme_minimal() +
    theme(legend.position = "none") +
    coord_flip() +
    ggtitle("Multiresistencia por serotipos") +
    labs(y = "% de aislados", x = "", fill = "") +
    # agregar Totales
    geom_text(label = paste("N =", TablaR_Final$Total), nudge_y = 6)

  ################################################################################

  # agregar conteos totales
  TablaR_Final[nrow(TablaR_Final) + 1, "Total"] <-  sum(TablaR_Final$Total)
  TablaR_Final$Serotipo <- as.character(TablaR_Final$Serotipo)
  TablaR_Final[nrow(TablaR_Final), "Serotipo"] <- "Total"

  # convertir en tibble
  TablaR_Final <<- as_tibble(TablaR_Final)

}
