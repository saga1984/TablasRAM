#' @title Carga archivo de resultados genotipicos de WGS (por ResFinder) y
#' obtiene perfiles de genes RAM
#'
#' @description Carga el archivo de genes RAM
#' Limpia el archivo de genes RAM
#' Filtra el archivo de genes RAM
#' Obtiene Especificidad
#' Obtiene Sensibilidad
#' Obtiene conteo de perfiles de genes RAM (combinaciones de genes por aislados)
#' Relaciona y compara resultados de genes RAM con ASTs
#' Obtiene tabla final de conteos de AST con ciertos perfiles de genes RAM (o ausencia de estos) y sus IDs
#'
#' @param archivo_genotipoRuta del archivo de genes RAM (por ResFinder)
#' @return Una tabla de conteos de resultados de AST correspondientes con perfiles de genes RAM y sus IDs
#' @export

GenotipoCategorias <- function(archivo_genotipo) {
  
  # crear objeto de R
  sent_2019_gen <<- read.csv(archivo_genotipo,
                             header = FALSE,
                             col.names = paste("V",1:1000))
  
  ############# LIMPIAR DATA FRAME DE DATOS GENOTIPICOS #############
  
  # contar el total de NAs en todas las columnas de los dfs
  empty_cols_Gen <- nrow(sent_2019_gen) == colSums(is.na(sent_2019_gen))
  
  # eliminar columnas con todos los valores igual a NAs
  sent_2019_gen <- sent_2019_gen[, !empty_cols_Gen]
  
  if (sent_2019_gen[1, 1] == "Especie") {
    sent_2019_gen[,1] <- NULL
  }
  
  # eliminar, si existe, fila con la palabra ID
  sent_2019_gen <- sent_2019_gen[!grepl("ID",sent_2019_gen[,1]),]
  
  # volver global
  Sent_2019_Gen <<- sent_2019_gen
  
  # encontrar numero de genes totales y si hay un gen constitutivo
  # crear nueva columna de Tipo (Resistente o Susceptible) en archivo de ASTs categoria
  # conjunta para todos los antimicrobianos de la misma categoria
  
  #################### contar numero de genes encontrados ######################
  # quitar columna de IDs
  Sent_2019_Niveles <- Sent_2019_Gen[,-1]
  
  # definir nombres de columnas
  nombres <- 1:ncol(Sent_2019_Niveles)
  
  # volver todo factor
  Sent_2019_Niveles[, nombres] <- as_tibble(lapply(Sent_2019_Niveles[,nombres], as.factor))
  
  # sacar niveles
  niveles_all <- Sent_2019_Niveles %>% sapply(levels)
  
  # coercionar lista
  niveles_all <- unlist(niveles_all)
  
  # volver caracter para remover espacios vacios
  niveles_all <- as.character(niveles_all)
  
  # remover espacios vacios
  niveles_all_2 <- niveles_all[niveles_all != ""]
  
  # volver de nuevo factor
  niveles_all_2 <- as.factor(niveles_all_2)
  
  #obtener numero de genes
  numero_genes <<- length(levels(niveles_all_2))
  
  # encontrar si algun gen es constitutivo
  # Sent_2019_Gen[colSums(Sent_2019_Gen == "aac(6')-Iaa") >= nrow(ASTs_categoria)]
  ############# FILTRAR IDs DE INTERES #############
  
  # regex
  Regex <- ""
  Regex <-  case_when(
    Categoria == "Tetraciclinas" ~ "[to][cet][tr][(][ABGLMT][)]",
    Categoria == "Betalactamicos"~ "[abchpmr][bmlefptu][acgoprsx][CTL][MATE][PRXMY][:print:][:print:][^,]*[:digit:]*",
    Categoria == "Fenicoles" ~ "[cfo0][lmaeq][oltx][:print:][:alnum:]*",
    Categoria == "Aminoglucosidos" ~ "[asr][panmtr][prcdhmt][ A-Z || ( ][^,]*",
    Categoria == "Sulfonamidas" ~ "[SsFf][uo][l][ A-Z || 0-9 ]",
    Categoria == "Diaminopirimidinas" ~ "dfr[:upper:]*[^,]",
    Categoria == "Polimixinas" ~ "[AaIiMmBbCcLlPp][clrhmg][ropxm][A-Z || -]*[^,]",
    Categoria == "Diaminopiridinas" ~ "dfr[:upper:]",
    Categoria == "Rifamicinas" ~ "[Aair][Rrgop][Rritxho]",
    Categoria == "Quinolonas" ~ "[GgQqNoPp][ayenoq][rprx][:upper:][0-9 || _ ]*[^,]*",
    Categoria == "Macrolidos" ~ "[lcem][rasfhiltegp][htfmbprae][ A-Z || ( ][^,]*",
    TRUE ~ "")
  
  # genes
  Gen <- unite(Sent_2019_Gen,
               col = "Genes",
               colnames(Sent_2019_Gen)[-1],
               sep = ",")
  
  ######## clasificar en R y S ########
  # crear nueva columna de Tipo (R o S) en archivo de genes
  Gen <- Gen %>% mutate(
    Tipo = case_when(
      str_detect(Gen$Genes, Regex) ~ "Resistente",
      TRUE ~ "Susceptible")
  )
  
  # crear nueva columna de Perfiles "blaP,ampC,parC,floR,etc" en archivo de genes
  Gen$Perfiles <- str_extract_all(
    Gen$Genes, regex(Regex)
  )
  
  # asignar nombres de columnas a df final
  colnames(Gen) <- c("ID", "Genes", "Resistencia", "Perfil")
  
  # remover columna
  Gen$Genes <- NULL
  
  # encontrar fila con mayor numero de elementos
  numero_mayor_elementos <- max(lengths(Gen$Perfil))
  
  # modificar el numero de elemetos de la lista (convertir a numero_mayor_elementos)
  for (i in 1:length(Gen$Perfil)){
    length(Gen$Perfil[[i]]) <- numero_mayor_elementos
  }
  
  # convertir la lista de elementos a data frame y transponer
  Gen2 <- data.frame(Gen$Perfil) %>% t()
  
  # unir data frames
  Gen3 <- cbind(Gen[, c("ID","Resistencia")], Gen2)
  
  
  # volver a crear columna perfil con NAs
  Gen4 <- unite(Gen3,
                col = "Perfil_NA",
                colnames(Gen3)[-(1:2)],
                sep = ",")
  
  # remover NAs
  Gen4 <- gsub(
    c('NA|,NA|NA,'), '', Gen4$Perfil_NA) %>%
    tibble()
  
  # asiganr nombre a columna de genes de categoria elegida
  colnames(Gen4) <- "Perfil"
  
  # unir columna perfil limpia
  Gen5 <- cbind(Gen3[,c("ID", "Resistencia")], Gen4)
  
  # asignar variable Gen y convertir a tibble
  Gen <- Gen5
  Gen <<- as_tibble(Gen)
  
  ################## UNIR GENOTIPO CON FENOTIPO ###################
  
  # crear df completo
  Completo <- data.frame(merge(Gen, ASTs_categoria, by = "ID"))
  Completo <- type.convert(Completo, as.is = TRUE)
  Completo <<- na.omit(Completo)
  
  # cambiar nombre de columnas
  colnames(Completo) <- c("ID", "Genotipo", "Perfil", "Fenotipo")
  
  # coercionar NA y espacios en blanco a "Sin_Gen"
  Completo$Perfil[is.na(Completo$Perfil)] <- ""
  Completo$Perfil[Completo$Perfil == ""] <- "Sin_Gen"
  
  # comparar resistencias entre genotipo y fenotipo
  Completo$Versus <- ifelse(Completo$Genotipo == Completo$Fenotipo, "IGUAL", "DIFERENTE")
  
  # buscar aislados raros clasificados como:
  # falsos positivos (no existe gen pero es resistente por AST)
  # genes apagados (existe un gen pero es susceptible por AST)
  Completo$Raros <- ifelse(
    Completo$Perfil != "Sin_Gen" & Completo$Fenotipo == "Susceptible", "Apagado",
    ifelse(
      Completo$Perfil == "Sin_Gen" & Completo$Fenotipo == "Resistente", "Falso Positivo",
      "Normal"))
  
  # volver global
  Completo$Perfil <- unlist(Completo$Perfil) # quitar formato de lista a Perfil
  Completo <<- data.frame(Completo)
  
  ############# SENSIBILIDAD ##############
  
  # columnas filtradas
  col1 <- Completo$Genotipo == "Resistente"
  col2 <- Completo$Fenotipo == "Resistente"
  
  # sumas
  suma_col1 <- sum(col1)
  suma_col2 <- sum(col2)
  
  # obtener sensiblidad
  Sensibilidad <- (suma_col1 / suma_col2) * 100
  sensibilidad <<- capture.output(cat("Sensibilidad: ", round(Sensibilidad,2), ",",
                                      " ", "(", suma_col1, ")/(", suma_col2, ")", sep = ""))
  # imprimir sensibilidad
  cat("",sensibilidad,"", sep = "\n")
  
  ############# ESPECIFICIDAD ##############
  
  # columnas filtradas
  col1 <- Completo$Genotipo == "Susceptible"
  col2 <- Completo$Fenotipo == "Susceptible"
  
  # sumas
  suma_col1 <- sum(col1)
  suma_col2 <- sum(col2)
  
  # obtener sensiblidad
  Especificidad <- (suma_col1 / suma_col2) * 100
  especificidad <<- capture.output(cat("Especificidad: ", round(Especificidad,2), ",",
                                       " ", "(", suma_col1, ")/(", suma_col2, ")", sep = ""))
  # imprimir sensibilidad
  cat("",especificidad,"", sep = "\n")
  
  ################# PERFILES ######################
  
  # crear df de perfiles
  perfiles <- Completo %>% count(Perfil)
  # asignar nombres de columnas
  colnames(perfiles) <- c("Perfil", "No_Aislados")
  # obtener el total
  perfiles[(nrow(perfiles) + 1),"No_Aislados"] <- sum(perfiles$No_Aislados)
  # agregar el Total
  perfiles[nrow(perfiles), "Perfil"] <- "Total"
  
  ####### asignar valor de fin (columna donde esta "Sin_Gen") #######
  # variable donde esta "Sin_Gen"
  constitutivo <- str_detect(Completo$Perfil,
                             "aac[:punct:]6[:punct:][:punct:]-Iaa,|aac[:punct:]6[:punct:][:punct:]-Iaa")
  
  
  # si no existe Sin_Gen es muy probable que exista el gen constitutivo aac(6')-Iaa
  # se debe remover
  if (sum(constitutivo[TRUE]) == nrow(Completo) & Categoria == "Aminoglucosidos") {
    
    # reemplazar gen constitutivo por strng vacio
    Completo$Perfil <- str_replace_all(
      Completo$Perfil, "aac[:punct:]6[:punct:][:punct:]-Iaa,|aac[:punct:]6[:punct:][:punct:]-Iaa", "")
    
    # reemplazar string vacio por "Sin_Gen"
    Completo$Perfil[Completo$Perfil == ""] <- "Sin_Gen"
    
    # modificar columna de Genotipo
    Completo$Genotipo[Completo$Perfil == "Sin_Gen"] <- "Susceptible"
    
    # comparar resistencias entre genotipo y fenotipo
    Completo$Versus <- ifelse(Completo$Genotipo == Completo$Fenotipo, "IGUAL", "DIFERENTE")
    
    # buscar aislados raros clasificados como:
    # falsos positivos (no existe gen pero es resistente por AST)
    # genes apagados (existe un gen pero es susceptible por AST)
    Completo$Raros <- ifelse(
      Completo$Perfil != "Sin_Gen" & Completo$Fenotipo == "Susceptible", "Apagado",
      ifelse(
        Completo$Perfil == "Sin_Gen" & Completo$Fenotipo == "Resistente", "Falso Positivo",
        "Normal"))
    
    # volver global
    Completo$Perfil <- unlist(Completo$Perfil) # quitar formato de lista a Perfil
    Completo <<- data.frame(Completo)
    
    ############### recalcular sensibilidad y sensibilidad ###############
    
    ############# SENSIBILIDAD 2 ##############
    
    # columnas filtradas
    col1 <- Completo$Genotipo == "Resistente"
    col2 <- Completo$Fenotipo == "Resistente"
    # sumas
    suma_col1 <- sum(col1)
    suma_col2 <- sum(col2)
    # obtener sensiblidad
    Sensibilidad <- (suma_col1 / suma_col2) * 100
    sensibilidad <<- capture.output(cat("Sensibilidad corregida: ", round(Sensibilidad,2), ",",
                                        " ", "(", suma_col1, ")/(", suma_col2, ")", sep = ""))
    # imprimir sensibilidad
    cat("",sensibilidad,"", sep = "\n")
    
    ############# ESPECIFICIDAD 2 ##############
    
    # columnas filtradas
    col1 <- Completo$Genotipo == "Susceptible"
    col2 <- Completo$Fenotipo == "Susceptible"
    # sumas
    suma_col1 <- sum(col1)
    suma_col2 <- sum(col2)
    # obtener sensiblidad
    Especificidad <- (suma_col1 / suma_col2) * 100
    especificidad <<- capture.output(cat("Especificidad corregida: ", round(Especificidad,2), ",",
                                         " ", "(", suma_col1, ")/(", suma_col2, ")", sep = ""))
    # imprimir sensibilidad
    cat("",especificidad,"", sep = "\n")
    
    ###################### crear perfiles #########################
    # crear df de perfiles
    perfiles <- Completo %>% count(Perfil)
    # asignar nombres de columnas
    colnames(perfiles) <- c("Perfil", "No_Aislados")
    # obtener el total
    perfiles[(nrow(perfiles) + 1),"No_Aislados"] <- sum(perfiles$No_Aislados)
    # agregar el Total
    perfiles[nrow(perfiles), "Perfil"] <- "Total"
    
  }
  
  ##################### crear Sin_Gen ############################
  # re-asignar variable donde esta "Sin_Gen"
  Sin_Gen <<- which(perfiles$Perfil == "Sin_Gen")
  
  # asignar valor de "fin"
  if (Sin_Gen == 1) {
    # resignar valor de fin
    fin <- nrow(perfiles) - 1
  } else {
    # obtener fila final de aislados con genes
    fin <- which(perfiles$Perfil == "Sin_Gen") - 1
  }
  fin
  # sumar aislados con genes
  perfiles[fin, 3] <- sum(perfiles$No_Aislados[-c(Sin_Gen, nrow(perfiles))])
  # agregar valor de "Sin_Gen"
  perfiles[perfiles$Perfil == "Sin_Gen", 3] <- perfiles[perfiles$Perfil == "Sin_Gen", 2]
  # volver a asiganr cnombres de columnas
  colnames(perfiles) <- c("Perfil", "No_Aislados", "Genotipo_Total")
  # sumas genotipo con gen y sin gen
  perfiles[nrow(perfiles), 3] <- sum(perfiles$Genotipo_Total, na.rm = T)
  # cambiar NAs por 0s
  perfiles[is.na(perfiles)] <- ""
  # volver global
  Perfiles <<- data.frame(perfiles)
  
  # imprimir data frame
  print(Perfiles)
  
  ################ DATA FRAMES FINALES #################
  
  # totales
  Totales_Resistente <- na.omit(Completo[Completo$Fenotipo == "Resistente",])
  Totales_Susceptible <- na.omit(Completo[Completo$Fenotipo == "Susceptible",])
  
  ########## volver globales ###########
  Totales_Resistente <<- Totales_Resistente
  Totales_Susceptible <<- Totales_Susceptible
  
  # coercionar columna de perfiles
  Completo$Perfil <- as.factor(Completo$Perfil)
  
  # ver perfiles
  niveles <- levels(Completo$Perfil)
  
  ### volver global ###
  niveles <<- niveles
  
  ########### crear listas de "Fenotipo Resistente" ###########
  Fenotipos_Resistentes <- vector(mode = "list", length = length(niveles))
  Fenotipos_Susceptibles <- vector(mode = "list", length = length(niveles))
  
  #### poblar listas con daa frames de IDs de cada nivel o perfil de genes ####
  for (i in 1:length(niveles)){
    # crear data frames y guardarlos en lista
    Fenotipos_Resistentes[[i]] <- assign(paste("Fenotipo_Resistente_",niveles[i], sep = ""),
                                         na.omit( Completo[Completo$Perfil == niveles[i] &
                                                             Completo$Fenotipo == "Resistente",]) )
  }
  
  for (i in 1:length(niveles)){
    # crear data frames y guardarlos en lista
    Fenotipos_Susceptibles[[i]] <- assign(paste("Fenotipo_Susceptible_", niveles[i], sep = ""),
                                          na.omit( Completo[Completo$Perfil == niveles[i] &
                                                              Completo$Fenotipo == "Susceptible",]) )
  }
  
  ############## volver globales ###############
  Fenotipos_Resistentes <<- Fenotipos_Resistentes
  Fenotipos_Susceptibles <<- Fenotipos_Susceptibles
  
  ############### LISTAS DE IDs Y CONTEOS #################
  
  ######## resistentes y susceptibles totales #########
  ID_Totales_Resistente <<- Totales_Resistente$ID
  ID_Totales_Suseptible <<- Totales_Susceptible$ID
  ID_Totales_Resistente_Conteo <<- length(ID_Totales_Resistente)
  ID_Totales_Suseptible_Conteo <<- length(ID_Totales_Suseptible)
  
  # listas de Resistentes y Susceptibles para guardar vectores de IDs y conteos
  ID_Fenotipos_Resistentes <- vector(
    mode = "list", length = length(niveles))
  
  ID_Fenotipos_Susceptibles <- vector(
    mode = "list", length = length(niveles))
  
  ID_Fenotipos_Resistentes_Conteos <- vector(
    mode = "list", length = length(niveles))
  
  ID_Fenotipos_Susceptibles_Conteos <- vector(
    mode = "list", length = length(niveles))
  
  ########### Poblar listas de Resistentes ##############
  for (i in 1:length(Fenotipos_Resistentes)){
    ID_Fenotipos_Resistentes[[i]] <- assign(
      paste("ID_Fenotipos_Resistentes_", niveles[i], sep = ""),
      na.omit(Fenotipos_Resistentes[[i]][1])
    )
    names(ID_Fenotipos_Resistentes)[i] <- niveles[i]
    ID_Fenotipos_Resistentes_Conteos[[i]] <- assign(
      paste("ID_Fenotipos_Resistentes_Conteo_", niveles[i], sep = ""),
      nrow(Fenotipos_Resistentes[[i]])
    )
    names(ID_Fenotipos_Resistentes_Conteos)[i] <- niveles[i]
  }
  
  ######### Poblar listas de Susceptibles ##########
  for (i in 1:length(Fenotipos_Susceptibles)){
    ID_Fenotipos_Susceptibles[[i]] <- assign(
      paste("ID_Fenotipos_Susceptibles_", niveles[i], sep = ""),
      na.omit( Fenotipos_Susceptibles[[i]][1])
    )
    names(ID_Fenotipos_Susceptibles)[i] <- niveles[i] # nombrar
    ID_Fenotipos_Susceptibles_Conteos[[i]] <- assign(
      paste("ID_Fenotipos_Susceptibles_Conteo_", niveles[i], sep = ""),
      nrow(Fenotipos_Susceptibles[[i]])
    )
    names(ID_Fenotipos_Susceptibles_Conteos)[i] <- niveles[i] # nombrar
  }
  
  ################## volver globales ####################
  ID_Fenotipos_Resistentes <<- ID_Fenotipos_Resistentes
  ID_Fenotipos_Susceptibles <<- ID_Fenotipos_Susceptibles
  ID_Fenotipos_Resistentes_Conteos <<- ID_Fenotipos_Resistentes_Conteos
  ID_Fenotipos_Susceptibles_Conteos <<- ID_Fenotipos_Susceptibles_Conteos
  
  #################### TABLA FINAL #######################
  
  ########### agregar AST_Resistentes #############
  
  # volver lista a Data frame
  AST_R <- bind_rows(ID_Fenotipos_Resistentes_Conteos) %>% t() %>% data.frame()
  
  # AST_R[nrow(AST_R) + 1,] <- AST_R %>% sum() 
  names(AST_R) <- "AST_Resistentes"
  AST_R[, "Perfil"] <- rownames(AST_R)
  #AST_R[nrow(AST_R), "Perfil"] <- "Total"
  
  # volver global
  AST_R <<- AST_R
  
  ########### agregar AST_Susceptibles ############
  
  # volver lista a Data frame
  AST_S <- bind_rows(ID_Fenotipos_Susceptibles_Conteos) %>% t() %>% data.frame()
  
  # preparar AST_S para hacer merge
  #AST_S[nrow(AST_S)+1,] <- AST_S %>% sum() 
  names(AST_S) <- "AST_Susceptibles"
  AST_S[, "Perfil"] <- rownames(AST_S)
  #AST_S[nrow(AST_S), "Perfil"] <- "Total"
  
  # volver global
  AST_S <<- AST_S
  
  ############ unir AST_R y AST_S y Perfiles ###########
  # quitar totales de df Perfiles
  Perfiles2 <- Perfiles[-nrow(Perfiles),]
  
  # unir AST_R y AST_S
  AST_R_S <- merge(AST_R, AST_S, by = "Perfil")
  # unir AST_R_S con Perfiles
  ASTs_Perfiles <- merge(AST_R_S, Perfiles2[, c("Perfil", "No_Aislados")], by = "Perfil")
  
  # volver global
  ASTs_Perfiles <<- ASTs_Perfiles
  
  ########### agregar AST_ID_Resistentes ###############
  
  # llenar df vacios con NAs para no perder perfiles
  IDs_Fenotipos_Resistentes <- map_if(ID_Fenotipos_Resistentes, ~ nrow(.) == 0, ~ data.frame(matrix(NA, nrow = 1, ncol = 0)))
  
  if(sum(Completo$Fenotipo == "Susceptible") == nrow(Completo)){
    
    # crear un solo df en base a df guardados en listas
    IDs_Fenotipos_Resistentes_0 <<- bind_rows(IDs_Fenotipos_Resistentes, .id = "Perfil") %>% 
      data.frame()
    
    # crear directamente IDs_Fenotipos_Resistentes 
    IDs_Fenotipos_Resistentes_0$ID <- c(rep("Sin_Gen", times = length(ID_Fenotipos_Resistentes))) %>% 
      as.vector()
    
    # asiganr nombres
    names(IDs_Fenotipos_Resistentes_0) <- c("Perfil", "ID_Resistentes")
    
    # crear directamente IDs_Fenotipos_Resistentes y volver global
    IDs_Fenotipos_Resistentes <- IDs_Fenotipos_Resistentes_0
    IDs_Fenotipos_Resistentes <<- IDs_Fenotipos_Resistentes
    
  }else{
    
    # convertir lista de data frames en data frame
    IDs_Fenotipos_Resistentes_0 <<- bind_rows(IDs_Fenotipos_Resistentes, .id = "Perfil") %>%
      data.frame()
    
    # crear data frame donde se agrupan IDs en forma de listas
    IDs_Fenotipos_Resistentes_1 <<- IDs_Fenotipos_Resistentes_0 %>%
      pivot_wider(names_from = "Perfil", values_from = "ID", values_fn = list) %>%
      t() %>%
      data.frame()
    
    ######### quitar formato de listas ##########
    
    # asignar nombres 
    names(IDs_Fenotipos_Resistentes_1) <- "IDs"
  
    # encontrar fila con mayor numero de elementos
    numero_mayor_elementos <- max(lengths(IDs_Fenotipos_Resistentes_1$IDs))
    
    # modificar el numero de elemetos de la lista (convertir a numero_mayor_elementos)
    for(i in 1:length(IDs_Fenotipos_Resistentes_1$IDs)){
      length(IDs_Fenotipos_Resistentes_1$IDs[[i]]) <- numero_mayor_elementos
    }
    
    # convertir la lista de elementos a data frame y transponer
    IDs_Fenotipos_Resistentes_2 <- bind_rows(IDs_Fenotipos_Resistentes_1$IDs) %>% t() %>% data.frame()
    
    # unir IDs en una sola columna
    IDs_Fenotipos_Resistentes_3 <<- unite(IDs_Fenotipos_Resistentes_2,
                                          col = "ID_Resistentes",
                                          colnames(IDs_Fenotipos_Resistentes_2),
                                          sep = ",")
    
    # agregar columna de Perfil
    IDs_Fenotipos_Resistentes_3[, "Perfil"] <- rownames(IDs_Fenotipos_Resistentes_3)
    
    # remover NAs
    IDs_Fenotipos_Resistentes_3$ID_Resistentes <- str_replace_all(
      IDs_Fenotipos_Resistentes_3$ID_Resistentes,
      c(",NA|NA,"), "") %>% as.vector()
    
    # cambiar NA unicos
    IDs_Fenotipos_Resistentes_3$ID_Resistentes <- str_replace_all(
      IDs_Fenotipos_Resistentes_3$ID_Resistentes,
      c("NA"),"Sin_ID") %>% as.vector()
    
    # volver global
    IDs_Fenotipos_Resistentes <- IDs_Fenotipos_Resistentes_3
    IDs_Fenotipos_Resistentes <<- IDs_Fenotipos_Resistentes
    
  }
  
  ########### agregar AST_ID_Susceptibles ###############
  
  # llenar df vacios con NAs para no perder perfiles
  IDs_Fenotipos_Susceptibles <- map_if(ID_Fenotipos_Susceptibles, ~ nrow(.) == 0, ~ data.frame(matrix(NA, nrow = 1, ncol = 0)))
  
  if (sum(Completo$Fenotipo == "Resistente") == nrow(Completo)) {
    
    # crear un solo df en base a df guardados en listas
    IDs_Fenotipos_Susceptibles_0 <<- bind_rows(IDs_Fenotipos_Susceptibles, .id = "Perfil") %>% 
      data.frame()
    
    # crear directamente IDs_Fenotipos_Resistentes 
    IDs_Fenotipos_Susceptibles_0$ID <- c(rep("Sin_Gen", times = length(ID_Fenotipos_Susceptibles))) %>% 
      as.vector()
    
    # asiganr nombres
    names(IDs_Fenotipos_Susceptibles_0) <- c("Perfil", "ID_Sensibles")
    
    # crear directamente IDs_Fenotipos_Resistentes_3
    IDs_Fenotipos_Susceptibles <- IDs_Fenotipos_Susceptibles_0
    IDs_Fenotipos_Susceptibles <<- IDs_Fenotipos_Susceptibles
    
  }else{
    
    # crear un solo df en base a df guardados en listas
    IDs_Fenotipos_Susceptibles_0 <<- bind_rows(IDs_Fenotipos_Susceptibles, .id = "Perfil") %>% 
      data.frame()
    
    # crear data frame donde se agrupan IDs en forma de listas
    IDs_Fenotipos_Susceptibles_1 <<- IDs_Fenotipos_Susceptibles_0 %>%
      pivot_wider(names_from = "Perfil", values_from = "ID", values_fn = list) %>%
      t() %>%
      data.frame()
    
    ######### quitar formato de listas ##########
    # asiganr nombres 
    names(IDs_Fenotipos_Susceptibles_1) <- "IDs"
  
    # encontrar fila con mayor numero de elementos
    numero_mayor_elementos <- max(lengths(IDs_Fenotipos_Susceptibles_1$IDs))
    
    # modificar el numero de elemetos de la lista (convertir a numero_mayor_elementos)
    for (i in 1:length(IDs_Fenotipos_Susceptibles_1$IDs)){
      length(IDs_Fenotipos_Susceptibles_1$IDs[[i]]) <- numero_mayor_elementos
    }
    
    # convertir la lista de elementos a data frame y transponer
    IDs_Fenotipos_Susceptibles_2 <- bind_rows(IDs_Fenotipos_Susceptibles_1$IDs) %>% t() %>% data.frame()
    
    # unir IDs en una sola columna
    IDs_Fenotipos_Susceptibles_3 <<- unite(IDs_Fenotipos_Susceptibles_2,
                                           col = "ID_Susceptibles",
                                           colnames(IDs_Fenotipos_Susceptibles_2),
                                           sep = ",")
    
    # agregar columna de Perfil
    IDs_Fenotipos_Susceptibles_3[, "Perfil"] <- rownames(IDs_Fenotipos_Susceptibles_3)
    
    # remover NAs
    IDs_Fenotipos_Susceptibles_3$ID_Susceptibles <- str_replace_all(
      IDs_Fenotipos_Susceptibles_3$ID_Susceptibles,
      c(",NA|NA,"),
      "") %>% as.vector()
    
    # cambiar NA unicos
    IDs_Fenotipos_Susceptibles_3$ID_Susceptibles <- str_replace_all(
      IDs_Fenotipos_Susceptibles_3$ID_Susceptibles,
      c("NA"), "Sin_ID") %>% as.vector()
    
    # volver global
    colnames(IDs_Fenotipos_Susceptibles_3) <- c("ID_Sensibles", "Perfil")
    IDs_Fenotipos_Susceptibles <- IDs_Fenotipos_Susceptibles_3
    IDs_Fenotipos_Susceptibles <<- IDs_Fenotipos_Susceptibles
    
  }
  
  ######## crear data frame final ##########
  rownames(IDs_Fenotipos_Resistentes) <- NULL
  rownames(IDs_Fenotipos_Susceptibles) <- NULL
  IDs_Fenotipos_Resistentes <<- IDs_Fenotipos_Resistentes 
  IDs_Fenotipos_Susceptibles <<- IDs_Fenotipos_Susceptibles
  
  # df intermedio AST
  final_IDs <- merge(IDs_Fenotipos_Resistentes,
                     IDs_Fenotipos_Susceptibles,
                     by = "Perfil")
  
  # volver global
  final_IDs <<- final_IDs
  
  # df final "ASTs con Genotipos Totales"
  Final <<- merge(ASTs_Perfiles,
                  final_IDs,
                  by = "Perfil")
  
  # df Final
  colnames(Final) <- c("Perfil",
                       "AST_Resistentes",
                       "AST_Sensibles",
                       "No_Aislados",
                       "ID_Resistentes",
                       "ID_Sensibles")
  
  # agregar totales
  Final <- as_tibble(Final)
  Final[nrow(Final) + 1, "Perfil"] <- "Total"
  
  # obtener vector de sumas
  sumas <- c(sum(Final$AST_Resistentes, na.rm = T),
             sum(Final$AST_Sensibles, na.rm = T),
             sum(Final$No_Aislados, na.rm = T))
  Final <- data.frame(Final)
  
  # poblar df final con sumas totales
  Final[nrow(Final), c("AST_Resistentes", "AST_Sensibles", "No_Aislados")] <- sumas
  
  # remover la palabra "Total"
  Final[nrow(Final),c("ID_Resistentes", "ID_Sensibles")] <- c(
    especificidad,
    sensibilidad)
  
  # volver global
  Final_df <<- Final
  
  ########################### graficar df_final ############################
  
  # subset de Final_df
  Final_df_corto <- Final_df[,c(1,2,3)]
  
  # volverlo formato largo 3 cols (2 categoricas 1 numerica)
  Final_df_long <<- Final_df_corto %>% 
    pivot_longer(cols = !Perfil,
                 names_to = "Interpretacion",
                 values_to = "Conteo")
  
  # ordenar
  Final_df_long$Perfil <- with(data = Final_df_long,
                               reorder(Perfil, Conteo, fun = "length" ))
  
  # graficar
  ggPerfil <<- ggplot(data = Final_df_long,
                      mapping = aes(x = Perfil, 
                                    y = Conteo,
                                    fill = Interpretacion)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal() +
    coord_flip() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    ggtitle(paste("Perfil de Genes de Aislados Resistentes y Sensibles por AST ", "(",Categoria, ")",sep = "")) +
    labs(y = "Conteo", x = "") +
    scale_fill_discrete(labels=c("AST Resistentes", "AST Sensibles"))
  
}
