#' @title Guarda Perfiles RAM y Tabla Final por categorias
#' (conteos e IDs de perfiles RAM y ASTs)
#'
#' @description Guarda los resultados mas importantes que son:
#' Archivo Perfiles que contiene perfiles de Genes RAM y sus conteos (asi como su ausencia)
#' Archivo Final que contiene la tabla de resultados en conteos de aislados con cierto perfil
#' de genes RAM y sus ausencia, relacionados a resultados fenotipicos.
#'
#' @param ruta Ruta del directorio donde se creara el directorio para guardar resultados
#' @return Crea directorio de categoria de antimicrobiano correspondiente y
#' guarda dentro archivo de perfiles RAM y tabla final. En el directorio de ruta guarda:
#' tablas de multiresistencia y patrones de antimicrobianos y grafica de multiresistencia en uno
#' de los formatos disponibles (jpeg, tiff, bmp y png, default jpeg), en alta resolucion
#'
#' @export

GuardarCategoriasRAM <- function(ruta, nombre, formato_img = jpeg) {
 
   if (exists("Final_df") & exists("Perfiles") & exists("Patrones_Final")) {
    # definir variables
    directorio_final <- paste(ruta, Categoria, sep = "")
    ################################## Tabla Principal ####################
    archivo_final_tabla <- paste(directorio_final, "/", nombre, "_",
                                 str_to_title(Categoria), ".csv", sep = "")
    ################################ Perfiles #############################
    archivo_final_perfiles <- paste(directorio_final, "/", "Perfiles_",
                                    str_to_title(Categoria), ".csv", sep = "")
    ######################## Patrones Antimicrobianos #####################
    archivo_final_patrones <- paste(ruta, "Patrones_Antimicrobianos", 
                                    ".csv", sep = "")
    ########################### Multiresistencia ##########################
    archivo_final_multiresistencia <- paste(ruta, "Tabla_Multiresistencia", 
                                    ".csv", sep = "")
    ######################## Imagen Multiresistencia ######################
    formato_img(paste(ruta, "Multirresistencia.", as.character(substitute(formato_img)), sep = ""),
                res = 300,
                width = 2000,
                height = 2000)
    print(Multiresistencia)
    dev.off()
    ##########################################################################
    # crear directorio
    dir.create(directorio_final)
    # crear archivo1
    write.csv(Final_df, archivo_final_tabla, row.names = F)
    # crear archivo2
    write.csv(Perfiles, archivo_final_perfiles, row.names = F)
    # crear archivos 3 y 4, una sola vez sobre "ruta", solo si no existe
    if (!file.exists(archivo_final_patrones)) {
      write.csv(Patrones_Final, archivo_final_patrones, row.names = F)
    }
    if (!file.exists(archivo_final_multiresistencia)) {
      write.csv(TablaR_Final, archivo_final_multiresistencia, row.names = F)
      write.csv(Patrones_Final, archivo_final_patrones, row.names = F)
    }
  } else {
    print("La categoria o los archivos 'Final' y/o 'Perfiles' no existen")
  }

}


