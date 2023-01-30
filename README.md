# TablasRAM
TablasRAM Importa un archivo de genes RAM y un archivo de resultados de pruebas de susceptibilidad a antimicrobianos (AST), los compara, obtiene perfiles de genes RAM y patrones de antimicrobianos en aislados multiresistentes, aislados multiresistentes en Tabla e imagen. Cuenta correspondencias entre Genotipo-Fenotipo. Calcula sensibilidad y especificidad y devuelve IDs. Por categorias RAM

## Forma de correr las funciones de este paquete:     

  1.- FenotipoCategorias(archivo_fenotipo)     
  2.- GenotipoCategorias(archivo_genotipo)      
  3.- GuardarCategoriasRAM(ruta,nombre)      
  
## Ejemplo de formatos de archivos de entrada:    

  ### Archivo_genotipo:    

Especie	ID	Genes_RAM		    
Salmonella_enterica	ID_01	aac(6')-Iaa	qnrB19	parC_T57S     
Salmonella_enterica	ID_02	aac(6')-Iaa	aadA1	aph(4)-Ia	aac(3)-IV	aph(3')-Ia	blaCTX-M-65    
Salmonella_enterica	ID_03	aac(6')-Iaa	fosL1	floR	qnrB19     


 ### Archivo_fenotipo:

ID  Amikacina	AK_mg/l Gentemicina	GN_mg/l    
ID_01	Susceptible	2	Susceptible	2    
ID_02	Susceptible	2	Susceptible	1    
ID_03	Susceptible	2	Susceptible	1    

## Antimicrobianos y Categorias incluidos en el paquete:   
         
Amikacina ~ Aminoglucosidos   
Ampicilina ~ Betalactamicos    
Azitromicina ~ Macrolidos     
Ceftazidima ~ Betalactamicos     
Ciprofloxacino ~ Quinolonas      
Cloranfenicol ~ Fenicoles     
Colistina ~ Polimixinas     
Ertapenem ~ Betalactamicos.....
Gentamicina ~ Aminoglucosidos     
Imipenem ~ Imipenem    
Levofloxacino ~ Quinolonas    
Meropenem" ~ "Betalactamicos     
Tetraciclina ~ Tetraciclinas    
Tigeciclina" ~ "Tetraciclinas    

## Archivos de salida:    
### Tabla_Multiresistencia    
  Serotipo	R1	R3	Total	ID_R1	ID_R3    
Hadar	0	100	2		ID01,ID02    
Newport	0	100	1		ID14    
Thompson	0	100	1		ID61    
Albany	20	80	5	ID11	ID97,ID97,ID25,ID59    
Senftenberg	33.33333333	66.66666667	3	ID18	ID99,ID34    

### Patrones_Antimicrobianos     
Patrones_Antimicrobianos	Conteo	ID     
CHL-TET-AMI-GEN-LVX	2	ID57,ID58    
