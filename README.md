# TablasRAM
TablasRAM Importa un archivo de genes RAM y un archivo de resultados de pruebas de susceptibilidad a antimicrobianos (AST), los compara, obtiene perfiles de genes RAM y patrones de antimicrobianos en aislados multiresistentes, aislados multiresistentes en Tabla e imagen. Cuenta correspondencias entre Genotipo-Fenotipo. Calcula sensibilidad y especificidad y devuelve IDs. Por categorias RAM

## Forma de correr las funciones de este paquete:     

  1.- FenotipoCategorias(archivo_fenotipo)     
  2.- GenotipoCategorias(archivo_genotipo)      
  3.- GuardarCategoriasRAM(ruta,nombre)      
  
## Ejemplo de formatos de archivos de entrada:    

  ### Archivo_genotipo:    

Especie	            | ID   | Genes_RAM                                                      |
--------------------|------|----------------------------------------------------------------|
Salmonella_enterica	| ID01 | aac(6')-Iaa	qnrB19	parC_T57S                                 |
Salmonella_enterica |	ID02 | aac(6')-Iaa	aadA1	aph(4)-Ia	aac(3)-IV	aph(3')-Ia	blaCTX-M-65 | 
Salmonella_enterica |	ID03 | aac(6')-Iaa	fosL1	floR	qnrB19                                |

### Archivo_fenotipo:

ID   | Amikacina	  | AK_mg/l | Gentemicina	| GN_mg/l |
-----|--------------|---------|-------------|---------|  
ID01 | Susceptible	| 2	      | Susceptible	| 2       |
ID02 | Susceptible	| 2	      | Susceptible	| 1       |
ID03 | Susceptible	| 2	      | Susceptible	| 1       |

## Antimicrobianos y Categorias incluidos en el paquete:   
         
Amikacina ~ Aminoglucosidos   
Ampicilina ~ Betalactamicos    
Azitromicina ~ Macrolidos     
Ceftazidima ~ Betalactamicos     
Ciprofloxacino ~ Quinolonas      
Cloranfenicol ~ Fenicoles     
Colistina ~ Polimixinas     
Ertapenem ~ Betalactamicos     
Gentamicina ~ Aminoglucosidos     
Imipenem ~ Imipenem    
Levofloxacino ~ Quinolonas    
Meropenem" ~ "Betalactamicos     
Tetraciclina ~ Tetraciclinas    
Tigeciclina" ~ "Tetraciclinas    

## Archivos de salida:    
### Tabla_Multiresistencia    
Serotipo	 | R1 | R3	| Total |	ID_R1 |	ID_R3               |    
-----------|----|-----|-------|-------|---------------------|
Hadar	     |0	  |100	|2      |	      |ID01,ID02            |   
Newport	   |0   |100  |1      |		    |ID14                 |
Thompson	 |0   |100  |1      |		    |ID61                 |    
Albany	   |20  |80   |5      |ID11   |ID97,ID97,ID25,ID59  |
Senftenberg|33.3|66.67|3      |ID18   |ID99,ID34            |    

### Patrones_Antimicrobianos     
Patrones_Antimicrobianos	Conteo	ID     
CHL-TET-AMI-GEN-LVX	2	ID57,ID58    
GEN-CHL-CIP	1	ID51
GEN-TAZ-CHL-CIP	1	ID33
TOTAL	64	

### Perfil_Genes_RAM_Aminoglucosidos 
Perfil	No_Aislados	Genotipo_Total
aac(3)-IId,aph(3')-Ia	1	
aadA1	2	
aadA1,aph(4)-Ia,aac(3)-IV	21	
aadA1,aph(4)-Ia,aac(3)-IV,aph(3')-Ia	17	
aadA2	4	
aadA2,aadA2b,aph(4)-Ia,aac(3)-IV	1	
aadA2b	1	
aadA5,aadA7	1	
aph(3'')-Ib	1	
aph(3'')-Ib,aadA1,aph(6)-Id	1	
aph(4)-Ia,aac(3)-IV	4	
aph(4)-Ia,aac(3)-IV,aadA1,aadA2b	1	
aph(6)-Id,aph(3'')-Ib	8	63
Sin_Gen	89	89
Total	152	152

### Tabla_Genotipo_Fenotipo
Perfil	AST_Resistentes	AST_Susceptibles	Genotipo_Total	ID_AST_Resistentes	ID_AST_Susceptibles
aac(3)-IId,aph(3')-Ia	1	0		ID72	NA
aadA1	0	2		NA	ID43,ID87
aph(4)-Ia,aac(3)-IV,aadA1,aadA2b	1	0		ID34	NA
aph(6)-Id,aph(3'')-Ib	1	7	63	ID59	ID97,ID06,ID08,ID22,ID97,ID21,ID25
Total	53	99	152	Especificidad: 89.9, (89)/(99)	Sensibilidad: 118.87, (63)/(53)


### Grafica Multiresistencia

![Multiresistencia](https://user-images.githubusercontent.com/113209694/215827673-a7dd7594-5284-4012-a786-4cbe955aab71.png)





