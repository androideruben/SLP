##########################################################################################################################
#programador: 
#examen 2, diciembre del 2018
##########################################################################################################################

library(car)
library(ggplot2)
library(reshape2)

##########################################################################################################################

#1. Lee las dos bases de datos siguientes:
#1a. datos hombres:

Input = ("
Distancia Tiempo 
 100       9.9     
 200      19.8     
 400      43.8     
 800     103.7     
 1000    136.0     
 1500    213.1     
 2000    296.2     
 3000    457.6      
 5000    793.0     
10000   1650.8     
20000   3464.4     
25000   4495.6     
30000   5490.4     
         ")
hombres <- read.table(textConnection(Input), header=TRUE)

hombres$DistanciaLog <- log(hombres$Distancia) #agregando log(Distancia)
hombres$TiempoLog <- log(hombres$Tiempo) #agregando log(Tiempo)

#1b. datos mujeres:

Input = ("
Distancia Tiempo
  60        7.2        
 100       10.8       
 200       22.1       
 400       51.0       
 800      117.0      
1500      241.4      
         ")
mujeres <- read.table(textConnection(Input), header=TRUE)

######## pon abajo tu codigo de los datos mujeres con log(Distancia) y log(Tiempo) agregados
######### y ejecuta esta parte





#2a. Para la base de datos hombres, estima la ordenada al origen y la beta
#del modelo en el pdf (adjunto): 

#####################Escribe y ejecuta abajo el codigo del modelo 
#TiempoLog~ DistanciaLog
#de los datos hombres, no olvides ejecutar summary y anova




#2b. Para la base de datos mujeres, estima la ordenada al origen y la beta
#del modelo en el pdf (adjunto):

#####################Escribe y ejecuta abajo el codigo del modelo 
#TiempoLog~ DistanciaLog 
#de los datos mujeres, no olvides ejecutar summary y anova





#3. Pon dummy variable para sexo, y concatena los datos
#Dummy variable para identificar hombres de mujeres de la siguiente manera:
hombres$Sexo <- 1
mujeres$Sexo <- 0

#ejecuta esta concatenacion: 
data.hm <- as.data.frame(rbind(hombres, mujeres))

#4. Modelo de regresion

#####################Escribe y ejecuta abajo el codigo del modelo 
# TiempoLog~DistanciaLog+ Sexo+ Sexo:DistanciaLog 
#de los datos data.hm no olvides ejecutar summary y anova









##########################################################################################################################
#end of program
##########################################################################################################################






