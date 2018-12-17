######################################################################
#programador: 
#examen 2, diciembre del 2018
######################################################################

library(car)
library(ggplot2)
library(reshape2)

######################################################################

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

				mujeres$DistanciaLog <- log(mujeres$Distancia) #agregando log(Distancia)
				mujeres$TiempoLog <- log(mujeres$Tiempo) #agregando log(Tiempo)



#2a. Para la base de datos hombres, estima la ordenada al origen y la beta
#del modelo en el pdf (adjunto): 

#####################Escribe y ejecuta abajo el codigo del modelo 
#TiempoLog~ DistanciaLog
#de los datos hombres, no olvides ejecutar summary y anova

				hombres.lm <- lm(TiempoLog~ DistanciaLog, data=hombres)
				summary(hombres.lm)
				anova(hombres.lm)

#2b. Para la base de datos mujeres, estima la ordenada al origen y la beta
#del modelo en el pdf (adjunto):

#####################Escribe y ejecuta abajo el codigo del modelo 
#TiempoLog~ DistanciaLog 
#de los datos mujeres, no olvides ejecutar summary y anova

				mujeres.lm <- lm(TiempoLog~ DistanciaLog, data=mujeres)
				summary(mujeres.lm)
				anova(mujeres.lm)



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


				hombresmujeres.lm <- lm(TiempoLog~DistanciaLog+ Sexo+ Sexo:DistanciaLog, data=data.hm)
				summary(hombresmujeres.lm)
				anova(hombresmujeres.lm)

				##Estimacion de tiempos si hombres corrieran 60, mujeres corrieran 200 
				###hay differentes respuestas porque son diferentes modelos. 
				###Compara tus resultados de la opcion que elegiste.
				###(estos calculos se pueden hacer a mano o siguiendo esta parte del programa):
				
				###### 4 opcion I: tiempos estimados por los modelos separados:
				
				#a. hombres corren la distancia 60 usando el modelo de hombres:
				TiempoLog= -2.823196+ 1.112214* log(60)
				exp(TiempoLog)

				#b. mujeres corren la distancia 200 usando el modelo de mujeres:
				TiempoLog= -2.69216+ 1.11167* log(200)
				exp(TiempoLog)
				
				
				###### 4 opcion II: tiempos estimados por los modelos juntos usando datos 
				######concatenados:
				
				#a. hombres corren la distancia 60 usando el modelo de hombres y mujeres 
				#(coeficientes significantes)
				TiempoLog= -2.6921619+ 1.1116747* log(60)
				exp(TiempoLog)

				#b. mujeres corren la distancia 200 usando el modelo de hombres y mujeres 
				#(coeficientes significantes):
				TiempoLog= -2.6921619+ 1.1116747* log(200)
				exp(TiempoLog)
				
				###### 4 opcion III: tiempos estimados por los modelos juntos usando 
				######datos concatenados (hombres es Sexo=1):
				
				#a. hombres corren la distancia 60 usando el modelo de hombres y mujeres 
				#(coeficientes significantes o no)
				TiempoLog= -2.6921619+ 1.1116747* log(60)- 0.1310339*1+ 0.0005397*1
				exp(TiempoLog)

				#b. mujeres corren la distancia 200 usando el modelo de hombres y mujeres 
				#(coeficientes significantes o no):
				TiempoLog= -2.6921619+ 1.1116747* log(200)- 0.1310339*0+ 0.0005397*0
				exp(TiempoLog)

######################################################################
#end of program
######################################################################






