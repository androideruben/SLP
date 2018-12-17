/**************************************************************************************************************************
*programador: 
*examen 2, diciembre del 2018
**************************************************************************************************************************/

%let program_name=examen2;

ods pdf file = "C:\Users\Ruben.MontesdeOca\Documents\ruben\&program_name &sysdate9..pdf";


*1. Lee las dos bases de datos siguientes:;
*1a. datos hombres:;
data work.hombres;
input Distancia Tiempo;
cards;
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
;
run;

data work.hombres;
set work.hombres;

DistanciaLog=log(Distancia); *agregando log(Distancia) a los datos;
TiempoLog=log(Tiempo); *agregando log(Tiempo) a los datos;

run;

*1b. datos mujeres:;

data work.mujeres;
input Distancia Tiempo;
cards;
  60        7.2        
 100       10.8       
 200       22.1       
 400       51.0       
 800      117.0      
1500      241.4      
;
run;

******* pon abajo tu codigo de los datos mujeres con log(Distancia) y log(Tiempo) agregados
y ejecuta esta parte (ve como se le hizo para los datos hombres;


				data work.mujeres;
				set work.mujeres;

				DistanciaLog=log(Distancia); *agregando log(Distancia) a los datos;
				TiempoLog=log(Tiempo); *agregando log(Tiempo) a los datos;

				run;



*2a. Para la base de datos hombres, estima la ordenada al origen y la beta
del modelo en el pdf (adjunto):;

*********************Escribe y ejecuta abajo el codigo del modelo 
TiempoLog= DistanciaLog 
*de los datos hombres;


				proc reg data=work.hombres;
				title "proc reg TiempoLog= DistanciaLog";
				title2 "(work.hombres)";
				model TiempoLog= DistanciaLog;
				run;




*2b. Para la base de datos mujeres, estima la ordenada al origen y la beta
del modelo en el pdf (adjunto):;

*********************Escribe y ejecuta abajo el codigo del modelo 
TiempoLog= DistanciaLog 
*de los datos mujeres;


				proc reg data=work.mujeres;
				title "proc reg TiempoLog= DistanciaLog";
				title2 "(work.mujeres)";
				model TiempoLog= DistanciaLog;
				run;





*3. Pon dummy variable para sexo, y concatena los datos
Dummy variable para identificar hombres de mujeres:;

*Simplemente ejecuta esto:;
data work.hombres;
set work.hombres;
Sexo=1; *dummy variable para sexo;
run;

*y esto:;
data work.mujeres;
set work.mujeres;
Sexo=0; *dummy variable para sexo;
run;

*ahora ejecuta esta concatenacion:;
data work.hm;
set work.hombres work.mujeres;
SexoDistanciaLog=Sexo*DistanciaLog; *agregando interaccion;
run;

*4. Modelo de regresion:;

*********************Escribe y ejecuta abajo el codigo del modelo 
TiempoLog=DistanciaLog+ Sexo+ Sexo*DistanciaLog de los datos work.hm;

				proc reg data=work.hm;
				title "proc reg TiempoLog=DistanciaLog Sexo SexoDistanciaLog";
				title2 "(work.hm)";
				model TiempoLog=DistanciaLog Sexo SexoDistanciaLog;
				run;

quit;
title; title2;

**Estimacion de tiempos si hombres corrieran 60, mujeres corrieran 200 
hay differentes respuestas porque son diferentes modelos. Compara tus resultados de la opcion que elegiste.
(estos calculos se pueden hacer a mano o siguiendo esta parte del programa):;

****** 4 opcion I: tiempos estimados por los modelos separados:;
data work.calculos;
format TiempoLog Tiempo 10.7;

				*a. hombres corren la distancia 60 usando el modelo de hombres:;
				TiempoLogH= -2.823196 +1.112214* log(60);
				TiempoH=exp(TiempoLogH);

				*b. mujeres corren la distancia 200 usando el modelo de mujeres:;
				TiempoLogM= -2.69216 +1.11167* log(200);
				TiempoM=exp(TiempoLogM);

put	TiempoLog:;
put Tiempo:;
run;
proc print label noobs data=work.calculos;
format Tiempo: 10.7;

title "4 opcion I: tiempos estimados por los modelos separados";
var TiempoH TiempoM;
label 
TiempoH="Tiempo Hombres (Distancia=60)" 
TiempoM="Tiempo Mujeres (Distancia=200)";
run;

****** 4 opcion II: tiempos estimados por los modelos juntos usando datos concatenados:;
data work.calculos;
format TiempoLog Tiempo 10.7;
				
				*a. hombres corren la distancia 60 usando el modelo de hombres y mujeres (coeficientes significantes);
				TiempoLogH= -2.6921619 +1.1116747* log(60);
				TiempoH=exp(TiempoLogH);

				*b. mujeres corren la distancia 200 usando el modelo de hombres y mujeres (coeficientes significantes):;
				TiempoLogM= -2.6921619 +1.1116747* log(200);
				TiempoM=exp(TiempoLogM);
				
put	TiempoLog:;
put Tiempo:;
run;
proc print label noobs data=work.calculos;
format Tiempo: 10.7;

title "4 opcion II: tiempos estimados por los modelos juntos usando datos concatenados";
title2 "(usando solo coeficientes significativos)";
var TiempoH TiempoM;
label 
TiempoH="Tiempo Hombres (Distancia=60)" 
TiempoM="Tiempo Mujeres (Distancia=200)";
run;

				
****** 4 opcion III: tiempos estimados por los modelos juntos usando datos concatenados (hombres es Sexo=1):;
data work.calculos;
format TiempoLog Tiempo 10.7;
				
				*a. hombres corren la distancia 60 usando el modelo de hombres y mujeres (coeficientes significantes);
				TiempoLogH= -2.6921619 +1.1116747* log(60) -0.1310339*1 +0.0005397*1;
				TiempoH=exp(TiempoLogH);

				*b. mujeres corren la distancia 200 usando el modelo de hombres y mujeres (coeficientes significantes):;
				TiempoLogM= -2.6921619 +1.1116747* log(200) -0.1310339*0 +0.0005397*0;
				TiempoM=exp(TiempoLogM);
				
put	TiempoLog:;
put Tiempo:;
run;
proc print label noobs data=work.calculos;
format Tiempo: 10.7;

title "4 opcion III: tiempos estimados por los modelos juntos usando datos concatenados (hombres es Sexo=1)";
title2 "(usando coeficientes significativos o no)";
var TiempoH TiempoM;
label 
TiempoH="Tiempo Hombres (Distancia=60)" 
TiempoM="Tiempo Mujeres (Distancia=200)";
run;

ods pdf close;

**************************************************************************************************************************
*end of program
**************************************************************************************************************************






