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






*2a. Para la base de datos hombres, estima la ordenada al origen y la beta
del modelo en el pdf (adjunto):;

*********************Escribe y ejecuta abajo el codigo del modelo 
TiempoLog= DistanciaLog 
*de los datos hombres;







*2b. Para la base de datos mujeres, estima la ordenada al origen y la beta
del modelo en el pdf (adjunto):;

*********************Escribe y ejecuta abajo el codigo del modelo 
TiempoLog= DistanciaLog 
*de los datos mujeres;







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

quit;

ods pdf close;

**************************************************************************************************************************
*end of program
**************************************************************************************************************************






