/**************************************************************************
datos de Sarah
2013 a 2017
***************************************************************************/

options mprint orientation=landscape;


%let program_name=trampeo_allyears.sas;

title "Datos de Sarah";

***0. Macro para importar todos los datos:;
%macro bring(dataout, range);

	proc import out= work.&dataout 
            datafile= "/folders/myfolders/rm_sas/sarah/data/trampeo2013-2017.xlsx" 
            dbms=xlsx replace;
     range="&range"; 
     getnames=yes;
	run;

	proc print data=&dataout(obs=1);
	title3 "&dataout";
	run;

	proc contents;
	run;

%mend bring;

%bring(tr2013A, 2013a$a1:bg17);
%bring(tr2014A, 2014a$a1:aq185);
%bring(tr2015A, 2015a$a1:bf81);
%bring(tr2016A, 2016a$a2:bf310);
%bring(tr2017A, 2017a$a1:bc310);

***1. Poner nombres comunes en todos los anios, por ejemplo, lat sera latitud, 
	_1 sera medida_1, etcetera:;
*VAR1 	latitud 	longitud 	altitud 	Municipio 	Localidad 	Gusano _1 to _52;
data work.tr2013;
set work.tr2013A(rename=(var1=trampa));

**solo convertir unas cuantas observaciones como ejemplo, Sarah hara mas de limpieza de datos:;
medida_1=_1*1; *convertir _1 a variable numerica multiplicando por uno;
medida_2=_2*1;
medida_3=_3*1;

altitudN=altitud*1;
latitudN=latitud*1;
trampaC=put(trampa, 10.); **convertir numerica a character;

run;

*VAR1 	lat 	long 	altura 	Municipio 	T__cnico 	Localidad _1 to _36;
data work.tr2014;
set work.tr2014A(rename=(var1=trampa altura=altitud T__cnico=tecnico));

medida_1=_1*1;
medida_2=_2*1;
medida_3=_3*1;

altitudN=altitud*1;
*latitudN=latitud*1;
trampaC=put(trampa, 10.);

run;

*Nombre_de_Predio 	No__de__________trampa 	Latitud 	Longitud 	Municipio 	Localidad 	Tecnico 	Riego______Temporal _1 to _50;
data work.tr2015;
set work.tr2015A(rename=(No__de__________trampa=trampa));

medida_1=_1*1;
medida_2=_2*1;
medida_3=_3*1;

*altitudN=altitud*1;
latitudN=latitud*1;
trampaC=put(trampa, 10.);

run;

*Trampa_Sicafi 	Referencia 	Latitud 	Longitud 	Municipio 	Localidad 	Riego______Temporal _1 to _51;
data work.tr2016;
set work.tr2016A(rename=(Trampa_Sicafi=trampa Riego______Temporal=riego_temporal));

medida_1=_1*1;
medida_2=_2*1;
medida_3=_3*1;

*altitudN=altitud*1;
latitudN=latitud*1;
trampaC=put(trampa, 18.);

run;

*tra 	lat 	long 	MUNICIPIO 	LOCALIDAD sem2 to sem51;
data work.tr2017;
set work.tr2017A(rename=(tra=trampa lat=latitud long=longitud));

rename sem2-sem51=_2-_51; **renombrar variables para que todas tengan el mismo nombre por todos los anios;

medida_1=sem2*1;**que paso con sem1?
medida_2=sem3*1;
medida_3=sem4*1;

altitudN=altitud*1;
latitudN=latitud*1;
trampaC=put(trampa, 18.);

run;

***3. Poner todos los datos juntos en formato largo (muchos de los modelos usan este formato)
suponemos que el numero de insectos depende del municipio, trampa y anio (Sarah, tu decides cual es tu 
hipotesis u objetivo de investigacion, podria ser comparar numero de insectos por trampa, sin tomar en cuenta el municipio
hay muchas opciones);
data work.all_Long0;

set
tr2013(in=a keep=municipio localidad medida_1 medida_2 medida_3 trampaC)
tr2014(in=b keep=municipio localidad medida_1 medida_2 medida_3 trampaC)
tr2015(in=c keep=municipio localidad medida_1 medida_2 medida_3 trampaC)
tr2016(in=d keep=municipio localidad medida_1 medida_2 medida_3 trampaC)
tr2017(in=e keep=municipio localidad medida_1 medida_2 medida_3 trampaC);

if a then year=2013;
if b then year=2014;
if c then year=2015;
if d then year=2016;
if e then year=2017;
 
medida=median(medida_1, medida_2, medida_3); **esta sera mi variable Y en los modelos, la variable Y la decides tu Sarah;
 
proc sort; by year municipio trampaC; 
run;

**transponer datos:;
proc transpose data=all_Long0(keep=year municipio trampaC medida) 
	out=all_Long1(keep=year municipio trampaC col1);
by year municipio trampaC;
var medida;
run;
data all_Long;
set all_Long1(rename=(col1=medida));

if medida>0 then ln_medida=log(medida);
	else ln_medida=.; **aqui perderiamos varias observaciones en el modelo, no es bueno pero ilustra un problema que tiene solucion;
run;


**crear output de partes importantes:;
ods html file = "/folders/myfolders/rm_sas/sarah/code/results/&program_name &sysdate9..html";

**Imprimir unos renglones para saber como son los datos
observa que municipio y trampa ocurren varias veces en un anio debido a que usariamos informacion de las trampas.
Si las trampas no son importantes, hay que rehacer los datos "sumarizando" numero de polillas por anio por municipio:;
proc print data=all_Long(obs=10);
title3 "3. Entendiendo los datos";
run;
proc freq data=all_Long;
title4 "Varias medidas missing, probablemente se necesita una mejor definicion de medida
(tal vez usar mas de tres medidas, eso te toca Sarah)";
tables year*medida/list missing;
run;
title4;

**4. Modelos;
****4a. Generalized Estimating Equations (GEE) takes into account the dependency of obs by specifying a 
'working correlation structure':
score= beta0+ beta1 x medida+ beta2 x time+ corr+ error;
**este modelo tiene futuro, aunque necesita mas trabajo, por ejemplo, ver como es la distribucion de medida. Si se ve normal
bien;
proc genmod data=all_Long;
title3 "4a. Generalized Estimating Equations (GEE)";
title4 "buen modelo para estudio de poblaciones, muchas medidas pedidas dan malos resultados";
title5 "hay que limpiar mas los datos";
class trampaC municipio; **Municipio necesita limpieza, tal vez se puedan juntar unos cuantos que tengan algo en comun.
de lo contrario, hay que ver opciones para tener un buen modelo;
model medida= year;*time is continuous, not a class!. Not necessary medida*time to have within subject effects;
repeated subject =municipio/ type=exch corrw;*correlation structure is exch, corrw is to view correlations;
run;
quit;
title4; title5;

**4b. Este modelo de Poisson tambien tiene posibilidades y es mas intuitivo, aunque las medidas atraves de los anios 
estan correlacionadas, no son independientes y no considero eso aqui. 
Pero nuevamente, hay que ver como es la districbuion de medida. Y la definicion de medida
es una que puse para probar ideas. Se tomarosn los conteos al mismo tiempo (yo creo que no), 
por que hay mas medidas en unos anios que en otros? Esto desbalancea el disenio del experimento, y GEE or Linear Mixed Models
pareceria la mejor opcion, pero algo complicados estos modelos;
**consultar https://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_genmod_sect006.htm;
proc genmod data=all_Long;
title3 "4b. Regresion de Poisson";
title4 "(buen modelo para cuando Y es conteo distribuida como Poisson)";
title5 "Hay que limpiar mas los datos definiendo mejor la variable Y, analisis no es bueno por el momento";

class municipio year; **Municipio necesita limpieza, tal vez se puedan juntar unos cuantos que tengan algo en comun.
de lo contrario, hay que ver opciones para tener un buen modelo;
model medida=year/dist=poisson link=log offset=ln_medida type1 type3;
run;
title4; title5;

/*************
fin del programa
*****************/

ods html;
ods listing;
   