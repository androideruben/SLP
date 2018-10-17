/**************************************************************************
datos de Sarah
2013 a 2017
***************************************************************************/


%macro bring(dataout, range);

proc import out= work.&dataout 
            datafile= "/folders/myfolders/rm_sas/sarah/data/trampeo2013-2017.xlsx" 
            dbms=xlsx replace;
     range="&range"; 
     getnames=yes;
run;

proc print data=&dataout(obs=1);
title "&dataout";
run;

proc contents;
run;

%mend bring;

%bring(tr2013A, 2013a$a1:bg17);
%bring(tr2014A, 2014a$a1:aq185);
%bring(tr2015A, 2015a$a1:bf81);
%bring(tr2016A, 2016a$a2:bf310);
%bring(tr2017A, 2017a$a1:bc310);

*VAR1 	latitud 	longitud 	altitud 	Municipio 	Localidad 	Gusano _1 to _52;
data work.tr2013;
set work.tr2013A(rename=(var1=trampa));

medida_1=_1*1;
medida_2=_2*1;
medida_3=_3*1;

altitudN=altitud*1;
latitudN=latitud*1;
trampaC=put(trampa, 10.);

run;


*VAR1 	lat 	long 	altura 	Municipio 	T__cnico 	Localidad _1 to _36;
data work.tr2014;
set work.tr2014A(rename=(var1=trampa altura=altitud T__cnico=tecnico));

medida_1=_1*1;
medida_2=_2*1;
medida_3=_3*1;

altitudN=altitud*1;
latitudN=latitud*1;
trampaC=put(trampa, 10.);

run;


*Nombre_de_Predio 	No__de__________trampa 	Latitud 	Longitud 	Municipio 	Localidad 	Tecnico 	Riego______Temporal _1 to _50;
data work.tr2015;
set work.tr2015A(rename=(No__de__________trampa=trampa));

medida_1=_1*1;
medida_2=_2*1;
medida_3=_3*1;

altitudN=altitud*1;
latitudN=latitud*1;
trampaC=put(trampa, 10.);

run;

*Trampa_Sicafi 	Referencia 	Latitud 	Longitud 	Municipio 	Localidad 	Riego______Temporal _1 to _51;
data work.tr2016;
set work.tr2016A(rename=(Trampa_Sicafi=trampa Riego______Temporal=riego_temporal));

medida_1=_1*1;
medida_2=_2*1;
medida_3=_3*1;

altitudN=altitud*1;
latitudN=latitud*1;
trampaC=put(trampa, 18.);

run;

*tra 	lat 	long 	MUNICIPIO 	LOCALIDAD sem2 to sem51;
data work.tr2017;
set work.tr2017A(rename=(tra=trampa lat=latitud long=longitud));

rename sem2-sem51=_2-_51;

medida_1=_1*1;
medida_2=_2*1;
medida_3=_3*1;

altitudN=altitud*1;
latitudN=latitud*1;
trampaC=put(trampa, 18.);

run;

data work.all_Long0;

set
tr2013(in=a keep=municipio localidad medida_1 medida_2 medida_3 altitudN latitudN trampaC)
tr2014(in=b keep=municipio localidad medida_1 medida_2 medida_3 altitudN latitudN trampaC)
tr2015(in=c keep=municipio localidad medida_1 medida_2 medida_3 altitudN latitudN trampaC)
tr2016(in=d keep=municipio localidad medida_1 medida_2 medida_3 altitudN latitudN trampaC)
tr2017(in=e keep=municipio localidad medida_1 medida_2 medida_3 altitudN latitudN trampaC);

if a then year=2013;
if b then year=2014;
if c then year=2015;
if d then year=2016;
if e then year=2017;
 
medida=median(medida_1, medida_2, medida_3); 
 
proc sort; by year municipio trampaC; 
run;

proc transpose data=all_Long0(keep=year municipio trampaC medida) 
	out=all_Long1(keep=year municipio trampaC col1);
by year municipio trampaC;
var medida;
run;
data all__Long;
set all_Long1(rename=(col1=medida));
run;
proc contents data=all_Long; run;
proc print data=all_long;
where year=2013;
run;

*Generalized Estimating Equations (gee) takes into account the dependency of obs by specifying a 
'working correlation structure':
score= beta0+ beta1 [chem1 chem2 chem3 chem4]+ beta2 x time+ corr+ error;
proc genmod data=all_Long;*GLM (uisng MLE) and genmod;
class trampaC municipio;
model medida= year;*time is continuous, not a class!. Not necessary chem*time to have within subject effects;
repeated subject =municipio/ type=exch corrw;*correlation structure is exch, corrw is to view correlations;
run;
quit;