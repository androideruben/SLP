/**************************************************************************
datos de Sarah
trampeo 2013 a 2017
***************************************************************************/

title "Trampeo por municipio y year";
title2 "Datos Sarah";
title3;

%macro bring(dataout, range);

proc import out= work.&dataout 
            datafile= "H:\Ruben.MontesdeOca\Documents\trampeo2013-2017.xlsx" 
            dbms=xlsx replace;
     range="&range"; 
     getnames=yes;
run;

proc print data=&dataout(obs=1);
title4 "&dataout";
run;

data work.&dataout;
set work.&dataout;

	*this array groups all the character variables together into one array;
  	array vars(*) _character_;                                                                                                            
  	do i=1 to dim(vars); 
                                                                                                                 
	*use the UPCASE function to uppercase each value;                                                                                     
    vars(i)=upcase(vars(i));                                                                                                            
  	end;                                                                                                                                  
drop i; 

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

medida_2=_2*1;
medida_3=_3*1;
medida_4=_4*1;

altitudN=altitud*1;
latitudN=latitud*1;
trampaC=put(trampa, 10.);

run;

*VAR1 	lat 	long 	altura 	Municipio 	T__cnico 	Localidad _1 to _36;
data work.tr2014;
set work.tr2014A(rename=(var1=trampa altura=altitud T_cnico=tecnico));

medida_2=_2*1;
medida_3=_3*1;
medida_4=_4*1;

altitudN=altitud*1;
latitudN=latitud*1;
trampaC=put(trampa, 10.);

run;

*Nombre_de_Predio 	No__de__________trampa 	Latitud 	Longitud 	Municipio 	Localidad 	Tecnico 	Riego______Temporal _1 to _50;
data work.tr2015;
set work.tr2015A(rename=(No__de__________trampa=trampa));

medida_2=_2*1;
medida_3=_3*1;
medida_4=_4*1;

*altitudN=altitud*1;
latitudN=latitud*1;
trampaC=put(trampa, 10.);

run;

*Trampa_Sicafi 	Referencia 	Latitud 	Longitud 	Municipio 	Localidad 	Riego______Temporal _1 to _51;
data work.tr2016;
set work.tr2016A(rename=(Trampa_Sicafi=trampa Riego______Temporal=riego_temporal));

medida_2=_2*1;
medida_3=_3*1;
medida_4=_4*1;

*altitudN=altitud*1;
latitudN=latitud*1;
trampaC=put(trampa, 18.);

run;

*tra 	lat 	long 	MUNICIPIO 	LOCALIDAD sem2 to sem51;
data work.tr2017;
set work.tr2017A(rename=(tra=trampa lat=latitud long=longitud));

rename sem2-sem51=_2-_51;

medida_2=sem2*1;
medida_3=sem3*1;
medida_4=sem4*1;

*altitudN=altitud*1;
latitudN=latitud*1;
trampaC=put(trampa, 18.);

run;

data work.all_Long0;

set
tr2013(in=a keep=municipio localidad medida_2 medida_3 medida_4 latitudN trampaC)
tr2014(in=b keep=municipio localidad medida_2 medida_3 medida_4 latitudN trampaC)
tr2015(in=c keep=municipio localidad medida_2 medida_3 medida_4 latitudN trampaC)
tr2016(in=d keep=municipio localidad medida_2 medida_3 medida_4 latitudN trampaC)
tr2017(in=e keep=municipio localidad medida_2 medida_3 medida_4 latitudN trampaC);

if a then year=2013;
if b then year=2014;
if c then year=2015;
if d then year=2016;
if e then year=2017;
 
medida=max(medida_2, medida_3, medida_4); *I am trying to make sense of so many medidas over apparently time;
 
proc sort; by year municipio trampaC; 
run;
proc transpose data=work.all_Long0(keep=year municipio trampaC medida) 
	out=work.all_Long1(keep=year municipio trampaC col1);
by year municipio trampaC;
var medida;
run;
data work.all_Long;
set work.all_Long1(rename=(col1=medida));
run;

ods pdf file = "H:\Ruben.MontesdeOca\Documents\&program_name &sysdate9..pdf";

proc freq data=work.all_Long;
title4 "0. Too many missing in collected data (model accepts missing observations, but these are too many)";
tables medida municipio trampaC/list missing;
run;

*too many missing, needs more data cleaning by defining a better medida, this is a shortcut that needs more work:;
data work.all_Long;
set work.all_Long;
if medida ne . & municipio ne '' & trampaC ne ''; *temporary solution: model accepts missin observations, but these are too many;
run;

*1. See if medida distribution is Poisson:;
proc freq data=work.all_Long;
title4 "1. See if medida from collected data is Poisson (it is, using temporary data)";
tables medida/out=FreqOut plots=FreqPlot(scale=percent);
run;

*2. Estimate the rate parameter lambda for experimental data:;
proc genmod data=work.all_Long;
title4 "2. Estimate the lambda parameter for collected data";
model medida=/dist=poisson;
output out=PoissonFit p=lambda;
run;

*3. Compute theoretic &Lambda from a Poisson distribution:;
data _null_;
title4 "3. Compute theoretic &Lambda from a Poisson distribution";
set PoissonFit;
call symputx("Lambda", Lambda);
stop;
run;

*The PDF function for range of x values;
proc means max data=work.all_Long noprint;
var medida;
output out=work.maximum max(medida)=max_medida;
run;
proc print noobs data=work.maximum;
title4 "3(cont'd). Max of medidas";
var max_medida;
run;

*probability density function;
data work.PDF;
do t = 0 to 38; * 0 to max(x);
   Y = pdf("Poisson", t, &Lambda);
   output;
end;
run;

*4. Use bar chart to plot data. To overlay a bar chart and scatter plot, use the VBARPARM stmt instead of VBAR;
data Discrete;
merge FreqOut PDF;
Prop = Percent / 100; *convert to same scale as PDF;
run;
 
*5. Plot experimental data and theoretical Poisson;
proc sgplot data=Discrete;
title4 "5. Plot experimental data and theoretical Poisson";
   vbarparm category=medida response=Prop/legendlabel='Datos Sarah';
   scatter x=T y=Y / legendlabel='Poisson'
      markerattrs=GraphDataDefault(symbol=CIRCLEFILLED size=10);
run;

*6. Two types of models for repeated measurements, thus, correlated 
also known as longitudinal data over the years.
A. Generalized Estimating Equations (GEE) and B. Linear Mixed Models (LMM);

**Both give similar estimates;
proc genmod data=work.all_Long;*GLM (uisng MLE) and genmod;
title4 "6a. GEE";
class trampaC municipio;
model medida= year;*time is continuous, not a class!. Not necessary chem*time to have within subject effects;
repeated subject=municipio/type=exch corrw;*correlation structure is exch, corrw is to view correlations;
run;
proc mixed data=work.all_Long;
title4 "6b. LMM";
class trampaC municipio;
model medida= year/solution;
random intercept/subject=municipio;*adding random intercept takes care of repeated measurements;
run;

/*
proc glimmix data=work.all_Long;
title4 "6c. LMM with proc glimmix";
class trampaC municipio;
model medida= year/dist=normal;
random intercept/subject=municipio;
run;
*/

*7. GEE and LMM:;
proc genmod data=work.all_Long;
title4 "7a. GEE (too many trampas)";
class trampaC municipio;
model medida= year municipio;
repeated subject=trampaC/type=exch corrw;
run;
proc mixed data=work.all_Long;
title4 "7b. LMM (too many trampas)";
class trampaC municipio;
model medida= year municipio/solution;
random intercept/subject=trampaC;*adding random intercept takes care of repeated measurements;
run;

**7. Best model is that coefficients are really zero statistically:;
*https://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_genmod_sect006.htm;
proc genmod data=work.all_Long;*GLM (uisng MLE) and genmod;
title4 "7c. GEE for Poisson (This is a good model):";
class trampaC municipio;
model medida= year/dist=poisson link=log;
repeated subject=municipio/type=exch corrw;*correlation structure is exch, corrw is to view correlations;
run;

ods pdf close;
