/**************************************************************************
datos de Sarah
2013 a 2017
***************************************************************************/


%macro bring(dataout, range);

proc import out= work.&dataout 
            datafile= "H:\Ruben.MontesdeOca\Documents\trampeo2013-2017.xlsx" 
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
proc transpose data=all_Long0(keep=year municipio trampaC medida) 
	out=all_Long1(keep=year municipio trampaC col1);
by year municipio trampaC;
var medida;
run;
data all_Long;
set all_Long1(rename=(col1=medida));
run;

ods pdf file = "H:\Ruben.MontesdeOca\Documents\&program_name &sysdate9..pdf";

proc freq data=all_Long;
title4 "0. Too many missing (model accepts missin observations, but these are too many)";
tables medida*year* municipio* trampaC/list missing;
run;

*too many missing, needs more data cleaning by defining a better medida, this is a shortcut that needs more work:;
data work.all_Long;
set work.all_Long;
if medida ne . and municipio ne ''; *temporary solution: model accepts missin observations, but these are too many;
run;

*1. See if medida distribution is Poisson:;
proc freq data=all_Long;
title4 "1. See if medida distribution is Poisson (it does)";
tables medida/out=FreqOut plots=FreqPlot(scale=percent);
run;

*2. Estimate the rate parameter lambda for experimental data:;
proc genmod data=all_Long;
title4 "2. Estimate the rate parameter lambda for experimental data";
   model medida=/dist=poisson;
   output out=PoissonFit p=lambda;
run;

*3. Compute theoretic &Lambda from a Poisson density:;
data _null_;
title4 "3. Compute theoretic &Lambda from a Poisson density";
set PoissonFit;
call symputx("Lambda", Lambda);
stop;
run;

*The PDF function for range of x values;
data PDF;
do t = 0 to 355; * 0 to max(x);
   Y = pdf("Poisson", t, &Lambda);
   output;
end;
run;

*4. Use bar chart to plot data. To overlay a bar chart and scatter plot, use the VBARPARM stmt instead of VBAR;
data Discrete;
merge FreqOut PDF;
Prop = Percent / 100; *convert to same scale as PDF;
run;
 
*5. Overlay experimental data and theoretical Poisson;
proc sgplot data=Discrete; * VBARPARM is SAS 9.3 stmt;
where medida ne .;
   vbarparm category=medida response=Prop / legendlabel='Sample';
   scatter x=T y=Y / legendlabel='PMF'
      markerattrs=GraphDataDefault(symbol=CIRCLEFILLED size=10);
   title "Poisson Distribution (mas o menos)";
run;

*Generalized Estimating Equations (gee) takes into account the dependency of obs by specifying a 
'working correlation structure':
score= beta0+ beta1 [chem1 chem2 chem3 chem4]+ beta2 x time+ corr+ error;
proc genmod data=all_Long;*GLM (uisng MLE) and genmod;
class trampaC municipio;
model medida= year;*time is continuous, not a class!. Not necessary chem*time to have within subject effects;
repeated subject=municipio/type=exch corrw;*correlation structure is exch, corrw is to view correlations;
run;
quit;

proc genmod data=all_Long;*GLM (uisng MLE) and genmod;
where trampaC ne '';
class trampaC municipio;
model medida= year;*time is continuous, not a class!. Not necessary chem*time to have within subject effects;
repeated subject=trampaC/type=exch corrw;*correlation structure is exch, corrw is to view correlations;
run;
quit;

ods pdf close;
