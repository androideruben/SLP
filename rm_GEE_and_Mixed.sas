%let_program_name=rm_GEE_and_Mixed;
*
https://view.officeapps.live.com/op/view.aspx?src=http://www.pitt.edu/~super4/33011-34001/33151-33161.ppt

http://www.chime.ucla.edu/publications/docs/CHIME_Seminar_Liu_09202010.pdf;


**6 patients with depression are given a drug that increases levels
of a 'happy chemical' in the brain.
Outcome: depression score (measured at baseline, 2, 3, and 6 months).
Predictors: time, chem, gender.;

/*****************************************************************************************
1. data
*****************************************************************************************/

data stanford;
input id Gender time1 time2 time3 time4 chem1 chem2 chem3 chem4
;
datalines;
1 1 20 18 15 20 1000 1100 1200 1300
2 2 22 24 18 22 1000 1000 1005 950
3 1 14 10 24 10 1000 1999 800 1700
4 1 38 34 32 34 1000 1100 1150 1100
5 2 25 29 25 29 1000 1000 1050 1010
6 2 30 28 26 14 1000 1100 1109 1500
;
run;

proc transpose data=stanford out=stanfordL1(rename=col1=happy_score);
by id;
var time:;
proc sort; by id _name_;
run;

proc transpose data=stanford out=stanfordL2(rename=col1= chem);
by id;
var chem:;
proc sort; by id _name_;
run;

proc transpose data=stanford out=stanfordL3(rename=col1= gender);
by id;
var gender:;
proc sort; by id _name_;
run;

data stanfordL(drop=_name_);
merge stanfordL1 stanfordL2(drop=_name_) stanfordL3(drop=_name_);
by id;

if _name_='time1' then time=1;
	else if _name_='time2' then time=2;
	else if _name_='time3' then time=3;
	else if _name_='time4' then time=4;

run;

*also:;
data stanfordL(keep=time happy_score chem gender id);
set stanford;
time=0; happy_score=time1; chem=chem1; output;
time=2; happy_score=time2; chem=chem2; output;
time=3; happy_score=time3; chem=chem3; output;
time=6; happy_score=time4; chem=chem4; output;
run;

/*****************************************************************************************
2. models
*****************************************************************************************/

*repeated measures rANOVA:;
data ranova;
set stanford;
avgchem=(chem1+chem2+chem3+chem4)/4;
if avgchem<1100 then group='low';
if avgchem>=1100 then group='high';
run;
proc glm data=ranova;
class group;
model time1-time4=group/nouni;
repeated time/summary;
run;
quit;

*ignore correlations, naive analysis. N is 24 as if they were idependent:;
proc reg data=stanfordL;
model happy_score= chem time ;
run;
quit;

**ucla notes:;
proc reg data=stanfordL;
title4 "Naive regression using proc reg";
model happy_score= chem time gender;
run;
quit;
proc mixed data=stanfordL;
title4 "1. LMM using proc mixed (UCLA slides 43)";
model happy_score=time chem gender/s;
random intercept/subject=id;*adding random intercept takes care of repeated measurements;
run;
quit;
proc gee data=stanfordL;
title4 "GEE model using proc gee (needs more work)";
class id;
model happy_score=time chem gender/dist=poisson link=log;
repeated subject=id/corr=exch;
run;
quit;
proc genmod data=stanfordL;*GLM (uisng MLE) and genmod;
title4 "GEE model using proc genmod (UCLA slide 43)";
class id;
model happy_score= chem time gender;*time is continuous, not a class!. Not necessary chem*time to have within subject effects;
repeated subject =id/ type=exch corrw;*correlation structure is exch, corrw is to view correlations;
run;
quit;
proc corr data=stanford;
var time:;
run;

*Generalized Estimating Equations (gee) takes into account the dependency of obs by specifying a 
'working correlation structure':
score= beta0+ beta1 [chem1 chem2 chem3 chem4]+ beta2 x time+ corr+ error;
proc genmod data=stanfordL;*GLM (uisng MLE) and genmod;
title4 "GEE model using genmod";
class id;
model happy_score= chem time;*time is continuous, not a class!. Not necessary chem*time to have within subject effects;
repeated subject =id/ type=exch corrw;*correlation structure is exch, corrw is to view correlations;
run;
quit;

proc corr data=stanford;
var time1 time2 time3 time4;
run;

*model with random intercept;
proc mixed data=stanfordL;
model happy_score=time chem/s;
random intercept/subject=id;*adding random intercept takes care of repeated measurements;
run;
quit;

*page 72;
proc mixed data=stanfordL;
model happy_score=time chem/s;
random intercept/subject=id;*adding random intercept takes care of repeated measurements;
run;
quit;

ods pdf file = "\\fda.gov\WODC\CTP_Sandbox\OS\DPHS\StatisticsBranch\Team 2\Montes de Oca\rm_RESEARCH\SciRounds\1. LMMandGEE\code\results\&program_name &sysdate9..pdf";

proc mixed data=stanfordL;
title4 "1. LMM using proc mixed (UCLA slides 43)";
class id;
model happy_score= chem time gender/s;
random intercept/subject=id;*adding random intercept takes care of repeated measurements;
run;
quit;

proc genmod data=stanfordL;*GLM (uisng MLE) and genmod;
title4 "2. GEE model using proc genmod (UCLA slide 46)";
class id;
model happy_score= chem time gender;*time is continuous, not a class!. Not necessary chem*time to have within subject effects;
repeated subject =id/ type=exch corrw;*correlation structure is exch, corrw is to view correlations;
run;
quit;

proc gee data=stanfordL;*GLM (uisng MLE) and genmod;
title4 "2. GEE model using proc gee";
title5 "(https://www.youtube.com/watch?v=Asfa1XnzT7c)";
class id;
model happy_score= chem time gender/dist=nor;
repeated subject =id/type=exch corrw;*correlation structure is exch, corrw is to view correlations;
*missmodel chem;*tests if missing is MAR;
run;
quit;

ods pdf close;

/****************************************************************/ 
/*          S A S   S A M P L E   L I B R A R Y                 */ 
/*                                                              */ 
/*    NAME: GEEEX2                                              */ 
/*   TITLE: Example 2 for PROC GEE                              */ 
/* PRODUCT: STAT                                                */ 
/*  SYSTEM: ALL                                                 */ 
/*    KEYS: Poisson regression                                  */ 
/*   PROCS: GEE                                                 */ 
/*    DATA:                                                     */ 
/*                                                              */ 
/* SUPPORT: guilin                                              */ 
/*     REF: PROC GEE, EXAMPLE 2                                 */ 
/*    MISC:                                                     */ 
/****************************************************************/ 
 
 
data Seizure; 
   input ID Count Visit  Trt Age Weeks; 
   datalines; 
104 11 0 0 31 8 
104 5 1 0 31 2 
104 3 2 0 31 2 
104 3 3 0 31 2 
104 3 4 0 31 2 
106 11 0 0 30 8 
106 3 1 0 30 2 
106 5 2 0 30 2 
106 3 3 0 30 2 
106 3 4 0 30 2 
107 6 0 0 25 8 
107 2 1 0 25 2 
107 4 2 0 25 2 
107 0 3 0 25 2 
107 5 4 0 25 2 
114 8 0 0 36 8 
114 4 1 0 36 2 
114 4 2 0 36 2 
114 1 3 0 36 2 
114 4 4 0 36 2 
116 66 0 0 22 8 
116 7 1 0 22 2 
116 18 2 0 22 2 
116 9 3 0 22 2 
116 21 4 0 22 2 
118 27 0 0 29 8 
118 5 1 0 29 2 
118 2 2 0 29 2 
118 8 3 0 29 2 
118 7 4 0 29 2 
123 12 0 0 31 8 
123 6 1 0 31 2 
123 4 2 0 31 2 
123 0 3 0 31 2 
123 2 4 0 31 2 
126 52 0 0 42 8 
126 40 1 0 42 2 
126 20 2 0 42 2 
126 23 3 0 42 2 
126 12 4 0 42 2 
130 23 0 0 37 8 
130 5 1 0 37 2 
130 6 2 0 37 2 
130 6 3 0 37 2 
130 5 4 0 37 2 
135 10 0 0 28 8 
135 14 1 0 28 2 
135 13 2 0 28 2 
135 6 3 0 28 2 
135 0 4 0 28 2 
141 52 0 0 36 8 
141 26 1 0 36 2 
141 12 2 0 36 2 
141 6 3 0 36 2 
141 22 4 0 36 2 
145 33 0 0 24 8 
145 12 1 0 24 2 
145 6 2 0 24 2 
145 8 3 0 24 2 
145 4 4 0 24 2 
201 18 0 0 23 8 
201 4 1 0 23 2 
201 4 2 0 23 2 
201 6 3 0 23 2 
201 2 4 0 23 2 
202 42 0 0 36 8 
202 7 1 0 36 2 
202 9 2 0 36 2 
202 12 3 0 36 2 
202 14 4 0 36 2 
205 87 0 0 26 8 
205 16 1 0 26 2 
205 24 2 0 26 2 
205 10 3 0 26 2 
205 9 4 0 26 2 
206 50 0 0 26 8 
206 11 1 0 26 2 
206 0 2 0 26 2 
206 0 3 0 26 2 
206 5 4 0 26 2 
210 18 0 0 28 8 
210 0 1 0 28 2 
210 0 2 0 28 2 
210 3 3 0 28 2 
210 3 4 0 28 2 
213 111 0 0 31 8 
213 37 1 0 31 2 
213 29 2 0 31 2 
213 28 3 0 31 2 
213 29 4 0 31 2 
215 18 0 0 32 8 
215 3 1 0 32 2 
215 5 2 0 32 2 
215 2 3 0 32 2 
215 5 4 0 32 2 
217 20 0 0 21 8 
217 3 1 0 21 2 
217 0 2 0 21 2 
217 6 3 0 21 2 
217 7 4 0 21 2 
219 12 0 0 29 8 
219 3 1 0 29 2 
219 4 2 0 29 2 
219 3 3 0 29 2 
219 4 4 0 29 2 
220 9 0 0 21 8 
220 3 1 0 21 2 
220 4 2 0 21 2 
220 3 3 0 21 2 
220 4 4 0 21 2 
222 17 0 0 32 8 
222 2 1 0 32 2 
222 3 2 0 32 2 
222 3 3 0 32 2 
222 5 4 0 32 2 
226 28 0 0 25 8 
226 8 1 0 25 2 
226 12 2 0 25 2 
226 2 3 0 25 2 
226 8 4 0 25 2 
227 55 0 0 30 8 
227 18 1 0 30 2 
227 24 2 0 30 2 
227 76 3 0 30 2 
227 25 4 0 30 2 
230 9 0 0 40 8 
230 2 1 0 40 2 
230 1 2 0 40 2 
230 2 3 0 40 2 
230 1 4 0 40 2 
234 10 0 0 19 8 
234 3 1 0 19 2 
234 1 2 0 19 2 
234 4 3 0 19 2 
234 2 4 0 19 2 
238 47 0 0 22 8 
238 13 1 0 22 2 
238 15 2 0 22 2 
238 13 3 0 22 2 
238 12 4 0 22 2 
101 76 0 1 18 8 
101 11 1 1 18 2 
101 14 2 1 18 2 
101 9 3 1 18 2 
101 8 4 1 18 2 
102 38 0 1 32 8 
102 8 1 1 32 2 
102 7 2 1 32 2 
102 9 3 1 32 2 
102 4 4 1 32 2 
103 19 0 1 20 8 
103 0 1 1 20 2 
103 4 2 1 20 2 
103 3 3 1 20 2 
103 0 4 1 20 2 
108 10 0 1 30 8 
108 3 1 1 30 2 
108 6 2 1 30 2 
108 1 3 1 30 2 
108 3 4 1 30 2 
110 19 0 1 18 8 
110 2 1 1 18 2 
110 6 2 1 18 2 
110 7 3 1 18 2 
110 4 4 1 18 2 
111 24 0 1 24 8 
111 4 1 1 24 2 
111 3 2 1 24 2 
111 1 3 1 24 2 
111 3 4 1 24 2 
112 31 0 1 30 8 
112 22 1 1 30 2 
112 17 2 1 30 2 
112 19 3 1 30 2 
112 16 4 1 30 2 
113 14 0 1 35 8 
113 5 1 1 35 2 
113 4 2 1 35 2 
113 7 3 1 35 2 
113 4 4 1 35 2 
117 11 0 1 27 8 
117 2 1 1 27 2 
117 4 2 1 27 2 
117 0 3 1 27 2 
117 4 4 1 27 2 
121 67 0 1 20 8 
121 3 1 1 20 2 
121 7 2 1 20 2 
121 7 3 1 20 2 
121 7 4 1 20 2 
122 41 0 1 22 8 
122 4 1 1 22 2 
122 18 2 1 22 2 
122 2 3 1 22 2 
122 5 4 1 22 2 
124 7 0 1 28 8 
124 2 1 1 28 2 
124 1 2 1 28 2 
124 1 3 1 28 2 
124 0 4 1 28 2 
128 22 0 1 23 8 
128 0 1 1 23 2 
128 2 2 1 23 2 
128 4 3 1 23 2 
128 0 4 1 23 2 
129 13 0 1 40 8 
129 5 1 1 40 2 
129 4 2 1 40 2 
129 0 3 1 40 2 
129 3 4 1 40 2 
137 46 0 1 33 8 
137 11 1 1 33 2 
137 14 2 1 33 2 
137 25 3 1 33 2 
137 15 4 1 33 2 
139 36 0 1 21 8 
139 10 1 1 21 2 
139 5 2 1 21 2 
139 3 3 1 21 2 
139 8 4 1 21 2 
143 38 0 1 35 8 
143 19 1 1 35 2 
143 7 2 1 35 2 
143 6 3 1 35 2 
143 7 4 1 35 2 
147 7 0 1 25 8 
147 1 1 1 25 2 
147 1 2 1 25 2 
147 2 3 1 25 2 
147 3 4 1 25 2 
203 36 0 1 26 8 
203 6 1 1 26 2 
203 10 2 1 26 2 
203 8 3 1 26 2 
203 8 4 1 26 2 
204 11 0 1 25 8 
204 2 1 1 25 2 
204 1 2 1 25 2 
204 0 3 1 25 2 
204 0 4 1 25 2 
207 151 0 1 22 8 
207 102 1 1 22 2 
207 65 2 1 22 2 
207 72 3 1 22 2 
207 63 4 1 22 2 
208 22 0 1 32 8 
208 4 1 1 32 2 
208 3 2 1 32 2 
208 2 3 1 32 2 
208 4 4 1 32 2 
209 41 0 1 25 8 
209 8 1 1 25 2 
209 6 2 1 25 2 
209 5 3 1 25 2 
209 7 4 1 25 2 
211 32 0 1 35 8 
211 1 1 1 35 2 
211 3 2 1 35 2 
211 1 3 1 35 2 
211 5 4 1 35 2 
214 56 0 1 21 8 
214 18 1 1 21 2 
214 11 2 1 21 2 
214 28 3 1 21 2 
214 13 4 1 21 2 
218 24 0 1 41 8 
218 6 1 1 41 2 
218 3 2 1 41 2 
218 4 3 1 41 2 
218 0 4 1 41 2 
221 16 0 1 32 8 
221 3 1 1 32 2 
221 5 2 1 32 2 
221 4 3 1 32 2 
221 3 4 1 32 2 
225 22 0 1 26 8 
225 1 1 1 26 2 
225 23 2 1 26 2 
225 19 3 1 26 2 
225 8 4 1 26 2 
228 25 0 1 21 8 
228 2 1 1 21 2 
228 3 2 1 21 2 
228 0 3 1 21 2 
228 1 4 1 21 2 
232 13 0 1 36 8 
232 0 1 1 36 2 
232 0 2 1 36 2 
232 0 3 1 36 2 
232 0 4 1 36 2 
236 12 0 1 37 8 
236 1 1 1 37 2 
236 4 2 1 37 2 
236 3 3 1 37 2 
236 2 4 1 37 2 
; 
 
data Seizure; 
   set Seizure; 
   if ID ne 207; 
   if Visit = 0 then do; 
      X1=0; 
      Ltime = log(8); 
   end; 
   else do; 
      X1=1; 
      Ltime=log(2); 
   end; 
run; 
 
**https://blogs.sas.com/content/iml/2012/04/04/fitting-a-poisson-distribution-to-data-in-sas.html;
proc freq data=seizure;
tables count/out=FreqOut plots=FreqPlot(scale=percent);
run;
/* 1. Estimate the rate parameter with PROC GENMOD: http://support.sas.com/kb/24/166.html */
proc genmod data=seizure;
   model count = / dist=poisson;
   output out=PoissonFit p=lambda;
run;
/* 2. Compute Poisson density for estimated parameter value */
/* 2.1 Create macro variable with parameter estimate */ 
data _null_;
set PoissonFit;
call symputx("Lambda", Lambda);
stop;
run;
 
/* 2.2 Use PDF function for range of x values */
data PMF;
do t = 0 to 111; /* 0 to max(x) */
   Y = pdf("Poisson", t, &Lambda);
   output;
end;
run;
/* 3. Use bar chart to plot data. To overlay a bar chart and 
      scatter plot, use the VBARPARM stmt instead of VBAR. */
data Discrete;
merge FreqOut PMF;
Prop = Percent / 100; /* convert to same scale as PDF */
run;
 
/* 3.2 Overlay VBARPARM and scatter plot of (x, pdf(x)) */
proc sgplot data=Discrete; /* VBARPARM is SAS 9.3 stmt */
   vbarparm category=count response=Prop / legendlabel='Sample';
   scatter x=T y=Y / legendlabel='PMF'
      markerattrs=GraphDataDefault(symbol=CIRCLEFILLED size=10);
   title "Emails per 30-Minute Period and Poisson Distribution";
run;
/* 4. Create a Q-Q plot */
/* 4.1 Compute theoretical quantiles */
proc sort data=seizure; by count; run;    /* 1 */
data QQ;
set seizure nobs=nobs;
v = (_N_ - 0.375) / (nobs + 0.25);   /* 2 */
q = quantile("Poisson", v, &Lambda); /* 3 */
run;
 
proc sgplot data=QQ noautolegend;    /* 4 */
scatter x=q y=count;
lineparm x=0 y=0 slope=1; /* SAS 9.3 statement */
xaxis label="Poisson Quantiles" grid; 
yaxis label="Observed Data" grid;
title "Poisson Q-Q Plot of seizure count";
run;

proc genmod data=seizure;
model count= / dist=poisson;
   output out=PoissonFit p=lambda;
run;
proc genmod data=seizure;
model Count = X1 Trt X1*Trt  / dist=poisson;
output out=PoissonFit p=lambda;
run;
proc gee data=Seizure; 
class ID Visit; 
model Count = X1 Trt X1*Trt / dist=poisson link=log offset= Ltime; 
repeated subject=ID/within=Visit type=unstr covb corrw; 
run; 

