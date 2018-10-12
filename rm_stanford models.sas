
*https://view.officeapps.live.com/op/view.aspx?src=http://www.pitt.edu/~super4/33011-34001/33151-33161.ppt;

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

*Generalized Estimating Equations (gee) takes into account the dependency of obs by specifying a 
'working correlation structure':
score= beta0+ beta1 [chem1 chem2 chem3 chem4]+ beta2 x time+ corr+ error;
proc genmod data=stanfordL;*GLM (uisng MLE) and genmod;
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
