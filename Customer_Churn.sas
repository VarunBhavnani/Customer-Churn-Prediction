proc import file="E:\Users\vnb210000\Desktop\SAS data set\Assignment 5\Churn_telecom_new.csv"
out=churn dbms=csv;run;

proc print data = churn (obs = 10);run;

/*Creating separate datasets for Churn and Non-Churn*/
data WORK.churn_y(drop = Customer_ID);set churn;
if Churn= 1;run;
proc print data = WORK.churn_y (obs = 10);run;


data WORK.churn_n(drop = Customer_ID);set churn;
if Churn = 0;run;
proc print data = WORK.churn_n (obs = 10);run;

/*Calculating the Means*/

proc means data=WORK.churn_y mean;
output out = WORK.churn_ym mean= /autoname;
run;

proc means data=WORK.churn_n mean;
output out = WORK.churn_nm mean= /autoname;
run;

data WORK.churn_meanDiff (drop = _TYPE_ _FREQ_);
set WORK.churn_ym WORK.churn_nm;
run;

proc print data = WORK.churn_meanDiff;run;

proc transpose data = WORK.churn_meanDiff out=WORK.chMean_trans;run;

proc print data = WORK.chMean_trans;run;

data WORK.perChange;set WORK.chMean_trans(rename =(COL1 = yes COL2 = no));
perChng = abs(((yes - no)/no)*100);run;

proc print data = WORK.perChange;run;

proc sort data=WORK.perChange out= chDiff_sorted;by descending perChng;run;
proc print data= chDiff_sorted;run;

/*The top 10 variables to be used for the analysis*/

/*Randomly Sampling the data*/

proc surveyselect data=churn rat=0.7 
out= churn_select outall 
method=srs; 
run;


/*Splitting it into training and validation*/
data churn_train churn_test; 
set churn_select; 
if selected =1 then output churn_train; 
else output churn_test; 
run;

proc print data = churn_train (obs = 10);run;

proc print data = churn_test (obs = 10);run;


/*-----------------------------------------APPROACH 1:-----------------------------------------*/
/*Check for multicollinearity*/
/*Iteration 1*/

proc corr data = churn;
var change_mou change_rev blck_dat_mean blck_dat_range roam_mean drop_dat_mean mou_opkd_mean roam_range threeway_mean custcare_mean;
run;

/*Iteration 2*/
proc corr data = churn;
var change_mou blck_dat_mean roam_mean drop_dat_mean mou_opkd_mean threeway_mean custcare_mean mou_cdat_Mean callfwdv_Mean ccrndmou_Mean;
run;

/*Iteration 3*/
proc corr data = churn;
var change_mou blck_dat_mean roam_mean drop_dat_mean mou_opkd_mean threeway_mean custcare_mean callfwdv_Mean cc_mou_Mean opk_dat_Mean;
run;

/*Iteration 4*/
proc corr data = churn;
var change_mou blck_dat_mean roam_mean drop_dat_mean mou_opkd_mean threeway_mean custcare_mean callfwdv_Mean plcd_dat_Mean comp_dat_Mean;
run;

/*Iteration 5*/
proc corr data = churn;
var change_mou blck_dat_mean roam_mean drop_dat_mean mou_opkd_mean threeway_mean custcare_mean callfwdv_Mean plcd_dat_Mean callwait_Mean;
run;

/*Logistics Regression*/
proc logistic data=churn_train outmodel=train_out descending;
model Churn = change_mou blck_dat_mean roam_mean drop_dat_mean mou_opkd_mean threeway_mean custcare_mean callfwdv_Mean plcd_dat_Mean callwait_Mean/expb;
run;

/*First 10 variables and then remove them based on significanse*/
proc logistic data=churn_train outmodel=train_out descending;
model Churn = change_mou blck_dat_mean roam_mean drop_dat_mean mou_opkd_mean threeway_mean custcare_mean callfwdv_Mean plcd_dat_Mean callwait_Mean/expb;
run;


/*FINAL MODEL*/
proc logistic data=churn_train outmodel=train_out descending;
model Churn = roam_mean threeway_mean custcare_mean callwait_Mean eqpdays /expb;
run;


proc corr data = churn_train;
var roam_mean threeway_mean custcare_mean callwait_Mean eqpdays;
run;


/*hit ratio on train data*/

/*Prediction using SCORE*/
PROC LOGISTIC INMODEL=train_out;
SCORE DATA= churn OUT=churn_predicted FITSTAT;
RUN;

proc print data=churn_predicted(obs=10);run;

data churn_predicted;set churn_predicted;
if Churn = I_Churn then prediction = 1; else prediction = 0;run;

proc sql;
select AVG(prediction)*100 as hit_ratio
from churn_predicted
quit;

/*hit ratio on test data*/
proc logistic data=churn_train outmodel=train_out descending;
model Churn = roam_mean threeway_mean custcare_mean callwait_Mean eqpdays /expb;
run;


/*Prediction using SCORE*/
PROC LOGISTIC INMODEL=train_out;
SCORE DATA= churn_test OUT=churn_predicted FITSTAT;
RUN;

proc print data=churn_predicted(obs=10);run;

data churn_predicted;set churn_predicted;
if Churn = I_Churn then prediction = 1; else prediction = 0;run;

proc sql;
select AVG(prediction)*100 as hit_ratio
from churn_predicted
quit;



/*--------------------APPROACH 2:-----------------------------------------------------------------*/
/*First 10 variables*/
proc logistic data=churn_train outmodel=train_out descending;
model Churn = change_mou change_rev blck_dat_mean blck_dat_range roam_mean drop_dat_mean mou_opkd_mean roam_range threeway_mean custcare_mean /expb;
run;

proc logistic data=churn_train outmodel=train_out descending;
model Churn = change_mou change_rev roam_mean roam_range threeway_mean custcare_mean mou_cdat_Mean callfwdv_Mean ccrndmou_Mean cc_mou_Mean /expb;
run;

proc logistic data=churn_train outmodel=train_out descending;
model Churn = change_mou change_rev roam_mean roam_range threeway_mean custcare_mean ccrndmou_Mean cc_mou_Mean plcd_dat_Mean comp_dat_Mean /expb;
run;

proc logistic data=churn_train outmodel=train_out descending;
model Churn = change_mou change_rev roam_mean roam_range threeway_mean custcare_mean ccrndmou_Mean cc_mou_Mean callwait_Mean iwylis_vce_Mean /expb;
run;

proc corr data = churn_train;
var change_mou change_rev roam_mean roam_range threeway_mean custcare_mean ccrndmou_Mean cc_mou_Mean callwait_Mean iwylis_vce_Mean;
run;

proc logistic data=churn_train outmodel=train_out descending;
model Churn = change_mou roam_mean threeway_mean custcare_mean callwait_Mean iwylis_vce_Mean eqpdays vceovr_Range hnd_price /expb;
run;

proc corr data = churn_train;
var change_mou roam_mean threeway_mean custcare_mean callwait_Mean iwylis_vce_Mean eqpdays vceovr_Range hnd_price ovrrev_Mean;
run;

proc logistic data=churn_train outmodel=train_out descending;
model Churn = roam_mean threeway_mean custcare_mean eqpdays vceovr_Range hnd_price complete_Mean attempt_Mean income totmrc_Mean/expb;
run;


proc corr data = churn_train;
var roam_mean threeway_mean custcare_mean eqpdays vceovr_Range hnd_price complete_Mean attempt_Mean income totmrc_Mean;
run;

/*FINAL MODEL*/
proc logistic data=churn_train outmodel=train_out descending;
model Churn = roam_mean threeway_mean custcare_mean eqpdays vceovr_Range hnd_price income totmrc_Mean/expb;
run;
/*hit ratio on train data*/

/*Prediction using SCORE*/
PROC LOGISTIC INMODEL=train_out;
SCORE DATA= churn_train OUT=churn_predicted FITSTAT;
RUN;

proc print data=churn_predicted(obs=10);run;

data churn_predicted;set churn_predicted;
if Churn = I_Churn then prediction = 1; else prediction = 0;run;

proc sql;
select AVG(prediction)*100 as hit_ratio
from churn_predicted
quit;

/*hit ratio on test data*/

/*Prediction using SCORE*/
PROC LOGISTIC INMODEL=train_out;
SCORE DATA= churn_test OUT=churn_predicted FITSTAT;
RUN;

proc print data=churn_predicted(obs=10);run;

data churn_predicted;set churn_predicted;
if Churn = I_Churn then prediction = 1; else prediction = 0;run;

proc sql;
select AVG(prediction)*100 as hit_ratio
from churn_predicted
quit;


/*Rough work-----------------------------------------------------------------------------------------------------*/

/*Top 5 and bottom 5*/
proc logistic data=churn_train outmodel=train_out descending;
model Churn = change_mou blck_dat_mean roam_mean drop_dat_mean mou_opkd_mean  mou_peav_Range complete_Range comp_vce_Range attempt_Range numbcars/expb;
run;

/*Iteration 6*/
proc corr data = churn;
var change_mou roam_mean threeway_mean custcare_mean plcd_dat_Mean callwait_Mean eqpdays threeway_Range peak_dat_Mean iwylis_vce_Mean;
run;


/*Iteration 7*/
proc corr data = churn;
var change_mou roam_mean threeway_mean custcare_mean plcd_dat_Mean callwait_Mean eqpdays iwylis_vce_Mean mtrcycle mouiwylisv_Mean;
run;

/*Iteration 8*/
proc corr data = churn;
var change_mou roam_mean threeway_mean custcare_mean plcd_dat_Mean callwait_Mean eqpdays iwylis_vce_Mean mtrcycle unan_dat_Mean;
run;


/*Logistics Regression*/
proc logistic data=churn_train outmodel=train_out descending;
model Churn = change_mou roam_mean threeway_mean custcare_mean plcd_dat_Mean callwait_Mean eqpdays iwylis_vce_Mean mtrcycle unan_dat_Mean/expb;
run;


/*Logistics Regression with significant variables*/
proc logistic data=churn_train outmodel=train_out descending;
model Churn = change_mou roam_mean threeway_mean custcare_mean plcd_dat_Mean eqpdays iwylis_vce_Mean mtrcycle vceovr_Range inonemin_Mean/expb;
run;

proc corr data = churn;
var change_mou roam_mean threeway_mean custcare_mean plcd_dat_Mean eqpdays iwylis_vce_Mean mtrcycle vceovr_Range inonemin_Mean;
run;


