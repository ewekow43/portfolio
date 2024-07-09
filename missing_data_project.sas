/*
This SAS script automates the process of analyzing missing values in a dataset.
*/

libname data "C:\Users\eweko\OneDrive\Za³¹czniki\Pulpit\ABT_beh_sas";
run;

/* Obtain metadata information of the dataset abt_sam_beh_train */
proc contents data=data.abt_sam_beh_train out=zmienne_train noprint;
run;

/* Keep only numeric variables */
data zmienne_train_num;
set zmienne_train;
where type=1;
keep name;
run;

data zmienne_train_num;
set zmienne_train_num;
id=_N_;
call symputx ("liczba_zmiennych_train", id);
run;


%let makrozmienna=1234;
%put to_jest_makrozmienna &makrozmienna.;
%put liczba zmiennych w zbiorze train=&liczba_zmiennych_train.;


/* Create an empty report dataset with the specified structure */
data raport;
length nazwa $32;
length N_braki_train 8.;
length N_braki_valid 8.;
length proc_braki_train 8.;
length proc_braki_valid 8.;
length average_braki_train 8.;
length roznica_braki_train 8.;
length average_braki_valid 8.;
length roznica_braki_valid 8.;
format average_braki_train percent10.;
format roznica_braki_train percent10.;
format average_braki_valid percent10.;
format roznica_braki_valid percent10.;
format proc_braki_train percent10.;
format proc_braki_valid percent10.;
stop;
run;


/* Macro to iterate over each numeric variable and calculate missing values statistics */

%macro petla_zmiennych;

%do i=1 %to %eval(&liczba_zmiennych_train.);
data test1; 
set zmienne_train_num;
where id=&i.;
call symputx("nazwa_zmiennej_num",name);
%put nazwa_zmiennej=&nazwa_zmiennej_num.;
run;

proc univariate data=data.abt_sam_beh_valid noprint; 
var &nazwa_zmiennej_num.;
output out=braki_valid n=n nmiss=nmiss ;
run;

proc univariate data=data.abt_sam_beh_train noprint; 
var &nazwa_zmiennej_num.;
output out=braki_train n=n nmiss=nmiss ;
run;

data proc_braki_valid;
set braki_valid;
suma = n + nmiss ;
procent_brakow = nmiss/suma;
call symputx("N_brakow_zmiennych_valid",nmiss);
call symputx("proc_brakow_zmiennych_valid",procent_brakow);

run;

data proc_braki_train;
set braki_train;
suma = n + nmiss ;
procent_brakow = nmiss/suma;
call symputx("N_brakow_zmiennych_train",nmiss);
call symputx("proc_brakow_zmiennych_train",procent_brakow);

run;

/*Start of the code for train*/

proc univariate data=data.abt_sam_beh_train noprint; 
by period;
var &nazwa_zmiennej_num.;
output out=braki_train n=n nmiss=nmiss ;
run;

data braki_train2;
set braki_train;
suma = n + nmiss ;
procent_brakow = nmiss/suma;
format procent_brakow percent10.;
run;

proc univariate data=braki_train2 noprint;
var procent_brakow;
output out=AVG_braki_train mean=average;
run;

data temp;
set AVG_braki_train;
call symputx("average_train",average);
run;

data braki_train_3;
set braki_train2;
average_train=&average_train.;
roznica_train=abs(average_train-procent_brakow);
format average_train percent10.;
format roznica_train percent10.;
run; 

proc univariate data=braki_train_3 noprint;
var roznica_train;
output out=AVG_braki_train mean=average;
run;

data temp;
set AVG_braki_train;
call symputx("average_train_roznica",average);
run;
/*end of code for train*/

/*Start of the code for valid*/
proc univariate data=data.abt_sam_beh_valid noprint; 
by period;
var &nazwa_zmiennej_num.;
output out=braki_valid n=n nmiss=nmiss ;
run;


data braki_valid2;
set braki_valid;
suma = n + nmiss ;
procent_brakow = nmiss/suma;
format procent_brakow percent10.;
run;

proc univariate data=braki_valid2 noprint;
var procent_brakow;
output out=AVG_braki_valid mean=average;
run;

data temp;
set AVG_braki_valid;
call symputx("average_valid",average);
run;

data braki_valid_3;
set braki_valid2;
average_valid=&average_valid.;
roznica_valid=abs(average_valid-procent_brakow);
format average_valid percent10.;
format roznica_valid percent10.;
run; 

proc univariate data=braki_valid_3 noprint;
var roznica_valid;
output out=AVG_braki_valid mean=average;
run;

data temp;
set AVG_braki_valid;
call symputx("average_valid_roznica",average);
run;
/*end of code for valid*/

data wiersz;

nazwa="&nazwa_zmiennej_num.";
n_braki_train=&N_brakow_zmiennych_train.;
n_braki_valid=&N_brakow_zmiennych_valid.;
proc_braki_train=&proc_brakow_zmiennych_train.;
proc_braki_valid=&proc_brakow_zmiennych_valid.;
average_braki_train=&average_train.;
roznica_braki_train=&average_train_roznica.;
average_braki_valid= &average_valid.;
roznica_braki_valid=&average_valid_roznica.;
output;
run;

proc append base=raport data=wiersz force;
run;

%end;


%mend petla_zmiennych;
%petla_zmiennych;


/* Save the raport dataset*/

data data.raport;
set raport;
run;

/*Exports the raport dataset to an Excel file.*/

ods _all_ close;
ods excel style=MEADOW file="C:\Users\eweko\OneDrive\Za³¹czniki\Pulpit\ABT_beh_sas\raport.xlsx";
proc print data=data.raport;
run;
ods excel close;


