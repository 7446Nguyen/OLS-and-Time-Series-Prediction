FILENAME REFFILE '/home/u38493344/Applied Stats/Project 1/timeSeriesB.xlsx';
PROC IMPORT DATAFILE=REFFILE
	DBMS=xlsx
	OUT=ts;
	getnames = yes;
RUN;
proc print data = ts;
run; 

/*No Lag*/
proc autoreg data = ts;
model AvgPrice = monthYear/dwprob;
run;

/*AR(1)*/
proc autoreg data = ts plots = residual;
model AvgPrice = MonthNumber/nlag=(1) dwprob;
output out = residData residual= residual;
run;

proc sgplot data = residData;
scatter x = MonthNumber y = residual;
series x = MonthNumber y = residual;
xaxis label= "Month";
run;

/*AR(2)*/
proc autoreg data = ts;
model AvgPrice = MonthNumber/nlag=(2) dwprob;
run;

/*AR(3)*/
proc autoreg data = ts;
model AvgPrice = MonthNumber/nlag=(3) dwprob;
run;

/*AR(4)*/
proc autoreg data = ts;
model AvgPrice = MonthNumber/nlag=(4) dwprob;
run;

/*AR(5)*/
proc autoreg data = ts;
model AvgPrice = MonthNumber/nlag=(5) dwprob;
run;

/*AR(6)*/
proc autoreg data = ts;
model AvgPrice = MonthNumber/nlag=(6) dwprob;
run;


/*Part D*/
data predict;
input MonthNumber monthYear @@;
cards;
48 201507
49 201508
50 201509
51 201510
52 201511
53 201512
54 201601
55 201602
56 201603
57 201604
58 201605
59 201606
;
run;

data forPred;
set ts predict;
run;

proc autoreg data = forPred plots(unpack);
model AvgPrice = MonthNumber/nlag=(1) dwprob;
output out = preds p = prediction lcl = lower ucl = upper pm = trend residual=resid;
run;
proc print data = preds; run;

proc sgplot data = preds;
band x = MonthNumber upper = upper lower = lower;
scatter x = MonthNumber y = prediction;
series x = MonthNumber y = prediction;
series x = MonthNumber y = trend/lineattrs = (color=black);
xaxis label= "Month";
run;
