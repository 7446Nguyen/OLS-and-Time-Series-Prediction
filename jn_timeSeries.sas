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
proc autoreg data = ts;
model AvgPrice = monthYear/nlag=(1) dwprob;
run;

/*AR(3)*/
proc autoreg data = ts;
model AvgPrice = monthYear/nlag=(3) dwprob;
run;

/*AR(5)*/
proc autoreg data = ts;
model AvgPrice = monthYear/nlag=(5) dwprob;
run;

/*AR(6)*/
proc autoreg data = ts;
model AvgPrice = monthYear/nlag=(6) dwprob;
run;


/*Part D*/
data predict;
input MonthNumber monthYear @@;
cards;
6 201506
7 201507
8 201508
9 201509
10 201510
11 201511
12 201512
1 201601
2 201602
3 201603
4 201604
5 201605
6 201606
;
run;

data forPred;
set ts predict;
run;

proc autoreg data = forPred plots(unpack);
model AvgPrice = monthYear/nlag=(3) dwprob;
output out = preds p = prediction lcl = lower ucl = upper pm = trend;
run;
proc print data = preds; run;
