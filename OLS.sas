libname x1 XLSX 
 'C:/Users/Pablo/Desktop/KG6372/cleanData.xlsx';                                                                                                                                                                                                                
FILENAME REFFILE 'C:/Users/Pablo/Desktop/KG6372/cleanData.xlsx';
PROC IMPORT DATAFILE=REFFILE	
DBMS=XLSX	
OUT= DF;	
GETNAMES=YES;    
run; 
proc print data = DF(obs=10);
run;
proc glmselect data=DF 
testdata=DF                                                                                                                                                                                 
seed=1 plots(stepAxis=number)=(criterionPanel ASEPlot CRITERIONPANEL);   
CLASS PRODUCT_TYPE; 
model PRICE_DOC = ID FULL_SQ LIFE_SQ FLOOR MAX_FLOOR NUM_ROOM KITCH_SQ PRODUCT_TYPE	RAION_POPUL	GREEN_ZONE_PART	INDUST_PART	CHILDREN_PRESCHOOL	PRESCHOOL_QUOTA	CHILDREN_SCHOOL	HEALTHCARE_CENTERS_RAION UNIVERSITY_TOP_20_RAION SHOPPING_CENTERS_RAION	OFFICE_RAION RAILROAD_TERMINAL_RAION BIG_MARKET_RAION FULL_ALL X0_6_ALL	X7_14_ALL X0_17_ALL	X16_29_ALL X0_13_ALL BUILD_COUNT_BLOCK BUILD_COUNT_WOOD	BUILD_COUNT_FRAME BUILD_COUNT_BRICK	BUILD_COUNT_BEFORE_1920	BUILD_COUNT_1921_1945 BUILD_COUNT_1946_1970	BUILD_COUNT_1971_1995 BUILD_COUNT_AFTER_1995 METRO_MIN_AVTO	METRO_KM_AVTO METRO_MIN_WALK METRO_KM_WALK SCHOOL_KM PARK_KM GREEN_ZONE_KM INDUSTRIAL_KM RAILROAD_STATION_WALK_KM RAILROAD_STATION_WALK_MIN	ID_RAILROAD_STATION_WALK RAILROAD_STATION_AVTO_KM RAILROAD_STATION_AVTO_MIN	PUBLIC_TRANSPORT_STATION_KM 
PUBLIC_TRANS_STATION_TIME_WALK 
KREMLIN_KM BIG_ROAD1_KM BIG_ROAD2_KM RAILROAD_KM BUS_TERMINAL_AVTO_KM	BIG_MARKET_KM MARKET_SHOP_KM FITNESS_KM	SWIM_POOL_KM ICE_RINK_KM STADIUM_KM	BASKETBALL_KM PUBLIC_HEALTHCARE_KM UNIVERSITY_KM WORKPLACES_KM SHOPPING_CENTERS_KM OFFICE_KM BIG_CHURCH_KM
/ selection=stepwise( choose=CV stop=CV include = 4) CVdetails;                                                                                                                                                                                                                                               
run;
quit;
