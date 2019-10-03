/*Project 1*/
/*Lasso Selection*/
FILENAME REFFILE '/home/u38493344/Applied Stats/cleanData.xlsx';
FILENAME REFFILE 'C:/Users/Pablo/Desktop/KG6372/cleanData.xlsx';
PROC IMPORT DATAFILE=REFFILE
	DBMS=xlsx
	OUT=df;
	getnames = yes;
RUN;
proc print data = df;
run; 

data df1;
set df;
randNumber = ranuni(11);
run;

data train;
set df1;
if randNumber <= 1/4 then delete;
run;

data test;
set df1;
if randNumber > 1/4 then delete;
run;


title "LASSO Selection";
proc glmselect data =train testdata = test
	seed=1 plots(stepAxis = number)=(criterionPanel ASEPlot CRITERIONPANEL);
	class product_type;
	model price_doc = id	timestamp	full_sq	life_sq	floor	max_floor	num_room	kitch_sq	product_type	
	raion_popul	green_zone_part	indust_part	children_preschool	preschool_quota	children_school	healthcare_centers_raion	university_top_20_raion	
	shopping_centers_raion	office_raion	railroad_terminal_raion	big_market_raion	full_all	X0_6_all	X7_14_all	X0_17_all	X16_29_all	
	X0_13_all	build_count_block	build_count_wood	build_count_frame	build_count_brick	build_count_before_1920	build_count_1921_1945	
	build_count_1946_1970	build_count_1971_1995	build_count_after_1995	metro_min_avto	metro_km_avto	metro_min_walk	metro_km_walk	school_km	
	park_km	green_zone_km	industrial_km	railroad_station_walk_km	railroad_station_walk_min	ID_railroad_station_walk	railroad_station_avto_km	
	railroad_station_avto_min	public_transport_station_km	public_trans_station_time_walk	kremlin_km	big_road1_km	big_road2_km	
	railroad_km	bus_terminal_avto_km	big_market_km	market_shop_km	fitness_km	swim_pool_km	ice_rink_km	stadium_km	basketball_km	
	public_healthcare_km	university_km	workplaces_km	shopping_centers_km	office_km	big_church_km

/selection = LASSO(choose = CV stop=CV) CVdetails;
run;



/*VIF*/
/* Multicollinearity Investigation of VIF and Tolerance */ 
proc reg data=df1; 
model price_doc = year_month	year	month	day	id	timestamp	full_sq	life_sq	floor	max_floor	num_room	kitch_sq	
	raion_popul	green_zone_part	indust_part	children_preschool	preschool_quota	children_school	healthcare_centers_raion	university_top_20_raion	
	shopping_centers_raion	office_raion	railroad_terminal_raion	big_market_raion	full_all	X0_6_all	X7_14_all	X0_17_all	X16_29_all	
	X0_13_all	build_count_block	build_count_wood	build_count_frame	build_count_brick	build_count_before_1920	build_count_1921_1945	
	build_count_1946_1970	build_count_1971_1995	build_count_after_1995	metro_min_avto	metro_km_avto	metro_min_walk	metro_km_walk	school_km	
	park_km	green_zone_km	industrial_km	railroad_station_walk_km	railroad_station_walk_min	ID_railroad_station_walk	railroad_station_avto_km	
	railroad_station_avto_min	public_transport_station_km	public_trans_station_time_walk	kremlin_km	big_road1_km	big_road2_km	
	railroad_km	bus_terminal_avto_km	big_market_km	market_shop_km	fitness_km	swim_pool_km	ice_rink_km	stadium_km	basketball_km	
	public_healthcare_km	university_km	workplaces_km	shopping_centers_km	office_km	big_church_km/ vif tol collin;
title 'Price - Multicollinearity Check';
run;

/*Remove High VID Variables*/
DATA df2(DROP = year_month id raion_popul X0_6_all X7_14_all X0_17_all X16_29_all X0_13_all build_count_wood 
build_count_1921_1945 build_count_1946_1970 build_count_1971_1995 metro_min_avto park_km railroad_station_walk_km
railroad_station_avto_km stadium_km); 
SET df1;
RUN;

/*VIF Test after Removal of High VIF*/
/* Multicollinearity Investigation of VIF and Tolerance */ 
proc reg data=df2; 
model price_doc = year	month	day	timestamp	full_sq	life_sq	floor	max_floor	num_room	kitch_sq	
	green_zone_part	indust_part	children_preschool	preschool_quota	children_school	healthcare_centers_raion	university_top_20_raion	
	shopping_centers_raion	office_raion	railroad_terminal_raion	big_market_raion	full_all build_count_block	build_count_frame	build_count_brick	build_count_before_1920	
	build_count_after_1995 metro_km_avto	metro_min_walk	metro_km_walk	school_km	
	green_zone_km	industrial_km	railroad_station_walk_min	ID_railroad_station_walk	
	railroad_station_avto_min	public_transport_station_km	public_trans_station_time_walk	kremlin_km	big_road1_km	big_road2_km	
	railroad_km	bus_terminal_avto_km	big_market_km	market_shop_km	fitness_km	swim_pool_km	ice_rink_km	basketball_km	
	public_healthcare_km	university_km	workplaces_km	shopping_centers_km	office_km	big_church_km/ vif tol collin;
title 'Price - Multicollinearity Check';
run;

/*Remove more High VID Variables*/
DATA df3(DROP = year month day children_preschool
metro_km_avo metro_min_walk public_trans_station_time_walk basketball_km); 
SET df2;
RUN;

proc reg data=df3; 
model price_doc = timestamp	full_sq	life_sq	floor	max_floor	num_room	kitch_sq	
	green_zone_part	indust_part	preschool_quota	children_school	healthcare_centers_raion	university_top_20_raion	
	shopping_centers_raion	office_raion	railroad_terminal_raion	big_market_raion	full_all build_count_block	build_count_frame	build_count_brick	build_count_before_1920	
	build_count_after_1995	metro_km_walk	school_km	
	green_zone_km	industrial_km	railroad_station_walk_min	ID_railroad_station_walk	
	railroad_station_avto_min	public_transport_station_km	big_road1_km	big_road2_km	
	railroad_km	bus_terminal_avto_km	big_market_km	market_shop_km	fitness_km	swim_pool_km	ice_rink_km	
	public_healthcare_km	university_km	workplaces_km	shopping_centers_km	office_km	big_church_km/ vif tol collin;
title 'Price - Multicollinearity Check';
run;
