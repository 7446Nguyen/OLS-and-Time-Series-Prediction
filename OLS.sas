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
/* stepwise regression */
proc glmselect data=DF 
testdata=DF                                                                                                                                                                                 
seed=1 plots(stepAxis=number)=(criterionPanel ASEPlot CRITERIONPANEL);   
CLASS PRODUCT_TYPE; 
model PRICE_DOC = year_month	year	month	day	id	timestamp	full_sq	life_sq	floor	max_floor	num_room	kitch_sq	product_type	raion_popul	green_zone_part	indust_part	children_preschool	preschool_quota	children_school	healthcare_centers_raion	university_top_20_raion	shopping_centers_raion	office_raion	railroad_terminal_raion	big_market_raion	full_all	X0_6_all	X7_14_all	X0_17_all	X16_29_all	X0_13_all	build_count_block	build_count_wood	build_count_frame	build_count_brick	build_count_before_1920	build_count_1921_1945	build_count_1946_1970	build_count_1971_1995	build_count_after_1995	metro_min_avto	metro_km_avto	metro_min_walk	metro_km_walk	school_km	park_km	green_zone_km	industrial_km	railroad_station_walk_km	railroad_station_walk_min	ID_railroad_station_walk	railroad_station_avto_km	railroad_station_avto_min	public_transport_station_km	public_trans_station_time_walk	kremlin_km	big_road1_km	big_road2_km	railroad_km	bus_terminal_avto_km	big_market_km	market_shop_km	fitness_km	swim_pool_km	ice_rink_km	stadium_km	basketball_km	public_healthcare_km	university_km	workplaces_km	shopping_centers_km	office_km	big_church_km
/ selection=stepwise( choose=CV stop=CV include = 4) CVdetails;                                                                                                                                                                                                                                               
run;
quit;

/* backward elimination */
proc glmselect data=DF 
testdata=DF                                                                                                                                                                                 
seed=1 plots(stepAxis=number)=(criterionPanel ASEPlot CRITERIONPANEL);   
CLASS PRODUCT_TYPE; 
model PRICE_DOC = year_month	year	month	day	id	timestamp	full_sq	life_sq	floor	max_floor	num_room	kitch_sq	product_type	raion_popul	green_zone_part	indust_part	children_preschool	preschool_quota	children_school	healthcare_centers_raion	university_top_20_raion	shopping_centers_raion	office_raion	railroad_terminal_raion	big_market_raion	full_all	X0_6_all	X7_14_all	X0_17_all	X16_29_all	X0_13_all	build_count_block	build_count_wood	build_count_frame	build_count_brick	build_count_before_1920	build_count_1921_1945	build_count_1946_1970	build_count_1971_1995	build_count_after_1995	metro_min_avto	metro_km_avto	metro_min_walk	metro_km_walk	school_km	park_km	green_zone_km	industrial_km	railroad_station_walk_km	railroad_station_walk_min	ID_railroad_station_walk	railroad_station_avto_km	railroad_station_avto_min	public_transport_station_km	public_trans_station_time_walk	kremlin_km	big_road1_km	big_road2_km	railroad_km	bus_terminal_avto_km	big_market_km	market_shop_km	fitness_km	swim_pool_km	ice_rink_km	stadium_km	basketball_km	public_healthcare_km	university_km	workplaces_km	shopping_centers_km	office_km	big_church_km
/ selection=backward( choose=CV stop=CV include = 4) CVdetails;                                                                                                                                                                                                                                               
run;
quit;

/* forward selection */
proc glmselect data=DF 
testdata=DF                                                                                                                                                                                 
seed=1 plots(stepAxis=number)=(criterionPanel ASEPlot CRITERIONPANEL);   
CLASS PRODUCT_TYPE; 
model PRICE_DOC = year_month	year	month	day	id	timestamp	full_sq	life_sq	floor	max_floor	num_room	kitch_sq	product_type	raion_popul	green_zone_part	indust_part	children_preschool	preschool_quota	children_school	healthcare_centers_raion	university_top_20_raion	shopping_centers_raion	office_raion	railroad_terminal_raion	big_market_raion	full_all	X0_6_all	X7_14_all	X0_17_all	X16_29_all	X0_13_all	build_count_block	build_count_wood	build_count_frame	build_count_brick	build_count_before_1920	build_count_1921_1945	build_count_1946_1970	build_count_1971_1995	build_count_after_1995	metro_min_avto	metro_km_avto	metro_min_walk	metro_km_walk	school_km	park_km	green_zone_km	industrial_km	railroad_station_walk_km	railroad_station_walk_min	ID_railroad_station_walk	railroad_station_avto_km	railroad_station_avto_min	public_transport_station_km	public_trans_station_time_walk	kremlin_km	big_road1_km	big_road2_km	railroad_km	bus_terminal_avto_km	big_market_km	market_shop_km	fitness_km	swim_pool_km	ice_rink_km	stadium_km	basketball_km	public_healthcare_km	university_km	workplaces_km	shopping_centers_km	office_km	big_church_km
/ selection=forward( choose=CV stop=CV include = 4) CVdetails;                                                                                                                                                                                                                                               
run;
quit;

proc reg data=DF;
      model PRICE_DOC = year_month	year	month	day	id	timestamp	full_sq	life_sq	floor	max_floor	num_room	kitch_sq	raion_popul	green_zone_part	indust_part	children_preschool	preschool_quota	children_school	healthcare_centers_raion	university_top_20_raion	shopping_centers_raion	office_raion	railroad_terminal_raion	big_market_raion	full_all	X0_6_all	X7_14_all	X0_17_all	X16_29_all	X0_13_all	build_count_block	build_count_wood	build_count_frame	build_count_brick	build_count_before_1920	build_count_1921_1945	build_count_1946_1970	build_count_1971_1995	build_count_after_1995	metro_min_avto	metro_km_avto	metro_min_walk	metro_km_walk	school_km	park_km	green_zone_km	industrial_km	railroad_station_walk_km	railroad_station_walk_min	ID_railroad_station_walk	railroad_station_avto_km	railroad_station_avto_min	public_transport_station_km	public_trans_station_time_walk	kremlin_km	big_road1_km	big_road2_km	railroad_km	bus_terminal_avto_km	big_market_km	market_shop_km	fitness_km	swim_pool_km	ice_rink_km	stadium_km	basketball_km	public_healthcare_km	university_km	workplaces_km	shopping_centers_km	office_km	big_church_km 
            / tol vif collin;
run;

proc glm data = DF;
class PRODUCT_TYPE;
model PRICE_DOC = year	full_sq	life_sq	floor	max_floor	num_room	kitch_sq	product_type	green_zone_part	indust_part	children_preschool	preschool_quota	children_school	healthcare_centers_raion	university_top_20_raion	shopping_centers_raion	railroad_terminal_raion	big_market_raion	build_count_block	build_count_wood	build_count_frame	build_count_brick	build_count_before_1920	build_count_1921_1945	build_count_1946_1970	build_count_1971_1995	build_count_after_1995	metro_min_avto	metro_km_avto	metro_min_walk	metro_km_walk	school_km	park_km	green_zone_km	industrial_km	railroad_station_walk_km	railroad_station_walk_min	ID_railroad_station_walk	railroad_station_avto_km	railroad_station_avto_min	public_transport_station_km	public_trans_station_time_walk	kremlin_km	big_road1_km	big_road2_km	railroad_km	bus_terminal_avto_km	big_market_km	market_shop_km	fitness_km	swim_pool_km	ice_rink_km	stadium_km	basketball_km	public_healthcare_km	university_km	workplaces_km	shopping_centers_km	office_km	big_church_km
run;
