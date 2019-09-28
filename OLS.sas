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
model PRICE_DOC = year	month	day	id	timestamp	full_sq	life_sq	floor	max_floor	num_room	kitch_sq	product_type	raion_popul	green_zone_part	indust_part	children_preschool	preschool_quota	children_school	healthcare_centers_raion	university_top_20_raion	shopping_centers_raion	office_raion	railroad_terminal_raion	big_market_raion	full_all	X0_6_all	X7_14_all	X0_17_all	X16_29_all	X0_13_all	build_count_block	build_count_wood	build_count_frame	build_count_brick	build_count_before_1920	build_count_1921_1945	build_count_1946_1970	build_count_1971_1995	build_count_after_1995	metro_min_avto	metro_km_avto	metro_min_walk	metro_km_walk	school_km	park_km	green_zone_km	industrial_km	railroad_station_walk_km	railroad_station_walk_min	ID_railroad_station_walk	railroad_station_avto_km	railroad_station_avto_min	public_transport_station_km	public_trans_station_time_walk	kremlin_km	big_road1_km	big_road2_km	railroad_km	bus_terminal_avto_km	big_market_km	market_shop_km	fitness_km	swim_pool_km	ice_rink_km	stadium_km	basketball_km	public_healthcare_km	university_km	workplaces_km	shopping_centers_km	office_km	big_church_km
/ selection=stepwise( choose=CV stop=CV include = 4) CVdetails;                                                                                                                                                                                                                                               
run;
quit;

/* backward elimination */
proc glmselect data=DF 
testdata=DF                                                                                                                                                                                 
seed=1 plots(stepAxis=number)=(criterionPanel ASEPlot CRITERIONPANEL);   
CLASS PRODUCT_TYPE; 
model PRICE_DOC = ID FULL_SQ LIFE_SQ FLOOR MAX_FLOOR NUM_ROOM KITCH_SQ PRODUCT_TYPE	RAION_POPUL	GREEN_ZONE_PART	INDUST_PART	CHILDREN_PRESCHOOL	PRESCHOOL_QUOTA	CHILDREN_SCHOOL	HEALTHCARE_CENTERS_RAION UNIVERSITY_TOP_20_RAION SHOPPING_CENTERS_RAION	OFFICE_RAION RAILROAD_TERMINAL_RAION BIG_MARKET_RAION FULL_ALL X0_6_ALL	X7_14_ALL X0_17_ALL	X16_29_ALL X0_13_ALL BUILD_COUNT_BLOCK BUILD_COUNT_WOOD	BUILD_COUNT_FRAME BUILD_COUNT_BRICK	BUILD_COUNT_BEFORE_1920	BUILD_COUNT_1921_1945 BUILD_COUNT_1946_1970	BUILD_COUNT_1971_1995 BUILD_COUNT_AFTER_1995 METRO_MIN_AVTO	METRO_KM_AVTO METRO_MIN_WALK METRO_KM_WALK SCHOOL_KM PARK_KM GREEN_ZONE_KM INDUSTRIAL_KM RAILROAD_STATION_WALK_KM RAILROAD_STATION_WALK_MIN	ID_RAILROAD_STATION_WALK RAILROAD_STATION_AVTO_KM RAILROAD_STATION_AVTO_MIN	PUBLIC_TRANSPORT_STATION_KM 
PUBLIC_TRANS_STATION_TIME_WALK 
KREMLIN_KM BIG_ROAD1_KM BIG_ROAD2_KM RAILROAD_KM BUS_TERMINAL_AVTO_KM	BIG_MARKET_KM MARKET_SHOP_KM FITNESS_KM	SWIM_POOL_KM ICE_RINK_KM STADIUM_KM	BASKETBALL_KM PUBLIC_HEALTHCARE_KM UNIVERSITY_KM WORKPLACES_KM SHOPPING_CENTERS_KM OFFICE_KM BIG_CHURCH_KM
/ selection=backward( choose=CV stop=CV include = 4) CVdetails;                                                                                                                                                                                                                                               
run;
quit;
