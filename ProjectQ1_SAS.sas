FILENAME REFFILE 'C:/Users/Pablo/Desktop/KG6372/cleanData.xlsx';
PROC IMPORT DATAFILE=REFFILE
DBMS=XLSX
OUT= DF;
GETNAMES=YES;
run;
proc print data = DF(obs=10);
run;
/* backward elimination - this is now the best model. It reached 0.4500 with interaction between preschool_children and preschool_quota. 0.4618 with more interaction*/
title "Backward Elimination";
proc glmselect data=DF 
testdata=DF                                                                                                                                                                                 
seed=1 plots(stepAxis=number)=(criterionPanel ASEPlot CRITERIONPANEL);
model PRICE_DOC = id timestamp full_sq life_sq floor max_floor num_room kitch_sq product_type raion_popul green_zone_part indust_part children_preschool preschool_quota children_school healthcare_centers_raion university_top_20_raion shopping_centers_raion office_raion railroad_terminal_raion big_market_raion full_all X0_17_all X16_29_all X0_13_all build_count_block build_count_wood build_count_frame build_count_brick build_count_before_1920 build_count_1946_1970 build_count_1971_1995 build_count_after_1995 metro_min_avto metro_km_avto metro_min_walk school_km green_zone_km industrial_km railroad_station_walk_km ID_railroad_station_walk railroad_station_avto_km railroad_station_avto_min public_transport_station_km kremlin_km big_road1_km big_road2_km railroad_km bus_terminal_avto_km big_market_km market_shop_km fitness_km swim_pool_km ice_rink_km stadium_km basketball_km public_healthcare_km university_km workplaces_km shopping_centers_km office_km big_church_km children_preschool*preschool_quota x16_29_all*x0_6_all x16_29_all*x0_13_all x0_17_all*x0_13_all x0_17_all*x16_29_all x7_14_all*x0_17_all x7_14_all*x0_17_all x7_14_all*x0_17_all x16_29_all*x0_13_all x16_29_all*x0_17_all x16_29_all*x7_14_all children_school*school_km build_count_block*build_count_before_1920 build_count_block*build_count_1921_1945 build_count_block*build_count_1946_1970 build_count_block*build_count_1971_1995 build_count_block*build_count_after_1995 build_count_wood*build_count_before_1920 build_count_wood*build_count_1921_1945 build_count_wood*build_count_1946_1970 build_count_wood*build_count_1971_1995 build_count_wood*build_count_after_1995 build_count_frame*build_count_before_1920 build_count_frame*build_count_1921_1945 build_count_frame*build_count_1946_1970 build_count_frame*build_count_1971_1995 build_count_frame*build_count_after_1995 build_count_brick*build_count_before_1920 build_count_brick*build_count_1921_1945 build_count_brick*build_count_1946_1970 build_count_brick*build_count_1971_1995 build_count_brick*build_count_after_1995 fitness_km*X16_29_all swim_pool_km*X0_6_all swim_pool_km*X7_14_all swim_pool_km*X0_17_all swim_pool_km*X0_13_all swim_pool_km*X16_29_all ice_rink_km*X0_6_all ice_rink_km*X7_14_all ice_rink_km*X0_17_all ice_rink_km*X0_13_all ice_rink_km*X16_29_all stadium_km*X16_29_all office_km*X16_29_all
/ selection=backward( choose=CV stop=CV include = 107) CVdetails;                                                                                                                                                                                                                                               
run;
quit;

title "Forward Selection";
proc glmselect data=DF 
testdata=DF
seed=1 plots(stepAxis=number)=(criterionPanel ASEPlot CRITERIONPANEL);   
model PRICE_DOC = id life_sq floor max_floor kitch_sq indust_part kremlin_km / selection=forward( choose=CV stop=CV ) CVdetails;                                                                                                                                                                                                                                               
run;
quit;

title "Stepwise Regression";
proc glmselect data=DF 
testdata=DF
seed=1 plots(stepAxis=number)=(criterionPanel ASEPlot CRITERIONPANEL);
model PRICE_DOC = id timestamp full_sq life_sq floor max_floor num_room kitch_sq product_type raion_popul green_zone_part indust_part children_preschool preschool_quota children_school healthcare_centers_raion university_top_20_raion shopping_centers_raion office_raion railroad_terminal_raion big_market_raion full_all X0_17_all X16_29_all X0_13_all build_count_block build_count_wood build_count_frame build_count_brick build_count_before_1920 build_count_1946_1970 build_count_1971_1995 build_count_after_1995 metro_min_avto metro_km_avto metro_min_walk school_km green_zone_km industrial_km railroad_station_walk_km ID_railroad_station_walk railroad_station_avto_km railroad_station_avto_min public_transport_station_km kremlin_km big_road1_km big_road2_km railroad_km bus_terminal_avto_km big_market_km market_shop_km fitness_km swim_pool_km ice_rink_km stadium_km basketball_km public_healthcare_km university_km workplaces_km shopping_centers_km office_km big_church_km children_preschool*preschool_quota x16_29_all*x0_6_all x16_29_all*x0_13_all x0_17_all*x0_13_all x0_17_all*x16_29_all x7_14_all*x0_17_all x7_14_all*x0_17_all x7_14_all*x0_17_all x16_29_all*x0_13_all x16_29_all*x0_17_all x16_29_all*x7_14_all children_school*school_km build_count_block*build_count_before_1920 build_count_block*build_count_1921_1945 build_count_block*build_count_1946_1970 build_count_block*build_count_1971_1995 build_count_block*build_count_after_1995 build_count_wood*build_count_before_1920 build_count_wood*build_count_1921_1945 build_count_wood*build_count_1946_1970 build_count_wood*build_count_1971_1995 build_count_wood*build_count_after_1995 build_count_frame*build_count_before_1920 build_count_frame*build_count_1921_1945 build_count_frame*build_count_1946_1970 build_count_frame*build_count_1971_1995 build_count_frame*build_count_after_1995 build_count_brick*build_count_before_1920 build_count_brick*build_count_1921_1945 build_count_brick*build_count_1946_1970 build_count_brick*build_count_1971_1995 build_count_brick*build_count_after_1995 fitness_km*X16_29_all swim_pool_km*X0_6_all swim_pool_km*X7_14_all swim_pool_km*X0_17_all swim_pool_km*X0_13_all swim_pool_km*X16_29_all ice_rink_km*X0_6_all ice_rink_km*X7_14_all ice_rink_km*X0_17_all ice_rink_km*X0_13_all ice_rink_km*X16_29_all stadium_km*X16_29_all office_km*X16_29_all
/ selection=stepwise(choose=CV stop=CV include = 30) CVdetails;                                                                                                                                                                                                                                               
run;
quit;
/* LASSO selection */
title "LASSO Selection";
/* R-squared is not a great measure for LASSO here */
proc glmselect data =DF
	seed=1 plots(stepAxis = number)=(criterionPanel ASEPlot CRITERIONPANEL);
	model price_doc = id timestamp full_sq life_sq floor max_floor num_room kitch_sq product_type raion_popul green_zone_part indust_part children_preschool preschool_quota children_school healthcare_centers_raion university_top_20_raion shopping_centers_raion office_raion railroad_terminal_raion big_market_raion full_all X0_6_all X7_14_all X0_17_all X16_29_all X0_13_all build_count_block build_count_wood build_count_frame build_count_brick build_count_before_1920 build_count_1921_1945 build_count_1946_1970 build_count_1971_1995 build_count_after_1995 metro_min_avto metro_km_avto metro_min_walk metro_km_walk school_km park_km green_zone_km industrial_km railroad_station_walk_km railroad_station_walk_min ID_railroad_station_walk railroad_station_avto_km railroad_station_avto_min public_transport_station_km public_trans_station_time_walk kremlin_km big_road1_km big_road2_km railroad_km bus_terminal_avto_km big_market_km market_shop_km fitness_km swim_pool_km ice_rink_km stadium_km basketball_km public_healthcare_km university_km workplaces_km shopping_centers_km office_km big_church_km price_doc
/selection = LASSO(choose = CV stop=CV) CVdetails;
run;
quit;
/* The start of our custom model (below) uses interaction terms and all variables suggested by our best OLS linear model (from backward) */
proc glmselect data=DF 
testdata=DF                                                                                                                                                                                 
seed=1 plots(stepAxis=number)=(criterionPanel ASEPlot CRITERIONPANEL);   
model PRICE_DOC = id life_sq floor max_floor num_room kitch_sq product_type green_zone_part indust_part preschool_quota children_school healthcare_centers_raion university_top_20_raion shopping_centers_raion railroad_terminal_raion big_market_raion X0_17_all X16_29_all build_count_block build_count_wood build_count_frame build_count_brick build_count_before_1920 build_count_1921_1945 build_count_1946_1970 build_count_1971_1995 build_count_after_1995 metro_km_avto school_km green_zone_km industrial_km ID_railroad_station_walk railroad_station_avto_km public_transport_station_km public_trans_station_time_walk kremlin_km big_road1_km big_road2_km railroad_km bus_terminal_avto_km big_market_km market_shop_km fitness_km swim_pool_km ice_rink_km stadium_km public_healthcare_km university_km workplaces_km shopping_centers_km office_km big_church_km x16_29_all*x0_6_all x16_29_all*x0_13_all X0_17_all*X0_13_all X0_17_all*X16_29_all X0_17_all*X7_14_all X16_29_all*X7_14_all children_school*school_km build_count_block*build_count_before_1920 build_count_block*build_count_1921_1945 build_count_block*build_count_1946_1970 build_count_block*build_count_1971_1995 build_count_block*build_count_after_1995 build_count_wood*build_count_before_1920 build_count_wood*build_count_1921_1945 build_count_wood*build_count_1946_1970 build_count_wood*build_count_1971_1995 build_count_wood*build_count_after_1995 build_count_frame*build_count_before_1920 build_count_frame*build_count_1921_1945 build_count_frame*build_count_1946_1970 build_count_frame*build_count_1971_1995 build_count_frame*build_count_after_1995 build_count_brick*build_count_before_1920 build_count_brick*build_count_1921_1945 build_count_brick*build_count_1946_1970 build_count_brick*build_count_1971_1995 build_count_brick*build_count_after_1995 X16_29_all*fitness_km swim_pool_km*X0_6_all swim_pool_km*X7_14_all X0_17_all*swim_pool_km X0_13_all*swim_pool_km ice_rink_km*X0_6_all ice_rink_km*X7_14_all X0_17_all*ice_rink_km X0_13_all*ice_rink_km office_km*X16_29_all
/ selection=backward( choose=CV stop=CV include = 40) CVdetails;                                                                                                                                                                                                                                               
run;
quit;
/* first, a correlation matrix to check collinearity */
proc corr data = DF noprob output=OutCorr nomiss
cov;
var price_doc id timestamp full_sq life_sq floor max_floor num_room kitch_sq product_type raion_popul green_zone_part indust_part children_preschool preschool_quota children_school healthcare_centers_raion university_top_20_raion shopping_centers_raion office_raion railroad_terminal_raion big_market_raion full_all X0_17_all X16_29_all X0_13_all build_count_block build_count_wood build_count_frame build_count_brick build_count_before_1920 build_count_1921_1945 build_count_1946_1970 build_count_1971_1995 build_count_after_1995 metro_min_avto metro_km_avto school_km green_zone_km industrial_km railroad_station_walk_km ID_railroad_station_walk railroad_station_avto_km railroad_station_avto_min public_transport_station_km public_trans_station_time_walk kremlin_km big_road1_km big_road2_km railroad_km bus_terminal_avto_km big_market_km market_shop_km fitness_km swim_pool_km ice_rink_km stadium_km basketball_km public_healthcare_km university_km workplaces_km shopping_centers_km office_km big_church_km;
run;

/* then, a variance importance factor matrix to also check collinearity. We don't want anything that falls below a tolerance of 0.1*/
proc reg data=df; 
model price_doc = id life_sq floor max_floor num_room kitch_sq product_type green_zone_part indust_part preschool_quota children_school healthcare_centers_raion university_top_20_raion shopping_centers_raion railroad_terminal_raion big_market_raion X0_17_all X16_29_all build_count_block build_count_wood build_count_frame build_count_brick build_count_before_1920 build_count_1921_1945 build_count_1946_1970 build_count_1971_1995 build_count_after_1995 metro_km_avto school_km green_zone_km industrial_km ID_railroad_station_walk railroad_station_avto_km public_transport_station_km public_trans_station_time_walk kremlin_km big_road1_km big_road2_km railroad_km bus_terminal_avto_km big_market_km market_shop_km fitness_km swim_pool_km ice_rink_km stadium_km public_healthcare_km university_km workplaces_km shopping_centers_km office_km big_church_km / vif tol collin;
run;

/* start interaction term visuals */
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*************************************************** Plotting Interaction Terms Below ****************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
ods graphics on;
proc glm data=DF;
   model price_doc = X0_17_all | X16_29_all / solution;
   ods select ParameterEstimates ContourFit;
   store GLMModel;
run;
/* step two (final step) for plotting interaction terms */
proc plm restore=GLMModel noinfo;
 effectplot slicefit(x=X0_17_all sliceby=X16_29_all) / clm;
run;
proc glm data=DF;
   model price_doc = children_school | school_km / solution;
   ods select ParameterEstimates ContourFit;
   store GLMModel;
run;
/* step two (final step) for plotting interaction terms */
proc plm restore=GLMModel noinfo;
 effectplot slicefit(x=children_school sliceby=school_km) / clm;
run;
proc glm data=DF;
   model price_doc = build_count_block | build_count_1921_1945 / solution;
   ods select ParameterEstimates ContourFit;
   store GLMModel;
run;
/* step two (final step) for plotting interaction terms */
proc plm restore=GLMModel noinfo;
 effectplot slicefit(x=build_count_block sliceby=build_count_1921_1945) / clm;
run;
proc glm data=DF;
   model price_doc = build_count_block | build_count_1946_1970 / solution;
   ods select ParameterEstimates ContourFit;
   store GLMModel;
run;
/* step two (final step) for plotting interaction terms */
proc plm restore=GLMModel noinfo;
 effectplot slicefit(x=build_count_block sliceby=build_count_1946_1970) / clm;
run;
proc glm data=DF;
   model price_doc = build_count_block | build_count_1971_1995 / solution;
   ods select ParameterEstimates ContourFit;
   store GLMModel;
run;
/* step two (final step) for plotting interaction terms */
proc plm restore=GLMModel noinfo;
 effectplot slicefit(x=build_count_block sliceby=build_count_1971_1995) / clm;
run;
proc glm data=DF;
   model price_doc = build_count_block | build_count_after_1995 / solution;
   ods select ParameterEstimates ContourFit;
   store GLMModel;
run;
/* step two (final step) for plotting interaction terms */
proc plm restore=GLMModel noinfo;
 effectplot slicefit(x=build_count_block sliceby=build_count_after_1995) / clm;
run;
proc glm data=DF;
   model price_doc = build_count_wood | build_count_before_1920 / solution;
   ods select ParameterEstimates ContourFit;
   store GLMModel;
run;
/* step two (final step) for plotting interaction terms */
proc plm restore=GLMModel noinfo;
 effectplot slicefit(x=build_count_wood sliceby=build_count_before_1920) / clm;
run;
proc glm data=DF;
   model price_doc = build_count_wood | build_count_1946_1970 / solution;
   ods select ParameterEstimates ContourFit;
   store GLMModel;
run;
/* step two (final step) for plotting interaction terms */
proc plm restore=GLMModel noinfo;
 effectplot slicefit(x=build_count_wood sliceby=build_count_1946_1970) / clm;
run;
proc glm data=DF;
   model price_doc = build_count_wood | build_count_after_1995 / solution;
   ods select ParameterEstimates ContourFit;
   store GLMModel;
run;
/* step two (final step) for plotting interaction terms */
proc plm restore=GLMModel noinfo;
 effectplot slicefit(x=build_count_wood sliceby=build_count_after_1995) / clm;
run;
proc glm data=DF;
   model price_doc = build_count_frame | build_count_before_1920 / solution;
   ods select ParameterEstimates ContourFit;
   store GLMModel;
run;
/* step two (final step) for plotting interaction terms */
proc plm restore=GLMModel noinfo;
 effectplot slicefit(x=build_count_frame sliceby=build_count_before_1920) / clm;
run;
proc glm data=DF;
   model price_doc = build_count_frame | build_count_1921_1945 / solution;
   ods select ParameterEstimates ContourFit;
   store GLMModel;
run;
/* step two (final step) for plotting interaction terms */
proc plm restore=GLMModel noinfo;
 effectplot slicefit(x=build_count_frame sliceby=build_count_1921_1945) / clm;
run;
proc glm data=DF;
   model price_doc = build_count_frame | build_count_1946_1970 / solution;
   ods select ParameterEstimates ContourFit;
   store GLMModel;
run;
/* step two (final step) for plotting interaction terms */
proc plm restore=GLMModel noinfo;
 effectplot slicefit(x=build_count_frame sliceby=build_count_1946_1970) / clm;
run;
proc glm data=DF;
   model price_doc = build_count_frame | build_count_after_1995 / solution;
   ods select ParameterEstimates ContourFit;
   store GLMModel;
run;
/* step two (final step) for plotting interaction terms */
proc plm restore=GLMModel noinfo;
 effectplot slicefit(x=build_count_frame sliceby=build_count_after_1995) / clm;
run;
proc glm data=DF;
   model price_doc = build_count_brick | build_count_1946_1970 / solution;
   ods select ParameterEstimates ContourFit;
   store GLMModel;
run;
/* step two (final step) for plotting interaction terms */
proc plm restore=GLMModel noinfo;
 effectplot slicefit(x=build_count_brick sliceby=build_count_1946_1970) / clm;
run;
proc glm data=DF;
   model price_doc = build_count_brick | build_count_1971_1995 / solution;
   ods select ParameterEstimates ContourFit;
   store GLMModel;
run;
/* step two (final step) for plotting interaction terms */
proc plm restore=GLMModel noinfo;
 effectplot slicefit(x=build_count_brick sliceby=build_count_1971_1995) / clm;
run;
proc glm data=DF;
   model price_doc = build_count_brick | build_count_after_1995 / solution;
   ods select ParameterEstimates ContourFit;
   store GLMModel;
run;
/* step two (final step) for plotting interaction terms */
proc plm restore=GLMModel noinfo;
 effectplot slicefit(x=build_count_brick sliceby=build_count_after_1995) / clm;
run;
proc glm data=DF;
   model price_doc = office_km | X16_29_all / solution;
   ods select ParameterEstimates ContourFit;
   store GLMModel;
run;
/* step two (final step) for plotting interaction terms */
proc plm restore=GLMModel noinfo;
 effectplot slicefit(x=office_km sliceby=X16_29_all) / clm;
run;
ods graphics off;
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*************************************************** Plotting Interaction Terms Above ****************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/

/* After updating using the suggestion from Backward on our interactions, filtering using correlation matrix and dropping using VIF threshold,
we continued removing interaction terms based on interaction plots. The above plots visualize the terms we left in the model below, which is
our best model. Although we reduced our adjusted R-squared by using the above methods (corr matrix, VIF, interaction plots), we feel this 
was because the models were overfit. Below, we are confident in the fit.*/
ods graphics on;
proc glm data = DF plots(unpack) = ALL;
proc glm data = DF plots(unpack) = (DIAGNOSTICS RESIDUALS);
model price_doc = id life_sq floor max_floor num_room kitch_sq product_type green_zone_part indust_part preschool_quota children_school healthcare_centers_raion university_top_20_raion shopping_centers_raion railroad_terminal_raion big_market_raion X0_17_all X16_29_all build_count_block build_count_wood build_count_frame build_count_brick build_count_before_1920 build_count_1921_1945 build_count_1946_1970 build_count_1971_1995 build_count_after_1995 metro_km_avto school_km green_zone_km industrial_km ID_railroad_station_walk railroad_station_avto_km public_transport_station_km public_trans_station_time_walk kremlin_km big_road1_km big_road2_km railroad_km bus_terminal_avto_km big_market_km market_shop_km fitness_km swim_pool_km ice_rink_km stadium_km public_healthcare_km university_km workplaces_km shopping_centers_km office_km big_church_km X0_17_all*X16_29_all children_school*school_km build_count_block*build_count_1921_1945 build_count_block*build_count_1946_1970 build_count_block*build_count_1971_1995 build_count_block*build_count_after_1995 build_count_wood*build_count_before_1920 build_count_wood*build_count_1946_1970 build_count_wood*build_count_after_1995 build_count_frame*build_count_before_1920 build_count_frame*build_count_1921_1945 build_count_frame*build_count_1946_1970 build_count_frame*build_count_after_1995 build_count_brick*build_count_1946_1970 build_count_brick*build_count_1971_1995 build_count_brick*build_count_after_1995 office_km*X16_29_all / cli;
run;

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*********************************************** After updating model using OLS **********************************************************/
/**************************************************** correlation matrix *****************************************************************/
/********************************************************and VIF model *******************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
%let inputData = DF;
%let numObs = 19836; *number of our cleanData observations (19,835) + 1;
%let numVarsLasso = 13;
%let lassoVars = id life_sq floor kitch_sq indust_part office_raion green_zone_km ID_railroad_station_walk kremlin_km fitness_km swim_pool_km university_km office_km;
*below are the linear variables selected outright by our best performing Ordinary Least Squares method (Backward Elimination);
%let numVarsOLS = 62;
%let OLSVars = id full_sq life_sq floor max_floor num_room kitch_sq product_type raion_popul green_zone_part indust_part children_preschool preschool_quota children_school healthcare_centers_raion university_top_20_raion shopping_centers_raion office_raion railroad_terminal_raion big_market_raion full_all X0_17_all X16_29_all X0_13_all build_count_block build_count_wood build_count_frame build_count_brick build_count_before_1920 build_count_1921_1945 build_count_1946_1970 build_count_1971_1995 build_count_after_1995 metro_min_avto metro_km_avto school_km green_zone_km industrial_km railroad_station_walk_km ID_railroad_station_walk railroad_station_avto_km railroad_station_avto_min public_transport_station_km public_trans_station_time_walk kremlin_km big_road1_km big_road2_km railroad_km bus_terminal_avto_km big_market_km market_shop_km fitness_km swim_pool_km ice_rink_km stadium_km basketball_km public_healthcare_km university_km workplaces_km shopping_centers_km office_km big_church_km;
%let numVarsOLSFwd = 7; * we may want to use these in the future to compare, but for now, we just need four models (same for OLSVarsFwd below);
%let OLSVarsFwd = id life_sq floor max_floor kitch_sq indust_part kremlin_km;
/* below we are setting the variables for the SQL we will eventually use for our custom model */
%let numVarsCustom = 69;
%let customOLSVars = id life_sq floor max_floor num_room kitch_sq product_type green_zone_part indust_part preschool_quota children_school healthcare_centers_raion university_top_20_raion shopping_centers_raion railroad_terminal_raion big_market_raion X0_17_all X16_29_all build_count_block build_count_wood build_count_frame build_count_brick build_count_before_1920 build_count_1921_1945 build_count_1946_1970 build_count_1971_1995 build_count_after_1995 metro_km_avto school_km green_zone_km industrial_km ID_railroad_station_walk railroad_station_avto_km public_transport_station_km public_trans_station_time_walk kremlin_km big_road1_km big_road2_km railroad_km bus_terminal_avto_km big_market_km market_shop_km fitness_km swim_pool_km ice_rink_km stadium_km public_healthcare_km university_km workplaces_km shopping_centers_km office_km big_church_km X0_17_all*X16_29_all children_school*school_km build_count_block*build_count_1921_1945 build_count_block*build_count_1946_1970 build_count_block*build_count_1971_1995 build_count_block*build_count_after_1995 build_count_wood*build_count_before_1920 build_count_wood*build_count_1946_1970 build_count_wood*build_count_after_1995 build_count_frame*build_count_before_1920 build_count_frame*build_count_1921_1945 build_count_frame*build_count_1946_1970 build_count_frame*build_count_after_1995 build_count_brick*build_count_1946_1970 build_count_brick*build_count_1971_1995 build_count_brick*build_count_after_1995 office_km*X16_29_all;
/* dependent variable is price_doc */
%let depVar = price_doc;

data DF; 
set &inputData;
randNumber = ranuni(11);
if _n_ < &numObs;
run;
/* build our training data set for external cross-validation. We will train in 25% blocks, test on 75%. We feel this is reasonable. 
   75% for training */
data dfTrain;
set &inputData;
if randNumber <= 1/4 then delete;
run;
/* build our test data set for external cross-validation; 25% for testing */
data dfTest;
set &inputData;
if randNumber > 1/4 then delete;
run;

ods graphics on;
title "Selection Method LASSO Using LASSO Variables and Cross Validation";
proc glmselect data=dfTrain testdata = dfTest
			   seed=1 plots(stepAxis=number)=(criterionPanel ASEPlot CRITERIONPANEL);
model &depVar = &lassoVars
		   / selection=LASSO( choose=CV stop=CV ) CVdetails;
		   score data=dfTest out=scoredLASSO;
run;
quit;
ods graphics off;

ods graphics on;
title "Selection Method Backward Elimination Using LASSO Variables and OLS";
proc glmselect data=dfTrain testdata = dfTest
			   plots(stepAxis=number)=(criterionPanel ASEPlot CRITERIONPANEL);
model &depVar = &lassoVars
		   / selection=backward( choose=adjrsq stop=adjrsq include = &numVarsLASSO ) CVdetails;
		   score data=dfTest out=scoredOLSLASSO;
run;
quit;
ods graphics off;

ods graphics on;
title "Selection Method Backward Elimination Using OLS Variables and OLS";
proc glmselect data=dfTrain testdata = dfTest
			   plots(stepAxis=number)=(criterionPanel ASEPlot CRITERIONPANEL);
model &depVar = &OLSVars
		   / selection=backward( choose=adjrsq stop=adjrsq include = &numVarsOLS ) CVdetails;
		   score data=dfTest out=scoredOLS;
run;
quit;

ods graphics on;/* This sets up the SQL for our custom model cross validation */
title "Selection Method Custom Combined Backward Elimination, Corr Matrix, VIF Using OLS Variables";
proc glmselect data=dfTrain testdata = dfTest
			   plots(stepAxis=number)=(criterionPanel ASEPlot CRITERIONPANEL);
model &depVar = &customOLSVars
		   / selection=backward( choose=adjrsq stop=adjrsq include = &numVarsCustom ) CVdetails;
		   score data=dfTest out=scoredOLSvarsCustom;
run;
quit;
ods graphics off;

/* Calculate Sums of Squares from LASSO and OLS outputs */
proc sql;
 create table fitLasso as
   select 
	count(&depVar) as n
	,css(&depVar) as totSS
	,sum((&depVar - p_&depvar)**2) as errSSLasso
   from scoredLasso;
  create table fitOLSLasso as
    select sum((&depVar - p_&depvar)**2) as errSSOLSLasso /*THIS IS THE SAME AS BELOW */
	from scoredOLSLasso;
  create table fitOLS as
    select sum((&depVar - p_&depvar)**2) as errSSOLS /* THIS IS THE SAME AS ABOVE */
	from scoredOLS;
  create table customModel as /* this is for our custom model, which will be our Regression Model */
    select sum((&depVar - p_&depvar)**2) as errSScustomOLS
	from scoredOLSvarsCustom;
quit;
run;

/* Calculate rsq and adjRsq using sums of squares from LASSO and OLS outputs */
data allMeasures;
merge fitLasso fitOLSLasso fitOLS customModel;
  rsqLasso = (1 - errSSLasso / totSS);
  rsqOLSLasso = (1 - errSSOLSLasso / totSS);
  rsqOLS = (1 - errSSOLS / totSS);
  rsqCustomOLS = (1 - errSScustomOLS / totSS); /* this is for our custom model */
  adjRsqLasso = (1 - errSSLasso / totSS)*((n - 1) / (n - &numVarsLasso - 1));
  adjRsqOLSLasso = (1 - errSSOLSLasso / totSS)*((n - 1) / (n - &numVarsLasso - 1));
  adjRsqOLS = (1 - errSSOLS / totSS)*((n - 1) / (n - &numVarsOLS - 1));
  adjRsqCustomOLS = (1 - errSScustomOLS / totSS)*((n - 1) / (n - &numVarsCustom - 1)); /* this is for our custom model */
  run;

  title "Goodness of Fit Measures Using Test Data - All Models (LASSO with LASSO Vars, OLS with LASSO Vars, OLS, and Custom OLS)";
  proc print data = allMeasures;
  run;
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/******************************************************* DO NOT ENTER THE DANGER ZONE ****************************************************/
/*********************************************************** DANGER: KEEP OUT ************************************************************/
/************************************************************ BEWARE OF DOG **************************************************************/
/*************************************************************** AND CAT *****************************************************************/
/******************************************************* AND OF OVER-FIT MODELS **********************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/

/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*************************************************** Plotting Interaction Terms Below ****************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
/*****************************************************************************************************************************************/
