# IS CLIMATE CHANGE RELEVANT FOR THE REAL ESTATE MARKET? A MACHINE LEARNING APPROACH
The following sections exhibit various aspects of the work presented to the Undergraduate Program in Economics at the Faculdade de Ciências Econômicas of UFRGS, as a partial requirement for the Bachelor's degree in Economics.

EXAMINATION BOARD:

Prof. Dr. Hudson da Silva Torrent – Advisor
(UFRGS)

Prof. Dr. Fernando Augusto Boeira Sabino da Silva 
(UFRGS)

Prof. Dr. Sabino da Silva Pôrto Júnior
(UFRGS)

# ABSTRACT

Climate change, a pressing global challenge, has wide-ranging implications for various aspects of our lives, including housing prices. This paper delves into the intricate relationship between climate change and housing prices in the United States. Using a comprehensive dataset and employing machine learning techniques, we analyze the relevance of climate variables for housing prices. Our findings suggest that climate change variables can influence housing prices, particularly in the short term, but the relationship is complex and varies by region. Understanding these dynamics is crucial for informed decision-making, sustainable urban development and climate risk mitigation.

# EMPIRICAL ANALYSIS

In brief, the study analyzes data spanning several decades, incorporating climate-related variables such as anomalies in temperature, precipitation, and drought. To model housing returns, the paper utilizes stepwise boosting, an iterative algorithm that gradually integrates variables to balance model complexity and mitigate the risk of overfitting.

In assessing how climate change variables contribute to predictive performance, multiple models were tested, incorporating macroeconomic factors, financial factors, non-economic factors, non-financial factors, and measures of uncertainties. Finally, the study also examines the relevance of climate-related variables in housing return modeling, particularly by analyzing their selection rates within the boosting algorithm.

# DATA

The study utilized five groups of independent variables and six variations of 'Real Housing Returns' as dependent variables. 

The acronyms used in the modeling process and their meanings can be found at https://github.com/brunotag18/ClimateFinance-UFRGS/blob/main/Variables%20Description.xlsx

**Dependent Variables:** FHFA Index Returns
- Overall Real Housing Returns
- Aggregate Real Housing Returns
- Northeast Real Housing Returns
- Midwest Real Housing Returns
- South Real Housing Returns
- West Real Housing Returns

**Group 1:** Macro&Financial Factors
- From Ludvigson and Ng (2009)
- Available at https://www.sydneyludvigson.com/

**Group 2:** Macro&Financial Uncertainty Factors
- From Ludvigson et al. (2021)
- Available at https://www.sydneyludvigson.com/
  
**Group 3:** Non-Macro&Financial Uncertainty Factors
- From Ludvigson et al. (2021)
- Available at https://www.sydneyludvigson.com/
  
**Group 4:** Climate Factors from National Center for Environmental Information
- Average Temperature
- Maximum Temperature
- Minimum Temperature
- Precipitation
- Cooling Degree Days
- Heating Degree Days
- Palmer Drought Severity Index (PDSI)
- Palmer Hydrological Drought Index (PHDI)
- Palmer Modified Drought Index (PMDI)
- Palmer Z-Index


# METHODOLOGY

At the core of the stepwise boosting methodology lies the following logic. It seeks to construct a parsimonious yet highly effective linear model within the challenge of high-dimensional data. To circumvent the inconsistency, stepwise boosting constructs this model in an incremental fashion, systematically incorporating variables one by one. This iterative approach endeavors to arrive at the optimal model, one that encapsulates best the relationships between variables. The outcome is a function that aptly balances predictive accuracy and model simplicity. In this section, we delve into the intricacies of this stepwise boosting methodology and elucidate its application within the context of our research, culminating in a robust framework for predictive analysis.

Further details on the Step-wise Boosting method and the corresponding package utilized in the project are available at https://cran.r-project.org/web/packages/mboost/index.html

# FORECASTING PROCEDURES AND PERFORMANCE

Our approach involved an examination of the predictive power of various sets of variables for each dependent variable and forecast horizon. This assessment was performed through a series of six distinct models. The primary objective of this endeavor was to evaluate the individual contributions of each set of variables toward enhancing predictive accuracy.

The suite of variables available for model selection expanded iteratively. Specifically, each subsequent model inherited the pool of variables from the preceding one, augmented by the introduction of a fresh set of variables. The initial model, used as the benchmark, had at its disposal only the lags of the dependent variable. In contrast, the sixth and final model had not only not only the lags of the dependent variable at its disposal but also the entirety of the five sets of variables enumerated earlier. This progression was designed to systematically probe the incremental value of each variable set.

Furthermore, a pivotal aspect of our methodology involved the normalization of variables within each training window. This step was implemented to safeguard against the inadvertent infiltration of test set information into the model. Achieved through the standardization of variables using mean and standard deviation, this normalization process ensured that our models operated untainted by data leakage from the test set. 

The forecasting procedure itself was executed through an out-of-sample rolling window approach. This entailed the training of a new model in each distinct window, with the objective of evaluating predictive performance. The forecasted periods varied according to the specific dependent variable and the forecast horizon. The predicted period for the overall real housing returns was from July 1991 to June 2021, from August 1991 to June 2021, from September 1991 to June 2021, and from December 1991 to June 2021 for the horizons of 1, 3, 6 and 12 months respectively. For regional housing returns and the aggregate the periods of forecast were from October 1998 to May 2021, from November 1998 to May 2021, from December 1998 to May 2021 and from March 1999 to May 2021 for the same horizons. These smaller data sets are a result of the division of the original data into training set and test set in a 1:2 ratio.

To gauge the efficacy of each model and elucidate the impact of different variable sets on predictive power, we employed four key statistical metrics. These included the Root Mean Square Error (RMSE) and the Mean Absolute Error (MAE). The RMSE quantifies the square root of the average squared prediction errors, offering insight into the magnitude of prediction deviations. Meanwhile, the MAE represents the mean of absolute prediction errors, serving as a robust measure of the overall prediction accuracy.

The two-sided Giacomini-White test (GW) was also employed to assess the statistical difference between models, employing the first model as a benchmark. The test was used in two forms, the first using RMSE and the second MAE. Therefore, using the results of these metrics, we were able to make judgments about model superiority when statistically significant differences emerged.
Additionally, we incorporated the Model Confidence Set (MCS) (Hansen, Peter, et al., 2011) as another statistical procedure within our analytical framework. The MCS, through a battery of tests involving forecasted and actual values, helps to delineate the best-performing model. It does so under the null hypothesis assumption of equal predictive power among the models under consideration.

# RESULTS

Analysis of the performance of various predictive models with respect to overall real housing returns (Y), encompassing different predictive horizons. An examination of these results, particularly, when we focus on the short-term horizon (h = 1), Model 6, which incorporates climate change volatility variables, emerges as the frontrunner, displaying superior performance as indicated by RMSE, MAE, and the MCS Rank

In contrast, for longer horizons, the benchmark model (Model 1) consistently maintains its superiority, outperforming models that include climate change variables. Notably, only when h = 12 does another model (Model 3) manage to surpass the benchmark. In this specific case, the distinction becomes evident solely through the MCS Rank, as both Giacomini-White (GW) Tests show no statistically significant difference.

Turning our attention to the aggregate variable encompassing data from all four regions (YA), both GW Tests indicate no statistically significant difference between the models in any horizon. However, the MCS Ranks consistently excludes the first model across various horizons. Instead, models 2, 3, and 4 emerge as the preferred choices under specific h values.

For the regional variables, none of the models incorporating climate change variables manage to surpass the benchmark model in any of the statistical metrics employed. The first model consistently outperforms the others, as evidenced by all measurement criteria, except for the West (YW) region. Here, models 2 and 3 consistently secure the top position in the MCS Rank, despite statistical differences in RMSE only appearing at h = 12.

# VARIABLE SELECTION

As the selection of variables holds paramount importance in the context of step-wise boosting, this section seeks to evaluate the relevance of climate change variables in the modeling of housing returns by looking at its selection rate. 

The first noteworthy result is that, for the overall real housing returns, in Model 6, a significant proportion of the 15 most frequently selected variables are associated with Climate Change Volatility. Notably, the lags of Cooling Degree Days Anomaly and Heating Degree Days Anomaly stand out as key contributors within this subset. It is pertinent to underscore that, apart from the lags of housing returns, only the variables from the first set, specifically the macro factors, consistently feature among the top 15 variables in Models 2 through 4. Only with the introduction of climate change factors in Models 5 and 6 can we see a shift in this trend.

The analysis of the frequency of variable selection reveals an interesting pattern, with select climate change variables exhibiting frequencies exceeding 80% for specific  horizons of prediction. Notably, the tenth lag of Cooling Degree Days Anomaly Volatility and the fourth lag of Heating Degree Days Anomaly Volatility were consistently selected in 100% of instances when h = 6.

For the aggregate housing returns, the prevalence of climate change variables is notably less pronounced compared to the previous case. An extreme instance of this phenomenon occurs at h = 6, where only a single climate change variable finds its way into the selection.

Interestingly, our analysis reveals the emergence of diverse factors beyond the Cooling Degree Days Anomaly and Heating Degree Days Anomaly within the top 15 selected variables. These include Precipitation Anomaly, PDSI Anomaly, PMDI Anomaly, and Z-Index Anomaly. Nevertheless, it remains a consistent pattern that climate change factors continue to be the predominant representatives from variable sets beyond the first one in the selection.

The results of regional housing returns, while a definitive pattern is not observed it is evident that climate change variables bear significance, albeit in a nuanced manner. Notably, for the Midwest (YM) region, climate change variables seem to have some importance, particularly at h = 1, 3, and 12. Similarly, for the Northeast region, climate change variables exhibit relevance at h = 3, 6, and 12.

Examining the frequency of variable selection, certain climate change factors consistently emerge with selection frequencies surpassing 80%. This recurring selection, despite models containing these factors not consistently demonstrating substantial improvements in forecasting accuracy, might be an indicator of the importance of such factors.

# APPENDIX A - DEPENDENT VARIABLES

# APPENDIX B - REGIONAL RESULTS
			
![image](https://github.com/brunotag18/ClimateFinance-UFRGS/assets/85254516/ac2d0470-ad3a-4b36-8675-541759c56e42)


