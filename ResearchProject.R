---
title: "Research Project"
output: html_document
---

```{r setup,include=FALSE}
knitr::opts_chunk$set(echo = FALSE,scipen=999,comment="")
library(stargazer)
library(knitr)
library(broom)
library(multcomp)
```

March 10, 2019

ECON 380

Prof. Lhost

<center> ___Educational Attainment Contributing to Median Home Price___ </center>



#### Introduction:

  Median home price in the United States has changed substantially since the 50's, growing from $7,354 in 1950 to $119,600 in 2017 (Ramsey Solutions 2019). House pricing is determined by a number of factors such as location, demographics, economic growth (wages), etc. Gibbons and Machin (2008) focus on specific attributes and amenities that drive house prices up in their study, and found positive correlations between elementary school quality and home prices, where demand for better schooling subsequently increased house prices. A clear correlation exists between greater household income in the presence of higher education, and house prices are a signal of wealth in an area; this study explores how other variables might reversely signal house prices in a county. In particular, we aim to test if different levels of educational attainment of people within a county leads to different median home prices. 
  


#### Data:
  
  The dataset compiled in this study comes from a number of sources. Data on the median home price by FIPS county code was retrieved from the National Association of Realtors. The datasets on educational attainment by FIPS code and median household income by FIPS code were obtained from the Economic Research Service of the United States Department of Agriculture. The dataset on race was obtained from Kaggle, which used the American Community Survey of the Census Bureau for 2017 to arrange the data in order of the FIPS codes.

  
  
#### Methods:

  Our dependent variable is house price, represented by the log of median house price at the county level in 2017. Measuring median home price is a more reflective average of county house price as it is not as affected by extreme values like the mean would be. Fifty percent of transactions occur at a price that is lower than the median price and the other fifty percent occur at a price that is higher using the median price. Independent variables are divided into two types. The first are variables we feel best reflect the educational attainment for a county. These include percentage of residents without a high school diploma, the percentage of residents with a high school diploma only, the percentage of residents with some associate or college degree, and the percentage of residents with a bachelor's degree or higher. All residents surveyed for educational attainment are aged 25+ and entries are by county.
  
  The second type of independent variables act as control factors in order to prevent omitted variable bias. These are the median household income by county, percentage of residents in the county that is Hispanic, percentage of residents in the county that is White, percentage of residents in the county that are Black, percentage of residents in the county that is Native American, and percentage of residents in the county that is Asian.
  
  This study aims to determine if there is a relationship between home prices and education levels of those who live in a county, controlling for factors like median household income and the racial makeup of each county as well. While there are studies on increasing home price values and the positive correlation with better elementary, middle, and high schools, this study focuses on high school and higher education statistics. 





```{r, results="asis",echo=FALSE}
cat("
<style>
td {
padding-left: 5px;
padding-right: 5px;
padding-top: 0px;
padding-bottom: 0px;
}
</style>
")
```

```{r,warning=FALSE,message=FALSE,results="asis"}
library(stargazer)
mydata <- read.csv("datafinal.csv")

stargazer(mydata [ , 4:14], title="Summary Statistics Table", type = "html", summary.stat = c("n","mean","sd", "min", "median", "max"), covariate.labels = c("Median Home Price", "Percent w/o HS","Percent w/ HS", "Percent w/ some college","Percent w/ Bachelor's or higher", "Median Household Income", "Percent Hispanic", "Percent White", "Percent Black", "Percent Native", "Percent Asian"))
```



  Each variable in the table above has a substantial number of observations, but we were missing data for some observations, which might have had an effect on our results. In particular, we didn't have data on the median home prices for some counties. Our data was compiled from the sources mentioned above, and merged so that every county has uniform data distribution. For both the outcome variable and all the explanatory variables, we examine data for 3,119 counties. To merge our data, we removed statewide data from some datasets to focus on the county level only. Counties were then compiled so that none were doubly accounted for under a slightly different name. All variables listed in the summary statistics include all the variables compiled in our final merged dataset.


The following regression models were run:

Model 1 (Simple Linear Regression):

$$log(Median Home Price)=\beta_0+ \beta_1(Percent w/o HS)_i + u_i $$

Model 2 (1st Multiple Linear Regression):

$$log(Median Home Price)=\beta_0 + \beta_1(Percent w/ HS)_i + \beta_2(Percent w/ some college)_i + \beta_3(Percent w/ Bachelor's or higher)_i + u_i  $$

Model 3 (2nd Multiple Linear Regression): 

$$log(Median Home Price)=\beta_0 + \beta_1(Percent w/o HS)_i + \beta_2(Percent w/ HS)_i + \beta_3(Percent w/ some college)_i + \beta_4(Percent w/ Bachelor's or higher)_i  + \beta_5(Median Household Income)_i + \beta_6(Percent Hispanic)_i + \beta_7(Percent White)_i + \beta_8(Percent Black)_i  + \beta_9(Percent Native)_i + \beta_10(Percent Asian)_i + u_i $$
```{r, echo=FALSE}
model1 <- lm(I(log(MedPrice)) ~ PerNoGED, data=mydata)

model2 <- lm(I(log(MedPrice)) ~ PerGED + PerAssoc + PerBAPlus, data=mydata)

model3 <- lm(I(log(MedPrice)) ~ PerNoGED + PerGED + PerAssoc + PerBAPlus + MedIncome + PerHisp + PerWhite + PerBlack + PerNative + PerAsian , data=mydata)
```


  Model 1 regresses the log of median house price on the percentage of those that have less than a high school diploma. This model is utilized as a comparative measure for the rest of the educational explanatory variables. In Model 2, percent of adults with a GED, percent of Adults with some college or an associates degree, and percent of adults with a bachelor's degree and beyond are added as explanatory variables. This model was regressed to see how much education levels of the adults living in a county accounted for the variation in house prices. Model 3 incorporates the race statistics onto Model 2, as well as median household income for each county. Median household income per county was added to further control the education levels, since greater levels of higher education and higher income are highly correlated. We also added percent of Hispanic people, percent of White people, percent of Black people, percent of Native American people, and percent of Asian people per county. The third model is our most detailed, and increases the R squared value from only education variables in Model 2. Racial statistical breakdown has a significant effect on explaining house prices, perhaps more than educational levels, but that doesn't mean that educational attainment is a poor economic indicator of the housing prices.


```{r,message=FALSE,warning=FALSE,results='asis'}
#library(stargazer)
#stargazer(model1, model2, model3, type = "html",  report=('vc*p'))
```

#### Results:
  
  
  The results of our 3 regressions are summarized in the table below:


```{r,message=FALSE,warning=FALSE,results='asis'}
library(stargazer)
stargazer(model1, model2, model3, title="Regression Results", type = "html", report=('vc*p'),keep.stat=c("n","rsq","adj.rsq"), dep.var.labels = c("Log(Median Home Price)"), covariate.labels = c("Percent w/o HS","Percent w/ HS", "Percent w/ some college","Percent w/ Bachelor's or higher", "Median Household Income", "Percent Hispanic", "Percent White", "Percent Black", "Percent Native", "Percent Asian"))
```



  Model 1 is a simple linear regression. Again, all residents surveyed for educational attainment are aged 25+. At zero percent of residents without a high school diploma in a county, the expected median home price is $222,348.22. The only coefficient estimate in this model means that if other factors are held constant, an increase of 1% in the residents without a high school diploma, on average, will decrease the expected median home price by 3.5%. This result is significant at the 1% level, with a p-value of 0.000. Intuitively, this makes sense because a decrease in the educational attainment of the majority of a county's residents should decrease the median home price of that county. The R-squared value of 0.218 means that about 21.8% of the variation in the logarithm of median home prices can be explained by the percent of residents who are high school dropouts. We should realize that this is the case because there are other factors that determine the median house price that  are incorporated in our multiple regression models.


  
  For Model 2, if all the explanatory variables equal zero, the expected median home price is $56, 669.99. Holding all the other factors constant, an increase of 1% in the residents with a high school diploma is expected to decrease the median home price by 0.3%. The coefficient estimate of percent with a high school diploma is statistically significant at the 10% level, with a p-value of 0.059. Similarly, we interpret our coefficient estimates of percent with some college and percent with a Bachelor's degree or higher. A unit increase in percentage of residents with some college leads to an expected increase of 0.6% in the median home price. This is statistically significant at the 1% level, with a p-value of 0.00001. Note that a percentage increase in residents with a high school diploma only in a county is expected to decrease the median home price. The existence of a negative relationship between the log(median home price) and both percent w/HS in Model 1 and percent w/ HS in Model 2 lead us to think more about this result. We concluded that the effect of being a high school dropout was more negative than that of being a high school diploma. In addition, being able to afford an expensive house, on average, requires more than just a high school diploma. Finally, a 1% increase in the residents with a bachelor's degree or higher will increase the expected median home price by 3.8%. This expected effect is greater than that of a unit increase in residents with only some associate or college degree. All of the results for model 2 are statistically significant at some standard level of significance. The R-squared value of 0.563 means that about 56.3% of the variation in the logarithm of median home prices can be explained by our explanatory variables for this model. From an intuitive point of view, the results for this model make sense: the higher the proportion of a county's population that at least has a bachelor's degree, the more likely the housing prices are to increase. This positive relationship is consistent with the fact that well-educated people tend be paid higher, on average, and this makes them  financially able to afford more expensive homes. This leads us to talk about model 3 in which we include median household income just for that reason. It is thought to have a modest correlation with the median home price. Other than that, we also included the racial makeup of a county in Model 3.

  For Model 3,  we can interpret our coefficient estimates of our explanatory variables, similar to Models 1 and 2.  For all education attainment variables in Model 3, the coefficient estimates are not statistically significant at any standard level. This is not to say that education variables do not have an effect on house pricing; perhaps it is because the educational attainment variables are interrelated with the Median Household Income variable. The coefficient estimate of Median Household Income means that, holding everything else constant, an increase of $1 in the median household income is expected to increase the median home price by 0.002%. This result is statistically significant at the 1% level, as the p-value is 0.000. The coefficient estimate of Percent Hispanic means that, holding everything else constant, an increase of 1% in the Hispanic population of a county is expected to decrease the median home price by 2.0%. The coefficient estimate of  Percent White means that, holding all else constant, an increase of 1% in the White population of a county is expected to decrease the median home price by 2.1%. The coefficient estimate of  Percent Black means that, holding everything else constant, an increase of 1% in the Black population of a county  is expected to decrease the median home price by 2.1%. The coefficient estimate of Percent Native means that, holding all else constant, an increase of 1% in the Native American population of a county is expected to decrease the median home price by 2.3%. The coefficient estimate of Percent Asian means that, holding everything else constant, an increase of 1% in the Asian population of a county is expected to decrease the median home price by 0.6%. These results for the coefficient estimates of Percent Hispanic, Percent White, Percent Black and Percent Native are all statistically significant at the 1% level, as the p-value is 0.000, except for Percent Asian, whose coefficient estimate is not statistically significant at any given level, as the p-value is 0.196.
  
  
  
#### Conclusion:

  This study found significant relationships between house prices and all four education levels observed. Notable relationships were also found between house prices and the percentages of the hispanic, white and black populations. Despite our inability to prove causal relationships, the nature of the relationships we looked at were not exactly as we predicted. The results in Model 1 and 2 provide evidence of an intuitive relationship between certain levels of educational attainment and house prices. Model 1 shows that the number of high school dropouts is correlated with lower house prices. Model 2 shows that having a bachelor's degree or higher is correlated with higher house prices. The negative relationship between percentage of high school diploma holders and house prices in Model 2 could be explained by the fact that there are many public high schools funded by the government. Furthermore, the cost of education increases as you enter higher levels of education. Having just a high school diploma, therefore, does not typically lead to better-paid opportunities like having at least a four-year degree does, which explains why the latter is correlated with higher house prices. 
  
  All explanatory variables in Model 3 have a negative relationship with the median house price except for the Median Household Income. The reason why the two variables Percent w/ some college and Percent w/ Bachelor's or higher have different signs in Model 3 compared to Model 2 might be that we have included Median Household Income in this regression along with the racial distribution of each county in Model 3. This could potentially mean that these other variables are better indicators of the median home price in a county. We can back that claim by observing the fact the value of R-squared increases from 0.563 in Model 2 to 0.686 in Model 3. We can conclude from Model 3 that median household income and race are perhaps better indicators of housing prices. Income levels determine ability to afford high-priced homes, and racial breakdowns influence people's decision of where to live and who to live with to a great extent. In conclusion, since Model 3 is the most comprehensive model out of the three that we have regressed, our study does not provide enough evidence to support our hypothesis that people who are well-educated tend to live in more expensive homes, in comparison to people who are not as well-educated.



  

<center> ___Works Cited:___ </center>

https://www.nar.realtor/research-and-statistics/housing-statistics/conty-median-home-prices-and-monthly-mortgage-payment.

https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/.

https://www.kaggle.com/muonneutrino/us-census-demographic-data.

Gibbons, S., & Machin, S. (2008). Valuing school quality, better transport, and lower crime: evidence from house prices. oxford review of Economic Policy, 24(1), 99-119.

Ramsey Solutions. (2018, November 27). Housing Trends Since 1950: The Difference Will Shock You. Retrieved from https://www.daveramsey.com/blog/housing-trends.

Brasington, D., & Haurin, D. R. (2006). Educational outcomes and house values: A test of the value added approach. Journal of Regional Science, 46(2), 245-268. (mentioned but not on powerpoint).


  
  

    