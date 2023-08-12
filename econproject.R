## We start with the descriptive analysis of short-term interest rates and inflation rates. 

## 1. Importing our Inflation dataset

library(readr)
## Make sure to specify the directory for your own convenience.
CANADA <- read_csv("C:/Users/Administrator/Desktop/Bocconi AY 2021-2022/Study Material Second Term/Econometrics/Group Project/CANADA.csv", 
                   col_types = cols(LOCATION = col_skip(), 
                                    INDICATOR = col_skip(), SUBJECT = col_skip(), 
                                    MEASURE = col_skip(), FREQUENCY = col_skip(), 
                                    DATE = col_date(format = "%Y-%m")))

## 2. Getting a statistical summary for our variables.

summary(CANADA[c(2:9)])

## 3. Plotting Our Variables from 1985 to 2022

library(TSstudio)

ts_plot(CANADA,
        title = "Canada - Monthly Measures of Explanatory Variables",
        Xtitle = "1985-2022",
        slider = TRUE)

## We can use the interactive time series plot to include/exclude and(or) move the
## time frame and(or) zoom in/out to get an idea of variables that tend to move together.
## If we want to look at all variables then it makes sense to scale them down with their 
## respective standard deviations

CANADA_new = CANADA
CANADA_new[c(2:9)] = sapply(CANADA_new[c(2:9)], function(x) scale(x, scale=TRUE))

## We do another multiple time series plot with our scaled dataset

ts_plot(CANADA_new,
        title = "Canada - Monthly Measures of Explanatory Variables",
        Xtitle = "1985-2022",
        Ytitle = "Scaled Data",
        slider = TRUE)

## It is important to note we will be using the original dataset in our model.
## Also we get a good idea on how closely inflation and short term interest rates
## are correlated when we use the scaled graph and omit other variables. We also
## observe how Share Prices and Unemployment rate is negatively correlated when
## we use the same principle. Same applies with Output Gap and Unemployment rate. 
## These observations will be quantitatively evaluated.

## 4. Trying a Linear Regression model for our variables.

## We start by predicting interest rates using inflation first as we realized a pattern.

simple.fit = lm(formula = INTEREST ~ INFLATION, data = CANADA)
summary(simple.fit)
cor(CANADA[c(2)], CANADA[c(3)])

## We observe the p-value is very small and Inflation has a lot of explanatory power.
## In addition the correlation coefficient is around 0.706
## Therefore, we keep it in our model and include GDPGAP (Linear and Cubic Interpolated).

simple.fit = lm(formula = INTEREST ~ INFLATION + GDPGAP_LINEAR + GDPGAP_CUBIC, data = CANADA)
summary(simple.fit)

## We observe a little improvement in our R-squared value and take a look at the p-values
## of our new variables.Both Linear and Cubic interpolation of Annually Output Gap 
## Percentage of Potential GDP have very little explanatory power in our model with
## p-value over 0.1. We know that Canada's target inflation rate has been %2 from
## 1980s and the equilibrium rate in Canada is time variant, this is the simplified
## version of the Taylor's Rule. 

## Before we move on with additional variables we will exclude Output gap due to low
## explanatory power in our model. Instead we introduce GDP Growth Rate. Let's put both
## Cubic Spline and Linear Interpolation to our model. Also we will extend our data points
## going back to 1980.

## Make sure to specify the directory for your own convenience.
CANADA <- read_csv("C:/Users/Administrator/Desktop/Bocconi AY 2021-2022/Study Material Second Term/Econometrics/Group Project/CANADA2.csv", 
                   col_types = cols(LOCATION = col_skip(), 
                                    INDICATOR = col_skip(), SUBJECT = col_skip(), 
                                    MEASURE = col_skip(), FREQUENCY = col_skip(), 
                                    DATE = col_date(format = "%Y-%m")))
## It might be useful to go back to line 30 and change the "9" to "7" and look at the 
## time-series analysis again. Our observations still hold true.

simple.fit = lm(formula = INTEREST ~ INFLATION + GDP_LINEAR + GDP_CUBIC,
                           data = CANADA)
summary(simple.fit)

## Our adjusted R-squared score increased about 0.20. We observe GDP_Cubic has 
## very low explanatory power with 0.5230 p-value and exclude it to run another test.

simple.fit = lm(formula = INTEREST ~ INFLATION + GDP_LINEAR, data = CANADA)
summary(simple.fit)

cor(CANADA[c(5)], CANADA[c(6)]) ## Obviously different interpolations of GDP Growth
## Quarterly are expected to be correlated. In our case linear interpolation serves
## better.

## Our adjusted R-squared value increased by a very small amount, 
## we decide to leave out Cubic Spline Interpolation of GDP Growth rate. 
## We continue by adding Share Prices in our model

simple.fit = lm(formula = INTEREST ~ INFLATION + GDP_LINEAR + SHARE_PRICES, data = CANADA)
summary(simple.fit)

## We now have adjusted R-squared value of 0.8593 compared to the previous result of 0.7328
## We also observe that the p-value of Share-Prices is quite low. A noteworthy,
## point is that the p-value of GDP Growth Rate has dropped once Share Prices
## are introduced. It is plausible to do another test excluding GDP Growth Rate.

simple.fit = lm(formula = INTEREST ~ INFLATION + SHARE_PRICES, data = CANADA)
summary(simple.fit)

## As expected adjusted R-squared value decreased. Also, as expected, the drop isn't 
## huge since the p-value of our excluded variable has dropped when Share Prices were 
## introduced. However, the contribution of our excluded variable is still significant 
## enough to keep it in our model. We move on with testing unemployment Rate.

simple.fit = lm(formula = INTEREST ~ INFLATION + GDP_LINEAR + SHARE_PRICES + UNEMPLOYMENT, 
                data = CANADA)
summary(simple.fit)

cor(CANADA[c(4)], CANADA[c(7)]) #Result is around -0.645

## We look at the correlation coefficient of Unemployment Rate and Share Prices.
## They are negatively correlated and it explains the very small increase in adjusted
## R-squared value. Our current value is 0.8622 and we acknowledge Unemployment Rate is
## the variable which has the least explanatory power with the highest p-value.

## It is now safe to conclude our specifications of the model being with Inflation Rate,
## GDP Growth Rate (Linear), Share prices and Unemployment Rate as our explanatory 
## variables since our adjusted R-squared value is at its highest.

## Here is the final model:

simple.fit = lm(formula = INTEREST ~ INFLATION + GDP_LINEAR + SHARE_PRICES + UNEMPLOYMENT, 
                data = CANADA)
summary(simple.fit)

## 5. Diagnostics on our final model

## Let's move on with alternative model specifications. Particularly, Ramsey's RESET
## Test is useful to check if we should add specific powers of our variables or not.

library(lmtest)
resettest(simple.fit, power = 2:3, type = "regressor")

## We end up with a very low p-value. The null hypothesis that all coefficients 
## of the variables which has power 2 and 3 are 0 is rejected. With a p-value
## of 2.2e-16. Our model suffers from misspecification.

## Let's do our formal test for heteroscedasticity.

gqtest(simple.fit, order.by = ~ INFLATION + GDP_LINEAR + SHARE_PRICES + UNEMPLOYMENT,
       data = CANADA, fraction = 100)

## We reject the null hypothesis of the GQ test and try to do a non-linear
## transformation to achieve homoscedasticity.

simple.fit = lm(formula = log(INTEREST) ~ INFLATION + GDP_LINEAR + SHARE_PRICES + UNEMPLOYMENT, 
                data = CANADA)

## Testing the model again.

gqtest(simple.fit, order.by = ~ INFLATION + GDP_LINEAR + SHARE_PRICES + UNEMPLOYMENT,
       data = CANADA, fraction = 100)
summary(simple.fit)

## We lost some explanatory power of our model, adjusted R-squared score decreased
## to 0.8188. However, we achieved homoscedasticity.

## Let's test for serial correlation of errors.

library(car)
durbinWatsonTest(simple.fit)

## It seems like auto correlation is present in our model. DW test has p-value of 0!
## Is it expected or not? (Economic point of view is discussed in the slides.

## Moving on with the normality test of the error term.
library(tseries)
jarque.bera.test(residuals(simple.fit))

## At 90% confidence level we do not reject the null hypothesis. Errors are normally
## distributed.

## 6. Discussing Alternative Model Specifications

## We have two problems with the OLS assumptions. 1. True model is not linear.
## 2. True model is autocorrelated. We propose the alternative model as the following.
## Cross products and powers of our variables should be added to the regression and
## All the diagnostic tests should be repeated. After that, we could try more non-linear
## transformations on our variables to deal with autocorrelation. We would then run all the tests
## again. If the problem is still there moving to GLS is the feasible option.