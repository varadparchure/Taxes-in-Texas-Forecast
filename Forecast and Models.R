library(fpp3)

temp = readr::read_csv("EconDataset.csv")
tail(temp)

## Part 2(B)
#There will be a holdout period that is used to compare actual data to forecasted values. 
#Please restrict your sample by excluding the last four observations. After restricting your 
#sample, if the data are transformed in any way prior to testing (e.g. a Box-Cox transformation),
#please discuss why.
  
# Creating a Tax Variable 

Tax = temp%>%mutate(Date=yearquarter(mdy(Date)))%>%
  as_tsibble(index = Date)
Tax

# Data from Jan 1994 Jan to Oct 2022
TaxHold = Tax%>%filter_index(~"2022 Oct")

# Check End 
tail(TaxHold)

#seasonal plot
TaxHold%>%gg_season((Total_Taxes))

#Check series before transformation
TaxHold %>%
  autoplot(Total_Taxes, color = "#008080")

#We will need a box-cox transformation cause series shows potential non-linear 
#growth, lets check value of lambda using guerrero function

lambdaHOLD=TaxHold%>%features(Total_Taxes,guerrero)%>%
  pull(lambda_guerrero)
lambdaHOLD



#log transformation:
TaxHold%>%autoplot(log(Total_Taxes), color = "#008080")
#Here, we plot the logarithmic transform, where we see we have largely stabilized 
#the volatility and mitigated the evidence of non-linear trend.



#Part C
#Discuss whether or not your data appear to be generated from a
#covariance stationary model. If your forecasting method requires a
#transformation is necessary, please discuss which transformation you
#used and why. A unit root test will be necessary for this part if you use
#SARIMA-based methods or vector autoregressions.


TaxHold=TaxHold%>%mutate(transform=log(Total_Taxes))

#lets look at unit root. Covariance stationary models do not have unit roots. 

#check the possibility of a seasonal unit root
TaxHold%>%features(transform,unitroot_nsdiffs)
#this gives us 1 suggesting that we need to seasonally difference our series once 

#is there a non-seasonal unit root
TaxHold%>%features(transform,unitroot_ndiffs)
#this gives us 1 suggesting that we need to non-seasonally difference our series once 


#Lets consider KPSS test

seasonal_diff <- TaxHold %>% features(transform, unitroot_kpss, seasonal = TRUE)
non_seasonal_diff <- TaxHold %>% features(transform, unitroot_kpss, seasonal = FALSE)

# Print the results
print(seasonal_diff)
print(non_seasonal_diff)

#we reject the null-hypo in both the cases, we continue to difference our series

#acf and pacf charts
TaxHold%>%
  gg_tsdisplay(difference(log(Total_Taxes),4)%>%difference(),lag_max=40,
                           plot_type="partial")+
  labs(title="Seasonally differenced", y="")




#Model 1 - SARIMA

sarima1 = TaxHold%>%model(ARIMA(log(Total_Taxes)~0+pdq(1,1,2)+
                                  PDQ(0,1,1)))%>%report() 

sarima2 = TaxHold%>%model(ARIMA(log(Total_Taxes)~0+pdq(2,1,2)+
                                  PDQ(0,1,1)))%>%report() 

sarima3 = TaxHold%>%model(ARIMA(log(Total_Taxes)~0+pdq(3,1,2)+
                                  PDQ(1,1,1)))%>%report() 

# estimating the models and creating a mable
models=TaxHold%>%model(sarima1=ARIMA(log(Total_Taxes)~0+pdq(1,1,2)+PDQ(0,1,1)),
                       sarima2=ARIMA(log(Total_Taxes)~0+pdq(2,1,2)+PDQ(0,1,1)),
                       sarima3=ARIMA(log(Total_Taxes)~0+pdq(3,1,2)+PDQ(1,1,1)))
                     
models%>%glance()%>%select(.model,AIC,BIC)%>%as.data.frame()


#forecast using model 1
sarima1%>%forecast(h=4)%>%
  autoplot(Tax%>%filter_index("2017 Q1"~"2023 Q4"))

#forecast using model 2

sarima2%>%forecast(h=4)%>%
  autoplot(Tax%>%filter_index("2017 Q1"~"2023 Q4"))

#forecast using model 3

sarima3%>%forecast(h=4)%>%
  autoplot(Tax%>%filter_index("2017 Q1"~"2023 Q4"))

# selected sarima 1 - (1,1,2)x(0,1,1)

# diagnostic testing
sarima1%>%residuals()%>%gg_tsdisplay(.resid, lag_max = 28, plot_type = "partial")

sarima1%>%augment()%>%features(.innov,ljung_box,lag=28, dof=5 )
#here we fail to reject null hypothesis, is autocorellation is present

#This suggests that there is not enough evidence to conclude that there is
#significant autocorrelation in the residuals at the specified lag intervals.

#This implies that, despite the complex patterns and fluctuations in the original
# data, the model has successfully captured these patterns, and the residuals 
#do not exhibit any systematic correlation over time. Therefore, the residuals
#can be considered as "white noise" or random fluctuations around the model's
#predictions.
 
candidate=TaxHold%>%model(ARIMA(log(Total_Taxes)~
                                   0+pdq(1,1,2)+PDQ(0,1,1)))

candidate%>%forecast(h=4)%>%
  autoplot(Tax%>%filter_index("2017 Q1"~"2023 Q4"))



#Neural Net
#in case of NNETAR, the algorithm is a feed-forward algorithm lacking back-propagation.

#There are three layers, the input layer, the hidden layer, and the output layer.



#check missing values
hold=Tax%>%summarise(count = sum(is.na(Total_Taxes)))
sum(hold$count)


#no missing value, we continue

#Neural Net Models 

#Prelim Model

prelimNET=TaxHold%>%model(NNETAR(log(Total_Taxes)~
          AR(P=0,p=3),n_networks=200))

#few fourier terms
SMALLfourierNET=TaxHold%>%model(NNETAR(log(Total_Taxes)~AR(P=0,p=3)+
                fourier(period=4,K=2),n_networks=200))

#additional fourier terms 
BigfourierNET=TaxHold%>%model(NNETAR(log(Total_Taxes)~AR(P=0,p=3)+
              fourier(period=4,K=2)+fourier(period=12,K=2),n_networks=200))

#we are selecting next 
BigfourierNET=TaxHold%>%model(NNETAR(log(Total_Taxes)~AR(P=0,p=2)+
              fourier(period=4,K=2)+fourier(period=8,K=2),n_networks=200))



#Forecast 

prelimNET%>%
  forecast(h=4,times=200,bootstrap=TRUE)%>%
  autoplot(Tax%>%filter_index("2017 Q1"~"2023 Q4"))

SMALLfourierNET%>%
  forecast(h=4,times=200,bootstrap=TRUE)%>%
  autoplot(Tax%>%filter_index("2017 Q1"~"2023 Q4"))

BigfourierNET%>%
  forecast(h=4,times=200,bootstrap=TRUE)%>%
  autoplot(Tax%>%filter_index("2017 Q1"~"2023 Q4"))


#lets look at SMALL and BIG fourier models
report(SMALLfourierNET)
#0.0021

report(BigfourierNET)
#0.0025

#σ2 (sigma squared) represents the estimated variance of the error term in 
#the model. It gives an indication of the variability or randomness in the 
#observed data that is not explained by the model.

#here sigma squared for the small fourier is smaller than big fourier
#but values are very close, so lets check RMSE


#lets look at RMSE and MAE for small and big fourier models

#for small fourier
smallres<-SMALLfourierNET%>%
  forecast(h=4,times=200,bootstrap=TRUE)

accuracy_SMALLfourierNET <- accuracy(smallres, Tax %>% filter_index("2023 Q1" ~ "2023 Q4"))
print(accuracy_SMALLfourierNET)

#for big fourier
bigres<-BigfourierNET%>%
  forecast(h=4,times=200,bootstrap=TRUE)

accuracy_BIGfourierNET <- accuracy(bigres, Tax %>% filter_index("2023 Q1" ~ "2023 Q4"))
print(accuracy_BIGfourierNET)


#big fouruer has a lower RMSEcompared to small fourier (2631).
#A lower RMSE indicates better accuracy in predicting future values.

#prioritizing models with lower RMSE would be advisable. Minimizing prediction 
#errors helps ensure that the forecasts closely align with the actual tax 
#revenue, thus supporting informed decision-making by policymakers and 
#stakeholders.


#compare Neural net and SARIMA for final forecast

#RMSE for candidate SARIMA model 

SARIMAres<-candidate%>%forecast(h=4)
accuracy_SARIMA <- accuracy(SARIMAres, Tax %>% filter_index("2023 Q1" ~ "2023 Q4"))
print(accuracy_SARIMA)

#RMSE for Big Fourier NNET
print(accuracy_BIGfourierNET)

#here SARIMA has the low RMSE when compared to our NNET model, we will proceed 
#with SARIMA for our final forecast

FINAL=Tax%>%model(ARIMA(log(Total_Taxes)~
                                   0+pdq(1,1,2)+PDQ(0,1,1)))

FINAL%>%forecast(h=6)%>%
  autoplot(Tax%>%filter_index("2017 Q1"~"2025 Q2"))




