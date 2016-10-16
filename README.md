
![mlr](https://mlr-org.github.io/mlr-tutorial/images/mlrLogo_blue_141x64.png): Machine Learning in R

==========================

Updates to MLR for Forecasting: October 15th, 2016
==========================

The goal of this project is to develop:

1. Forecasting time series models in the MLR framework
2. Measures for evaluating forecast models
3. Resampling methods that work for time based data
4. Automated Preprocessing for lag and difference features

As a basic introduction we will simulate data from an arima process and place it into an xts object.
```{r}
library(xts)
library(lubridate)
set.seed(1234)
dat = arima.sim(model = list(ar = c(.5,.2), ma = c(.4), order = c(2,0,1)), n = 200)
times = (as.POSIXlt("1992-01-14")) + lubridate::days(1:200)
dat = xts(dat,order.by = times)
colnames(dat) = c("arma_test")
```

# Forecast Regression Tasks

Just like with `makeRegrTask()` we will use `makeForecastRegrTask()` to create a task for forecasting. The main difference between `Forecast` tasks and the normal tasks is that our data must be an xts object.
```{r}
Timeregr.task = makeForecastRegrTask(id = "test", data = dat, target = "arma_test",
                                     frequency = 7L)
Timeregr.task
# Task: test
# Type: fcregr
# Observations: 200
# Dates:
#  Start: 1992-01-15 
# End: 1992-08-01
# Frequency: 7
# Features:
# numerics  factors  ordered 
#        1        0        0 
# Missings: FALSE
# Has weights: FALSE
# Has blocking: FALSE
```

## makeLearner for forecast regression tasks
Notice that we still inheret a Supervised task and our type is a forecast regression. We also specify a frequency in the task which is equal to our 'seasonality'. Examples of frequency include 7 for weekly seasonal data, 365 for yearly seasonal data, and 52 for yearly weekly data. 

Now we create an arima model from the package `forecast` using `makeLearner()` by calling the learner class `fcregr.Arima`. An important parameter is the `h` parameter, which is used to specify that we are forecasting 10 periods ahead

```{r}
arm = makeLearner("fcregr.Arima", order = c(2L,0L,1L), h = 10L, include.mean = FALSE)
arm
# Learner fcregr.Arima from package forecast
# Type: fcregr
# Name: AutoRegressive Integrated Moving Average; Short name: Arima
# Class: fcregr.Arima
# Properties: numerics,ts,quantile
# Predict-Type: response
# Hyperparameters: order=2,0,1,h=10,include.mean=FALSE
```

## Resampling

We now have two new cross validation resampling strategies, `GrowingCV` and `FixedCV`. They are both rolling forecasting origin techniques established in [Hyndman and Athanasopoulos (2013)](https://www.otexts.org/fpp/2/5) and first made popular in R by the `caret` package's `createTimeSlices()` function. We specify:

1. horizon - the number of periods to forecast
2. initialWindow - our left-most starting time slice (needs to be changed to initial.window for standards)
3. size - The number of rows in our time series
4. skip - An optional parameter that allow to skip every n'th window.
```{r}
resamp_desc = makeResampleDesc("GrowingCV", horizon = 10L,
                               initialWindow = 100L,
                               size = nrow(dat), skip = 15L)
resamp_desc
# Window description:
#  growing with 6 iterations:
#  100 observations in initial window and 10 horizon.
# Predict: test
# Stratification: FALSE
```

Note that we should need to remove stratification, as it does not really make sense in the context of time series to stratify our data (unless we can somehow use this for panel data). The wonderful graphic posted below comes from the `caret` website and gives an intuitive idea of the sliding windows for both the growth and fixed options.

![Build Status](http://topepo.github.io/caret/main_files/figure-html/Split_time-1.png)

Taking our model, task, resampling strategy, and an additonal parameter for scoring our model, we use `resample()` to train our model.
```{r}
resamp_arm = resample(arm,Timeregr.task, resamp_desc, measures = mase)
resamp_arm
# Resample Result
# Task: test
# Learner: fcregr.Arima
# Aggr perf: mase.test.mean=0.0629
# Runtime: 0.238438
```
## Tuning

The forecasting features fully integrate into mlr, allowing us to also make a parameter set to tune over. Here we make a very small parameter space and will use F1-racing to tune our parameters.
```{r}

par_set = makeParamSet(
  makeIntegerVectorParam(id = "order",
                         len = 3L,
                         lower = c(0L,0L,0L),
                         upper = c(2L,1L,1L),
                         tunable = TRUE),
  makeLogicalParam(id = "include.mean", default = FALSE, tunable = TRUE),
  makeIntegerParam(id = "h", default = 10L, lower = 10L, upper = 11L, tunable = FALSE)
)

#Specify tune by grid estimation
ctrl = makeTuneControlIrace(maxExperiments = 180L)

#

configureMlr(on.learner.error = "warn")
res = tuneParams("fcregr.Arima", task = Timeregr.task, resampling = resamp_desc, par.set = par_set,
                 control = ctrl, measures = mase)

```
Note that we have to do something very odd when specifying `h`. We specify the upper bound of `h` as 11 as irace will not work if the lower and upper bound of a parameter is the same value, even if the parameter has been specified to `tune = FALSE`. What this means is that inside of `makePrediction.TaskDescForecastRegr` we have to do a weird thing where, even though our prediction will at times be length 11, we cut it by the length of the truth variable `y[1:length(truth)]`. This is only going to complicate things in the future I'm sure. But irace works.

It is interesting to note that Arima does not select our sample data's original underlying process and instead selects a (1,0,1) model.
```{r}
res

# Tune result:
# Op. pars: order=1,0,1; include.mean=FALSE; h=10
# mase.test.mean=0.0618
```

This may be due to how small the data set is.
```{r}
as.data.frame(res$opt.path)[4,]
#   order1 order2 order3 include.mean  h mase.test.mean dob eol error.message exec.time
# 4      2      0      1        FALSE 10     0.06288381   1  NA          <NA>     0.156
```

We can now use our learned model with tuning to pass over the entire data set and give our final prediction. However there is currently a bug in the predict function.
```{r}
lrn = setHyperPars(makeLearner("fcregr.Arima"), par.vals = res$x)
m = train(lrn, Timeregr.task)

# Should give back the following error
predict(m, task = Timeregr.task)
 
#  Error in (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,  : 
#   arguments imply differing number of rows: 200, 10
```
This error is caused by `y[1:length(truth)]` in `makePrediction.TaskDescForecastRegr`. We are only generating 10 new observations (forecasts), but the `predict` function returns a data frame the same size as task data with `NA`s after 10 response observations. Furthermore, predict requires either newdata or task, but for forecasting models we will sometimes not have new data

```{r}
predict(m)
# Error in predict.WrappedModel(m) : 
# Pass either a task object or a newdata data.frame to predict, but not both!
```

We can get back the raw estimates with no truth by making newdata a data frame of `NA`'s, though this is more likely to be a bug as it is a solution. In addition, this requires some strange if loops in `makePrediction.TaskDescForecastRegr` to get around when there is no truth values.
```{r}
predict(m, newdata = data.frame(rep(NA, 10)))
# Prediction: 10 observations
# predict.type: response
# threshold: 
# time: 0.00
#   response
# 1        0
# 2        0
# 3        0
# 4        0
# 5        0
# 6        0
# ... (10 rows, 1 cols)
```

# Pre-processing

A new function to create arbitrary lags and differences `createLagDiffFeatures()` has also been added. Notice that we get a weird doubel date thing, but our column names look nice

```{r}
Timeregr.task.lag = createLagDiffFeatures(Timeregr.task,lag = 2L:4L, difference = 0L, 
                                          seasonal.lag = 1L:2L)
tail(Timeregr.task.lag$env$data)
```
<!-- html table generated in R 3.3.1 by xtable 1.8-2 package -->
<!-- Sun Oct 16 01:26:45 2016 -->
<table border=1>
<tr> <th>  </th> <th> dates </th> <th> arma_test_lag2_diff0 </th> <th> arma_test_lag3_diff0 </th> <th> arma_test_lag4_diff0 </th> <th> arma_test_lag7_diff0 </th> <th> arma_test_lag14_diff0 </th>  </tr>
  <tr> <td align="right"> 1992-07-27 </td> <td align="right"> 712209600.00 </td> <td align="right"> -1.90 </td> <td align="right"> -0.62 </td> <td align="right"> 0.39 </td> <td align="right"> -0.01 </td> <td align="right"> -2.98 </td> </tr>
  <tr> <td align="right"> 1992-07-28 </td> <td align="right"> 712296000.00 </td> <td align="right"> -0.18 </td> <td align="right"> -1.90 </td> <td align="right"> -0.62 </td> <td align="right"> 0.53 </td> <td align="right"> -3.07 </td> </tr>
  <tr> <td align="right"> 1992-07-29 </td> <td align="right"> 712382400.00 </td> <td align="right"> 0.36 </td> <td align="right"> -0.18 </td> <td align="right"> -1.90 </td> <td align="right"> 0.32 </td> <td align="right"> -3.04 </td> </tr>
  <tr> <td align="right"> 1992-07-30 </td> <td align="right"> 712468800.00 </td> <td align="right"> -0.16 </td> <td align="right"> 0.36 </td> <td align="right"> -0.18 </td> <td align="right"> 0.39 </td> <td align="right"> -0.77 </td> </tr>
  <tr> <td align="right"> 1992-07-31 </td> <td align="right"> 712555200.00 </td> <td align="right"> 0.16 </td> <td align="right"> -0.16 </td> <td align="right"> 0.36 </td> <td align="right"> -0.62 </td> <td align="right"> 1.29 </td> </tr>
  <tr> <td align="right"> 1992-08-01 </td> <td align="right"> 712641600.00 </td> <td align="right"> 2.24 </td> <td align="right"> 0.16 </td> <td align="right"> -0.16 </td> <td align="right"> -1.90 </td> <td align="right"> 1.87 </td> </tr>
   </table>
   
A new preprocessing wrapper `makePreprocWrapperLambert()` has been added. This function uses the
`LambertW` package's `Guassianize()` function to help remove skewness and kurtosis from the data.

```{r}
lrn = makePreprocWrapperLambert("classif.lda", type = "h")
print(lrn)
# Learner classif.lda.preproc from package MASS
# Type: classif
# Name: ; Short name: 
# Class: PreprocWrapperLambert
# Properties: numerics,factors,prob,twoclass,multiclass
# Predict-Type: response
# Hyperparameters: target.proc=FALSE,type=h,methods=IGMM,verbose=FALSE
# train(lrn, iris.task)
```

The lambert W transform is a bijective function, but the current preprocessing wrapper does not allow us to invert our predictions back to the actual values when we make new predictions. This would be helpful if we wanted to use LambertW on our target, then get back answers that match with our real values instead of the transformed values.

# Models

Several new models have been included from forecast:

1. Exponential smoothing state space model with Box-Cox transformation (bats)
2. Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal Fourier components (tbats)
3. Exponential smoothing state space model (ets)
4. Neural Network Autoregressive model (nnetar)

The below code will run with the `Timeregr.task` we states above.
```{r}
batMod = makeLearner("fcregr.bats", h = 10)
m = train(batMod, Timeregr.task)
predict(m, newdata = data.frame(rep(NA, 10)))

#fc.tbats
tBatsMod = makeLearner("fcregr.tbats", h = 10)
m = train(tBatsMod, Timeregr.task)
predict(m, newdata = data.frame(rep(NA, 10)))

#fc.ets
etsMod = makeLearner("fcregr.ets", h = 10)
m = train(etsMod, Timeregr.task)
predict(m, newdata = data.frame(rep(NA, 10)))

# NOTE: Sometimes, this produces the error
## Error in .Call("etsTargetFunctionInit", y = y, nstate = nstate, errortype = switch(errortype,  : 
##   "etsTargetFunctionInit" not resolved from current namespace (forecast) 

# This is caused by the namespace of forecast and requires re-downloading forecast.
# Sometimes this can be fixed simply by resetting R.

#fc.nnetar
nnetarMod = makeLearner("fcregr.nnetar", h = 10)
nnetarMod
m = train(nnetarMod, Timeregr.task)
predict(m, newdata = data.frame(rep(NA, 10)))
```

And now we can also do GARCH models! Though we need to talk about how I am coalescing the parameters. The GARCH models come from the package `rugarch`, which has lists of parameter controls. Similar to ada's control function we have to do a little work to translate the GARCH models to `mlr`'s format.

```{r}
garchMod = makeLearner("fcregr.garch", model = "sGARCH",
                       garchOrder = c(1,1), n.ahead = 10,
                       armaOrder = c(2, 1))
m = train(garchMod, Timeregr.task)
predict(m, newdata = as.data.frame(rep(NA,10)))
```

### Tests

There are now tests for each of the forecasting models implimented here. However ets fails with a strange error from the `forecast` package's namespace

```{r}
devtools::test(filter = "fcregr")
# Loading mlr
# Testing mlr
# fcregr_arfima: .....
# fcregr_Arima: .....
# fcregr_bats: .....
# fcregr_ets: 1
# fcregr_garch: .....
# fcregr_tbats: .....
# learners_all_fcregr: ..............2Timing stopped at: 0.003 0 0.003 
# Failed --------------------------------------------------------------------------------------------------
# 1. Error: fcregr_ets (@test_fcregr_ets.R#20)  ------------------------------------------------------------
# "etsTargetFunctionInit" not resolved from current namespace (forecast)
# ...
# 2. Error: learners work: fcregr  (@test_learners_all_fcregr.R#21) ---------------------------------------
# "etsTargetFunctionInit" not resolved from current namespace (forecast)
```

I've posted an issue on `forecasts` github page and will email Dr. Hyndman (author) if he does not respond soon. The strangest thing about this error is that it only happens during testing. With a fresh restart of R you can use the ets functions just fine.

We also have tests for general forecasting and `createLagDiffFeatures`. The test for Lambert W pre-processing is available in the general forecasting tests.
```{r}
devtools::test(filter = "forecast")
Loading mlr
Testing mlr
forecast: .......

DONE ====================================================================================================

devtools::test(filter = "createLagDiffFeatures")
Loading mlr
Testing mlr
createLagDiffFeatures: ....

DONE ====================================================================================================

```

