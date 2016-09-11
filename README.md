
![mlr](https://mlr-org.github.io/mlr-tutorial/images/mlrLogo_blue_141x64.png): Machine Learning in R

==========================

Updates to MLR for Forecasting: September 19th, 2016
==========================

The goal of this project is to develop:

1. Forecasting time series models in the MLR framework
2. Measures for evaluating forecast models
3. Resampling methods that work for time based data
4. Preprocessing lag and difference features

As a basic introduction we will simulate data from an arima process and place it into an xts object.
```{r}
library(xts)
library(lubridate)
set.seed(1234)
dat = arima.sim(model = list(ar = c(.5,.2), ma = c(.4), order = c(2,0,1)), n = 5000)
times = Sys.time() + days(1:5000)
dat = xts(dat,order.by = times)
colnames(dat) = c("arma_test")
```

Now, just like with `makeRegrTask()` we will use `makeForecastRegrTask()` to create a task for forecasting. The main difference between `Forecast` tasks and the normal tasks is that our data must be an xts object.
```{r}
Timeregr.task = makeForecastRegrTask(id = "test", data = dat, target = "arma_test")
Timeregr.task
# Supervised task: test
# Type: regr
# Target: arma_test
# Observations: 5000
# Features:
# numerics  factors  ordered 
#        1        0        0 
# Missings: FALSE
# Has weights: FALSE
# Has blocking: FALSE
```

Notice that we still inheret a Supervised task and our type is a regression. Now we create an arima model from the package `forecast` using `makeLearner()` by calling the learner class `fcregr.Arima`. An important (and debate-ably placed) parameter is the `n.ahead` parameter, which is used to specify that we are predicting 10 steps ahead.

```{r}
arm = makeLearner("fcregr.Arima", order = c(2,0,1), n.ahead = 10L, include.mean = FALSE)
arm
# Learner fcregr.Arima from package forecast
# Type: regr
# Name: AutoRegressive Integrated Moving Average; Short name: Arima
# Class: fcregr.Arima
# Properties: numerics,ts
# Predict-Type: response
# Hyperparameters: order=2,0,1,n.ahead=10,include.mean=FALSE
```

We now have two new cross validation resampling strategies, `GrowingCV` and `FixedCV`. They are both rolling forecasting origin techniques established in [Hyndman and Athanasopoulos (2013)](https://www.otexts.org/fpp/2/5) and first made popular in R by the `caret` package's `createTimeSlices()` function. We specify:

1. horizon - the number of periods to forecast
2. initialWindow - our left-most starting time slice (needs to be changed to initial.window for standards)
3. size - The number of rows in our time series
4. skip - An optional parameter that allow to skip every n'th window.
```{r}
resamp_desc = makeResampleDesc("GrowingCV", horizon = 10L,
                               initialWindow = 1000L,
                               size = nrow(dat), skip = 50L)
resamp_desc
# Window description:
#  growing with 79 iterations:
#  1000 observations in initial window and 10 horizon.
# Predict: test
# Stratification: FALSE
```

The wonderful graphic posted below comes from the `caret` website and gives an intuitive idea of the sliding windows for both the growth and fixed options.

![Build Status](http://topepo.github.io/caret/main_files/figure-html/Split_time-1.png)

Taking our model, task, resampling strategy, and an additonal parameter for scoring our model, we use `resample()` to train our model.
```{r}
resamp_arm = resample(arm,Timeregr.task, resamp_desc, measures = mase)
resamp_arm
# Resample Result
# Task: test
# Learner: fcregr.Arima
# Aggr perf: mase.test.mean=0.00332
# Runtime: 4.164
```

The forecasting features fully integrate into mlr, allowing us to also make a parameter set to tune over.
```{r}
par_set = makeParamSet(
  makeIntegerVectorParam(id = "order",
                         len = 3L,
                         lower = c(0L,0L,0L),
                         upper = c(3L,1L,1L),
                         tunable = TRUE),
  makeIntegerVectorParam(id = "seasonal",
                         len = 3L,
                         lower = c(0L,0L,0L),
                         upper = c(1L,1L,1L),
                         tunable = TRUE),
  makeLogicalParam(id = "include.mean",
                   default = FALSE,
                   tunable = TRUE),
  makeNumericParam(id = "n.ahead",
                   default = 10,
                   tunable = FALSE,
                   lower = 10,
                   upper = 10)
)

#Specify tune by grid estimation
ctrl = makeTuneControlGrid()

#
res = tuneParams("fcregr.Arima", task = Timeregr.task, resampling = resamp_desc, par.set = par_set,
                 control = ctrl, measures = mase)
```

It is interesting to note that Arima does not select our sample data's original underlying process and instead selects a (0,0,0) model with a (0,1,0) seasonal structure.
```{r}
res

# Tune result:
# Op. pars: order=0,0,0; seasonal=0,1,0; include.mean=FALSE; n.ahead=10
# mase.test.mean=0.00294
```

This may be due to how low our measure is, as looking up the original models process does give a very good score.
```{r}
as.data.frame(res$opt.path)[139,]
#     order1 order2 order3 seasonal1 seasonal2 seasonal3 include.mean n.ahead mase.test.mean dob eol error.message
# 139      2      0      1         0         0         0        FALSE      10    0.003315028 139  NA          <NA>
    exec.time
139     3.724
```

We can now use our learned model with tuning to pass over the entire data set and give our final prediction. However there is currently a bug in the predict function.
```{r}
lrn = setHyperPars(makeLearner("fcregr.Arima"), par.vals = res$x)
m = train(lrn, Timeregr.task)

predict(m, task = Timeregr.task)
 
#  Error in (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,  : 
#   arguments imply differing number of rows: 5000, 10 
```
This error is caused by missing newdata. We are only generating 10 new observations (forecasts), but the underlying code believes we are running our model over our training data, returning 5000 observations. This error can be corrected by adding a subset of size 10 to our call, though the 'truth' responses are arbitrary and do not correspond to the next 10.

```{r}
predict(m, task = Timeregr.task, subset = 1:10)
# Prediction: 10 observations
# predict.type: response
# threshold: 
# time: 0.00
#   id      truth response
# 1  1 -1.0859353        0
# 2  2 -1.7280429        0
# 3  3 -1.5058091        0
# 4  4 -2.0405171        0
# 5  5 -0.5935022        0
# 6  6 -0.7395286        0
# ... (10 rows, 3 cols)
```

We can get back the raw estimates with no truth by making newdata a data frame of `NA`'s, though this is more likely to be a bug as it is a solution.
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

Updates to MLR for Forecasting: September 22nd, 2016
==========================

Issues with frequency have arisen when working with the `forecast` package due to frequency. While `xts` is robust, it assumes all dates are continuous, unique, and have a frequency of one. While this gives a robust structure, many of the methods in forecast are dependent on the datas frequency. To allow both packages to mesh we include a new parameter in `makeForecastRegrTask()` for frequency.

```{r}
Timeregr.task = makeForecastRegrTask(id = "test", data = dat,
                                     target = "arma_test", frequency = 1L)
# Task: test
# Type: regr
# Observations: 2000
# Dates:
#  Start: 2016-09-23 23:16:50 
#  End: 2022-03-15 23:16:50
# Frequency: 1
# Features:
# numerics  factors  ordered 
#        1        0        0 
# Missings: FALSE
# Has weights: FALSE
# Has blocking: FALSE
```
A new print statement for objects of class `TimeTask` was created for including dates and frequency.

A new function to create arbitrary lags and differences `createLagDiffFeatures()` has also been added. Notice that we get a weird doubel date thing, but our column names look nice

```{r}
Timeregr.task.lag = createLagDiffFeatures(Timeregr.task,lags = 2L:4L,difference = 0L)
tail(Timeregr.task.lag$env$data)
```
<table border=1>
<tr> <th>  </th> <th> dates </th> <th> arma_test </th> <th> arma_test_lag2_diff0 </th> <th> arma_test_lag3_diff0 </th> <th> arma_test_lag4_diff0 </th>  </tr>
  <tr> <td align="right"> 2022-03-10 23:49:34 </td> <td align="right"> 1646974174.52 </td> <td align="right"> -0.93 </td> <td align="right"> -0.58 </td> <td align="right"> -2.31 </td> <td align="right"> -0.98 </td> </tr>
  <tr> <td align="right"> 2022-03-11 23:49:34 </td> <td align="right"> 1647060574.52 </td> <td align="right"> -1.08 </td> <td align="right"> 0.28 </td> <td align="right"> -0.58 </td> <td align="right"> -2.31 </td> </tr>
  <tr> <td align="right"> 2022-03-12 23:49:34 </td> <td align="right"> 1647146974.52 </td> <td align="right"> -0.56 </td> <td align="right"> -0.93 </td> <td align="right"> 0.28 </td> <td align="right"> -0.58 </td> </tr>
  <tr> <td align="right"> 2022-03-13 23:49:34 </td> <td align="right"> 1647229774.52 </td> <td align="right"> -0.49 </td> <td align="right"> -1.08 </td> <td align="right"> -0.93 </td> <td align="right"> 0.28 </td> </tr>
  <tr> <td align="right"> 2022-03-14 23:49:34 </td> <td align="right"> 1647316174.52 </td> <td align="right"> -0.48 </td> <td align="right"> -0.56 </td> <td align="right"> -1.08 </td> <td align="right"> -0.93 </td> </tr>
  <tr> <td align="right"> 2022-03-15 23:49:34 </td> <td align="right"> 1647402574.52 </td> <td align="right"> 1.02 </td> <td align="right"> -0.49 </td> <td align="right"> -0.56 </td> <td align="right"> -1.08 </td> </tr>
   </table>
   
A new preprocessing wrapper `makePreprocWrapperLambert()` has been added. This function uses the
`LambertW` package's `Guassianize()` function to help remove skewness and kurtosis from the data.

```{r}
lrn = makePreprocWrapperLambert("classif.lda", type = "h")
print(lrn)
train(lrn, iris.task)
```

The lambert W transform is a bijective function, but the current preprocessing wrapper does not allow us to invert our predictions back to the actual values when we make new predictions. This would be helpful if we wanted to use LambertW on our target, then get back answers that coinside with our real values instead of the transformed values.

Several new models have been included from forecast:

1. Exponential smoothing state space model with Box-Cox transformation (bats)
2. Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal Fourier components (tbats)
3. Exponential smoothing state space model (ets)
4. Neural Network Autoregressive model (nnetar)

The below code will run with the `Timeregr.task` we states above.
```{r}
batsMod = makeLearner("fcregr.bats", h = 10)
batsMod
m = train(batMod, Timeregr.task)
predict(m, Timeregr.task, subset = 1:10)

#fc.tbats
tBatsMod = makeLearner("fcregr.tbats", h = 10)
tBatsMod
m = train(tBatsMod, Timeregr.task)
predict(m, Timeregr.task, subset = 1:10)

#fc.ets
etsMod = makeLearner("fcregr.ets", h = 10)
etsMod
m = train(etsMod, Timeregr.task)
predict(m, Timeregr.task, subset = 1:10)

#fc.nnetar
nnetarMod = makeLearner("fcregr.nnetar", h = 10)
nnetarMod
m = train(nnetarMod, Timeregr.task)
predict(m, Timeregr.task, subset = 1:10)
```

And the big finish for this week... GARCH models!!!

```{r}
garchMod = makeLearner("fcregr.garch", model = "sGARCH",
                       garchOrder = c(1,1), n.ahead = 10,
                       armaOrder = c(2, 1))
m = train(garchMod, Timeregr.task)
predict(m, newdata = as.data.frame(rep(NA,10)))
```

So not bad, we need to look over how I pull out the garch models list parameters and I think everything will be okay.

