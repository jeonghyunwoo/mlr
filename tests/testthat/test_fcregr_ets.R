context("fcregr_ets")

test_that("fcregr_ets", {

  parset.list = list(
    list(),
    list( model = "ANN"),
    list( model = "ZAZ", ic = "bic"),
    list(opt.crit = "amse", bounds = "usual"),
    list( model = "AZZ", lambda = 1)
  )
  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(y = ts(fcregr.train, start = 1, frequency = 1L))
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(forecast::ets, pars)
    })
    set.seed(getOption("mlr.debug.seed"))
    p = as.numeric(forecast::forecast(m, h = 1L)$mean)
    old.predicts.list[[i]] = p
  }

  parset.list[[1]]$h = 1L
  parset.list[[2]]$h = 1L
  parset.list[[3]]$h = 1L
  parset.list[[4]]$h = 1L
  parset.list[[5]]$h = 1L
  testSimpleParsets("fcregr.ets", fcregr.xts, fcregr.target,
                    fcregr.train.inds, old.predicts.list, parset.list)
})
