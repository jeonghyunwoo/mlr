context("fcregr_bats")

test_that("fcregr_bats", {

  parset.list = list(
    list(),
    list(use.box.cox = TRUE),
    list(use.damped.trend = TRUE),
    list(use.arma.errors = TRUE, use.box.cox = TRUE),
    list(use.arma.errors = TRUE, use.box.cox = TRUE, use.trend = TRUE)
  )
  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(y = ts(fcregr.train, start = 1, frequency = 1L))
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output({
      m = do.call(forecast::bats, pars)
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
  testSimpleParsets("fcregr.bats", fcregr.xts, fcregr.target,
                    fcregr.train.inds, old.predicts.list, parset.list)
})
