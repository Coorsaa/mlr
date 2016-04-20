context("regr_h2orandomForest")

test_that("regr_h2orandomForest", {
  requirePackages("h2o", default.method = "load")
  h2o::h2o.init()
  
  parset.list = list(
    list(seed = getOption("mlr.debug.seed")),
    list(seed = getOption("mlr.debug.seed"), ntrees = 10),
    list(seed = getOption("mlr.debug.seed"), ntrees = 10, mtries = 2),
    list(seed = getOption("mlr.debug.seed"), ntrees = 10, mtries = 4)
  )
  old.predicts.list = list()
  
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset,list(x = colnames(regr.train[, -regr.class.col]),
      y = regr.target, 
      training_frame = h2o::as.h2o(regr.train)))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(h2o::h2o.randomForest, parset)
    p  = predict(m, newdata = h2o::as.h2o(regr.test))
    old.predicts.list[[i]] = as.data.frame(p)[, 1L]
  }

  testSimpleParsets("regr.h2orandomForest", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)
})
