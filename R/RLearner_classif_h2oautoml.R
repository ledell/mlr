#' @export
makeRLearner.classif.h2o.automl = function() {
  makeRLearnerClassif(
    cl = "classif.h2o.automl",
    package = "h2o",
    par.set = makeParamSet(
      makeIntegerLearnerParam("max_models", lower = 1L, default = NULL),
      makeIntegerLearnerParam("max_runtime_secs", lower = 1L, default = 3600L),
      makeIntegerLearnerParam("seed", tunable = FALSE)
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob", "missings"),
    name = "H2O AutoML",
    short.name = "h2o.automl",
    note = '`validation_frame` and `leaderboard_frame` have been set to `training_frame` so that the full training frame is used for training (instead of automatically subsetting the training frame to create a validation frame and leaderboard frame).',
    callees = "h2o.automl"
  )
}

#' @export
trainLearner.classif.h2o.automl = function(.learner, .task, .subset, .weights = NULL, ...) {
  # check if h2o connection already exists, otherwise start one
  conn.up = tryCatch(h2o::h2o.getConnection(), error = function(err) return(FALSE))
  if (!inherits(conn.up, "H2OConnection")) {
    h2o::h2o.init()
  }
  y = getTaskTargetNames(.task)
  x = getTaskFeatureNames(.task)
  d = getTaskData(.task, subset = .subset)
  h2of = h2o::as.h2o(d)
  #distribution = ifelse(length(getTaskDesc(.task)$class.levels) == 2L, "bernoulli", "multinomial")
  # TO DO: Should we check that if two levels, that classification is always performed?
  # we could do this:
  # h2of[,y] = as.factor(h2of[,y])
  # TO DO: Should we overwrite project_name so that you can't add to an existing AutoML leaderboard?
  h2o::h2o.automl(y = y, x = x, training_frame = h2of, validation_frame = h2of, leaderboard_frame = h2of, ...)
}

#' @export
predictLearner.classif.h2o.automl = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  h2of = h2o::as.h2o(.newdata)
  p = h2o::h2o.predict(m, newdata = h2of, ...)
  p.df = as.data.frame(p)

  # check if class names are integers. if yes, colnames of p.df need to be adapted
  int = stri_detect_regex(p.df$predict, "^[[:digit:]]+$")
  pcol = stri_detect_regex(colnames(p.df), "^p[[:digit:]]+$")
  if (any(int) && any(pcol))
    colnames(p.df)[pcol] = stri_sub(colnames(p.df)[pcol], 2L)

  if (.learner$predict.type == "response") {
    return(p.df$predict)
  } else {
    p.df$predict = NULL
    return(as.matrix(p.df))
  }
}
