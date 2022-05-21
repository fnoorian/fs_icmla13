EvaluateExpressions <- function(expr.set, grammar.env) {
  # evaluates the value of expression collection in the grammar environment
  # also replaces bad values with 0 and NA

  LEN_THRES = 50

  # Get all the terminal features only
  feat.set = c()
  for (expr in expr.set) {
      if (nchar(expr) > LEN_THRES) next # skip too long features   

      calc.feat = eval(parse(text = expr), envir = grammar.env)
      feat.set = cbind(feat.set, calc.feat)
  }

  # cleanup and rename features
  feat.set[is.infinite(feat.set)] = 0
  feat.set[is.nan(feat.set)] = 0
  feat.set <- na.omit(feat.set) # remove NAs
  feat.set <- feat.set[,!duplicated(t(feat.set))] # remove duplicate columns
  feat.set <- feat.set[,which(!apply(feat.set, 2, FUN = function(x){!length(unique(x))>1}))] # remove const data
  colnames(feat.set) <- paste("G", 1:ncol(feat.set), sep = ".")   # set column names

  return (feat.set)
}

GetElectricityFeatures <- function(expr.set, grammar.env) {

  # evaluate the expression object
  feat.set = EvaluateExpressions(expr.set, grammar.env)
  if (ncol(feat.set) == 0 || nrow(feat.set) == 0)
      return (NULL)

  # create feature matrix
  feat.matrix <- cbind(load.max, calendar.matrix, lag(feat.set,1)) 
  colnames(feat.matrix)[1] = "Target"
  feat.matrix <- na.omit(feat.matrix)

  return (feat.matrix)
}

EvaluteElectricity <- function(expr.set, trainDate, testDate, learner, use625 = FALSE, no.monthly = FALSE, verbose=FALSE) {

  if (verbose == TRUE)
    verbose <- cat
  else
    verbose <- function(...) {}

  verbose(learner, ": Train On: ", trainDate, "\n")
  verbose("Test On: ", testDate, "\n")


  feat.matrix = GetElectricityFeatures(expr.set, daily.grammar.env)

  train.feats = feat.matrix[trainDate]
  test.feats = feat.matrix[testDate]

  fitControl <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 5)

  tuneGrid = createGrid(method=learner, len = 10, data = NULL)

  if (learner == "svmPoly")
    tuneGrid = createGrid(method=learner, len = 5, data = NULL)

  if (use625) {
    tuneGrid = expand.grid(.lambda = 0.0625, .sigma = 256)
    fitControl = trainControl(method = "cv", number = 2)
  }

  if (learner == "svmRadial") {
    mdl <- train(Target ~ ., train.feats,
               preProc = c("center", "scale"), 
               method = learner,
               tuneGrid = tuneGrid,
               trControl = fitControl,
               epsilon = 0.5)
  } else {
    mdl <- train(Target ~ ., train.feats,
               preProc = c("center", "scale"), 
               method = learner,
               tuneGrid = tuneGrid,
               trControl = fitControl)
  }

  dayahead.pred = predict(mdl, test.feats)

  res.mape.dayahead = mape(obs=test.feats$Target, pred=dayahead.pred)

  verbose("DayAhead:", round(res.mape.dayahead, 2), "\n")

  if (no.monthly) {
    return (data.frame(method=learner, 
                    trainDate = do.call(function(...) paste(..., sep="+"), as.list(trainDate)),
                    testDate = testDate,
                    day=res.mape.dayahead, 
                    month=0))
  }

  ##########################################
  monthly.grammar.env = GetTrainingLoadGrammarEnv(testDate)

  # the training data
  feat.matrix = GetElectricityFeatures(expr.set, monthly.grammar.env)

  # train on the training data
  train.feats = feat.matrix[trainDate]

  ##
  # Month ahead prediction
  test.data = load.max[testDate]
  days.list = index(test.data)

  results = NULL
  for (i in 1:length(days.list)) {
      current.day = days.list[i]

      current_day_dummy_load = xts(-1e9, current.day)
      monthly.grammar.env = AddToGrammarEnvironmet(monthly.grammar.env, current_day_dummy_load)

      feats = GetElectricityFeatures(expr.set, monthly.grammar.env)
      test.feats = feats[current.day]

      pred = predict(mdl, test.feats)

      pred = xts(pred, current.day)
      results = rbind(results, pred)

      monthly.grammar.env = UpdateGrammarEnvironmet(monthly.grammar.env, pred)
  }

  res.mape.monthahead = mape(obs=test.data, pred=results)
  verbose("MonthAhead:", round(res.mape.monthahead, 2), "\n")

  return (data.frame(method=learner, 
                    trainDate = do.call(function(...) paste(..., sep="+"), as.list(trainDate)),
                    testDate = testDate,
                    day=res.mape.dayahead, 
                    month=res.mape.monthahead))
}
