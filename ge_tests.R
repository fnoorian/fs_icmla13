library(caret)

library(doMC)
registerDoMC(cores = 8)

source("elecdata.R")
source("gram_func.R")
source("eleceval.R")
source("utils.R")


##################################################################
# This part tests different kernels for the "MyHistWin(H, 7)" simple feature

Test_Leaners <- function() {
  feats = "MyHistWin(H, 7)"
  cat("\nTesting On:", feats, "\n")

  strat.tester <- function(trainDate, testDate) {
    rbind(EvaluteElectricity(feats, trainDate, testDate, "svmLinear"),
                   EvaluteElectricity(feats, trainDate, testDate, "svmPoly"),
                   EvaluteElectricity(feats, trainDate, testDate, "svmRadial"),
                    EvaluteElectricity(feats, trainDate, testDate, "krlsPoly"),
                   EvaluteElectricity(feats, trainDate, testDate, "krlsRadial"))
  }

  results = NULL

  trainDate = c("1998-01/1998-02")
  testDate = "1999-01"
  results = rbind(results, strat.tester(trainDate, testDate))

  ##################

  trainDate = c("1997-12/1998-01")
  testDate = "1998-12"
  results = rbind(results, strat.tester(trainDate, testDate))

  ##################

  trainDate = c("1997-11/1997-12")
  testDate = "1998-11"
  results = rbind(results, strat.tester(trainDate, testDate))

  r.jan = results[1:5, ]
  r.dec = results[1:5 + 5, ]
  r.nov = results[1:5 + 2*5, ]

  df1 = data.frame(r.jan$month, r.dec$month, r.nov$month)
  df1$avg.month = rowMeans(df1)
  df2 = data.frame(r.jan$day, r.dec$day, r.nov$day)
  df2$avg.month = rowMeans(df2)
  df = data.frame(r.jan$method, round(df1, 2), round(df2, 2))

  print("Different Learners Test on Single Feature Set:")
  print(df)

}

####################################################################
# this part test radial kernel KRLS on different simple Feature sets

Test_SimpleFeatures <- function() {

  specific.tester <- function(feats) {
    trainDate = c("1998-01/1998-02")
    testDate = "1999-01"
    r = EvaluteElectricity(feats, trainDate, testDate, "krlsRadial")

    trainDate = c("1997-12/1998-01")
    testDate = "1998-12"
    r = rbind(r, EvaluteElectricity(feats, trainDate, testDate, "krlsRadial"))

    trainDate = c("1997-11/1997-12")
    testDate = "1998-11"
    r = rbind(r, EvaluteElectricity(feats, trainDate, testDate, "krlsRadial"))

    day.avg = round(mean(r$day), 2)
    monthly.avg = round(mean(r$month), 2)

    return (list(feats = feats, 
                jan.month = round(r$month[1], 2),
                dec.month = round(r$month[2], 2),
                nov.month = round(r$month[3], 2),
                month.avg = monthly.avg,
                jan.day = round(r$day[1], 2), 
                dec.day = round(r$day[2], 2),
                nov.day = round(r$day[3], 2), 
                day.avg = day.avg))
  }

  gram = c("MyHistWin(H, 7)", "temperature")
  b0 = specific.tester(gram)

  gram = c("MyHistWin(H, 7)")
  b1 = specific.tester(gram)

  gram = c("D1", "D2", "D3", "S")
  b2 = specific.tester(gram)

  gram = c("H", "diff(H)", "diff(D1)", "diff(D2)", "diff(D3)", "diff(S)")
  b3 = specific.tester(gram)


  gram = c("H", "diff(H)", "MyHistWin(lag(diff(D1),1), 7)", "MyHistWin(lag(diff(D2),1), 7)",
                           "MyHistWin(lag(diff(D3),1), 7)", "MyHistWin(lag(diff(S),1), 7)")
  b4 = specific.tester(gram)


  print("Radial KRLS Test on 5 expert-designed Features:")
  print(rbind(data.frame(b0[-1]), 
        data.frame(b1[-1]), 
        data.frame(b2[-1]), 
        data.frame(b3[-1]), 
        data.frame(b4[-1])))
}

####################################################################

Test_OHLC <- function(feat.names) {
  
  load("ga_res_ohlc.RData")

  dt = NULL

  i = 0
  for (expr.set in  feat.names) {
    i = i + 1

    trainDate = c("1997-01/1997-03", "1997-10/1998-03", "1998-10/1998-12")
    testDate = "1999-01"
    r = EvaluteElectricity(expr.set, trainDate, testDate, "krlsRadial", use625 = TRUE, no.monthly = TRUE)

    trainDate = c("1997-01/1997-03", "1997-10/1998-03", "1998-10/1998-11")
    testDate = "1998-12"
    r = rbind(r, EvaluteElectricity(expr.set, trainDate, testDate, "krlsRadial", use625 = TRUE, no.monthly = TRUE))

    trainDate = c("1997-01/1997-03", "1997-10/1998-03", "1998-10")
    testDate = "1998-11"
    r = rbind(r, EvaluteElectricity(expr.set, trainDate, testDate, "krlsRadial", use625 = TRUE, no.monthly = TRUE))

    dt = rbind(dt, data.frame(i, month.jan = round(r$month[1], 2),
                                 month.dec = round(r$month[2], 2),
                                 month.nov = round(r$month[3], 2),
                                 month.avg=  round(mean(r$month), 2),
                                 day.jan = round(r$day[1], 2),
                                 day.dec = round(r$day[2], 2),
                                 day.nov = round(r$day[3], 2),
                                 day.avg=  round(mean(r$day), 2)))
  }

  rmean2 = function(x) round(mean(x), 2)

  tm = 1:nrow(dt)
  min.list = data.frame(i="min", min(dt$month.jan[tm]), min(dt$month.dec[tm]), min(dt$month.nov[tm]), min(dt$month.avg[tm]),
          min(dt$day.jan[tm]),   min(dt$day.dec[tm]),   min(dt$day.nov[tm]),   min(dt$day.avg[tm]))
  colnames(min.list) = colnames(dt)

  avg.list = data.frame(i="avg", rmean2(dt$month.jan[tm]), rmean2(dt$month.dec[tm]), rmean2(dt$month.nov[tm]), rmean2(dt$month.avg[tm]),
                                 rmean2(dt$day.jan[tm]),   rmean2(dt$day.dec[tm]),   rmean2(dt$day.nov[tm]),   rmean2(dt$day.avg[tm]))
  colnames(avg.list) = colnames(dt)


  max.list = data.frame(i="max", max(dt$month.jan[tm]), max(dt$month.dec[tm]), max(dt$month.nov[tm]), max(dt$month.avg[tm]),
                                 max(dt$day.jan[tm]),   max(dt$day.dec[tm]),   max(dt$day.nov[tm]),   max(dt$day.avg[tm]))
  colnames(max.list) = colnames(dt)

  dtx = rbind(dt, min.list, avg.list, max.list)

  print("OHLC Grammar Test:")
  print(dtx)
}


###############################
Test_WVLT <- function() {
  
  load("ga_res_wvlt.RData")

  dt = NULL

  i = 0
  for (expr.set in feat.names) {
    i = i + 1

    trainDate = c("1997-11/1998-01", "1998-10/1998-12")
    testDate = "1999-01"
    r = EvaluteElectricity(expr.set, trainDate, testDate, "krlsRadial", use625 = FALSE, no.monthly = FALSE)

    trainDate = c("1997-11/1998-01", "1998-10/1998-11")
    testDate = "1998-12"
    r = rbind(r, EvaluteElectricity(expr.set, trainDate, testDate, "krlsRadial", use625 = FALSE, no.monthly = FALSE))

    trainDate = c("1997-11/1998-01", "1998-10")
    testDate = "1998-11"
    r = rbind(r, EvaluteElectricity(expr.set, trainDate, testDate, "krlsRadial", use625 = FALSE, no.monthly = FALSE))

    dt = rbind(dt, data.frame(i, month.jan = round(r$month[1], 2),
                                 month.dec = round(r$month[2], 2),
                                 month.nov = round(r$month[3], 2),
                                 month.avg=  round(mean(r$month), 2),
                                 day.jan = round(r$day[1], 2),
                                 day.dec = round(r$day[2], 2),
                                 day.nov = round(r$day[3], 2),
                                 day.avg=  round(mean(r$day), 2)))
  }

  rmean2 = function(x) round(mean(x), 2)

  tm = 1:nrow(dt)
  min.list = data.frame(i="min", min(dt$month.jan[tm]), min(dt$month.dec[tm]), min(dt$month.nov[tm]), min(dt$month.avg[tm]),
          min(dt$day.jan[tm]),   min(dt$day.dec[tm]),   min(dt$day.nov[tm]),   min(dt$day.avg[tm]))
  colnames(min.list) = colnames(dt)

  avg.list = data.frame(i="avg", rmean2(dt$month.jan[tm]), rmean2(dt$month.dec[tm]), rmean2(dt$month.nov[tm]), rmean2(dt$month.avg[tm]),
                                 rmean2(dt$day.jan[tm]),   rmean2(dt$day.dec[tm]),   rmean2(dt$day.nov[tm]),   rmean2(dt$day.avg[tm]))
  colnames(avg.list) = colnames(dt)


  max.list = data.frame(i="max", max(dt$month.jan[tm]), max(dt$month.dec[tm]), max(dt$month.nov[tm]), max(dt$month.avg[tm]),
                                 max(dt$day.jan[tm]),   max(dt$day.dec[tm]),   max(dt$day.nov[tm]),   max(dt$day.avg[tm]))
  colnames(max.list) = colnames(dt)

  dtx = rbind(dt, min.list, avg.list, max.list)

  print("Wavelet Grammar Test:")
  print(dtx)
}

##############################
Test_Leaners()
Test_SimpleFeatures()
Test_OHLC()
Test_WVLT()
