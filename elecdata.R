require(timeDate)
require(lubridate)
require(xts)

source("utils.R")

# set loacal time
Sys.setenv(TZ='UTC')
setRmetricsOptions(myFinCenter="UTC")

######################################
# load preprocessed data
load("elecdata.RData")

# other data preperation
all.data = rbind(ohlc.daily, test.ohlc.daily)
load.max = all.data$High

# cache computed data
if (!file.exists("daily_grammar_cache.RData")) {
    calendar.matrix = calendar_matrix(timeDate(index(load.max)), holidays) # precompute calendar matrix
    load.max.mra = GetMRA(load.max)  

    save(load.max.mra, calendar.matrix, file = "daily_grammar_cache.RData")
} else {
    load(file = "daily_grammar_cache.RData")
}

######################################
GetLoadGrammarEnv <- function() {

    grammar.env <- new.env()

    grammar.env$O = all.data$O
    grammar.env$H = all.data$H
    grammar.env$L = all.data$L
    grammar.env$C = all.data$C
 
    grammar.env$D1 = load.max.mra[,1]
    grammar.env$D2 = load.max.mra[,2]
    grammar.env$D3 = load.max.mra[,3]
    grammar.env$S = load.max.mra[,6]   

    grammar.env$temperature = temp.xts 

    return (grammar.env)
}

GetTrainingLoadGrammarEnv <- function(testRange) {

  grammar.env <- GetLoadGrammarEnv()
  testStart = index(first(grammar.env$H[testRange]))

  grammar.env$O <- grammar.env$O[index(grammar.env$O) < testStart]
  grammar.env$H <- grammar.env$H[index(grammar.env$H) < testStart]
  grammar.env$L <- grammar.env$L[index(grammar.env$L) < testStart]
  grammar.env$C <- grammar.env$C[index(grammar.env$C) < testStart]
  grammar.env$S <- grammar.env$S[index(grammar.env$S) < testStart]
  grammar.env$D1 <- grammar.env$D1[index(grammar.env$D1) < testStart]
  grammar.env$D2 <- grammar.env$D2[index(grammar.env$D2) < testStart]
  grammar.env$D3 <- grammar.env$D3[index(grammar.env$D3) < testStart]

  return (grammar.env)
}

AddToGrammarEnvironmet <- function(grammar.env, value) {

  ind = index(value)

  grammar.env$H = c(grammar.env$H, value)

  numind = which(index(grammar.env$H) == ind)
  new.mra = mra(as.numeric(grammar.env$H)[1:numind], n.levels=3, boundary="periodic", method="modwt")

  grammar.env$D1 = c(grammar.env$D1, xts(last(new.mra@D$D1), ind))
  grammar.env$D2 = c(grammar.env$D2, xts(last(new.mra@D$D2), ind))
  grammar.env$D3 = c(grammar.env$D3, xts(last(new.mra@D$D2), ind))
  grammar.env$S = c(grammar.env$S, xts(last(new.mra@S$S3), ind))

  return (grammar.env)
}

UpdateGrammarEnvironmet <- function(grammar.env, value) {

  ind = index(value)

  grammar.env$H[ind] = value

  numind = which(index(grammar.env$H) == ind)
  new.mra = mra(as.numeric(grammar.env$H)[1:numind], n.levels=3, boundary="periodic", method="modwt")

  grammar.env$D1[ind] = last(new.mra@D$D1)
  grammar.env$D2[ind] = last(new.mra@D$D2)
  grammar.env$D3[ind] = last(new.mra@D$D3)
  grammar.env$S[ind] = last(new.mra@S$S3)

  return (grammar.env)
}

daily.grammar.env = GetLoadGrammarEnv()