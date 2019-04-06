library(tidyverse)
library(MuMIn)
library(caret)
library(BayesVarSel)

# check data
mtcars %>% tidyr::gather(variable, value) %>% 
  ggplot(aes(x = value)) + geom_histogram(bins = 10) + 
  facet_wrap(~variable, scales = 'free_x')

# function to center x variables to make intercepts interpretable
center_colmeans <- function(x) { xcenter = colMeans(x, na.rm=TRUE)
                                 x - rep(xcenter, rep.int(nrow(x), ncol(x)))}
varsToCenter = c("disp","hp","drat","wt","qsec")
mtcars[varsToCenter] = center_colmeans(mtcars[varsToCenter])

# convert certain columns to factors
cols <- c("cyl", "vs", "am", "gear", "carb")
mtcars <- mtcars %>% mutate_at(cols, funs(factor(.))) # or ordered

# look at correlations
library(corrplot)
nums <- unlist(lapply(mtcars, is.numeric)) 
dfnums <- mtcars[,nums]
corrplot(cor(dfnums, use = "pairwise.complete.obs"), type = "upper", diag = FALSE,
         tl.col = "black", tl.srt = 45)

# check normality
mtcars %>% ggplot(aes(x = mpg)) + geom_density(adjust = .6)
shapiro.test(mtcars$mpg)

# k fold cv function for GLM
kcv_functionGLM = function(form,df,y){
  for(i in 1:k){
    valIndices <- which(folds==i, arr.ind=TRUE)
    train <- df[-valIndices, ]
    model = glm(form, family=gaussian(link="identity"), data=train)
    validation <- df[valIndices, ]
    pred[[i]] = predict(model, validation, allow.new.levels = TRUE)
  }
  return(sqrt(mean((unlist(pred)-y)^2))) # rmse
}

# find best model according to AICc
globalmodel <- lm(mpg ~ ., data = mtcars, na.action = na.pass)
combinations <- dredge(globalmodel)
head(combinations,10)
(winner <- get.models(combinations, subset = 1)[[1]]$call$formula)

# find best model according to leave one out cross validation
modelsm = list()
k = nrow(mtcars) # loo, or change to k fold
system.time(
  for(m in 1:50){ # number of top models to consider
    pred = list()
    mtcars <- mtcars[sample(nrow(mtcars)),]
    folds <- cut(seq(1,nrow(mtcars)),breaks=k,labels=FALSE)
    winner <- get.models(combinations, subset = m)[[1]]$call$formula
    modelsm[[m]] = kcv_functionGLM(winner,mtcars,mtcars$mpg)
  })
unlist(modelsm) # these are rmse for all the top models
get.models(combinations, subset = which.min(unlist(modelsm)))[[1]]$call$formula # this is best model acccording to loo cross-validation

#find best model according to Bayesian variable selection
bvs <- Bvs(formula = "mpg ~ .", data = mtcars) #can use n.keep, pBvs for parallel, GibbsBvs for p>25
bvs

# run recursive feature extraction in caret
control <- rfeControl(method="LOOCV")
system.time(
  results <- rfe(mpg ~ ., data=mtcars, method="lm", sizes=c(1:10), rfeControl=control)
)  
results
plot(results)
varImp(results)
results$optVariables # this is best model according to caret's rfe

