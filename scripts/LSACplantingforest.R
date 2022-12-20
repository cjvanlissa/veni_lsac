### Emotional Dysregulation # SEM-trees ###

########################################################################################################
# Over the following lines, code can be found in order grow SEM-trees or a SEM-forest                  #
# Important: Run  <creatingscales.R > first!                                                           #
########################################################################################################
library(worcs)
library(semtree)
library(semPlot)
library(lavaan)
library(tidySEM)
library(semtree)
library(metaforest)
library(future.apply)
library(yaml)
library(readxl)
library(tidyverse)
library(stringr)
library(psych)
library(tidyverse)
#All Data ------------------------------------------
#load data
#df_anal <- load_data(to_envir = FALSE)$df_anal
load(file="C:/Users/3384022/Desktop/AL/Data to use/revision/imputed.data_v5.RData")

#load the documentation with the selected variables
#selected_variables<-read_xlsx("Anita/LSAC documentation/selected_variables2.xlsx")
#final_selection<-read_xlsx("Anita/LSAC documentation/final_selection.xlsx")
final_selection<-read_xlsx("LSAC documentation/selected_variables1_v5.xlsx")
final_selection<-subset(final_selection, !is.na(lvl1.varname))

#load the latent scores
load(file = "Outputs/LSACcreatingScales/DVscores.RData")
load(file="Outputs/LSACcreatingScales/cfa.scores.RData")
predictor.scores<-cfa.scores
colnames(predictor.scores)<-gsub("[.]", "_", colnames(predictor.scores))

DVscores<-DVscores[,13:16]
# cfa.vars<-subset(final_selection,method=="CFA" | method=="log.model" & !is.na(lvl1.varname))
# cfa.vars<-cfa.vars$Variable.Name

#single-item predictors
single.items<-subset(final_selection,is.na(method) | method=="categorical" & !is.na(lvl1.varname))
single.items<-single.items$Variable.Name


#apply as.factor() to all categorical variables
facs<-final_selection[final_selection$method=="categorical" & !is.na(final_selection$method),]$Variable.Name
facs<-final_selection[final_selection$method=="categorical" | is.na(final_selection$method),]$Variable.Name

imp[facs] <- lapply(imp[facs], as.factor)

single.items.df<-imp[,colnames(imp) %in% single.items]


for(i in 1:ncol(single.items.df)){
  
  index<-which(colnames(single.items.df)[i]==final_selection$Variable.Name)
  colnames(single.items.df)[i]<-final_selection$lvl1.varname[index]
  
}

#put toghether  the single-item-predictors, latent-score-predictors and DV latent scores
df_anal<-cbind(single.items.df, predictor.scores[,-1], DVscores)
colnames(df_anal)<-gsub("[.]", "_", colnames(df_anal))
# #remove out of home activities because no latent scores were computed
# df_anal<-df_anal[,colnames(df_anal) != "out_of_home_activities"]


##summary tables-------------------------



# Latent scores data -----------------------
#Anita: Only numeric columns allowed in interaction terms. [from ranger()] => use only predictor with latent scores
# => include only latent score predictors
df_anal<-cbind(predictor.scores[,-1], DVscores)
colnames(df_anal)<-gsub("[.]", "_", colnames(df_anal))
#remove out of home activities because no latent scores were computed
df_anal<-df_anal[,colnames(df_anal) != "out_of_home_activities"]

#variable names of the DVs
DVs<-paste0("k", c(10,12,14,16), "_F")


# RI-CLPM-------------------------------------------------------------------------------------------------
#unconstrained model
RICLPM <- '
  # Create between components (random intercepts)
  RIx =~ 1*k10_parenting_consistent_mom + 1*k12_parenting_consistent_mom + 1*k14_parenting_consistent_mom + 1*k16_parenting_consistent_mom
  RIy =~ 1*k10_F + 1*k12_F + 1*k14_F + 1*k16_F
  
  # Create within-person centered variables
  wx1 =~ 1*k10_parenting_consistent_mom
  wx2 =~ 1*k12_parenting_consistent_mom
  wx3 =~ 1*k14_parenting_consistent_mom 
  wx4 =~ 1*k16_parenting_consistent_mom
  wy1 =~ 1*k10_F
  wy2 =~ 1*k12_F
  wy3 =~ 1*k14_F
  wy4 =~ 1*k16_F

  # Estimate lagged effects between within-person centered variables
  wx2 + wy2 ~ wx1 + wy1
  wx3 + wy3 ~ wx2 + wy2
  wx4 + wy4 ~ wx3 + wy3

  # Estimate covariance between within-person centered variables at first wave
  wx1 ~~ wy1 # Covariance
  
  # Estimate covariances between residuals of within-person centered variables 
  # (i.e., innovations)
  wx2 ~~wy2
  wx3 ~~ wy3
  wx4 ~~ wy4
  
  # Estimate variance and covariance of random intercepts
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy

  # Estimate (residual) variance of within-person centered variables
  wx1 ~~ wx1 # Variances
  wy1 ~~ wy1 
  wx2 ~~  wx2 # Residual variances
  wy2 ~~  wy2 
  wx3 ~~  wx3 
  wy3 ~~  wy3 
  wx4 ~~  wx4 
  wy4 ~~  wy4 
'


#constrained model
RICLPM.constr <- '
  # Create between components (random intercepts)
  RIx =~ 1*k10_parenting_consistent_mom + 1*k12_parenting_consistent_mom + 1*k14_parenting_consistent_mom + 1*k16_parenting_consistent_mom
  RIy =~ 1*k10_F + 1*k12_F + 1*k14_F + 1*k16_F
  
  # Create within-person centered variables
  wx1 =~ 1*k10_parenting_consistent_mom
  wx2 =~ 1*k12_parenting_consistent_mom
  wx3 =~ 1*k14_parenting_consistent_mom 
  wx4 =~ 1*k16_parenting_consistent_mom
  wy1 =~ 1*k10_F
  wy2 =~ 1*k12_F
  wy3 =~ 1*k14_F
  wy4 =~ 1*k16_F

  # Estimate lagged effects between within-person centered variables
  wx2 + wy2 ~ b2*wx1 + b1*wy1
  wx3 + wy3 ~ b2*wx2 + b1*wy2
  wx4 + wy4 ~ b2*wx3 + b1*wy3

  # Estimate covariance between within-person centered variables at first wave
  wx1 ~~ wp.cov*wy1 # Covariance
  
  # Estimate covariances between residuals of within-person centered variables 
  # (i.e., innovations)
  wx2 ~~ wp.cov*wy2
  wx3 ~~ wp.cov*wy3
  wx4 ~~ wp.cov*wy4
  
  # Estimate variance and covariance of random intercepts
  RIx ~~ RIx
  RIy ~~ RIy
  RIx ~~ RIy

  # Estimate (residual) variance of within-person centered variables
  wx1 ~~ wp.var*wx1 # Variances
  wy1 ~~ wp.var*wy1 
  wx2 ~~ wp.resvar*wx2 # Residual variances
  wy2 ~~ wp.resvar*wy2 
  wx3 ~~ wp.resvar*wx3 
  wy3 ~~ wp.resvar*wy3 
  wx4 ~~ wp.resvar*wx4 
  wy4 ~~ wp.resvar*wy4 
'
RICLPM.fit <- lavaan(RICLPM.constr,
                     data = df_anal, 
                     meanstructure = T, 
                     int.ov.free = T
)
summary(RICLPM.fit, standardized = T)


####################### Tuning parameters for semForest algorithm ################################

# setting some controls, current: default method, 1000 trees in forest
controls <- semforest.control()
controls$num.trees <- 5 # number of trees to grow
controls$sampling <- "bootstrap" # number of trees to grow
controls$mtry <- floor(sqrt(length(predvar)))
controls$semtree.control
controls$semtree.control$alpha <- 0.05
controls$semtree.control$min.N <- 50
controls$semtree.control$method <- "score"
controls$semtree.control$exclude.heywood <- TRUE
controls
library(parallel)
cl<-makeCluster(2)
res_rf<-semtree::semforest(RICLPM.fit, data = df_anal, control = controls)
stopCluster(cl)

saveRDS(res_rf, "Outputs/plantingforest/rf_test1.RData")




## NOT further adapted ## ----------------------------------------------------






set.seed(78326)
cl<-makeCluster(2) #change the 2 to your number of CPU cores
for(reps in 1:5){
  i = 1
  while(i < 2){
    res_rf <- try(semtree::semforest(m0, data = df_anal, control = controls))
    if(!inherits(res_rf, "try-error")) break
  }
  if(!inherits(res_rf, "try-error")) saveRDS(res_rf, paste0("Outputs/plantingforest/forest_test", reps, ".RData"))
}

parallel::stopCluster(cl)
rm(cl)

f <- list.files("Outputs/plantingforest", pattern = "^forest.+?RData$", full.names = T)
f <- lapply(f, readRDS)

res_rf <- f[[1]] #readRDS(f[which.max(dts)])
for(i in f[-1]){
  out <- try({merge(res_rf, i)})
  if(!inherits(out, "try-error")){
    res_rf <- out
  } else {
    cat("File ", i, " could not be merged.")
  }
}
res_rf


nullforests <- sapply(res_rf$forest, is.null)
res_rf$forest <- res_rf$forest[!nullforests]

saveRDS(res_rf, "Outputs/plantingforest/full_forest_test.RData")
res_rf <- readRDS("Outputs/plantingforest/full_forest_test.RData")


cl<-makeCluster(2)
vim <- semtree::varimp(res_rf)
parallel::stopCluster(cl)

saveRDS(vim, paste0("Outputs/plantingforest/vim_full_forest_test.RData"))
vim <- readRDS("Outputs/plantingforest/vim_full_forest_test.RData")

res_rf_VI <- list(variable.importance = semtree:::aggregateVarimp(vim, aggregate = "median", scale = "absolute", TRUE))
class(res_rf_VI) <- "ranger"
saveRDS(res_rf_VI, "Outputs/plantingforest/res_rf_VI_full_forest_test.RData")
res_rf_VI<-readRDS("Outputs/plantingforest/res_rf_VI_full_forest_test.RData")

metaforest::VarImpPlot(res_rf_VI)



# mxOption(model= NULL, key="Number of Threads", value=1)
# plan(multisession, workers = 10)
# res_rf <- par_forest(m0, data = df_anal[, c(paste0("de", 2:6), selected)], control = controls)
# 
# Change the Default settings in semforest.control() and semtree.control()
# set.seed(78326)
# cl<-makeCluster(2) #change the 2 to your number of CPU cores
# for(reps in 1:100){
#   i = 1
#   while(i < 20){
#     res_rf <- try(semtree::semforest(m0, data = df_anal[, c(paste0("de", 2:6), selected)], control = controls))
#     if(!inherits(res_rf, "try-error")) break
#     plan(multisession, workers = 10)
#   }
#   if(!inherits(res_rf, "try-error")) saveRDS(res_rf, paste0("forest_", reps, "_", Sys.time(), ".RData"))
# }
# 
# parallel::stopCluster(cl)
# rm(cl)
# 
# f <- list.files("results", pattern = "^forest.+?RData$", full.names = T)
# f <- lapply(f, readRDS)
# #dts <- as.Date(gsub("^forest_(.+?)\\.RData", "\\1", f))
# res_rf <- f[[1]]#readRDS(f[which.max(dts)])
# for(i in f[-1]){
#   out <- try({merge(res_rf, i)})
#   if(!inherits(out, "try-error")){
#     res_rf <- out
#   } else {
#     cat("File ", i, " could not be merged.")
#   }
# }
# nullforests <- sapply(res_rf$forest, is.null)
# res_rf$forest <- res_rf$forest[!nullforests]
# saveRDS(res_rf, "Outputs/plantingforest/full_forest.RData")
# res_rf <- readRDS("results/full_forest.RData")
# # vim <- varimp(res_rf)
# library(future)
# #plan(multisession, workers = 40)
# vim <- semtree::varimp(res_rf)
# saveRDS(vim, paste0("results/vim_", gsub("[: ]", "_", Sys.time()), ".RData"))
# vim <- readRDS("results/vim_2021-08-17_09_07_56.RData")
# VI <- list(variable.importance = semtree:::aggregateVarimp(vim, aggregate = "median", scale = "absolute", TRUE))
# class(VI) <- "ranger"
# metaforest::VarImpPlot(VI)
# 
# 
# # test <- semtree(m0, predictors=predvar[sample.int(length(predvar), 3)], control = NULL)
# # plot(predgrowthtree) # tree can be plotted
# # summary(predgrowthtree)
# 
# # RandomForest
# # may take a long time (previously on single core: 10 trees = 2.5 min, 100 trees = 23 min, 1000 trees = 3.5 hours)
# system.time(
#   predgrowthforest <- semforest(m0, data=df_anal, predictors=predvar, control=controls)
# )
# # summary(predgrowthforest)
# 
# 
# 
# saveRDS(predgrowthforest, "data/forestfile.RData")
# 

# Examine results ---------------------------------------------------------


#empty matrix..
M_dist <- semtree::proximity(res_rf)

dim(M_dist)


# # With full dataset -------------------------------------------------------
# 
# # setting some controls, current: default method, 1000 trees in forest
# controls <- semforest.control()
# controls$num.trees <- 10 # number of trees to grow
# controls$sampling <- "bootstrap" # number of trees to grow
# controls$mtry <- floor(sqrt(length(predvar)))
# controls$semtree.control
# controls$semtree.control$alpha <- 0.05
# controls$semtree.control$min.N <- 50
# controls$semtree.control$method <- "score"
# controls$semtree.control$exclude.heywood <- TRUE
# controls
# 
# mxOption(model= NULL, key="Number of Threads", value=1)
# plan(multisession, workers = 10)
# 
# for(reps in 1:5){
#   i = 1
#   while(i < 20){
#     res_rf <- try(semtree::semforest(m0, data = df_anal, control = controls))
#     if(!inherits(res_rf, "try-error")) break
#     plan(multisession, workers = 10)
#   }
#   if(!inherits(res_rf, "try-error")) saveRDS(res_rf, paste0("forest_all_", reps, "_", Sys.time(), ".RData"))
# }
# 
# #what is this?
# parallel::stopCluster(cl)
# rm(cl)
# 
# f <- list.files("results", pattern = "^forest.+?RData$", full.names = T)
# f <- lapply(f, readRDS)
# #dts <- as.Date(gsub("^forest_(.+?)\\.RData", "\\1", f))
# res_rf <- f[[1]]#readRDS(f[which.max(dts)])
# for(i in f[-1]){
#   out <- try({merge(res_rf, i)})
#   if(!inherits(out, "try-error")){
#     res_rf <- out
#   } else {
#     cat("File ", i, " could not be merged.")
#   }
# }
# nullforests <- sapply(res_rf$forest, is.null)
# res_rf$forest <- res_rf$forest[!nullforests]
# saveRDS(res_rf, "results/full_forest.RData")
# res_rf <- readRDS("results/full_forest.RData")
# # vim <- varimp(res_rf)
# library(future)
# plan(multisession, workers = 40)
# vim <- semtree::varimp(res_rf)
# saveRDS(vim, paste0("results/vim_", gsub("[: ]", "_", Sys.time()), ".RData"))
# vim <- readRDS("results/vim_2021-08-17_09_07_56.RData")
# VI <- list(variable.importance = semtree:::aggregateVarimp(vim, aggregate = "median", scale = "absolute", TRUE))
# class(VI) <- "ranger"
# metaforest::VarImpPlot(VI)
