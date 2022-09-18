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

#Data ------------------------------------------
#load data
#df_anal <- load_data(to_envir = FALSE)$df_anal
load(file="C:/Users/3384022/Desktop/AL/Data to use/revision/imputed.data.RData")

#load the documentation with the selected variables
#selected_variables<-read_xlsx("Anita/LSAC documentation/selected_variables2.xlsx")
#final_selection<-read_xlsx("Anita/LSAC documentation/final_selection.xlsx")
final_selection<-read_xlsx("Anita/LSAC documentation/revision/selected_variables1.xlsx")
final_selection<-subset(final_selection, !is.na(lvl1.varname))

#load the latent scores
load(file = "Anita/Outputs/LSACcreatingScales_revision/DV latent scores.RData")
load(file="Anita/Outputs/LSACcreatingScales_revision/predictor.scores.RData")
DVscores<-DVscores[,10:12]
# cfa.vars<-subset(final_selection,method=="CFA" | method=="log.model" & !is.na(lvl1.varname))
# cfa.vars<-cfa.vars$Variable.Name

#single-item predictors
single.items<-subset(final_selection,is.na(method) | method=="categorical" & !is.na(lvl1.varname))
single.items<-single.items$Variable.Name


#apply as.factor() to all categorical variables
facs<-final_selection[final_selection$method=="categorical" & !is.na(final_selection$method),]$Variable.Name
imp[facs] <- lapply(imp[facs], as.factor)

single.items.df<-imp[,colnames(imp) %in% single.items]


for(i in 1:ncol(single.items.df)){
  
  index<-which(colnames(single.items.df)[i]==final_selection$Variable.Name)
  colnames(single.items.df)[i]<-final_selection$lvl1.varname[index]
  
}

#put toghether  the single-item predictors in the same dataset as the predictor and DV latent scores
df_anal<-cbind(single.items.df, predictor.scores[,-1], DVscores)
colnames(df_anal)<-gsub("[.]", "_", colnames(df_anal))
#remove out of home activities because no latent scores were computed
df_anal<-df_anal[,colnames(df_anal) != "out_of_home_activities"]

#variable names of the DVs
DVs<-paste0("k", c(12,14,16), "_F")

# Preliminary variable selection ------------------------------------------

library(psych)
#analyze the number of underlying factors in the 5 waves?
# --> 1 factor
fa.parallel(DVscores)
#perform PCA with 1 principal component
dv <- principal(DVscores, nfactors = 1)
# (a single) latent variable score on emotion regulation for each individual (across all waves?? how does this make sense?) -- is this kind of the random intercept?
dv$scores
#add overall emotion regulation latent score (across all waves) to the data and remove the latent scores of the 5 waves??
df_pres <- data.frame(dv$scores, df_anal[, -which(colnames(df_anal) %in% DVs)])

library(ranger)
library(tuneRanger)
tunetask <- makeRegrTask(data = df_pres, target = "PC1")
res_tune <- tuneRanger(tunetask, num.trees = 1000, num.threads = 40, iters = 70, save.file.path = NULL)
#get the argument values to be put in the ranger() function below
res_tune$model$learner.model


set.seed(57)
res <- ranger(PC1~., data = df_pres, importance = "impurity_corrected", mtry = 66, min.node.size = 3, num.trees = 1000, splitrule = "variance")
VI <- ranger::importance_pvalues(res, method = "altmann", formula = PC1~., data = df_pres)
sum(VI[,2] < .05)
lastsig <- order(VI[,1], decreasing = TRUE)
lastsig <- max(which(VI[lastsig, 2] < .05))
selected <- rownames(VI)[VI[, 2] < .05]
p <- VarImpPlot(res, lastsig)
p <- p + 
  geom_point(data = data.frame(p$data, sig = (VI[,2] < .05)[as.character(p$data$Variable)]), aes(fill = sig), shape = 21) + 
  scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "black"))+
  theme(legend.position = "none")


# SEM forest --------------------------------------------------------------

# we use latent scores as "observed variables" - is that okay/customary?
basicgrowth <- lavaan::growth(
"
i =~ 1*k12_F + 1*k14_F + 1*k16_F 
s =~ 0*k12_F + 1*k14_F + 2*k16_F
q =~ 0*k12_F + 1*k14_F + 4*k16_F
" , data = df_anal)
#not identified


write_yaml(as.list(fitmeasures(basicgrowth)), "fitmeasures_growth.yml")

# Specifying model linear growth curve model using the openMX code (should be the same model as the lavaan model)
m0 <- as_ram("
#fix factor loadings
i =~ 1*k12_F + 1*k14_F + 1*k16_F 
s =~ 0*k12_F + 1*k14_F + 2*k16_F
q =~ 0*k12_F + 1*k14_F + 4*k16_F

#latent means: freely estimated
i ~ meani*1
s ~ means*1
q ~ meanq*1

#residual variances theta: fixed to be equal across waves
k12_F ~~ vres*k12_F
k14_F ~~ vres*k14_F
k16_F ~~ vres*k16_F

#intercepts nu: fix to 0?
k12_F   ~ 0*1
k14_F  ~ 0*1
k16_F  ~ 0*1


#covariances between latent variabels: fixed to 0?
i ~~ 0*s
i ~~ 0*q
s ~~ 0*q
i ~~ 0*i
s ~~ 0*s
q ~~ 0*q")

m0 <- run_mx(m0, data = df_anal)
table_results(m0)
table_fit(m0)
# fitting a single tree using the model

# system.time(
#   predgrowthtree <- semtree(m0, data = df_anal, control = semtree.control())
# ) # 856.14

#cl <- makeCluster(40)  # cluster of 2 CPUs created, in parallel
# A cluster can be stopped when not needed.
# Note – if you want to restart the cluster,
# you must use the sfClusterEval functions to load the packages for use with the new cluster.
# 
# stopCluster(cl)
# A SEM Forest can be grown in parallel by specifying the cluster to be used:
#   


####################### Tuning parameters for semForest algorithm ################################

# setting some controls, current: default method, 1000 trees in forest
controls <- semforest.control()
controls$num.trees <- 10 # number of trees to grow
controls$sampling <- "bootstrap" # number of trees to grow
controls$mtry <- floor(sqrt(length(predvar)))
controls$semtree.control
controls$semtree.control$alpha <- 0.05
controls$semtree.control$min.N <- 50
controls$semtree.control$method <- "score"
controls$semtree.control$exclude.heywood <- TRUE
controls

mxOption(model= NULL, key="Number of Threads", value=1)
plan(multisession, workers = 10)
res_rf <- par_forest(m0, data = df_anal[, c(paste0("de", 2:6), selected)], control = controls)
# Change the Default settings in semforest.control() and semtree.control()
# set.seed(78326)
# cl<-makeCluster(10) #change the 2 to your number of CPU cores
for(reps in 1:100){
  i = 1
  while(i < 20){
    res_rf <- try(semtree::semforest(m0, data = df_anal[, c(paste0("de", 2:6), selected)], control = controls))
    if(!inherits(res_rf, "try-error")) break
    plan(multisession, workers = 10)
  }
  if(!inherits(res_rf, "try-error")) saveRDS(res_rf, paste0("forest_", reps, "_", Sys.time(), ".RData"))
}

parallel::stopCluster(cl)
rm(cl)

f <- list.files("results", pattern = "^forest.+?RData$", full.names = T)
f <- lapply(f, readRDS)
#dts <- as.Date(gsub("^forest_(.+?)\\.RData", "\\1", f))
res_rf <- f[[1]]#readRDS(f[which.max(dts)])
for(i in f[-1]){
  out <- try({merge(res_rf, i)})
  if(!inherits(out, "try-error")){
    res_rf <- out
  } else {
    cat("File ", i, " could not be merged.")
  }
}
nullforests <- sapply(res_rf$forest, is.null)
res_rf$forest <- res_rf$forest[!nullforests]
saveRDS(res_rf, "results/full_forest.RData")
res_rf <- readRDS("results/full_forest.RData")
# vim <- varimp(res_rf)
library(future)
plan(multisession, workers = 40)
vim <- semtree::varimp(res_rf)
saveRDS(vim, paste0("results/vim_", gsub("[: ]", "_", Sys.time()), ".RData"))
vim <- readRDS("results/vim_2021-08-17_09_07_56.RData")
VI <- list(variable.importance = semtree:::aggregateVarimp(vim, aggregate = "median", scale = "absolute", TRUE))
class(VI) <- "ranger"
metaforest::VarImpPlot(VI)

# test <- semtree(m0, predictors=predvar[sample.int(length(predvar), 3)], control = controls)
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


# Examine results ---------------------------------------------------------

M_dist <- semtree::proximity(res_rf)



# With full dataset -------------------------------------------------------

# setting some controls, current: default method, 1000 trees in forest
controls <- semforest.control()
controls$num.trees <- 10 # number of trees to grow
controls$sampling <- "bootstrap" # number of trees to grow
controls$mtry <- floor(sqrt(length(predvar)))
controls$semtree.control
controls$semtree.control$alpha <- 0.05
controls$semtree.control$min.N <- 50
controls$semtree.control$method <- "score"
controls$semtree.control$exclude.heywood <- TRUE
controls

mxOption(model= NULL, key="Number of Threads", value=1)
plan(multisession, workers = 10)

for(reps in 1:100){
  i = 1
  while(i < 20){
    res_rf <- try(semtree::semforest(m0, data = df_anal, control = controls))
    if(!inherits(res_rf, "try-error")) break
    plan(multisession, workers = 10)
  }
  if(!inherits(res_rf, "try-error")) saveRDS(res_rf, paste0("forest_all_", reps, "_", Sys.time(), ".RData"))
}

parallel::stopCluster(cl)
rm(cl)

f <- list.files("results", pattern = "^forest.+?RData$", full.names = T)
f <- lapply(f, readRDS)
#dts <- as.Date(gsub("^forest_(.+?)\\.RData", "\\1", f))
res_rf <- f[[1]]#readRDS(f[which.max(dts)])
for(i in f[-1]){
  out <- try({merge(res_rf, i)})
  if(!inherits(out, "try-error")){
    res_rf <- out
  } else {
    cat("File ", i, " could not be merged.")
  }
}
nullforests <- sapply(res_rf$forest, is.null)
res_rf$forest <- res_rf$forest[!nullforests]
saveRDS(res_rf, "results/full_forest.RData")
res_rf <- readRDS("results/full_forest.RData")
# vim <- varimp(res_rf)
library(future)
plan(multisession, workers = 40)
vim <- semtree::varimp(res_rf)
saveRDS(vim, paste0("results/vim_", gsub("[: ]", "_", Sys.time()), ".RData"))
vim <- readRDS("results/vim_2021-08-17_09_07_56.RData")
VI <- list(variable.importance = semtree:::aggregateVarimp(vim, aggregate = "median", scale = "absolute", TRUE))
class(VI) <- "ranger"
metaforest::VarImpPlot(VI)
