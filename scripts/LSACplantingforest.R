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
load(file="C:/Users/3384022/Desktop/AL/Data to use/revision/imputed.data.RData")

#load the documentation with the selected variables
#selected_variables<-read_xlsx("Anita/LSAC documentation/selected_variables2.xlsx")
#final_selection<-read_xlsx("Anita/LSAC documentation/final_selection.xlsx")
final_selection<-read_xlsx("LSAC documentation/selected_variables1.xlsx")
final_selection<-subset(final_selection, !is.na(lvl1.varname))

#load the latent scores
load(file = "Outputs/LSACcreatingScales_revision/DV latent scores.RData")
load(file="Outputs/LSACcreatingScales_revision/predictor.scores.RData")
colnames(predictor.scores)<-gsub("[.]", "_", colnames(predictor.scores))

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

#put toghether  the single-item-predictors, latent-score-predictors and DV latent scores
df_anal<-cbind(single.items.df, predictor.scores[,-1], DVscores)
colnames(df_anal)<-gsub("[.]", "_", colnames(df_anal))
#remove out of home activities because no latent scores were computed
df_anal<-df_anal[,colnames(df_anal) != "out_of_home_activities"]


##summary tables-------------------------



# Latent scores data -----------------------
#Anita: Only numeric columns allowed in interaction terms. [from ranger()] => use only predictor with latent scores
# => include only latent score predictors
df_anal<-cbind(predictor.scores[,-1], DVscores)
colnames(df_anal)<-gsub("[.]", "_", colnames(df_anal))
#remove out of home activities because no latent scores were computed
df_anal<-df_anal[,colnames(df_anal) != "out_of_home_activities"]

#variable names of the DVs
DVs<-paste0("k", c(12,14,16), "_F")

# Preliminary variable selection ------------------------------------------

library(psych)
#analyze the number of underlying factors in the 5 waves?
# --> 1 component
fa.parallel(DVscores)
#perform PCA with 1 principal component
dv <- principal(DVscores, nfactors = 1)
# (a single) latent variable score on emotion regulation for each individual (across all waves?? how does this make sense?) -- is this kind of the random intercept?
dv$scores
#add overall emotion regulation latent score (across all waves) to the data and remove the latent scores of the 5 waves??
df_pres <- data.frame(dv$scores, df_anal[, -which(colnames(df_anal) %in% DVs)])
str(df_pres)
df_pres

library(ranger)
library(tuneRanger)
tunetask <- makeRegrTask(data = df_pres, target = "PC1")
res_tune <- tuneRanger(tunetask, num.trees = 1000, iters = 70, save.file.path = NULL)
#get the argument values to be put in the ranger() function below
res_tune$model$learner.model

## Set up interactions -----------------------
#27:parenting variables
parenting.vars<-grep("parenting", colnames(df_anal), value = TRUE)
#Error: Only numeric columns allowed in interaction terms.
interactions.mod<-paste("PC1~",paste(parenting.vars,"*.", collapse = " + "))

set.seed(57)
res <- ranger(as.formula(interactions.mod), data = df_pres, importance = "impurity_corrected", mtry = 66, min.node.size = 3, num.trees = 1000) #splitrule = "variance"
VI <- ranger::importance1_pvalues(res, method = "altmann", formula = as.formula(interactions.mod), data = df_pres)

save(VI, file="Outputs/plantingforest/VI (interactions).RData")
load("Outputs/plantingforest/VI (interactions).RData") #VI
VI

sum(VI[,2] < .05)
sum(VI[,2] < .01)

#preview of the important predictors or moderators
VI %>% 
  as.data.frame() %>% 
  rownames_to_column(var="var") %>%  
  arrange(-importance) %>% 
  filter(importance>10)

# #important variables and interactions based on pvalue<.05
# important<-
#   VI %>% 
#   as.data.frame() %>% 
#   rownames_to_column(var="var") %>%  
#   arrange(-importance) %>% 
#   filter(pvalue<0.05) 

#important variables and interactions based on pvalue<.01
important<-
  VI %>% 
  as.data.frame() %>% 
  rownames_to_column(var="var") %>%  
  arrange(-importance) %>% 
  filter(pvalue<0.01) 

#get a vector with (1)all important predictors on their own and (2)variables that are in an important interaction
important.vars<-str_split(important$var, ":", simplify = TRUE) %>% as.data.frame()
selected<-unique(c(important.vars$V1, important.vars$V2[important.vars$V2!=""]))
  
#df_anal before: ncol=87
#df_anal after: ncol=69
df_anal<-df_anal[, c(DVs, selected)]

#add interactions to the dataset
important.colnames<-important[grep(":", important$var),]$var
df_anal[,important.colnames]<-NA

important.interactions<-important.vars %>% subset(V2 != "")

for(i in 1:nrow(important.interactions)){
  df_anal[,69+i] <-df_anal[,important.interactions$V1[i]]*df_anal[,important.interactions$V2[i]]
}


#temporary reduction of the dataset to make sure the code runs
df_anal<-df_anal[,c(DVs,"SDQ_conduct_problems_p2:parenting_efficacy_p2","SDQ_conduct_problems_p2","pedsQL_social:parenting_disagree_relationship_p2",
                    "pedsQL_social", "parenting_disagree_relationship_p2","parenting_efficacy_p2:depressed_p2",
                    "parenting_efficacy_p2", "depressed_p2","SDQ_conduct_problems_p2:supportive_co_parenting_p2",
                    "pedsQL_emotional:parenting_warmth_p1", "pedsQL_emotional","parenting_warmth_p1"
                    )]
  
#change column names because ":" is not accepted in openMX
colnames(df_anal)<-gsub("[:]", "__x__", colnames(df_anal))

predvar <- names(df_anal)[!names(df_anal) %in% DVs]
length(predvar)


# lastsig <- order(VI[,1], decreasing = TRUE)
# lastsig <- max(which(VI[lastsig, 2] < .05))
# selected <- rownames(VI)[VI[, 2] < .05]
# p <- VarImpPlot(res, lastsig)
# p <- p + 
#   geom_point(data = data.frame(p$data, sig = (VI[,2] < .05)[as.character(p$data$Variable)]), aes(fill = sig), shape = 21) + 
#   scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "black"))+
#   theme(legend.position = "none")


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


#takes too long
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

cl<-makeCluster(2)
res_rf<-semtree::semforest(m0, data = df_anal, control = controls)
stopCluster(cl)

saveRDS(res_rf, "Outputs/plantingforest/rf_test1.RData")

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
