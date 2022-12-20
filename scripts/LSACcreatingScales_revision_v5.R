library(dplyr)
library(tidySEM)
library(lavaan)
library(psych)
library(ggplot2)
library(semTools)
library(readxl)
library(worcs)
library(stringr)
library(semPlot)
library(writexl)



#Function definitions --------------------------------------------------------
#a function to subset a dataset by its variable labels
sublab<-function(data, #a dataframe to be subsetted
                 labels #a vector with variable labels to keep in the data
){
  data<-data[,attr(data, "variable.labels") %in% labels]
  attr(data, "variable.labels")<-labels
  return(data)
}


# Load data & documentation ------------------
#load the imputed dataset
#load(file="C:/Users/3384022/Desktop/AL/Data to use/imputed.data.RData")
load(file="C:/Users/3384022/Desktop/AL/Data to use/revision/imputed.data_v5.RData")

#load hte documentation with the selected variables
#selected_variables<-read_xlsx("Anita/LSAC documentation/selected_variables2.xlsx")
#final_selection<-read_xlsx("Anita/LSAC documentation/final_selection.xlsx")
final_selection<-read_xlsx("LSAC documentation/selected_variables1_v5.xlsx")
#categorical CFA (i.e. ordered=TRUE) on dichotomous items is equivalent to logistic model =>  use ordered CFA for all scales
final_selection[final_selection$method=="log.model" & !is.na(final_selection$method),]$method<-"CFA"

#Recode: reverse coding
to.recode<-subset(final_selection,Note=="recode")
#to.recode<-subset(final_selection,Note=="recode" & !lvl1.varname %in% c("parenting.involvement.p1", "parenting.involvement.p2"))

recode.vars<-to.recode$Variable.Name

#reverse code variables
for(i in recode.vars){
  imp[,i] <-  max(imp[,i])+1 - imp[,i]
  if(min(imp[,i])<1){
    message(paste("Warning: Non positive levels.Check variable", i))
  }
}

#_____________________________________________________________________________________________________
## 1-CFA + conditional mod indices----------------------------------------------
#_____________________________________________________________________________________________________


#variables for which factor scores will be computed based on a single level factor analysis
cfa<-subset(final_selection,method=="CFA" & is.na(lvl2.varname) & !is.na(lvl1.varname))

#cfa<-subset(cfa, !lvl1.varname %in% c("parenting.involvement.p1","parenting.involvement.p2"))
cfa.vars<-unique(cfa$lvl1.varname)

#remove constructs with less than 3 observed variables (otherwise error in CFA)
freqs<-table(cfa$lvl1.varname) %>% as.data.frame()
to.remove<-freqs[freqs$Freq<3,]$Var1
cfa.vars<-cfa.vars[!cfa.vars %in% to.remove]

#The following loop runs a one factor CFA on each scale
#The output is the fit.cfa table which contains model fit measures, and alpha and omega as measures of reliability
# note that alpha is reported only for comparison but is inappropriate to use because its assumptions are violated for 
#most of the scales (uncorrelated errors, equal factor loadings)

set.seed(45898)
#table in which the fit measures will be saved for each model
fit.cfa<-data.frame(variable=cfa.vars,
                    CFI = NA,
                    RMSEA = NA,
                    SRMR = NA,
                    # communality=NA,
                    # prop.expl.var.by.F=NA,
                    # prop.expl.var.by.Y = NA,
                    omega_cat=NA,
                    alpha=NA,
                    n.mods=NA
)


fit<-list()
formula<-c()

cfa.scores<-matrix(NA, nrow = nrow(imp), ncol = length(cfa.vars)+1)
colnames(cfa.scores)<-c("ID", cfa.vars)
cfa.scores[,1]<-imp$hicid

weights<-list()
i<-2
#perform one factor cfa on each scale
for(i in 1:length(cfa.vars)){  #length(cfa.vars)
  print(paste("Iteration", i))
  
  #get the variables names of the belonging items
  varnames<-cfa[cfa$lvl1.varname==cfa.vars[i]  ,]$Variable.Name
  varlabels<-final_selection[final_selection$Variable.Name %in% varnames,]$Variable.Label
  varlabels<-sub(".*-", "", varlabels)
  names(varnames)<-varlabels
  #subset the dataset and standardize the variables
  df.vars<-scale(imp[,colnames(imp) %in% varnames])
  
  #build and fit the model
  formula[i]<-paste0(cfa.vars[i], " =~  ", paste0(colnames(df.vars), collapse = " +  "))
  fit[[i]]<-cfa(model = formula[i],imp, std.lv=TRUE, ordered=TRUE)
  
  # some models do not have a proper solution after the application of modification indices and shouldn"t be used
  # if cfa() throws a warning tt.mod will have value TRUE, otherwise it will be a normal lavaan object
  tt <- tryCatch(cfa(model = formula[i],imp, std.lv=TRUE, ordered=TRUE ),
                 error=function(e) message("Error"),
                 warning=function(w) {
                   message(w)
                   TRUE
                 }
  )
  
  if(is.logical(tt)){
    print(paste("Iteration:", i,"Main model: WARNING!"))
  }
  
  #save the model fit indicators
  fit.cfa[fit.cfa$variable==cfa.vars[i],c("CFI", "RMSEA", "SRMR")]<-round(fitmeasures(fit[[i]])[c("cfi","rmsea", "srmr")], digits=2)
  
  if(fit.cfa$CFI[i]>.90 & fit.cfa$RMSEA[i]<.08 & fit.cfa$SRMR[i]<.08 ){
    #save the factor scores
    scores<-lavPredict(fit[[i]], method = "EBM", fsm = TRUE)
    cfa.scores[,i+1]<-scores
    # #weights used to calculate the factor scores
    # b<-attr(scores, "fsm")[[1]][1:length(attr(scores, "fsm")[[1]])]
    # 
    # #Estimated model matrices
    # LAMBDA<-inspect(fit[[i]], "std")$lambda
    # THETA<-inspect(fit[[i]], "std")$theta
    # PSI<-inspect(fit[[i]], "std")$psi
    # SIGMA<-LAMBDA%*%PSI%*%t(LAMBDA)+THETA
    
    # #communality = sum(squared(loadings))
    #  communality<-sum(LAMBDA^2)
    # #Proportion of variance the common factor explains in the model implied cor matrix =
    # # = sum(squared(loadings))/sum(var(item))
    # prop.expl.var.by.F<- sum(LAMBDA^2)/sum(diag(SIGMA)) 
    # 
    # 
    # #communality according to Dave...
    # # = proportion of variance of the test score Y explained by the common factor (lower bound to the reliability of Y)
    # prop.expl.var.by.Y<-t(b)%*%LAMBDA%*%PSI%*%t(LAMBDA)%*%b/t(b)%*%SIGMA%*%b
    # 
    # fit.cfa[i,c("SSloadings", "prop.expl.var.by.F", "prop.expl.var.by.Y")] <- c(SSloadings, prop.expl.var.by.F, prop.expl.var.by.Y)
    # fit.cfa$prop.expl.var.by.Y[i]<-round(prop.expl.var.by.Y , digits=2)
    
    #save reliability estimates
    fit.cfa$omega_cat[i] <- round(semTools::reliability(fit[[i]])["omega2",],digits=2)
    fit.cfa$alpha[i]<-round(psych::alpha(df.vars,check.keys=TRUE)$total$std.alpha , digits=2)
    #indicate that no modifications were applied
    fit.cfa$n.mods[i] <- 0
    
  }else{
    
    modification<-c()
    modind<-modindices(fit[[i]],sort = TRUE, maximum.number = 1,minimum.value = 5)
    
    #if there are no modification indices skip this loop and go to the next variable
    if(dim(modind)[1]==0){
      break
      #else if there are mod indices apply them one at a time and refit the model every time
      #until either there are no more modification indices or the maximum number of modifications is reached (n.mods)
    }else{
      #paste toghether the modifications from each iteration, such that they accumulate over the iterations
      modification<-c(modification,paste0(modind$lhs, modind$op,modind$rhs))
      #refit the model with the modifications
      fit[[i]]<-cfa(model = c(formula[i],modification),imp, std.lv=TRUE, ordered=TRUE)
      
      # some models do not have a proper solution after the application of modification indices and shouldn"t be used
      # if cfa() throws a warning tt.mod will have value TRUE, otherwise it will be a normal lavaan object
      tt.mod <- tryCatch(cfa(model = c(formula[i],modification),imp, std.lv=TRUE),
                         error=function(e) e,
                         warning=function(w) {
                           message(w)
                           TRUE
                         }
      )
      #so if tt.mod is logical, i.e. there was a warning in the cfa() call =>break out of the j-loop and move to the next variable
      #the final model fit and factor scores are in this case the ones from the previous iteration or before modificaiton indices were applied
      if(is.logical(tt.mod)){
        print(paste("Iteration:", i,"Modification WARNING!"))
        #if there was no warning in the model => overwrite the model fit and the factor scores
      }else{
        #save the factor scores of the modified model
        scores<-lavPredict(fit[[i]], method = "EBM", fsm = TRUE)
        cfa.scores[,i+1]<-scores
        
        #save the fit measures of the model after application of the modification
        fit.cfa[fit.cfa$variable==cfa.vars[i],c("CFI", "RMSEA", "SRMR")]<-round(fitmeasures(fit[[i]])[c("cfi","rmsea", "srmr")], digits=2)
        fit.cfa$omega_cat[i] <- round(semTools::reliability(fit[[i]])["omega2",],digits=2)
        #this is the number of modifications(= iterations) undertaken
        fit.cfa$n.mods[i] <- 1
        
      }
      
    }
  } #end if for mods
  
} #end CFA conditional mods


#poor fit
poor.fit<-fit.cfa[fit.cfa$CFI<.90 | fit.cfa$RMSEA>.08 | fit.cfa$SRMR>.08 ,]

#good fit
good.fit<-fit.cfa[fit.cfa$CFI>=.90 & fit.cfa$RMSEA<=.08 & fit.cfa$SRMR<=.08 ,]

#the number of modified models
sum(fit.cfa$n.mods, na.rm = T)


#check for negative factor loadings - omega can be computed only if the loadings have the same sign
for (i in 1:nrow(fit.cfa)){
  LAMBDA<-inspect(fit[[i]], "std")$lambda
  if(any(LAMBDA<0)){
    print(paste("iteration: ", i))
  }
}






cfa.scores<-as.data.frame(cfa.scores)

#remove latent scores of variables with poor fit (except for parenting.warmth)
remove.latent.scores<-c(poor.fit$variable[poor.fit$variable!="k10.parenting.warmth.mom"])
cfa.scores<-cfa.scores[,!colnames(cfa.scores) %in% remove.latent.scores] 

save(fit, fit.cfa, poor.fit, good.fit, file = "Outputs/LSACcreatingScales/CFA predictor models & fit.RData")
save(cfa.scores, file = "Outputs/LSACcreatingScales/cfa.scores.RData")

write_xlsx(fit.cfa, path = "Outputs/LSACcreatingScales/fit.cfa.xlsx")
write_xlsx(poor.fit, path = "Outputs/LSACcreatingScales/poor.fit.xlsx")
write_xlsx(good.fit, path = "Outputs/LSACcreatingScales/good.fit.xlsx")


# Dependent variable: Measurement Invariance ------------------------------------------------------
#Two models of the DV have been investigated
# 1) separate one-factor models for each respondent
# 2) hierarchical CFA, in which the latent variables of the respondents are at the first
#    level and the latent variables of the waves are at the second level 

wave.names<-wave.names<-paste0("k", c(10,12,14,16))
resp.names<-c("mom", "dad", "sc")


# the following for loop builds different parts of the cfa models as strings
#the different models include some of these parts

alpha<-c() #chronbachs alpha
guttman<-c() # guttmans lambda
mod1<-c() #the core of the cfa model, consisting of formulas that relate respondent latent variables to observed variables
corrs.within<-c()#cross-respondent inter-wave residual item covariances 
corrs.between<-c() #inter-respondent cross-wave residual item covariances
fix.intercepts<-c() #strong to fix the intercepts of respondent-items across waves to the same values
fix.thresholds<-c()
free.latent.means<-c() 
mod1.lvl2<-c() #the part of the model that relates the latent variables of each respondent to a 2nd order latent variable
k=1

i<-wave.names[3]
for(i in wave.names){ #for each wave name k12, k14 or k16
  #subset the data such that includes only the SDQ emotional problems scale for wave i
  df.i <- sublab(data=imp, labels=grep(paste0(i, ".SDQ.emot."), attr(imp,"variable.labels"), value = TRUE))
  wave.vars<-colnames(df.i)
  
  #corrs.between
  # define cross-respondent inter-wave covariances between the same items 
  for (l in letters[1:5]){
    v<-wave.vars[substr(wave.vars,8,8) == l]
    combinations1.2<-as.data.frame(t(combn(x=v,m=2)))
    corrs.between<-c(corrs.between,paste0(combinations1.2$V1, "~~", combinations1.2$V2))
  }
  
 
  #define equal intercepts or thresholds (for ordinal variables) across waves for the same items
  for(q in 1:ncol(df.i)){
    f=0
    var<-colnames(df.i)[q]
    fix.intercepts<-c(fix.intercepts, paste0(var, "~", "int.",letters[q],"*1"))
    for(j in c("t1", "t2")){
      f=f+1
      fix.thresholds<-c(fix.thresholds, paste0(var, "|", letters[q],f,".th*",j))
    }
    
  }
  
  mod1.lvl2<-c(mod1.lvl2,paste0(i,".F =~ ", i, ".f.mom + ", i,".f.dad + ", i,".f.sc"))
  j<-"mom"
  #for each respondent mom, dad or sc...
  for(j in resp.names){ 
    
    #subset the data such that includes only the SDQ emotional problems scale for wave i and respondent j
    df.j <- sublab(data=imp, labels=grep(paste0(i, ".SDQ.emot.",j), attr(imp,"variable.labels"), value = TRUE))
    #mod1 is the core of the cfa model
    #it is a character vector with 9 elements, each consisting of a formula that defines one latent variable per wave per respondent,
    #i.e. there are 9 latent variables, each with 5 observed variables as indicators
    mod1<-c(mod1,paste0(i,".f.", j, " =~  ", paste0(colnames(df.j), collapse = " +  ")))
    
    #define inter-respondent cross-wave residual covariances between the same items
    #get all variables that belong to the same respondent across waves
    df.j2<-sublab(data=imp, labels=grep(paste0(".SDQ.emot.",j), attr(imp,"variable.labels"), value = TRUE))
    resp.vars<-colnames(df.j2)
    
    #loop to paste together corrs.within 
    for (s in letters[1:5]){ #for each letter a,b,c,d and e
      r<-resp.vars[substr(resp.vars,8,8) == s] #a vector with three variable names of questions of the same order across respondent types,  e.g. question 1 (letter a) 
      combinations1.1<-as.data.frame(t(combn(x=r,m=2))) #get all combinations of the three variables
      corrs.within<-c(corrs.within,paste0(combinations1.1$V1, "~~", combinations1.1$V2))
    }
    
    free.latent.means<-c(free.latent.means,paste0(i,".f.", j, "~ NA*1"))
    
    # res_paral <- fa.parallel(df.j)
    alpha[[i]][[j]] <- psych::alpha(df.j)
    guttman[[i]][[j]] <- psych::guttman(df.j)
    
  }
  
  
  
}
corrs.within<-unique(corrs.within)

guttman$k12$mom$lambda.2
guttman$k12$dad$lambda.2
guttman$k12$sc$lambda.2

guttman$k14$mom$lambda.2
guttman$k14$dad$lambda.2
guttman$k14$sc$lambda.2

guttman$k16$mom$lambda.2
guttman$k16$dad$lambda.2
guttman$k16$sc$lambda.2

#___________________________________________________________________________________________________________________
## Model 1: separate CFAs for each respondent -------------------------------------------
#___________________________________________________________________________________________________________________
#4 separate one factor models for each respondent and each year

### Configural  ------------------------------------------------------ 
#configural invariance
model1.configural<-c(mod1,corrs.within)#corrs.between, corrs.within


conf<-cfa(model=c(mod1,corrs.within),imp, std.lv=TRUE, ordered=TRUE)
summary(conf,fit.measures=TRUE,standardized=TRUE)

### Metric ----------------------------------------------------------------------
#Weak factorial invariance
mod1.metric<-mod1

#the three loops fix the factor loadings per respondent across waves
#get the index positions of the two-empty-space placeholders before each item for fixing the factor loadings
positions<-gregexpr("  ", mod1.metric)[[1]][1:5]
#fix the factor loadings per item to be the same across waves for each respondent
for (i in c(1,4,7,10)) { #for parent 1 in each wave (mom)
  #for each item -> set a letter[j] (i.e., j is the number of a letter in the alphabet)
  for(j in 1:5) substr(mod1.metric[i],positions[j],positions[j]+1)<-paste0(letters[j], "*")
}
for (i in c(2,5,8,11)) { #for parent 2 in each wave
  for(j in 6:10) substr(mod1.metric[i],positions[j-5],positions[j-5]+1)<-paste0(letters[j], "*") #positions[j-5] assures that olazs positions [1:5] are looped over 
}
for (i in c(3,6,9,12)) { #for study child in each wave
  for(j in 11:15) substr(mod1.metric[i],positions[j-10]-1,positions[j-10])<-paste0(letters[j], "*")
}

# for (i in c(3,6,9)) { #for study child in each wave
#   for(j in 11:15) substr(mod1.metric[i],positions[j-10],positions[j-10]+1)<-paste0(letters[j], "*")
# }




#model1.metric<-c(mod1.metric, corrs.within) 
metric<-cfa(model=c(mod1.metric, corrs.within),imp, std.lv=TRUE,ordered=TRUE)
summary(metric,fit.measures=TRUE)

anova(metric, conf)

#plot factor loadings per respondent per wave
df_plot <- parameterestimates(conf)
#subset only factor loadings;ie. parameters with lhs=factor and rhs=observed variables
df_plot <- df_plot[df_plot$lhs %in% df_plot$lhs[substr(df_plot$lhs,1,1)=="k"] & 
                     df_plot$rhs %in% df_plot$rhs[substr(df_plot$rhs,1,1)!="k"] &
                     df_plot$rhs !="", ]

ggplot(df_plot, aes(y = rhs, x = est)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper)) +
  facet_wrap(~lhs, ncol=3)

### Scalar  --------------------------------------------------------------------
#strong factorial invariance
model1.scalar<-c(mod1.metric, fix.thresholds, free.latent.means[1:9],corrs.within)

scalar<-cfa(model=model1.scalar,imp, std.lv=TRUE,ordered=TRUE)
summary(scalar,fit.measures=TRUE, standardized=TRUE)

# est<-inspect(scalar, "est")
# LAMBDA<-est$lambda
# PSI<-est$psi
# THETA<-est$theta
# SIGMA<-LAMBDA %*% PSI %*% t(LAMBDA) + THETA




#___________________________________________________________________________________________________________________
#  END: Model 1: separate CFAs for each respondent 
#___________________________________________________________________________________________________________________



#___________________________________________________________________________________________________________________
## Model 2 (hierarchical): two-level CFA ------------------------------------------------------------------
#___________________________________________________________________________________________________________________
measurement.inv2<-data.frame(model=c("configural", "metric", "scalar"),
                            CFI=NA,
                            RMSEA=NA,
                            SRMR=NA,
                            Chi.sq=NA,
                            df=NA)

### Configural  ---------------------------------------------------------------------------
model2.configural<-c(mod1,  corrs.within, mod1.lvl2) #corrs.between,


# conf2<-sem(model2.configural,imp, ordered = TRUE)
# summary(conf2, fit.measures=TRUE, standardized=TRUE)
# #plot the path diagram of the model
# semPaths(conf2,"path", weighted = FALSE)



conf2<-cfa(model=model2.configural,imp,std.lv=TRUE, ordered=T) 

summary(conf2, fit.measures=TRUE, standardized=TRUE)


measurement.inv2[1,c("CFI", "RMSEA", "SRMR", "Chi.sq", "df")]<-fitmeasures(conf2)[c("cfi", "rmsea", "srmr", "chisq", "df")] %>% round(digits=3)





### Metric  ---------------------------------------------------------------------------
model2.metric<-c(mod1.metric, corrs.within, mod1.lvl2)#corrs.between,

# metric2<-sem(model2.metric,imp)
# summary(metric2, fit.measures=TRUE)
# #plot the path diagram of the model: also shows the equality constrains
# semPaths(metric2,"path", weighted = FALSE)

metric2<-cfa(model=model2.metric,imp,std.lv=TRUE, ordered=TRUE) 
summary(metric2,fit.measures=TRUE, standardized=TRUE)


measurement.inv2[2,c("CFI", "RMSEA", "SRMR", "Chi.sq", "df")]<-fitmeasures(metric2)[c("cfi", "rmsea", "srmr", "chisq", "df")] %>% round(digits=3)

#highly significant -- but the model fit is similar
anova(metric2, conf2)

#plot factor loadings per respondent per wave
df_plot <- parameterestimates(conf2,  standardized = TRUE)
#subset only factor loadings;ie. parameters with lhs=factor and rhs=observed variables
df_plot <- df_plot[df_plot$lhs %in% df_plot$lhs[substr(df_plot$lhs,1,1)=="k"] & 
                     df_plot$rhs %in% df_plot$rhs[substr(df_plot$rhs,1,1)!="k"] &
                     df_plot$rhs !="", ]

ggplot(df_plot, aes(y = rhs, x = std.all)) +
  geom_point() +
  geom_errorbarh(aes(xmin = std.all-1.96*se, xmax = std.all+1.96*se)) +
  facet_wrap(~lhs)

### Scalar  ---------------------------------------------------------------------------
#strong factorial invariance
# model2.scalar<-c(mod1.metric, fix.thresholds,free.latent.means[4:9],corrs.within, mod1.lvl2)  #corrs.between
# scalar2<-sem(model2.scalar, std.lv=TRUE,imp)
# summary(scalar2, fit.measures=TRUE, standardized=TRUE)
# fitmeasures(scalar2)[c("cfi","rmsea", "srmr")]
# #plot the path diagram of the model: also shows the equality constrains
# semPaths(scalar2,"path", weighted = FALSE)
# 
# scalar2DWLS<-cfa(model=model2.scalar,imp,std.lv=TRUE, estimator="DWLS") 
# summary(scalar2DWLS,fit.measures=TRUE, standardized=TRUE)
# fitmeasures(scalar2DWLS)

#ordered=TRUE
model2.scalar<-c(mod1.metric, fix.thresholds,free.latent.means[1:9],corrs.within, mod1.lvl2)  #corrs.between

scalar2<-cfa(model=model2.scalar,imp,std.lv=TRUE, ordered=TRUE) 
summary(scalar2,fit.measures=TRUE, standardized=TRUE)

measurement.inv2[3,c("CFI", "RMSEA", "SRMR", "Chi.sq", "df")]<-fitmeasures(scalar2)[c("cfi", "rmsea", "srmr", "chisq", "df")] %>% round(digits=3)

anova(scalar2, metric2)

#The scalar model fits well
measurement.inv2

#### Scores -------------------
#note: takes about 15 min to run
DVscores <- lavPredict(scalar2, method = "EBM", fsm = TRUE)
hist(DVscores)
DVscores<-DVscores %>% as.data.frame()

DV.reliabilities<-data.frame(wave=c("k12.F","k14.F","k16.F"),
                             omega_hu=NA #higher order omega (Flora,2020) https://doi.org/10.1177/2515245920951747
                             #prop.expl.var.Y=NA #not meant for ordinal data so interpret with caution
)
# 
# b<-attr(DVscores, "fsm")[[1]]
# dim(LAMBDA)
# 
# #Estimated model matrices
# LAMBDA<-inspect(scalar2ord, "std")$lambda
# BETA<-inspect(scalar2ord, "std")$beta
# THETA<-inspect(scalar2ord, "std")$theta
# PSI<-inspect(scalar2ord, "std")$psi
# I<-diag(rep(1, times=length(diag(BETA))))
# SIGMA<-LAMBDA %*% solve(I-BETA) %*%PSI%*% t(solve(I-BETA)) %*% t(LAMBDA)+THETA
# 
# #prop. of variance the factor scores explain the model implied cov matrix
# prop.expl.var.Y_all<-diag(b%*%LAMBDA%*% solve(I-BETA) %*%PSI %*%t(solve(I-BETA)) %*% t(LAMBDA)%*%t(b)/b%*%SIGMA%*%t(b))
# #of the scores to be used in further analyses
# DV.reliabilities[,"prop.expl.var.Y"]<-prop.expl.var.Y_lvl2<-tail(prop.expl.var.Y_all,3)



#calculate omega_hierarchical at each wave
DV.reliabilities[,"omega_hu"]<-
  c(semTools::reliabilityL2(scalar2, secondFactor = "k12.F")["omegaL2"],
    semTools::reliabilityL2(scalar2, secondFactor = "k14.F")["omegaL2"],
    semTools::reliabilityL2(scalar2, secondFactor = "k16.F")["omegaL2"])


#round values
DV.reliabilities[,2:ncol(DV.reliabilities)]<-round(DV.reliabilities[,2:ncol(DV.reliabilities)], digits = 3)
#save the latent scores of the dependent variable
#save(conf, metric, scalar, conf2, metric2, scalar2, measurement.inv2, file = "Outputs/LSACcreatingScales/DV models.RData")
save(DVscores, file = "Outputs/LSACcreatingScales/DVscores.RData")
save(DV.reliabilities, file = "Outputs/LSACcreatingScales/DV reliabilities.RData")
#___________________________________________________________________________________________________________________
# END:: Model 2: two-level CFA 
#___________________________________________________________________________________________________________________














