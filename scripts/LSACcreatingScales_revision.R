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
load(file="C:/Users/3384022/Desktop/AL/Data to use/revision/imputed.data.RData")

#load hte documentation with the selected variables
#selected_variables<-read_xlsx("Anita/LSAC documentation/selected_variables2.xlsx")
#final_selection<-read_xlsx("Anita/LSAC documentation/final_selection.xlsx")
final_selection<-read_xlsx("Anita/LSAC documentation/revision/selected_variables1.xlsx")
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

#fit[[46]] negative.social.behvaviors: the model was not identified
formula="negative.social.behaviors =~  gse20a1 +  gse20a2 +  gse20a3 +  gse20a4 +  gse20a5 +  gse20a6 +  gse20a7 +  gse20a8 +  gse20a9 +  gse20a10 +  gse20a11 +  gse20a12 +  gse20a13 +  gse20a14 +  gse20a15 +  gse20a16 +  gse20a17"
fit.neg.soc.behav<-cfa(model = formula,imp, std.lv=TRUE, ordered=TRUE )


#poor fit
poor.fit<-fit.cfa[fit.cfa$CFI<.90 | fit.cfa$RMSEA>.08 | fit.cfa$SRMR>.08 ,]

#good fit
good.fit<-fit.cfa[fit.cfa$CFI>=.90 & fit.cfa$RMSEA<=.08 & fit.cfa$SRMR<=.08 ,]

#the number of modified models
sum(fit.cfa$n.mods)


#check for negative factor loadings - few are but the recoding of the items was correct (checked)
for (i in 1:nrow(fit.cfa)){
  LAMBDA<-inspect(fit[[i]], "std")$lambda
  if(any(LAMBDA<0)){
    print(paste("iteration: ", i))
  }
}





#EFA ---------------------------------------------------------------------------------

final_selection_prev<-read_xlsx("Anita/LSAC documentation/final_selection_adjusted.xlsx")


efa<-subset(final_selection_prev,method=="EFA" & !is.na(lvl1.varname))
efa.vars<-efa$lvl1.varname %>% unique()


#table in which the fit measures will be saved for each model
fit.efa<-data.frame(rowt.id=seq(1:17),
                    #overall model performance
                    variable=NA,
                    TLI = NA,
                    RMSEA = NA,
                    RMSR = NA,
                    # expl.var=NA,
                    cor.accuracy=NA,
                    #performance per factor
                    prop.expl.var.by.Y=NA,
                    # SSloadings=NA,
                    rho=NA,
                    max.determinancy.Rho2 = NA,
                    min.cor.competing.FS =NA,
                    validity=NA
                    
)

efa.scores<-data.frame(ID=imp$hicid)
EFA.factors<-list()
k<-0

set.seed(45898)
for(i in 1:length(efa.vars)){
  k=k+1
  fit.efa$variable[k] <- efa.vars[i]
  
  #get the variables names of the belonging items
  varnames<-efa[efa$lvl1.varname==efa.vars[i]  ,]$Variable.Name
  #subset the dataset and standardize the variables
  df.vars<-scale(imp[,colnames(imp) %in% varnames])
  
  #get the names and number of factors to be extracted
  EFA.factors[[i]]<-efa[efa$lvl1.varname==efa.vars[i]  ,]$EFA.factors[1]
  #split the factor names as different elements in a character vector
  EFA.factors[[i]]<-strsplit(EFA.factors[[i]], split = ", ")[[1]]
  #get the number of factors
  n.factors<-length(EFA.factors[[i]])
  
  #fit an exploratory factor analysis
  fit[[i]]<-fa(df.vars, nfactors = n.factors,rotate = "oblimin",scores="regression",fm="wls")
  
  #fill in the table for the overall model
  # expl.var<-fit[[i]]$Vaccounted["Proportion Var",]
  # fit.efa[k,c("TLI","RMSEA","RMSR", "expl.var")]<-c(fit[[i]]$TLI,fit[[i]]$RMSEA[1],fit[[i]]$rms, sum(expl.var))
  fit.efa[k,c("TLI","RMSEA","RMSR")]<-c(fit[[i]]$TLI,fit[[i]]$RMSEA[1],fit[[i]]$rms)
  #name and save the factor scores
  colnames(fit[[i]]$scores)<-EFA.factors[[i]]
  efa.scores<-cbind(efa.scores,fit[[i]]$scores)
  
  #compute the maximum difference between the correlation matrices of the factors and the factor scores
  #if the matrices are similar (i.e. the max value is low) => the correlations between the FS represent well the corrs between the factors
  max.cor.discrepancy <- max(abs(fit[[i]]$Phi-fit[[i]]$r.scores))
  fit.efa$cor.accuracy[k] <- round(max.cor.discrepancy,digits=2)
  
  #fit.efa$SSloadings[i]<-paste(round(fit[[i]]$Vaccounted["SS loadings",],digits=2), collapse = ";")
  
  for(j in 1:length(EFA.factors[[i]])){
    k=k+1
    #fill in the name of the specific factor
    fit.efa$variable[k] <- EFA.factors[[i]][j] 
    #  fit.efa$expl.var[k] <- expl.var[j]
    #  fit.efa$SSloadings[k] <- round(fit[[i]]$Vaccounted["SS loadings",j],digits=2)
    
    rho2<-fit[[i]]$R2[j]
    fit.efa$rho[k] <- round(sqrt(rho2),digits=2)
    fit.efa$max.determinancy.Rho2[k] <- round(rho2,digits=2)
    fit.efa$min.cor.competing.FS[k] <- round(2*rho2-1,digits=2)
    fit.efa$validity[k] <- round(fit[[i]]$valid[j],digits=2)
    
    #calculate reliability
    b<-fit[[i]]$weights
    lambdas<-fit[[i]]$loadings %>% c()
    LAMBDA<-matrix(lambdas, nrow = length(lambdas)/length(EFA.factors[[i]]), ncol = length(EFA.factors[[i]]))
    PSI<-fit[[i]]$Phi
    THETA<-fit[[i]]$residual
    SIGMA<-LAMBDA%*%PSI%*%t(LAMBDA)+THETA
    #proportion explained variance per factor
    prop.expl.var.by.Y<-diag(t(b)%*%LAMBDA%*%PSI%*%t(LAMBDA)%*%b/t(b)%*%SIGMA%*%b )
    
    fit.efa$prop.expl.var.by.Y[k]<-prop.expl.var.by.Y[j]
    
  }
  
}#end EFA loop
fit.efa[,3:ncol(fit.efa)]<-round(fit.efa[,3:ncol(fit.efa)], digits = 2)

cfa.scores








cfa.scores<-as.data.frame(cfa.scores)

#remove latent scores of variables with poor fit & the not identified model 
#remove.latent.scores<-c("negative.social.behaviors", poor.fit$variable)
remove.latent.scores<-c("negative.social.behaviors", efa.vars)
cfa.scores<-cfa.scores[,!colnames(cfa.scores) %in% remove.latent.scores] 

predictor.scores<-merge(cfa.scores, efa.scores, by="ID")
  
save(cfa.scores, fit, fit.cfa, poor.fit, good.fit, file = "Anita/Outputs/CFA predictor models & fit.RData")
save(cfa.scores, file = "Anita/Outputs/CFA predictors latent scores.RData")

save(predictor.scores, file = "Anita/Outputs/LSACcreatingScales_revision/predictor.scores.RData")

write_xlsx(fit.cfa, path = "Anita/Outputs/fit.cfa.WLSMVS.xlsx")
write_xlsx(poor.fit, path = "Anita/Outputs/poor.fit.WLSMVS.xlsx")
write_xlsx(good.fit, path = "Anita/Outputs/good.fit.WLSMVS.xlsx")
write_xlsx(fit.efa, path = "Anita/Outputs/LSACcreatingScales_revision/fit.efa.xlsx")



# Dependent variable: Measurement Invariance ------------------------------------------------------


wave.names<-wave.names<-paste0("k", c(12,14,16))
resp.names<-c("p1", "p2", "sc")


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
  
 
  #define equal intercepts or thresholds (ordinal) across waves for the same items
  for(q in 1:ncol(df.i)){
    f=0
    var<-colnames(df.i)[q]
    fix.intercepts<-c(fix.intercepts, paste0(var, "~", "int.",letters[q],"*1"))
    for(j in c("t1", "t2")){
      f=f+1
      fix.thresholds<-c(fix.thresholds, paste0(var, "|", letters[q],f,".th*",j))
    }
    
  }
  
  mod1.lvl2<-c(mod1.lvl2,paste0(i,".F =~ ", i, ".f.p1 + ", i,".f.p2 + ", i,".f.sc"))
  
  #for each respondent p1, p2 or sc...
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
    # 
    # dv.desc[[i]][[j]] <-list(alpha = res_alpha, parallel = res_paral)
  }
  
  
  
}
corrs.within<-unique(corrs.within)

guttman$k12$p1$lambda.2
guttman$k12$p2$lambda.2
guttman$k12$sc$lambda.2

guttman$k14$p1$lambda.2
guttman$k14$p2$lambda.2
guttman$k14$sc$lambda.2

guttman$k16$p1$lambda.2
guttman$k16$p2$lambda.2
guttman$k16$sc$lambda.2

alpha$k12$p1$total$std.alpha
alpha$k12$p2$total$std.alpha
alpha$k12$sc$total$std.alpha

alpha$k14$p1$total$std.alpha
alpha$k14$p2$total$std.alpha
alpha$k14$sc$total$std.alpha

alpha$k16$p1$total$std.alpha
alpha$k16$p2$total$std.alpha
alpha$k16$sc$total$std.alpha

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

#the three loops fix the factor loadings
#get the index positions of the two-empty-space placeholders for fixing the factor loadings
positions<-gregexpr("  ", mod1.metric)[[1]][1:5]
#fix the factor loadings per item to be the same across waves for each respondent
for (i in c(1,4,7)) { #for parent 1 in each wave
  #for each item -> set a letter
  for(j in 1:5) substr(mod1.metric[i],positions[j],positions[j]+1)<-paste0(letters[j], "*")
}
for (i in c(2,5,8)) { #for parent 2 in each wave
  for(j in 6:10) substr(mod1.metric[i],positions[j-5],positions[j-5]+1)<-paste0(letters[j], "*")
}
for (i in c(3,6,9)) { #for study child in each wave
  for(j in 11:15) substr(mod1.metric[i],positions[j-10],positions[j-10]+1)<-paste0(letters[j], "*")
}


model1.metric<-c(mod1.metric, corrs.within) #corrs.between,
metric<-cfa(model=c(mod1.metric, corrs.within),imp, std.lv=TRUE,ordered=TRUE)
summary(metric,fit.measures=TRUE)

#highly significant -- but the moel fit is fairly similar
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
  facet_wrap(~lhs)
#the plot shows similar factor loadings for the same participant across waves

### Scalar  --------------------------------------------------------------------
#strong factorial invariance
model1.scalar<-c(mod1.metric, fix.thresholds, free.latent.means[4:9],corrs.within)

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

metric2DWLS<-cfa(model=model2.metric,imp,std.lv=TRUE, ordered=TRUE, estimator="DWLS") 
summary(metric2DWLS,fit.measures=TRUE, standardized=TRUE)


measurement.inv2[2,c("CFI", "RMSEA", "SRMR", "Chi.sq", "df")]<-fitmeasures(metric2)[c("cfi", "rmsea", "srmr", "chisq", "df")] %>% round(digits=3)

#highly significant -- but the model fit is similar
anova(metric2DWLSord, conf2DWLSord)

#plot factor loadings per respondent per wave
df_plot <- parameterestimates(conf2DWLSord,  standardized = TRUE)
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
model2.scalar<-c(mod1.metric, fix.thresholds,free.latent.means[4:9],corrs.within, mod1.lvl2)  #corrs.between

scalar2<-cfa(model=model2.scalar,imp,std.lv=TRUE, ordered=TRUE) 
summary(scalar2,fit.measures=TRUE, standardized=TRUE)

measurement.inv2[3,c("CFI", "RMSEA", "SRMR", "Chi.sq", "df")]<-fitmeasures(scalar2)[c("cfi", "rmsea", "srmr", "chisq", "df")] %>% round(digits=3)

anova(scalar2, metric2)

# fits well!

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
save(conf, metric, scalar, conf2, metric2, scalar2, measurement.inv2, file = "Anita/Outputs/DV models.RData")
save(DVscores, file = "Anita/Outputs/DV latent scores.RData")
save(DV.reliabilities, file = "Anita/Outputs/DV reliabilities.RData")
#___________________________________________________________________________________________________________________
# END:: Model 2: two-level CFA 
#___________________________________________________________________________________________________________________














