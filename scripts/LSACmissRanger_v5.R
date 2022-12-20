#Decisions made:
# 1) do not use Parent living elsewhere variables - otherwise how to deal with the missingness in them?


library(foreign)
library(missRanger)
library(readxl)
library(writexl)

# DOCUMENTATION WRANGLING  -----------------------------------------------------------------
#load the full LSAC documentation file containing the selected variables
#doc<-read_xlsx("Anita/LSAC documentation/full_documentation_with_selection.xlsx")
doc<-read_xlsx("LSAC Documentation/full_documentation_with_selection_v5.xlsx")

#filter the full documentation such that it only contains selected variables
selected_variables<-subset(doc, !is.na(lvl1.varname))

#fix column names
colnames(selected_variables)<-sub(" ", ".",colnames(selected_variables))
colnames(selected_variables)[6]<-"Without.Age"
colnames(selected_variables)[8]<-"Question.Id"

#a placeholder to keep track of the removed variables
removed_variables<-data.frame()


#reorder
selected_variables<-selected_variables[order(selected_variables$Variable.Name),]
#selected_variables<-selected_variables[order(selected_variables$File.Order),]


#


#write_xlsx(selected_variables,"Anita/LSAC documentation/selected_variables2.xlsx")

# DATA WRANGLING ------------------------------------------------------------------------
#load the LSAC data files and subset only the relevant variables
lsac10<-read.spss("C:/Users/3384022/Desktop/AL/Data to use/lsacgrk10.sav", to.data.frame = TRUE, use.value.labels = FALSE)
lsac12<-read.spss("C:/Users/3384022/Desktop/AL/Data to use/lsacgrk12.sav", to.data.frame = TRUE, use.value.labels = FALSE)
lsac14<-read.spss("C:/Users/3384022/Desktop/AL/Data to use/lsacgrk14.sav", to.data.frame = TRUE, use.value.labels = FALSE)
lsac16<-read.spss("C:/Users/3384022/Desktop/AL/Data to use/lsacgrk16.sav", to.data.frame = TRUE, use.value.labels = FALSE)


#k16: from p1/p2 to mom/dad ----------------------------------------------
# recode the variables belonging to parenting.angry and parenting.consistent from p1/p2 to mom/dad

#variable names of the p1p2 variables
#p1p2.vars<-selected_variables[selected_variables$lvl1.varname %in% c("k16.parenting.angry.p1","k16.parenting.angry.p2"),]$Variable.Name
p1p2.vars<-data.frame(p1=selected_variables[selected_variables$lvl1.varname %in% c("k16.parenting.angry.p1","k16.parenting.consistent.p1"),]$Variable.Name,
                      p2=selected_variables[selected_variables$lvl1.varname %in% c("k16.parenting.angry.p2","k16.parenting.consistent.p2"),]$Variable.Name)
momdad.vars<-p1p2.vars
substr(momdad.vars[,1],6,6)<-"m"
substr(momdad.vars[,2],6,6)<-"f"
colnames(momdad.vars)<-c("m", "f")
momdad.vars.vec<-unlist(momdad.vars,use.names = FALSE)
lsac16<-matrix(NA, ncol = length(momdad.vars.vec), nrow = nrow(lsac16), dimnames = list(1:nrow(lsac16), momdad.vars.vec)) %>% as.data.frame() %>% 
  cbind(lsac16)


j<-1
i<-1
for(j in 1:nrow(p1p2.vars)){
  for(i in 1:nrow(lsac16)){
    
    #if p2 is mom
    if(!is.na(lsac16$ifd23b1[i]) & lsac16$ifd23b1[i]==1){
      lsac16[i,momdad.vars[j,"m"]]<-lsac16[i,p1p2.vars[j,"p2"]]
    
    #else if p2 is dad
    }else if(!is.na(lsac16$ifd23b1[i]) & lsac16$ifd23b1[i]==2){
      lsac16[i,momdad.vars[j,"f"]]<-lsac16[i,p1p2.vars[j,"p2"]]
    }
    
    #if mom is p1
    if(lsac16$if06im[i]==0 & !is.na(lsac16$if06im[i])){
      lsac16[i,momdad.vars[j,"m"]]<-lsac16[i,p1p2.vars[j,"p1"]]
      
    #else if dad is p1
    }else if(lsac16$if06if[i]==0 & !is.na(lsac16$if06im[i])){
      lsac16[i,momdad.vars[j,"f"]]<-lsac16[i,p1p2.vars[j,"p1"]]
    }
    
  }
}


#check missingness
apply(lsac16[,momdad.vars.vec],2,function(x) sum(is.na(x)) )/nrow(lsac16)

#finally, remove p1p2 variables from the documentation
selected_variables[selected_variables$Variable.Name %in% unlist(p1p2.vars),]$lvl1.varname<-NA



#subset relevant variables from the datasets ------
lsac10<-subset(lsac10, select = colnames(lsac10)[colnames(lsac10) %in% selected_variables$Variable.Name])

#from lsac12, lsac14 and lsac16 only the IVs and DVs are needed (parenting variables and SDQ emotional problems scale)
#get the relevant variable names
k10.SDQ.emot<-selected_variables[selected_variables$lvl2.varname=="k10.SDQ.emot" & !is.na(selected_variables$lvl2.varname),]$Variable.Name
k12.SDQ.emot<-selected_variables[selected_variables$lvl2.varname=="k12.SDQ.emot" & !is.na(selected_variables$lvl2.varname),]$Variable.Name
k14.SDQ.emot<-selected_variables[selected_variables$lvl2.varname=="k14.SDQ.emot" & !is.na(selected_variables$lvl2.varname),]$Variable.Name
k16.SDQ.emot<-selected_variables[selected_variables$lvl2.varname=="k16.SDQ.emot" & !is.na(selected_variables$lvl2.varname),]$Variable.Name

#parenting variables
k10.parenting.varnames<-selected_variables[grep("k10.parenting", selected_variables$lvl1.varname),]$Variable.Name
k12.parenting.varnames<-selected_variables[grep("k12.parenting", selected_variables$lvl1.varname),]$Variable.Name
k14.parenting.varnames<-selected_variables[grep("k14.parenting", selected_variables$lvl1.varname),]$Variable.Name
k16.parenting.varnames<-selected_variables[grep("k16.parenting", selected_variables$lvl1.varname),]$Variable.Name



#subset only the relevant variables from the datasets
lsac12<-subset(lsac12, select = c("hicid", k12.parenting.varnames,k12.SDQ.emot))
lsac14<-subset(lsac14, select = c("hicid", k14.parenting.varnames,k14.SDQ.emot))
lsac16<-subset(lsac16, select = c("hicid", k16.parenting.varnames,k16.SDQ.emot))

#merge the three data files
lsac1214<-merge(lsac12, lsac14, by = "hicid", all.x = TRUE, all.y = TRUE)
lsac121416<-merge(lsac1214,lsac16, by = "hicid", all.x = TRUE, all.y = TRUE)
lsac<-merge(lsac10, lsac121416, by = "hicid",all.x = TRUE, all.y = TRUE)



## variable order ------------------------------------------------------

# For the DV sort the column names such that they follow the pattern
# wave (k12, k14, k16) - respondent (parent1 parent2/mom dad study child) - question (1 to 5)
#so 1) sort according to the last character of the variable name (indicator of the question)
# and 2) sort according to the 6th chatacter which is an indicator of the respondent

SDQ.emot<-lapply(X=list(k10=k10.SDQ.emot,
                        k12=k12.SDQ.emot,
                        k14=k14.SDQ.emot,
                        k16=k16.SDQ.emot),
                 FUN = function(X){
                   X<-X[order(substr(X, 8,8))]
                   X<-X[order(factor(substr(X, 6,6) , levels = c("m", "f", "c")))]
                 })

#concantenate all variables of the dataset with the SDQ.emot variables at the end
var.order<-c( selected_variables[-c(grep("SDQ.emot",selected_variables$lvl1.varname),which(is.na(selected_variables$lvl1.varname))) ,]$Variable.Name,
              SDQ.emot$k10,
              SDQ.emot$k12,
              SDQ.emot$k14,
              SDQ.emot$k16
)

#set the column order in the dataset
lsac<-lsac[,var.order]
#sort the selected_variables file
selected_variables<-selected_variables[order(match(selected_variables$Variable.Name, var.order)),] 
# MISSING DATA ---------------------------------------------------------------------------------

# -1 Not applicable (when explicitly available as an option in the questionnaire)
# -2 Don’t know
# -3 Refused or not answered
# -4 Section refused
# -5 Item non-response on web form, and reason is unknown
# -9 Not asked due to one of the following reasons:
#     -A question was skipped due to the answer to a preceding question 
#     -A form was not returned or consent to participate was not given 
#     -One of the informants refused to participate 
#     -A form was partially completed .
# -99 Specific code for the one of the following reasons:
#     -Negative income (loss)
#     -Before baby’s birth-SC age when stopped living with PLE
#     -No set amount for expected child support

# Replace lsac missingness codes with NA
for (i in c(-1,-2,-3,-4,-5,-9,-99)){
  lsac[lsac == i]<- NA
}


## Screen missingness --------------------------------------------------------------

miss <- is.na(lsac)
#the proportion of missing data in each row
miss_row <- rowSums(miss)/ncol(miss)
#the proportion of missing data in each column
miss_col <- colSums(miss)/nrow(miss)
hist(miss_row, 100)
hist(miss_col, 100)
#columns or rows that have proportion of missingnes larger than 60%
sum(miss_col>0.6) #52
sum(miss_row>0.6) #162

removed.vars.missings<-selected_variables[selected_variables$Variable.Name %in% labels(which(miss_col>0.6)),]
removed.vars.missings$exclusion.reason<-"missing > .60"
removed_variables<-rbind(removed_variables, removed.vars.missings)

lsac <- lsac[!miss_row > .6, ]
lsac <- lsac[, -which(miss_col > .6)]

#final_selection <- selected_variables[selected_variables$Variable.Name %in% colnames(lsac),]
#write_xlsx(final_selection ,"Anita/LSAC documentation/final_selection.xlsx")

selected_variables1 <- selected_variables[selected_variables$Variable.Name %in% colnames(lsac),]
write_xlsx(selected_variables1 ,"LSAC documentation/selected_variables1_v5.xlsx")

selected_variables1<-read_xlsx("LSAC Documentation/selected_variables1_v5.xlsx")
## Imputation ---------------------------------------------------------------------

#pmm.k  number of non-missing values to be sampled for predictive mean mathching step (0 to avoid this step)
#num.trees  number of trees to be grown
#maxiter maximum of iterations for forests to be grown (irrelevant if no meaningful changes 
#   between iterations anymore) (e.g with 50 trees, algortithms stops after 3  iterations, as no
#   improvement between iteration 2 and 3 took place)

#____________________________________________________________________________________


# apply missForest algorithm (using faster ranger implementation)
# nrow = 3884
# ncol = 834
set.seed(123)
imp<-missRanger(lsac, pmm.k=10, num.trees=2, maxiter=1)

anyNA(imp)

#check order of the variable names -- correct
sum(colnames(imp) == selected_variables1$Variable.Name)

#add labels to the imputed dataset
#a vector with variable lables of the variables in the imp dataset
attr(imp, "variable.labels")<-selected_variables1$lvl1.varname


#save(imp, file="C:/Users/3384022/Desktop/AL/Data to use/imputed.data.RData")
save(imp, file="C:/Users/3384022/Desktop/AL/Data to use/revision/imputed.data_v5.RData")
