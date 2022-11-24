library(readxl)
library(worcs)
library(dplyr)
library(stringr)
library(writexl)

doc<-read_xlsx("LSAC documentation/full_documentation_with_selection_adj.xlsx")


#filter the full documentation such that it only contains selected variables
selected_variables<-subset(doc, !is.na(lvl1.varname))

#fix column names
colnames(selected_variables)<-sub(" ", ".",colnames(selected_variables))
colnames(selected_variables)[6]<-"Without.Age"
colnames(selected_variables)[8]<-"Question.Id"


# lvl1.varname ----------------------------------------------
parents.vars<-selected_variables[c(grep("p1", selected_variables$lvl1.varname),grep("p2", selected_variables$lvl1.varname)),c("Variable.Name", "lvl1.varname", "method", "Note")]

parents.vars$mo.varname<-NA
parents.vars$fa.varname<-NA

parents.vars <-as.data.frame(parents.vars)


#obtain the Variable.Name of mom/dad variables that correspond to p1/p2 variables 
for(i in 1:nrow(parents.vars)){
  #for each variable get the Question id (common for questions asked to multiple respondents)
  q.id<-doc[doc$`Variable Name`== parents.vars$Variable.Name[i],]$`Question id`
  
  mo.fa.vars<-doc[doc$`Question id` == q.id,]
  
  if(any(mo.fa.vars$`Person Label` %in% c("Mother@12/13", "Mother", "Father@12/13", "Father")&
         substr(mo.fa.vars$`Variable Name`,1,1)==substr(parents.vars$Variable.Name[i],1,1))){
    parents.vars[i,4:5]<-mo.fa.vars[mo.fa.vars$`Person Label` %in% c("Mother@12/13", "Mother", "Father@12/13", "Father") &
                                      substr(mo.fa.vars$`Variable Name`,1,1)==substr(parents.vars$Variable.Name[i],1,1)
                                      ,]$`Variable Name`
  #if there are no corresponding mom/dad variables => fill in "none"
  }else{
    parents.vars[i,4:5]<-"none"
    
  }

}


parents.vars$lvl1.varname.new<-NA
parents.vars$lvl1.varname.new<-gsub('.{3}$', '', parents.vars$lvl1.varname)

#remove indicators of the relationships between familz memebers - irrelevant if we know who mom and dad is
parents.vars[parents.vars$lvl1.varname== "relationship.p1.p2" | parents.vars$lvl1.varname=="relationship.p1.sc", c("mo.varname", "fa.varname")]<-"none"

#subset only variables that have a corresponding mom/dad variable
parents.vars.sub<-subset(parents.vars, mo.varname!="none")

#this for loop fills in a lvl1.varname for each mom/dad variable in the documentation
for(i in 1:nrow(parents.vars.sub)){
  doc[doc$`Variable Name` %in% parents.vars.sub$mo.varname[i],]$lvl1.varname<-paste0(parents.vars.sub$lvl1.varname.new[i], ".mom")
  doc[doc$`Variable Name` %in% parents.vars.sub$mo.varname[i],]$method<-parents.vars.sub$method[i]
  doc[doc$`Variable Name` %in% parents.vars.sub$mo.varname[i],]$Note<-parents.vars.sub$Note[i]
  
  doc[doc$`Variable Name` %in% parents.vars.sub$fa.varname[i],]$lvl1.varname<-paste0(parents.vars.sub$lvl1.varname.new[i], ".dad")
  doc[doc$`Variable Name` %in% parents.vars.sub$fa.varname[i],]$method<-parents.vars.sub$method[i]
  doc[doc$`Variable Name` %in% parents.vars.sub$fa.varname[i],]$Note<-parents.vars.sub$Note[i]
  
}

#remove lvl2.varname for p1/p2 variables
doc[unique(c(grep(".p1", doc$lvl1.varname),grep(".p2", doc$lvl1.varname))),]$lvl2.varname<-NA

#remove the lvl1.varname for p1/p2 variables
doc[unique(c(grep(".p1", doc$lvl1.varname),grep(".p2", doc$lvl1.varname))),]$lvl1.varname<-NA

#remove ple variables
#if the last 4 characters of lvl1.varname are ".ple" remove the lvl1.varname of this variable
doc$lvl1.varname[which(substr(doc$lvl1.varname, nchar(doc$lvl1.varname)-4+1, nchar(doc$lvl1.varname))==".ple")]<-NA


#check
doc[c(grep(".mom", doc$lvl1.varname),grep(".dad", doc$lvl1.varname)),]$lvl1.varname
doc[c(grep(".p1", doc$lvl1.varname),grep(".p2", doc$lvl1.varname)),]$lvl1.varname

#lvl2.varname ---------------------------------------------
#fill in second level of the follwoing v ariables
#"medical.burden.sc" "SDQ.prosocial"     "k12.SDQ.emot"      "SDQ.peer.problems" "financial.stress"  "k14.SDQ.emot"      "k16.SDQ.emot"
doc[grep("SDQ.prosocial", doc$lvl1.varname),c("lvl1.varname", "lvl2.varname")]$lvl2.varname<-"SDQ.prosocial"
doc[grep("SDQ.peer.problems", doc$lvl1.varname),c("lvl1.varname", "lvl2.varname")]$lvl2.varname<-"SDQ.peer.problems"

doc[grep("k12.SDQ.emot", doc$lvl1.varname),c("lvl1.varname", "lvl2.varname")]$lvl2.varname<-"k12.SDQ.emot"
doc[grep("k14.SDQ.emot", doc$lvl1.varname),c("lvl1.varname", "lvl2.varname")]$lvl2.varname<-"k14.SDQ.emot"
doc[grep("k16.SDQ.emot", doc$lvl1.varname),c("lvl1.varname", "lvl2.varname")]$lvl2.varname<-"k16.SDQ.emot"

#remove financial.stress as a lvl2.varname
doc[grep("financial.stress", doc$lvl2.varname),c("lvl1.varname", "lvl2.varname")]$lvl2.varname<-NA

#remove working hours of parents from wave k14
doc[c(10954,13814),]$lvl1.varname<-NA

doc$lvl2.varname %>% unique()



# get the parenting variables from all three waves
doc<-read_xlsx("LSAC Documentation/full_documentation_with_selection_v3.xlsx")

parenting.vars<-grep("parenting",doc$lvl1.varname, value=TRUE) %>%  unique() 

parenting.df<-data.frame(Variable.Name = doc[grep("parenting",doc$lvl1.varname), ]$`Variable Name`,
                         lvl1.varname = doc[grep("parenting",doc$lvl1.varname), ]$lvl1.varname,
                         k14=NA,
                         k16=NA
                         )

#obtain the Variable.Name of parenting variables for wave k14 and k16
for(i in 1:length(parenting.vars)){
  #for each variable get the Variable Name without wave indicator
  q.id<-doc[doc$lvl1.varname==parenting.vars[i] & !is.na(doc$lvl1.varname),]$`Variable Name`
  k14<- k16 <- q.id
  
  substr(k14,1,1)<-"h"
  substr(k16,1,1)<-"i"
  
  #check if the respective variable exists in the documentation
  if(all(k14 %in% doc$`Variable Name`)){
    parenting.df[parenting.df$Variable.Name %in% q.id,]$k14<-k14
  }else{
    parenting.df[parenting.df$Variable.Name %in% q.id,]$k14<-"none"
  }
  
  if(all(k16 %in% doc$`Variable Name`)){
    parenting.df[parenting.df$Variable.Name %in% q.id,]$k16<-k16
  }else{
    parenting.df[parenting.df$Variable.Name %in% q.id,]$k16<-"none"
  }
  
}


#fill in the documentation file such that for each parenting variable there is a k12, k14 and k16 lvl1.varname (if existing)






















write_xlsx(doc, "LSAC Documentation/full_documentation_with_selection_v3.xlsx")



doc<-read_xlsx("LSAC Documentation/full_documentation_with_selection_adj.xlsx")

parenting.vars<-grep("parenting",doc$lvl1.varname, value=TRUE) %>%  unique() 

parenting.df<-data.frame(Variable.Name = doc[grep("parenting",doc$lvl1.varname), ]$`Variable Name`,
                         lvl1.varname = doc[grep("parenting",doc$lvl1.varname), ]$lvl1.varname,
                         k14=NA,
                         k16=NA
)

#obtain the Variable.Name of parenting variables for wave k14 and k16
for(i in 1:length(parenting.vars)){
  #for each variable get the Variable Name without wave indicator
  q.id<-doc[doc$lvl1.varname==parenting.vars[i] & !is.na(doc$lvl1.varname),]$`Variable Name`
  k14<- k16 <- q.id
  
  substr(k14,1,1)<-"h"
  substr(k16,1,1)<-"i"
  
  #check if the respective variable exists in the documentation
  if(all(k14 %in% doc$`Variable Name`)){
    parenting.df[parenting.df$Variable.Name %in% q.id,]$k14<-k14
  }else{
    parenting.df[parenting.df$Variable.Name %in% q.id,]$k14<-"none"
  }
  
  if(all(k16 %in% doc$`Variable Name`)){
    parenting.df[parenting.df$Variable.Name %in% q.id,]$k16<-k16
  }else{
    parenting.df[parenting.df$Variable.Name %in% q.id,]$k16<-"none"
  }
  
}



  
  
  