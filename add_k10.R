doc<-read_xlsx("LSAC documentation/full_documentation_with_selection_v3.xlsx")
#filter the full documentation such that it only contains selected variables
selected_variables<-subset(doc, !is.na(lvl1.varname))


paste0("",doc$`Without age`)

#all selected variables at k12
all.vars.without.age.k12<-selected_variables$`Without age`
#subset the corresponding variables from k10
k10<-doc[doc$`Without age` %in% all.vars.without.age.k12 & doc$File=="K10",]
#which variables from k12 are not present in k10
a<-selected_variables[which(!all.vars.without.age.k12 %in% k10$`Without age`),]
a$lvl1.varname %>% unique()


#fill in lvl1 and lvl2.varname, method and Note columns for selecetd k10 variables depending on the corresponding entry from k12
i<-1
for( i in 1:nrow(doc[doc$File=="K10",])){
  print(i)
  wa.var<-doc[doc$File=="K10",]$`Without age`[i]
  
  if(nrow(doc[doc$File=="K12" & doc$`Without age`==wa.var & !is.na(doc$lvl1.varname),c("lvl1.varname", "lvl2.varname", "method", "Note")])==0){
    doc[doc$File=="K10" & doc$`Without age`==wa.var,c("lvl1.varname", "lvl2.varname", "method", "Note")]<-rep(NA,4)
  }else{
    doc[doc$File=="K10" & doc$`Without age`==wa.var,c("lvl1.varname", "lvl2.varname", "method", "Note")]<- doc[doc$File=="K12" & doc$`Without age`==wa.var &!is.na(doc$lvl1.varname),c("lvl1.varname", "lvl2.varname", "method", "Note")]
  }

}

#remove k12 variables except for SDQ.emot
sdq.emot.vars1<-grep("SDQ.emot", doc[doc$File=="K12" & !is.na(doc$lvl1.varname),]$lvl1.varname, value = TRUE)
sdq.emot.vars2<-grep("SDQ.emot", doc[doc$File=="K12" & !is.na(doc$lvl2.varname),]$lvl2.varname, value = TRUE)

doc[doc$File=="K12" & !is.na(doc$lvl1.varname) & !doc$lvl1.varname %in% sdq.emot.vars,
    c("lvl1.varname", "lvl2.varname", "method", "Note")]<-NA

#fix lvl1 adn lvl2 varname for SDQ.emot in k10
doc[doc$File=="K10" & doc$lvl1.varname %in% sdq.emot.vars1,]$lvl1.varname<-str_replace(doc[doc$File=="K10" & doc$lvl1.varname %in% sdq.emot.vars1,]$lvl1.varname, "k12","k10")
doc[doc$File=="K10" & doc$lvl2.varname %in% sdq.emot.vars2,]$lvl2.varname<-str_replace(doc[doc$File=="K10" & doc$lvl2.varname %in% sdq.emot.vars2,]$lvl2.varname, "k12","k10")

#check
doc[doc$File=="K10" & doc$lvl1.varname %in% grep("SDQ.emot",doc$lvl1.varname, value = TRUE),]$lvl1.varname
doc[doc$File=="K10" & doc$lvl2.varname %in% grep("SDQ.emot",doc$lvl2.varname, value = TRUE),]$lvl2.varname


write_xlsx(doc, path = "LSAC documentation/full_documentation_with_selection_v4.xlsx")
