### Emotional Dysregulation # Generating partial dependence ###

#############################
# Lukas Beinhauer
# 24/08/20
#############################

########################################################################################################
# Over the following lines, code can be found in order to generate partial dependence for the most and #
# least relevant variables as identified using the variable importance. Run the scripts <missRanger.R>,#
# <creatingscales.R>, <plantingforest.R>, and <assessingforest.R> first.!                              #
########################################################################################################

library(semtree)
library(metaforest)
library(gridExtra)

# # load data and variable importance
# forestdata <- readRDS("data/forestfile.RData")
# vim <- readRDS("data/forestvim.RData")

forestdata <- readRDS("Outputs/plantingforest/full_forest_test.RData")


######   extracting variable importance values and sorting variables by importance (ascending)   ######
# code stems from github.com/brandmaier/semtree (can also be found in semtree package)
sort.values = T 
aggregate = "mean" 
horiz = T
las = 1 
convergence = F 
scale = "absolute" 
xlim = NULL 
head = NULL 
tail = NULL 
na.omit = FALSE

vimp <- vim
x <- semtree:::aggregateVarimp(vimp, aggregate, scale, na.omit)

vnames <- vimp$var.names
low <- min(x, na.rm = T) - 1
filt <- is.na(x)
x[filt] <- low
srt <- sort(x, index.return = T)
x <- x[srt$ix]
vnames <- vnames[srt$ix]
x[x <= (low + 0.5)] <- NA

selection <- 1:length(x)

#Anita: impmat is not a matrix but just the vector with importances for each variable (vim)
impmat <- x[selection] #returns a matrix containing variable importance of all variables supplied to the semforest
#Anita: make it a matrix (column vector)
impmat<-matrix(impmat)


# select top 5 and bottom 5 of most relevant variables
indxvec <- matrix(c(1:5, (length(impmat)-4):length(impmat)), nrow=1)

#Anita, partial depndence - test
partDepTest<-partialDependence(forestdata, reference.var=names(x[1:2]), 
                  reference.param="residual")

# calculate partial dependence for selected variables concerning "mean linear slope" parameter
pDms_test <-  partialDependence(forestdata, reference.var=names(x[1:2]), 
                                                            reference.param="means")

saveRDS(partDepTest, "Outputs/partDependence/partDepTest.RData")

# calculate partial dependence for selected variables concerning "residual" parameter
pDres <- apply(indxvec, 2, FUN=function(x) partialDependence(forestdata, reference.var=names(impmat[x]), 
                                                             reference.param="residual"))
saveRDS(pDres, file="partDependence/pDres.RData")

# calculate partial dependence for selected variables concerning "mean intercept" parameter
pDmi <- apply(indxvec, 2, FUN=function(x) partialDependence(forestdata, reference.var=names(impmat[x]), 
                                                            reference.param="meani"))
saveRDS(pDmi, file="data/pDmi.RData")

# calculate partial dependence for selected variables concerning "mean linear slope" parameter
pDms <- apply(indxvec, 2, FUN=function(x) partialDependence(forestdata, reference.var=names(impmat[x]), 
                                                            reference.param="means"))
saveRDS(pDms, file="data/pDms.RData")

# calculate partial dependence for selected variables concerning "mean quadratic slope" parameter
pDmq <- apply(indxvec, 2, FUN=function(x) partialDependence(forestdata, reference.var=names(impmat[x]), 
                                                            reference.param="meanq"))
saveRDS(pDmq, file="data/pDmq.RData")