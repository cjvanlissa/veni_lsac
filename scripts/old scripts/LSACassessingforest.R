library(semtree)
library(metaforest)
library(future)

res_rf <- readRDS("Outputs/plantingforest/full_forest_test.RData")

# fac_labs <- list(
#   geslacht = c(Girl = 2,
#                Boy = 1),
#   brpmoe_lmh = c(High = 3,
#                  Medium = 2, Low = 1),
#   brpvad_lmh = c(High = 3, Medium = 2, Low = 1),
#   sesgez_low = c(`Low` = 1, `Med/hi` = 0),
#   bg11aa04 = c(
#     `None` = 8,
#     `Other` = 7,
#     Buddhist = 6,
#     Hindu = 5,
#     Islam = 4,
#     Reformed = 3,
#     `Dutch ref.` = 2,
#     `Catholic` = 1
#   ),
#   reli = c(Yes = 1,
#            No = 0),
#   alcohol = c("FALSE" = "No",
#               "TRUE" = "Yes"),
#   cigarettes = c("FALSE" = "No",
#                  "TRUE" = "Yes"),
#   drugs = c("FALSE" = "No",
#             "TRUE" = "Yes")
# )

#plan(multisession, workers = 40)
#vim <- semtree::varimp(res_rf)
#saveRDS(vim, paste0("results/vim_all_2", gsub("[: ]", "_", Sys.time()), ".RData"))

vim <- readRDS("Outputs/plantingforest/vim_full_forest_test.RData")
#VI <- list(variable.importance = semtree:::aggregateVarimp(vim, aggregate = "median", scale = "absolute", TRUE))
res_rf_VI<-readRDS("Outputs/plantingforest/res_rf_VI_full_forest_test.RData")

ren <- read.csv("scale_rename.csv", stringsAsFactors = F, header = FALSE)
res_rf_VI$variable.importance <- sort(res_rf_VI$variable.importance, decreasing = TRUE)



# # Legend
# leg <- read.csv("supplemental_table_1.csv", stringsAsFactors = FALSE)
# legtab <- data.frame(Predictor = ren$V2[match(names(VI$variable.importance)[1:30], ren$V1)], Description = leg$description.en[match(names(VI$variable.importance)[1:30], leg$variable.name)])
#                                         
# write.csv(legtab, "legtab.csv", row.names = FALSE)
# names(VI$variable.importance)[names(VI$variable.importance) %in% ren$V1] <- ren$V2[match(names(VI$variable.importance)[names(VI$variable.importance) %in% ren$V1], ren$V1)]
# saveRDS(VI$variable.importance, "variable_importance.RData")
# 
# names(VI$variable.importance) <- paste0(1:length(VI$variable.importance), ". ", c(rep("  ", 9), rep("", (length(VI$variable.importance)-9))), names(VI$variable.importance))
# class(VI) <- "ranger"
# v1 <- v2 <- VI
# v1$variable.importance <- v1$variable.importance[1:44]
# p1 <- metaforest::VarImpPlot(v1, 44)+theme(axis.text.y = element_text(hjust=0))+xlab(NULL)+scale_x_continuous(limits = c(0, max(VI$variable.importance)))
# v2$variable.importance <- v2$variable.importance[45:87]
# p2 <- metaforest::VarImpPlot(v2, 43)+theme(axis.text.y = element_text(hjust=0))+xlab(NULL)+scale_x_continuous(limits = c(0, max(VI$variable.importance)))
# library("cowplot")
# pcomb <- plot_grid(p1,p2,
#           ncol = 2, nrow = 1)
# ggsave("pcomb.pdf", pcomb, device = "pdf")
# saveRDS(pcomb, "varimp_comb_21-08-2021.RData")
# p <- metaforest::VarImpPlot(VI, length(VI$variable.importance))
# saveRDS(p, "varimp_21-08-2021.RData")


# # Partial dependence: too computationally intensive -----------------------
# #plan(multisession, workers = 10, gc = TRUE)
# source("pdp_growth.R")
# thesevars <- c("neuroticism", "BIS", "balancedrelated", "extraversion", "externalizing", 
#                "agreeableness", "leeftijd_moeder_11", "conflict_parents", "IRI_ped_aa", 
#                "CRSI_en_ab", "selfconc", "CRSI_ps_av", "CRSI_en_am", "conflictfrequency", 
#                "anxiety", "peermanag", "depression", "CRSI_wi_ab", "CRSI_wi_am", 
#                "CRSI_en_av", "dailyhassles", "leeftijd_vader_11", "CRSI_ps_am", 
#                "IRI_fan_aa", "intrusiveness", "leeftijd_target_11", "conpsy", 
#                "conflictemo", "DMD_guilt", "externalizing_psych", "geslacht", 
#                "tolerance", "supportivecriticism", "negativeaffect_father", 
#                "openness", "IRI_pet_aa", "drugs", "DMD_tired", "power_father", 
#                "DMD_angry", "CRSI_co_av", "pubert", "emotionalresponse", "IRI_emp_aa"
# ) #names(VI$variable.importance)[order(VI$variable.importance, decreasing = T)][1:44]
# 
# pdps <- vector("list", length = 44)
# pdps[[1]] <- plot_pdp(res_rf, "neuroticism")


# Clustering --------------------------------------------------------------
# klsym
# plan(multisession, workers = 10)
#didn't work - the output was an empty matrix
cl<-makeCluster(2)
M <- diversityMatrix(res_rf)
parallel::stopCluster(cl)

# a grid will show the proximity between every pair of observations.
# The proximity represents the percentage of trees where the two observations
# appear in the same leaf node.
# So the higher the value, the closer the observations.
# You can then use this proximity measure as the similarity or distance metric
# in your favorite clustering technique.