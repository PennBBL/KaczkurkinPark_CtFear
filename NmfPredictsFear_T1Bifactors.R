###################################
#### NMF AS PREDICTORS OF FEAR ####
###################################

#Load data
data.NMF <- readRDS("/data/jux/BBL/projects/pncT1AcrossDisorder/subjectData/n1394_T1_subjData.rds")

#Load libraries
library(ggplot2)

#CT components predicting Fear
NmfModel <- lm(phobias_4factorv2 ~ age + sex + Ct_Nmf18C1 + Ct_Nmf18C2 + Ct_Nmf18C3 + 
                     Ct_Nmf18C4 + Ct_Nmf18C5 + Ct_Nmf18C6 + Ct_Nmf18C7 + Ct_Nmf18C8 + Ct_Nmf18C9 + 
                     Ct_Nmf18C10 + Ct_Nmf18C11 + Ct_Nmf18C12 + Ct_Nmf18C13 + Ct_Nmf18C14 + Ct_Nmf18C15 + 
                     Ct_Nmf18C16 + Ct_Nmf18C17 + Ct_Nmf18C18, data = data.NMF)

NullModel <- lm(phobias_4factorv2 ~ age + sex, data = data.NMF)

#Look at model summaries
NmfSummary <- summary(NmfModel)
NullSummary <- summary(NullModel)

#Correlation between actual fear scores and predicted fear scores in a model with just demographics
cor.test(predict(NullModel), data.NMF$phobias_4factorv2)

#Correlation between actual fear scores and predicted fear scores in a model with demographics and the 18 NMF networks
cor.test(predict(NmfModel), data.NMF$phobias_4factorv2)

#Compare the null model to the Nmf model with an F test
anova(NmfModel,NullModel)

#Plot predicted fear score vs actual fear scores.
png(file='/data/jux/BBL/projects/pncT1AcrossDisorder/TablesFigures/Predicted_Actual_Fear.png',width=2000,height=2000,res=300)
par(mar=c(5,5,2,2), cex.axis=2, bty="l")
plot(predict(NmfModel), data.NMF$phobias_4factorv2, ylab="Actual Scores", xlab="Predicted Scores", cex.lab=2, pch=19, col="#F59611")
abline(a=0,b=1, col = "#F58311", lwd = 4)
dev.off()

