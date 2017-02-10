# ------------------- DATA IMPUTATION ---------------------- #

# Multivariate Imputation by Chained Equations
# Using predictive mean method
full5 <- full[,-c(3)]
tempData <- mice(full5,m=5,maxit=50,meth='pmm')
densityplot(tempData)

Imp.full5 <- complete(tempData,1)
model.Imp1 <- with(tempData, lm(Life ~ GNI + Infant + Adult + Diptheria + Measles +
		 Polio + Maternal + Water + Sanitation))

summary(pool(model.Imp1))
full3 <- cbind(full2,Imp.full5[10:12])

###### Imputed model ######
modelImp <- lme(update(formula(model32), ~ . +Maternal+Water+Sanitation),
	random= ~ 1+Infant+GNI+Measles|Country, data=na.omit(full3), control=ctrl, method="ML")
anova(modelImp,model32)

####### 3 Levels Imputed Data #########
modelImp3<-lme(update(formula(model32), ~ . +Maternal+Water+Sanitation),
	random=~1+Infant+GNI+Measles|Region/Country, data=na.omit(full3), control=ctrl, method="ML")

anv <- anova(modelImp3,modelImp,model32)
xtable(anv)
