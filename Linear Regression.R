# --------------------------- Classic Linear Regression --------------------------- #

full1<-na.omit(full[,-c(1:3,11:13)])

#> names(full1)
#[1] "Life" "GNI" "Infant" "Adult" "Diptheria" "Measles" "Polio"

#Saturated model
linmod <- lm(Life~.*.,data=full1,na.action=na.omit)
summary(linmod)

# Step-wise regression
# AIC
stepAIC<-stepAIC(linmod, direction="both")
summary(stepA)

names1 <- names(stepAIC$coefficients)[2:5]
X2 <- full[,names1]

predstepAIC <- predict(stepAIC,X2)
modelAIC <- lm(formula=formula(stepAIC$call), data=full1)

summary(modelAIC)

# New model without GNI:Diptheria and GNI:Infant
new.mod <- (update(formula(stepAIC), ~ . - GNI:Diptheria - GNI:Infant))
model.lm <- lm(new.mod, data=na.omit(full1))
summary(model.lm)

anova(model.lm,modelAIC)

# Exclude the last unsignificant variable Diptheria
new.mod1 <- (update(formula(model.lm), ~ . - Diptheria))
model.lm1 <- lm(new.mod1, data=na.omit(full1))
summary(model.lm1) # All variables are now significant

anova(model.lm1, model.lm, stepAIC) # Diptheria not significant
anova(model.lm1,stepAIC) # Slightly significant
stargazer(stepAIC, model.lm, model.lm1)

# ------------ Penalised Regression ------------- #
optlambda1<-optL1(response=full1$Life, penalized=full1[,2:length(full1)])

lasso <- penalized(response=full1$Life, penalized=full1[,2:length(full1)], lambda1=optlambda1$lambda)
coefficients(lasso) #only 2 covariates remain so we dont use it

# No interactions
linmod1 <- lm(Life~.,data=full1,na.action=na.omit)

# Step-wise regression
# AIC
stepAIC1 <- step(linmod1, trace=TRUE, direction="both")
summary(stepAIC1)

modelAIC1 <- lm(formula=formula(stepAIC1$call), data=full1)
summary(modelAIC1)

#Compare the models
anova(modelAIC,modelAIC1)
#modelAIC alot more superior
