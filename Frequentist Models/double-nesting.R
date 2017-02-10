# ------------------- Three-level model (Region) ---------------------- #
# ------------------- Double Nested Model ----------------------------- #

modeldouble <- lme(formula(model32),random=~1+Infant+GNI+Measles|Region/Country,
data = na.omit(full2), na.action=na.omit, control=ctrl, method="ML")
anova(modeldouble,model32,model21,model12, model.lm1)
stargazer(modeldouble,model32,model21,model12, model.lm1)

#Residual#
plot(resid(modelAIC, type = "pearson"))
plot(modelAIC, Country ~ resid(.), abline = 0, data=full)
