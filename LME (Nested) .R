###################### Model Fitting Using LME (Frequentist) ###########################

full2 <- full[,-c(11:13)]

# All possible methods using REML
ctrl <- lmeControl(maxIter=1500000, pnlsMaxIte=200000, msMaxIter=15000)

# ----------------------- RANDOM (INTERCEPT) ----------------------- # 

model01 <- lme(formula(modelAIC),random=~1|Country,data=full2,na.action=na.omit)

# ----------------------- 2 RANDOM EFFFECTS ----------------------- #

model11 <- lme(formula(model.lm1),random=~1+GNI|Country,
data = full2,na.action=na.omit,method="ML")

model12 <- lme(formula(model.lm1),random=~1+Infant|Country,
data = full2,na.action=na.omit,method="ML")

model13 <- lme(formula(model.lm1),random=~1+Adult|Country,
data = full,na.action=na.omit,method="ML")

model14 <- lme(formula(model.lm1),random=~1+Measles|Country,
data = full2,na.action=na.omit, control=ctrl,method="ML")

model15 <- lme(formula(model.lm1),random=~1+Polio|Country,
data = full2,na.action=na.omit, control=ctrl,method="ML")

anova(model11,model12,model13,model14,model15)

#model12 has the largest logLik

# ----------------------- 3 RANDOM EFFFECTS ----------------------- #

model21 <- lme(formula(model.lm1),random=~1+GNI+Infant|Country,
data = full2,na.action=na.omit, control=ctrl,method="ML")

model22 <- lme(formula(model.lm1),random=~1+GNI+Adult |Country,
data = full2,na.action=na.omit, control=ctrl,method="ML")

model23 <- lme(formula(model.lm1),random=~1+GNI+Measles |Country,
data = full2,na.action=na.omit, control=ctrl,method="ML")

model24 <- lme(formula(model.lm1),random=~1+GNI+Polio |Country,
data = full2,na.action=na.omit, control=ctrl,method="ML") #Doesn't run

model25 <- lme(formula(model.lm1),random=~1+Adult+Infant |Country,
data = full2,na.action=na.omit, control=ctrl,method="ML")

model26 <- lme(formula(model.lm1),random=~1+Adult+Measles |Country,
data = full2,na.action=na.omit, control=ctrl,method="ML")

model27 <- lme(formula(model.lm1),random=~1+Adult+Polio |Country,
data = full2,na.action=na.omit, control=ctrl,method="ML")

model28 <- lme(formula(model.lm1),random=~1+Infant+Measles |Country,
data = full2,na.action=na.omit, control=ctrl,method="ML")

model29 <- lme(formula(model.lm1),random=~1+Infant+Polio |Country,
data = full2,na.action=na.omit, control=ctrl,method="ML")

model210 <- lme(formula(model.lm1),random=~1+Measles+Polio |Country,
data = full2,na.action=na.omit, control=ctrl,method="ML")

anova(model21,model22,model23,model24,model26,
model27,model28,model29,model210)

# model21 has the largest logLik

# ---------------------- 4 RANDOM EFFFECT --------------------------- #

# Mixed model with random intercept and Infant + GNI + one
# random slope coefficient based on stepwise AIC with interactions
model31 <- lme(formula(model.lm1), random=~1+Infant+GNI+Adult|Country,
data = na.omit(full2), na.action=na.omit, control=ctrl, method="ML")

model32 <- lme(formula(model.lm1), random=~1+Infant+GNI+Measles|Country,
data = na.omit(full2), na.action=na.omit, control=ctrl, method="ML")

model33 <- lme(formula(model.lm1), random=~1+Infant+GNI+Polio|Country,
data = na.omit(full2), na.action=na.omit, control=ctrl, method="ML")
#Doesn't converge

# Mixed model with random intercept and Infant + Adult + one
# random slope coefficient based on stepwise AIC with interactions
model34 <- lme(formula(model.lm1), random=~1+Infant+Adult+Measles|Country,
data = na.omit(full2), na.action=na.omit, control=ctrl, method="ML")

model35 <- lme(formula(model.lm1), random=~1+Infant+Adult+Polio|Country,
data = na.omit(full2), na.action=na.omit, control=ctrl, method="ML")

# Mixed model with random intercept and Infant + Measles + one
# random slope coefficient based on stepwise AIC with interactions
model36 <- lme(formula(model.lm1), random=~1+Infant+Measles+Polio|Country,
data = na.omit(full2), na.action=na.omit, control=ctrl, method="ML")

# Mixed model with random intercept and GNI + Adult + one
# random slope coefficient based on stepwise AIC with interactions
model37<-lme(formula(model.lm1), random=~1+GNI+Adult+Measles|Country,
data = na.omit(full2), na.action=na.omit, control=ctrl, method="ML")

model38 <- lme(formula(model.lm1), random=~1+GNI+Adult+Polio|Country,
data = na.omit(full2), na.action=na.omit, control=ctrl, method="ML")
# Doesn't converge

# Mixed model with random intercept and GNI + Measles + one
# random slope coefficient based on stepwise AIC with interactions
model39 <- lme(formula(model.lm1), random=~1+GNI+Measles+Polio|Country,
data = na.omit(full2), na.action=na.omit, control=ctrl, method="ML")

# Mixed model with random intercept and Adult + Measles +
# one random slope coefficient based on stepwise AIC with interactions
model310 <- lme(formula(model.lm1), random=~1+Adult+Measles+Polio|Country,
data = na.omit(full2), na.action=na.omit, control=ctrl, method="ML")
# Doesn't converge

anova(model31,model32,model34,model35,model36,model37,model39)
# model31 has the largest logLik

anova(model32,model21,model12, model.lm1)
stargazer(model32,model21,model12, modelAIC)
# model32 has lowest logLik
