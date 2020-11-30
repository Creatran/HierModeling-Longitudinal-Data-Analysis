Mixed Effects Models

```{r}
library(nlme)
library(lme4)
library(lmerTest)
```



```{r}
# Load Data

# First 5 observations of data 
head(dat)
# Look at structure of data 
str(dat)
# Correct class of variables
dat[, fact_vars] <- lapply(dat[, fact_vars], as.factor)
# Correct order of variables
relevel(, ref = "")


# Check for outliers or outliers 
sum(is.na(dat))
boxplot(, breaks = 10) 

# Model Building 
# nlme package
a <- lme(Standardized.Score ~ Sex, random = ~1|School, dat, 
        na.action = na.omit,
        method = "REML") # Can also enter "ML" for maximum likelihood

a <- lme(glucose ~ AAA, random = c(~1|race, ~1|athlete), dat)

# lme4 package
b <- lmer(Duration ~ PtSex + (1|Hospital/Doctors) + (1|Hospitals/Nurses) + (1|Nurses:Doctors), data)

b <- lme4::lmer(Apt.Duration ~ 1 + (1|Doctor.ID),
               REML = T, # REML = F, if want maximum likelihood
               data)

# lmertest package
b <- lmerTest::lmer(Apt.Duration ~ Session + (1|Doctor.ID),
                   REML = T,
                   dat)

summary(a)


## Important to look for two things:
# 1) Check for Normality of the random effects
reffects <- unlist(ranef(a))
reffects_1 <- unlist(ranef(b)$Doctor.ID)
qqnorm(reffects)
qqline(reffects)
# 2) Check variance to see whether it is 0; if it is 0, there is no need to add as random effect.

# Check Model Assumptions 
par(mfrow = c(2,2)) #Make the plot screen into 4 parts
qqnorm(resid(a)) #Display the 1st plot of the actual response values v the estimated values
qqline(resid(a)) #Makes the expected line of values
plot(density(resid(a))) #Display 2nd plot of distribution of residuals
abline(v=0,lty=3) #Dotted line on the mean=0
#Homoskedasticity
plot(resid(a)) #Display 3rd plot as the distribution of the residuals
abline(h=0) #Straight line along resid=0
#Outliers
plot(sort(resid(a))) #Display 4th plot as the sorted distribution of residuals to see if there's any outliers.

shapiro.test(resid(a)) #Shapiro test is used to determine normality. If significant then non-normal.


#Residuals look normally distributed (density/QQ plot)
#Residuals look randomly distributed. 
#No extreme residual outliers seen in last plot. 
#Shapiro test does not show strong significance for non-normality (>0.01).

#Residuals look slightly skewed, but more of less normally distributed (density/QQ plot)
#Residuals look randomly distributed. 
#No extreme residual outliers seen in last plot. 
#Shapiro test does show strong significance for non-normality (p < 0.01).




```

Interpretation:

A linear mixed effects model was used to estimate the effect of sex on standardized scores.  

Their scores were modeled with the fixed effect of sex and random error to account for the within school design.

There was no significant effect of sex on standardized scores (p = 0.1967).



Interpretation: 
A linear mixed effects model was used to estimate the effect of an amino acid, AAA,  on glucose levels.  
Glucose levels were modeled with the fixed effect of amino acid, AAA, and random error to account for the within athlete and race design.
There was a significant effect of the amino acid, AAA, on an athlete's glucose levels (p < 0.001).



Interpretation: 

We were interested in looking at the effect of sex on standardized score after controlling
for the variation in school.  We found that at the 5% significance level, there was no statistically significant association between sex and standardized test score (p = 0.1967).



Interpretation: After taking into account the variation due to doctor, the estimated mean of appointment duration (for all doctors in population) is 19.8 minutes.

```{r}
#Interested in looking at the effect of age, sex, treatment and 
# hospital size on stress level of the patient after taking 
# into account the hospital and ward. 
a <- lme(stress ~ age + sex + treatment + hospsize, 
         random = ~ 1|hospital/ward, dat)
b <- lmer(stress ~ age + sex + treatment + hospsize + (1|hospital/ward),
         dat)
#Interpretation: We were interested in looking at the effect of treatment on stress as well as other 
#covariates which include age, sex, and hospital size. Furthermore, the variation in hospitals and ward were 
#also taken into account in our mixed linear effects model. We found that treatment and stress levels 
#were associated with one another.  In fact, patients who received training compared to the control group 
# saw a decrease in stress levels by 0.7 (p = < 0.001). 
```

```{r}
# ONE-WAY ANOVA 

#Show different groups 
levels(plant_data$group)
# Model Building 
a <- aov(weight ~ group, data = plant_data)
summary(a)

# Check Model Assumptions 

# 1. Homogeneity of variances
plot(a, 1)
# Points 17,15, and 4 can be considered outliers, so for better fit, might be good to remove. 
# We should see no strong patterns in residuals
#Levene's test can also be used to test homogeneity of varainces 

#2. Normality Assumptions
plot(a, 2)
#Checking whether residuals are normally distributed 
# Can also use Shapiro-Wilk Test 



# One-way random effect anova
a <- aov(weight ~ Error(sire), data = animal_data)

print(model.tables(a,"means"),digits=3)

```

```{r}
# TWO-WAY ANOVA


#Check balance of data 
table(tooth_data$supp, tooth_data$dose)
a <- aov(len ~ supp + dose, data = tooth_data)
summary(a)

# TWO-WAY RANDOM EFFECT ANOVA W/ INTERACTION
a <- aov(y ~ Error(machine*day), data = machine_data)
summary(a)
print(model.tables(a,"means"),digits=3)

```





```{r
## GLMM
# Model Building 

### LOGISTIC REGRESSION 
a <-  glmer(relapse ~ gender + change_in_bmi + treatment + (1|clinic), 
                  family = binomial(link = "logit"), 
            data = relapse_data)

#Interaction Term (Between Fixed Effects)?
b <-  glmer(relapse ~ gender*change_in_bmi + treatment + (1|clinic), 
            family = binomial(link = "logit"), 
            data = relapse_data)

### Poisson Regression
a <-  glmer(awards ~ female +  ses + honors + (1 | cid), data = hs_data, family = poisson(link = "log"))

b <- glmer(awards ~ female + ses*honors + (1 | cid), data = hs_data, family = poisson(link = "log"))


anova(a,b)

# Model Assumptions
#Normality
par(mfrow = c(2,2))
qqnorm(resid(a))
qqline(resid(a))
#Homoskedasticity
plot(density(resid(a)))
abline(v=0,lty=3)
plot(resid(a))
abline(h=0)

#Outliers
plot(sort(resid(a)))
```



```{r}
## Cox Regression

library(coxme)

#Model Building 

a <- coxme(Surv(Time, Mortality) ~ Age + Chol + (1 | ID), 
           data = mortality_data)

#Interaction term
b <- coxme(Surv(Time, Mortality) ~ Age*Chol + (1 | ID), 
           data = mortality_data)

anova(a,b) 
# No AIC value, but there is a p-value. P-value signifies whether there is a significant difference 
#between the two models 

#Interpretaton:
summary(b)
#HR: Hazard Ratio
#HR of Chol increase = e^(-2.01096391) = 0.13.
# No statistically signifiant findings!
```



```{r}

a <- lmer(t_score ~ 1 + time + time_2 + (1|subject_id) + (0 + time|subject_id) + (0 + time_2|subject_id), 
               data = t_score_data)

b <- lmer(t_score ~ 1 + time + time_2 + (1|subject_id) + (1 + time|subject_id) + (1 + time_2|subject_id), 
          data = t_score_data)

```





## Bayes Factor, Bayes Regression, Hierarchical Modeling

```{r}
library(BayesFactor)
library(blme)
library(bayesmeta)
```

### contingencytablebf() function

```{r}
## Eg1
# Comparison of categorical data in two independent groups
# GroupA: drug treatment
# GroupB: placebo
# H0: the proportions of aggressive mice are the same under two treatment

contingencyTableBF(table, sampleType = "indepMulti", fixedMargin = "rows")
bf
#Fixed Sample Size (sampletype = "jointMulti")
#Fixed Rows (or Column) totals (sampletype = "indepMulti")
#Both rows and columns are fixed (sampleType = "hypergeom")
#Nothing is fixed (sampleType="poisson")

#The  ±0% part is telling you that R has calculated an exact Bayes factor, 
#so the uncertainty about the Bayes factor is 0%.
?
# a = 1, corresponds to an assumption that you have very little a 
#priori knowledge about the contingency table(Gunel and Dickey (1974))

#Interpretation:

#We ran a Bayesian test of association using default priors and 
#a independent multinomial sampling plan.
#The resulting Bayes factor of 22.4 to 1 in favor of the alternative
#hypothesis indicates that there is strong evidence for the non-independence of
#treatment and aggression.

```



```{r}
# Comparison of paired categorical data
# Testing mouse aggresive behavior under two different test conditions:
# 1. cages separated
# 2. cages next to each other.
# H0: the proportions of aggressive mice are the same under the two test conditions.  

contingencyTableBF(table, sampleType = "jointMulti", fixedMargin = NULL)

#Interpretation:
#We ran a Bayesian test of association using default priors and a joint multinomial sampling plan.
#The resulting Bayes factor of 0.29 to 1 in favor of the alternative
#hypothesis indicates that there is moderate evidence for the independence of
#cage type and aggression.
```



### Bayesian Regression: regressionBF() & lmBF()

```{r}
output <- regressionBF(quality ~ ., data = wine_data[,7:12], progress=FALSE)

#Looking at the best models against intercept 
head(output)

#Looking at best models against full model 
head(output / output[31])

#Looking at other models against best model 
head(output/max(output), n = 3)

#The Bayes factors of 0.08 to 1 imply that the odds for the best 
#model over the second best model are about 12.5:1.

#Looking at a particular model: 
output1 <- lmBF(quality ~ total.sulfur.dioxide +
                  density +
                  pH +
                  sulphates +
                  alcohol, 
                data = wine_data)

# The Bayes factor of 1.14 x 10^114 suggests that the model containing total sulfur dioxide, 
# density, pH, sulphates, and alcohol is 1.14 x 10^114 better than the intercept only model. 


######
```





### bayesglm()

```{r}

fit <- bayesglm(y ~ x1 + x2, family = gaussian, 
                prior.scale = 1, #Prior Scale for the coefficients 
                prior.df = Inf) # Prior degrees of freedom for coefficient.  
                              #Use Inf to get normal prior distributions 
display(fit)


fit <- bayesglm(y ~ x1 + x2, family = binomial(link = "logit"), 
                prior.scale = 2.5, #prior scale for the coefficients - used for logit models 
                prior.df = Inf) # prior degrees of freedom for the coefficients (normal prior distributions)
summary(fit)
```

### Blmer(), bglmer(), lmBF()

```{r}
a <- blmer(DI ~ SLEEP*MEAL + (1|ID), 
           data = sleep_data, 
           cov.prior = NULL, #Imposes a prior over the covariance of the random effects/modeled coefficients , 
           fixef.prior = normal) #Imposes a prior over the fixed effects/modeled coefficients)

summary(a)

b <- lmBF(DI ~ SLEEP*MEAL + ID, data = sleep_data, whichRandom = "ID")
b

c <- bglmer(HighBP ~ SLEEP*MEAL + (1|ID), sleep_data, cov.prior = NULL, 
            fixef.prior = NULL, family = "binomial")
summary(c)

a <- blmer(Reaction ~ Days + (1|Subject), data=sleepstudy, 
           cov.prior = NULL, fixef.prior = normal)
summary(a)

b <- lmBF(Reaction ~ Days + Subject, data=sleepstudy, 
          whichRandom="Subject")
b
```



### Bayes Meta-Analysis

```{r}
ma <- bayesmeta(y = beta_mean, #vector of estimates
                sigma = beta_sd, # vector of standard errors associated with y 
                mu.prior.mean = 0, # to specify the prior distribution for the effect of mu
                mu.prior.sd = 10, # to specify the prior distribution for the effect of mu
                tau.prior = function(x){dhalfnormal(x,scale=0.5)} 
                # function returning the prior density for the heterogeneity parameter 
                ) #dhalfnormal refers to a half-normal distribution

print(ma)
summary(ma)
plot(ma)

ma$qposterior(mu = c(0.025, 0.975))
forest(ma, main="Bayesian Meta Analysis: Relationship between PSA Levels and Age")

```



## Question


```{r}
Q: model assumption check: lme



workshop:
why listed all possible combination of blmer?

bayesmeta:
mu.prior.mean = 0, 
mu.prior.sd = 1, 
tau.prior=function(x){dhalfnormal(x,scale=0.5)}
```
