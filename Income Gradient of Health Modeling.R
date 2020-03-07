#Clear the console, clear the workspace, detach all dataframes
cat("\014")
rm(list = ls())

#Set working directory
setwd("/Users/borishouenou/Downloads")

#Import the data, data.csv, with variable names in the first row. Attach dataset.
data = read.csv("dataincome1.csv") #,stringsAsFactors=F, na.strings=c(NA,"NA"," NA"))
#str(data)
attach(data)
head(data)
#data = data[data$age <= 10,]
#data1=data[complete.cases(data[c("age","numbreath","numresp","numpneu","compound", "locationid", "socialgpid", "M2VillRole","numgastro", "numfever","numother", "numseek", "M1EducLevel", "M2EducLevel")]),]
QU = as.numeric(Quarter)
summary(QU)
t=factor(QU)
summary(lfullinc2)
data1=subset(data, age<15)
## Create a formula
summary(hhid)
data("data1", package="pglm")
la <- pglm(nummorb ~ lfullinc2 + age + agefather+ agemother, data1,
           family = poisson, model = "random", print.level=3, method="nr",
           index=c(hhid,Quarter))
summary(la)
la <- pglm(pat ~ lag(logr, 0:5) + scisect + logk + factor(year), PatsRD,
           family = poisson, model = "pooling", index = c("cusip", "year"),
           print.level = 0, method="nr")



formula <- nummorb ~ lfullinc2 + age + agefather+ agemother 
## Fit Poisson
la <- pglm(formula=formula,
           model =  random , family = poisson(link=log) , print.level = 0)



modelPoisson <- pglm(formula = formula,
                     family  = poisson(link = "log"),
                     data    = data1)
summary(modelPoisson)
dispersiontest(modelPoisson,trafo=1)

## Test using robust sandwich covariance matrix estimators
coeftest(modelPoisson, vcov = sandwich)

## Exponentiated coefficients
exp(coef(modelPoisson))
## Fit quasi-Poisson and test for overdispersion. Dispersion parameter >1 indicates overdispersion and appeals for negBinomial
modelQuasiPoisson <- glm(formula = formula,
                         family  = quasipoisson(link = "log"),
                         data    = data1)
summary(modelQuasiPoisson)
## Fit negative binomial
modelNB <- pglm(formula = formula,family=negbin,data    = data1)
summary(modelNB)
## Exponentiated coefficients
exp(coef(modelNB))

## Fit hurdle
modelHurdle <- hurdle(formula = formula,
                      dist    = "negbin",
                      data    = data)
summary(modelHurdle)

## The hurdle part can be fit as follows.
hurdlePart <- glm(formula = I(numseek>0) ~ ngender + M1Age + M1EducLevel + Vaccination + hhsize ,
                  data    = data,
                  family  = binomial(link = "logit"))
coef(summary(hurdlePart))
## Model hurdle without vaccination
hurdleSimpler <- hurdle(formula = numseek ~ ngender + M1Age + M1EducLevel  + hhsize,
                        dist    = "negbin",
                        data    = data)
coef(summary(hurdleSimpler))
summary(modelHurdle)

## Exponentiated coefficients
expCoef <- exp(coef((modelHurdle)))
expCoef <- matrix(expCoef, ncol = 2)
rownames(expCoef) <- names(coef(hurdlePart))
colnames(expCoef) <- c("Count_model","Zero_hurdle_model")
expCoef

## LRT to compare these models
lmtest::lrtest(modelHurdle, hurdleSimpler)

## Fit zeroinfl
modelZeroInfl <- zeroinfl(formula = formula,
                          dist    = "negbin",
                          data    = data)
summary(modelZeroInfl)

## Exponentiated coefficients
expCoef <- exp(coef((modelZeroInfl)))
expCoef <- matrix(expCoef, ncol = 2)
rownames(expCoef) <- names(coef(hurdlePart))
colnames(expCoef) <- c("Count_model","Zero_inflation_model")
expCoef

## Comparing the models
lmtest::lrtest(modelHurdle, modelZeroInfl)
## Sample selection model with Utilization of healthcare in Kenya
numseekselect=utilization*(nseekcare==1)
ols <- lm(numseekselect ~ M1Age + factor(ngender))
summary(ols)
hist(numseekselect)
heckit.ml <- heckit(selection = nseekcare ~ M1Age + factor(ngender), outcome = numseek ~ M1Age+ factor(ngender), method = "ml")
summary(heckit.ml)
heckit.2step <- heckit(selection = nseekcare ~ M1Age + factor(ngender), outcome = numseek ~ M1Age+ factor(ngender), method = "2step")
summary(heckit.2step)






hist(fullinc)
#poisfit=goodfit(numbreath,"poisson")
#summary(poisfit)

#logit=glmmboot(Vaccination~fullinc+age+nchild+t, family=binomial(link="logit"), data=data, cluster=hhid)
summary(logit)

x=cbind(numbreath, numresp, numfever, numgastro, numseek, numother, numpneu,nummorb, nummorb2, nummorb3,nummorb4,nummorb5,nummorb6,nummorb7, health)
y=glm(Consultation~fullinc+age+nchild+factor(ngender)+VisitNumber+factor(M1EducLevel), data=data, family="binomial")
summary(y)
#Poisson regression
c1=glm(x[,1]~fullinc+age+ factor(ngender)+ nchild+ VisitNumber+t,family=poisson(link="log"), data=data)
summary(c1)
# Getting robust SE
cov.c1=vcovHC(c1, type="HC0")
std.err=sqrt(diag(cov.c1))
r.est=cbind(Estimate= coef(c1), "Robust SE" = std.err,
            "Pr(>|z|)" = 2 * pnorm(abs(coef(c1)/std.err), lower.tail=FALSE),
            LL = coef(c1) - 1.96 * std.err,
            UL = coef(c1) + 1.96 * std.err)

r.est

## update c1 model dropping nchild
c2 <- update(c1, . ~ . - nchild)
summary(c2)
## test model differences with chi square test
anova(c2, c1, test="Chisq")
with(c1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))
#poisson regression with random coef: interaction
c3=glm(x[,1]~fullinc+fullinc*age+fullinc*(factor(ngender))+fullinc*nchild+ fullinc*VisitNumber+fullinc*t, family=poisson(link="log"),data=data)
summary(c3)
# Getting robust SE
cov.c3=vcovHC(c3, type="HC0")
std.err=sqrt(diag(cov.c3))
r.est=cbind(Estimate= coef(c3), "Robust SE" = std.err,
            "Pr(>|z|)" = 2 * pnorm(abs(coef(c3)/std.err), lower.tail=FALSE),
            LL = coef(c3) - 1.96 * std.err,
            UL = coef(c3) + 1.96 * std.err)

r.est

## update c3 model dropping nchild
c4 <- update(c3, . ~ . - nchild)
summary(c4)
#poisson with respiratory symptoms
b1=glm(x[,2]~OffFarm+age+ factor(ngender)+nchild+VisitNumber+t,family=poisson(link="log"), data=data)
summary(b1)
# Getting robust SE
cov.b1=vcovHC(b1, type="HC0")
std.err=sqrt(diag(cov.b1))
r.est=cbind(Estimate= coef(b1), "Robust SE" = std.err,
            "Pr(>|z|)" = 2 * pnorm(abs(coef(b1)/std.err), lower.tail=FALSE),
            LL = coef(b1) - 1.96 * std.err,
            UL = coef(b1) + 1.96 * std.err)

r.est

## update b1 model dropping nchild
b2 <- update(b1, . ~ . - nchild)
summary(b2)
## test model differences with chi square test
anova(c2, c1, test="Chisq")
with(b1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

#poisson with fever symptoms
f1=glm(x[,3]~fullinc+age+ factor(ngender)+nchild+VisitNumber+t,family=poisson(link="log"), data=data)
summary(f1)
# Getting robust SE
cov.f1=vcovHC(f1, type="HC0")
std.err=sqrt(diag(cov.f1))
r.est=cbind(Estimate= coef(f1), "Robust SE" = std.err,
            "Pr(>|z|)" = 2 * pnorm(abs(coef(f1)/std.err), lower.tail=FALSE),
            LL = coef(f1) - 1.96 * std.err,
            UL = coef(f1) + 1.96 * std.err)

r.est

## update f1 model dropping nchild
f2 <- update(f1, . ~ . - nchild)
summary(f2)
## test model differences with chi square test
with(f1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))


#poisson with gastro symptoms
g1=glm(x[,4]~fullinc+age+ factor(ngender)+nchild+VisitNumber+t,family=poisson(link="log"), data=data)
summary(g1)
# Getting robust SE
cov.g1=vcovHC(g1, type="HC0")
std.err=sqrt(diag(cov.g1))
r.est=cbind(Estimate= coef(g1), "Robust SE" = std.err,
            "Pr(>|z|)" = 2 * pnorm(abs(coef(g1)/std.err), lower.tail=FALSE),
            LL = coef(g1) - 1.96 * std.err,
            UL = coef(g1) + 1.96 * std.err)

r.est

## update g1 model dropping nchild
g2 <- update(g1, . ~ . - nchild)
summary(g2)

with(g1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

#poisson with seek symptoms
s1=glm(x[,5]~fullinc+age+ factor(ngender)+nchild+VisitNumber+t,family=poisson(link="log"), data=data)
summary(s1)
# Getting robust SE
cov.s1=vcovHC(s1, type="HC0")
std.err=sqrt(diag(cov.s1))
r.est=cbind(Estimate= coef(s1), "Robust SE" = std.err,
            "Pr(>|z|)" = 2 * pnorm(abs(coef(s1)/std.err), lower.tail=FALSE),
            LL = coef(s1) - 1.96 * std.err,
            UL = coef(s1) + 1.96 * std.err)

r.est

## update s1 model dropping nchild
s2=update(s1, . ~ . - nchild)
summary(s2)

with(g1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))


#poisson with pneumonia symptoms
p1=glm(x[,7]~fullinc+age+ factor(ngender)+nchild+VisitNumber+t,family=poisson(link="log"), data=data)
summary(p1)
# Getting robust SE
cov.p1=vcovHC(p1, type="HC0")
std.err=sqrt(diag(cov.p1))
r.est=cbind(Estimate= coef(p1), "Robust SE" = std.err,
            "Pr(>|z|)" = 2 * pnorm(abs(coef(p1)/std.err), lower.tail=FALSE),
            LL = coef(p1) - 1.96 * std.err,
            UL = coef(p1) + 1.96 * std.err)

r.est

## update p1 model dropping nchild
p2=update(p1, . ~ . - nchild)
summary(p2)

with(s1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))


# aggregate Morbidity measure
n1=glm(x[,8]~avincome+avage+ factor(ngender)+ nchild+ avvisit+t,family=poisson(link="log"), data=data)
summary(n1)
# Getting robust SE
cov.n1=vcovHC(n1, type="HC0")
std.err=sqrt(diag(cov.n1))
r.est=cbind(Estimate= coef(n1), "Robust SE" = std.err,
            "Pr(>|z|)" = 2 * pnorm(abs(coef(n1)/std.err), lower.tail=FALSE),
            LL = coef(n1) - 1.96 * std.err,
            UL = coef(n1) + 1.96 * std.err)

r.est

## update p1 model dropping nchild
n2=update(n1, . ~ . - nchild)
summary(n2)

with(n1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))



h=polr(factor(x[,15]) ~ avincome+avage+ factor(ngender)+nchild+avvisit+t, data = data, Hess=TRUE)
summary(h)






y=glm(numfever ~ fullinc+ fullinc*age+ fullinc*nchild+fullinc*(factor(ngender))+fullinc*VisitNumber+fullinc*(factor(Quarter)),poisson, data=data)
summary(y)

data("data", package="pglm")
x=pglm(numfever ~ fullinc+ age+ nchild + as.numeric(Quarter),data, family = poisson, model = "within", print.level = 3, method="nr", index = c(hhid, Quarter))
summary(la)




