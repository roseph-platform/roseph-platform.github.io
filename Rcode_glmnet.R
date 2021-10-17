#setwd("/Users/Xinwei/Desktop/SEU-Data Analytics/materials")
prostate<-read.table('prostate.data.txt', sep= "", header=T)
prostate = prostate[,-1];

dim(prostate)
names(prostate)

#----------------standardize the data;
#prostate[,1:8] = scale(prostate[,1:8]);

Xmat = prostate[,1:8];

cor(Xmat)

pairs(Xmat);

plot(prostate$lcp, prostate$lcavol)


y = prostate[,9];

par(mfrow=c(2,2))
plot(Xmat[,1],y, main = names(prostate)[1]);
plot(Xmat[,2],y, main = names(prostate)[2]);
plot(Xmat[,3],y, main = names(prostate)[3]);
plot(Xmat[,4],y, main = names(prostate)[4]);

par(mfrow=c(2,2))
plot(Xmat[,5],y, main = names(prostate)[5]);
plot(Xmat[,6],y, main = names(prostate)[6]);
plot(Xmat[,7],y, main = names(prostate)[7]);
plot(Xmat[,8],y, main = names(prostate)[8]);

boxplot(y~Xmat[,5])

fit<-lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45,data=prostate)
summary(fit)

#prostate_alt = prostate;
#prostate_alt$age = prostate_alt$age/100;

fit_alt<-lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleaso+pgg45,data=prostate_alt)

fit_alt<-lm(lpsa ~lweight+age+lbph+svi+lcp+gleason+pgg45,data=prostate)
summary(fit_alt)


anova(fit)

plot(fit)

fit$coefficients

y_residual = fit$residuals;

yobs = prostate$lpsa;

yfit = predict(fit);

yobs-yfit

plot(yobs-yfit, y_residual)



#---------------------------------

#----------------------------------------
#install.packages("leaps")
library(leaps)

subset<-leaps(x=prostate[,1:8],y=prostate[,9]);

plot(x=subset$size,y=subset$Cp,xlab='size',ylab='Cp')

chosed = which.min(subset$Cp);
subset$Cp[chosed]

subset$which[chosed,]

#-------full model
fit1<-lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=prostate)

#------smallest model
fit0<-lm(lpsa~1,data=prostate)

#--------forward selection
fit.forward<-step(fit0,scope=list(lower=lpsa~1, upper=lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45),direction='forward')

summary(fit.forward)

#---------backward
fit.backward<-step(fit1,scope=list(lower=lpsa~1, upper=lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45),direction='backward')

summary(fit.backward)

#---------------------------------
#stepwise regression
library(MASS)

step1 = stepAIC(fit1, direction = "both")
summary(step1)

step2 = stepAIC(fit1, direction = "both", k = log(length(yobs)))
summary(step2)


###########################################
#----------------standardize the data;
prostate[,1:8] = scale(prostate[,1:8]);

Xmat = prostate[,1:8];

apply(Xmat,2, mean);
apply(Xmat,2, var);

##----------------------
library(MASS)
fit.ridge<-lm.ridge(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=prostate, lambda=seq(0,1000,10))

plot(fit.ridge)
#matplot( fit.ridge$lambda, t(fit.ridge$coef), type="l", col="blue", ylab="coefficients" )


##---------------------LASSO
library(lars)
#help(lars)
fit.lasso<-lars(x=as.matrix(prostate[,1:8]), y=prostate[,9], type="lasso")
plot(fit.lasso)

fit.lasso$lambda

Xtest = as.matrix(prostate[1:8,1:8]);

yhat_lasso<-predict.lars(fit.lasso, Xtest, type=c("fit"), mode='lambda')$fit
#yhat_lasso<-predict.lars(fit.lasso, Xtest, 3, type=c("fit"), mode='lambda')$fit

coef(fit.lasso, s=3);

library(glmnet)
#help(glmnet)
glm_lasso= glmnet(x=as.matrix(prostate[,1:8]), y=prostate[,9], family = 'gaussian', alpha = 1)

names(glm_lasso);

plot(glm_lasso)

CV_info=cv.glmnet(x=as.matrix(prostate[,1:8]), y=prostate[,9], nfolds=5, alpha = 1);

plot(CV_info$lambda, CV_info$cvm, xlim = c(0,0.5));

lambda = cv.glmnet(x=as.matrix(prostate[,1:8]), y=prostate[,9], nfolds=5, alpha = 1)$lambda.min

lambda

beta_est = as.vector(coef(glm_lasso, s = lambda))

yhat_glm_lasso<-predict(glm_lasso, Xtest, s = lambda);


