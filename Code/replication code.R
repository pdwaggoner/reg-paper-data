library(glmnet)
library(foreach)
library(pROC)
library(ggplot2)
library(ggfortify)
library(ggpubr)
library(gridExtra)
library(stargazer)
library(tidyverse)
library(here)

# read in the data for stage 1
dataset <- read_csv(here("Data", "politics_and_need_rescale.csv"))

# store some things
d <- dataset
f <- oppose_expansion ~ gop_governor + percent_favorable_aca + gop_leg + percent_uninsured + bal2012 + multiplier + percent_nonwhite + percent_metro
y <- d$oppose_expansion
X <- model.matrix(f, d)
n <- length(y)


# LASSO WITH ALPHA = 1
cv1 <- cv.glmnet(X, y, family = "binomial", nfold = 100, type.measure = "deviance", paralle = TRUE, alpha = 1)
lassomod <- glmnet(X, y, family = "binomial", lambda = cv1$lambda.1se, alpha = 1)
cv1.glmnet.fit <- (cv1$glmnet.fit)
tmp_coeffs <- coef(cv1.glmnet.fit, s = cv1$lambda.1se)
lassotable <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
stargazer(lassotable, type = "text", title = "LASSO Coefficients for Inital Dataset", summary = FALSE, rownames = FALSE)


# RIDGE WITH ALPHA = 0
cv2 <- cv.glmnet(X, y, family = "binomial", nfold = 100, type.measure = "deviance", paralle = TRUE, alpha = 0)
ridgemod <- glmnet(X, y, family = "binomial", lambda = cv2$lambda.1se, alpha = 0)
cv2.glmnet.fit <- (cv2$glmnet.fit)
tmp_coeffs <- coef(cv2.glmnet.fit, s = cv2$lambda.1se)
ridgetable <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
stargazer(ridgetable, type = "text", title = "Ridge Coefficients for Inital Dataset", summary = FALSE, rownames = FALSE)


# ELASTIC NET WITH 0 < ALPHA < 1
a <- seq(0.1, 0.9, 0.05)
search <- foreach(i = a, .combine = rbind) %dopar% {
  cv <- cv.glmnet(X, y, family = "binomial", nfold = 100, type.measure = "deviance", paralle = TRUE, alpha = i)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
}
cv3 <- search[search$cvm == min(search$cvm), ]
elasticnetmod <- glmnet(X, y, family = "binomial", lambda = cv3$lambda.1se, alpha = cv3$alpha)
cv3.1 <- cv.glmnet(X, y, family = "binomial", nfold = 100, type.measure = "deviance", paralle = TRUE, alpha = 0.6)
cv3.1.glmnet.fit <- (cv3.1$glmnet.fit)
tmp_coeffs <- coef(cv3.1.glmnet.fit, s = cv3$lambda.1se)
elasticnettable <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
stargazer(elasticnettable, type = "text", title = "Elastic Net Coefficients for Inital Dataset", summary = FALSE, rownames = FALSE)
cv3$alpha
cv2$lambda.1se


# ALL MODS IN ONE TABLE
## RIDGE
stargazer(ridgetable, 
          summary = FALSE, 
          single.row = TRUE, 
          title = "Output from Ridge: Inital Dataset", out = "ridge_initial.html")

## LASSO
stargazer(lassotable, 
          summary = FALSE, 
          single.row = TRUE, 
          title = "Output from LASSO: Inital Dataset", out = "LASSO_initial.html")

## EN
stargazer(elasticnettable, 
          summary = FALSE, 
          single.row = TRUE, 
          title = "Output from EN: Inital Dataset", out = "EN_initial.html")


# MSE
for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(X, y, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}

yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=X)
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=X)
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=X)
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=X)
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=X)
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=X)
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=X)
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=X)
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=X)
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=X)
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=X)

mse0 <- mean((y - yhat0)^2)
mse1 <- mean((y - yhat1)^2)
mse2 <- mean((y - yhat2)^2)
mse3 <- mean((y - yhat3)^2)
mse4 <- mean((y - yhat4)^2)
mse5 <- mean((y - yhat5)^2)
mse6 <- mean((y - yhat6)^2)
mse7 <- mean((y - yhat7)^2)
mse8 <- mean((y - yhat8)^2)
mse9 <- mean((y - yhat9)^2)
mse10 <- mean((y - yhat10)^2)

mses <- matrix(c(mse0,mse1,mse2,mse3,mse4,mse5,mse6,mse7,mse8,mse9,mse10),ncol=1,byrow=FALSE)
colnames(mses) <- c("MSE")
rownames(mses) <- c("Alpha=0 (Ridge)","Alpha=0.1","Alpha=0.1", "Alpha=0.3", "Alpha=0.4", "Alpha=0.5", "Alpha=0.6", "Alpha=0.7", "Alpha=0.8", "Alpha=0.9", "Alpha=1 (LASSO)")
MSEtab <- as.table(mses)

stargazer(MSEtab, 
          colnames = FALSE, rownames = FALSE, digits = 3,
          title = "Mean Squared Error at All levels of Alpha", out = "MSE_initial.html")



# plots
lassoplot <- autoplot(cv1$glmnet.fit, "lambda", label = TRUE, main = "LASSO (alpha = 1)") + ylim(-5,5) + theme(legend.position="right") + scale_colour_discrete(name = "Variables", labels = c("Intercept", "2012 Ballot", "GOP Governor", "GOP Legislature", "Multiplier", "% Favorable ACA", "% Metropolitan", "% Nonwhite", "% Uninsured")) + theme(legend.title = element_text(size=20)) + theme(legend.text = element_text(size = 18)) + geom_vline(mapping = NULL, data = NULL, xintercept = log(cv1$lambda.1se), na.rm = FALSE, show.legend = TRUE)

ridgeplot <- autoplot(cv2$glmnet.fit, "lambda", label = TRUE, main = "Ridge (alpha = 0)") + ylim(-2,2) + xlim(-4,4) + geom_vline(mapping = NULL, data = NULL, xintercept = log(cv2$lambda.1se), na.rm = FALSE, show.legend = TRUE)

elasticnetplot <- autoplot(cv3.1$glmnet.fit, "lambda", label = TRUE, main = "Elastic Net (alpha = 0.6)") + ylim(-5,5) + geom_vline(mapping = NULL, data = NULL, xintercept = log(cv3$lambda.1se), na.rm = FALSE, show.legend = TRUE)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

mylegend<-g_legend(lassoplot)


figure <- grid.arrange(arrangeGrob(ridgeplot + 
                                     theme_bw() +
                                     theme(legend.position="none"),
                                   lassoplot + 
                                     theme_bw() +
                                     theme(legend.position="none"), 
                                   elasticnetplot + 
                                     theme_bw() +
                                     theme(legend.position="none"), 
                      mylegend, nrow = 1))







# STAGE  2 ANALYSIS

# read in data for stage 2
dataset2 <- read_csv(here("Data", "seconddataset.csv"))

# store some things
d.2 <- dataset2
f.2 <- oppose_expansion ~ gop_governor + percent_favorable_aca + gop_leg + percent_uninsured + bal2012 + 
  multiplier + percent_nonwhite + percent_metro + fem_gov + fem_leg + shor_hou + shor_sen
y.2 <- d.2$oppose_expansion
X.2 <- model.matrix(f.2, d.2)
n.2 <- length(y.2)


# LASSO
cv1.2 <- cv.glmnet(X.2, y.2, family = "binomial", nfold = 100, type.measure = "deviance", paralle = TRUE, alpha = 1)
lassomod.2 <- glmnet(X.2, y.2, family = "binomial", lambda = cv1.2$lambda.1se, alpha = 1)
cv1.2.glmnet.fit <- (cv1.2$glmnet.fit)
tmp_coeffs <- coef(cv1.2.glmnet.fit, s = cv1.2$lambda.1se)
lasso2table <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
stargazer(lasso2table, type = "text", title = "Lasso Coefficients for Second Dataset", summary = FALSE, rownames = FALSE)
cv1.2$lambda.1se


# RIDGE
cv2.2 <- cv.glmnet(X.2, y.2, family = "binomial", nfold = 100, type.measure = "deviance", paralle = TRUE, alpha = 0)
ridgemod.2 <- glmnet(X.2, y.2, family = "binomial", lambda = cv2.2$lambda.1se, alpha = 0)
cv2.2.glmnet.fit <- (cv2.2$glmnet.fit)
tmp_coeffs <- coef(cv2.2.glmnet.fit, s = cv2.2$lambda.1se)
ridge2table <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
stargazer(ridge2table, type = "text", title = "Ridge Coefficients for Second Dataset", summary = FALSE, rownames = FALSE)
cv2.2$lambda.1se


# EN
a <- seq(0.1, 0.9, 0.05)
search <- foreach(i = a, .combine = rbind) %dopar% {
  cv <- cv.glmnet(X.2, y.2, family = "binomial", nfold = 100, type.measure = "deviance", paralle = TRUE, alpha = i)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
}
cv3.2 <- search[search$cvm == min(search$cvm), ]
elasticnetmod.2 <- glmnet(X.2, y.2, family = "binomial", lambda = cv3.2$lambda.1se, alpha = cv3.2$alpha)
cv3.2.1 <- cv.glmnet(X.2, y.2, family = "binomial", nfold = 100, type.measure = "deviance", paralle = TRUE, alpha = 0.25)
cv3.2.1.glmnet.fit <- (cv3.2.1$glmnet.fit)
tmp_coeffs <- coef(cv3.2.1.glmnet.fit, s = cv3.2$lambda.1se)
elasticnet2table <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
stargazer(elasticnet2table, type = "text", title = "Elastic Net Coefficients for Second Dataset", summary = FALSE, rownames = FALSE)
cv3.2$lambda.1se
cv3.2$alpha


# view all output together
## RIDGE
stargazer(ridge2table, 
          summary = FALSE, 
          single.row = TRUE, 
          title = "Output from Ridge: Stage 2", out = "ridge_2.html")

## LASSO
stargazer(lasso2table, 
          summary = FALSE, 
          single.row = TRUE, 
          title = "Output from LASSO: Stage 2", out = "LASSO_2.html")

## EN
stargazer(elasticnet2table, 
          summary = FALSE, 
          single.row = TRUE, 
          title = "Output from EN: Stage 2", out = "EN_2.html")


# MSE - stage 2
for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(X.2, y.2, type.measure="mse", alpha=i/10,family="gaussian"))
}
yhat0.2 <- predict(fit0, s=fit0$lambda.1se, newx=X.2)
yhat1.2 <- predict(fit1, s=fit1$lambda.1se, newx=X.2)
yhat2.2 <- predict(fit2, s=fit2$lambda.1se, newx=X.2)
yhat3.2 <- predict(fit3, s=fit3$lambda.1se, newx=X.2)
yhat4.2 <- predict(fit4, s=fit4$lambda.1se, newx=X.2)
yhat5.2 <- predict(fit5, s=fit5$lambda.1se, newx=X.2)
yhat6.2 <- predict(fit6, s=fit6$lambda.1se, newx=X.2)
yhat7.2 <- predict(fit7, s=fit7$lambda.1se, newx=X.2)
yhat8.2 <- predict(fit8, s=fit8$lambda.1se, newx=X.2)
yhat9.2 <- predict(fit9, s=fit9$lambda.1se, newx=X.2)
yhat10.2 <- predict(fit10, s=fit10$lambda.1se, newx=X.2)

mse0.2 <- mean((y.2 - yhat0.2)^2)
mse1.2 <- mean((y.2 - yhat1.2)^2)
mse2.2 <- mean((y.2 - yhat2.2)^2)
mse3.2 <- mean((y.2 - yhat3.2)^2)
mse4.2 <- mean((y.2 - yhat4.2)^2)
mse5.2 <- mean((y.2 - yhat5.2)^2)
mse6.2 <- mean((y.2 - yhat6.2)^2)
mse7.2 <- mean((y.2 - yhat7.2)^2)
mse8.2 <- mean((y.2 - yhat8.2)^2)
mse9.2 <- mean((y.2 - yhat9.2)^2)
mse10.2 <- mean((y.2 - yhat10.2)^2)

mses.2 <- matrix(c(mse0.2,mse1.2,mse2.2,mse3.2,mse4.2,mse5.2,mse6.2,mse7.2,mse8.2,mse9.2,mse10.2),ncol=1,byrow=FALSE)
colnames(mses.2) <- c("MSE")
rownames(mses.2) <- c("Alpha=0 (Ridge)","Alpha=0.1","Alpha=0.2", "Alpha=0.3", "Alpha=0.4", "Alpha=0.5", "Alpha=0.6", "Alpha=0.7", "Alpha=0.8", "Alpha=0.9", "Alpha=1 (LASSO)")
MSEtab.2 <- as.table(mses.2)
#stargazer(MSEtab.2, type = "text", title = "Mean Squared Errors (MSEs) for levels of Alpha in Second Test", colnames = FALSE, rownames = FALSE)

stargazer(MSEtab.2, 
          colnames = FALSE, rownames = FALSE, digits = 3,
          title = "Mean Squared Error at All levels of Alpha", out = "MSE_2.html")


# plots - stage 2
lassoplot.2 <- autoplot(cv1.2$glmnet.fit, "lambda", label = TRUE, main = "LASSO (alpha = 1)") + ylim(-5,5) + theme(legend.position="right") + scale_colour_discrete(name = "Variables", labels = c("Intercept", "2012 Ballot", "Female Governor", "% Female Legislature", "GOP Governor", "GOP Legislature", "Multiplier", "% Favorable ACA", "% Metropolitan", "% Nonwhite", "% Uninsured", "Shor: House", "Shor: Senate")) + theme(legend.title = element_text(size=20)) + theme(legend.text = element_text(size = 18)) + geom_vline(mapping = NULL, data = NULL, xintercept = log(cv1.2$lambda.1se), na.rm = FALSE, show.legend = TRUE)

ridgeplot.2 <- autoplot(cv2.2$glmnet.fit, "lambda", label = TRUE, main = "Ridge (alpha = 0)") + ylim(-2,2) + geom_vline(mapping = NULL, data = NULL, 
                                                                                                                            xintercept = log(cv2.2$lambda.1se), 
                                                                                                                            na.rm = FALSE, show.legend = TRUE)

elasticnetplot.2 <- autoplot(cv3.2.1$glmnet.fit, "lambda", label = TRUE, main = "Elastic Net (alpha = 0.25)") + ylim(-5,5) + geom_vline(mapping = NULL, data = NULL, xintercept = log(cv3.2$lambda.1se), na.rm = FALSE, show.legend = TRUE)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

mylegend.2<-g_legend(lassoplot.2)


# OLD FIGURE
#figure.2 <- grid.arrange(arrangeGrob(lassoplot.2 + theme(legend.position="none"), ridgeplot.2 + theme(legend.position="none"), elasticnetplot.2 + theme(legend.position="none"), mylegend.2, nrow = 1))

## make background black and white - cleaner (theme_bw)
figure.2 <- grid.arrange(arrangeGrob(ridgeplot.2 + 
                                     theme_bw() +
                                     theme(legend.position="none"),
                                   lassoplot.2 + 
                                     theme_bw() +
                                     theme(legend.position="none"), 
                                   elasticnetplot.2 + 
                                     theme_bw() +
                                     theme(legend.position="none"), 
                                   mylegend.2, nrow = 1))




# APPENDIX
## plot CV process for selecting lambda

# stage 1
par(mfrow = c(2,2))
plot(cv2, main = "Ridge: Stage 1")# ridge
plot(cv1, main = "LASSO: Stage 1") # LASSO
plot(cv3.1, main = "Elastic-net: Stage 1") # en

# stage 2
par(mfrow = c(2,2))
plot(cv2.2, main = "Ridge: Stage 2") # ridge
plot(cv1.2, main = "LASSO: Stage 2") # LASSO
plot(cv3.2.1, main = "Elastic-net: Stage 2") # en

