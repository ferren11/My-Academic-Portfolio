#Import Data
dataGraduate = read.csv("F://Kuliah/Semester5/BayesianDataAnalysis/Admission_Predict.csv", sep=",", header=TRUE)
head(dataGraduate)

#Summary and Str
summary(dataGraduate)
str(dataGraduate)

#Drop Serial.No. Variable
dataGraduate = subset(dataGraduate, select = -c(Serial.No.))
head(dataGraduate, 10)

#Convert Chance of.Admit (0: not admitted, 1: admitted)
dataGraduate$Chance.of.Admit = ifelse(dataGraduate$Chance.of.Admit <= 0.5, 0, 1)
#dataGraduate$Chance.of.Admit = factor(dataGraduate$Chance.of.Admit)
head(dataGraduate, 10)

dataGraduate$Research = factor(dataGraduate$Research)

library(fastDummies)
dataGraduate <- dummy_cols(dataGraduate, select_columns = "University.Rating", remove_first_dummy = TRUE, 
                           remove_selected_columns = TRUE)
head(dataGraduate)
#dataGraduate$University.Rating = factor(dataGraduate$University.Rating)

str(dataGraduate)

#=============
# MODEL BIASA
#=============
base_model = glm(Chance.of.Admit~., data = dataGraduate, family="binomial")
summary(base_model)
library(lmtest)
lrtest(base_model)
library(ResourceSelection)
#H0: Model sudah sesuai
#H1: Model belum sesuai
hoslem.test(dataGraduate$Chance.of.Admit, fitted(base_model))
#karena pval 0.95 > alpha 0.05, maka gagal tolak H0. Artinya model sdh sesuai

base_model2 = glm(Chance.of.Admit~University.Rating_2+LOR+CGPA, data=dataGraduate, family = "binomial",)
summary(base_model2)
lrtest(base_model2)
hoslem.test(dataGraduate$Chance.of.Admit, fitted(base_model2))
#karena pval 0.78 > alpha 0.05, maka gagal tolak H0. Artinya model sdh sesuai

base_model3 = glm(Chance.of.Admit~CGPA, data=dataGraduate, family = "binomial")
summary(base_model3)
lrtest(base_model3)
hoslem.test(dataGraduate$Chance.of.Admit, fitted(base_model3))
#karena pval 0.99 < alpha 0.05, maka gagal tolak H0. Artinya model sdh sesuai

base_model4 = glm(Chance.of.Admit~CGPA+GRE.Score, data=dataGraduate, family = "binomial")
summary(base_model4)

#Uji Multikolinearitas
library(car)
vif(base_model)
vif(base_model2)

#Evaluasi 
library(caret)
y = dataGraduate$Chance.of.Admit

yhat = fitted(base_model)
predict = as.factor(ifelse(round(yhat)==1, "admitted", "fail"))
actual = as.factor(ifelse(y==1, "admitted", "fail"))
confMatrix = confusionMatrix(predict, actual)
confMatrix

yhat2 = fitted(base_model2)
predict2 = as.factor(ifelse(round(yhat2)==1, "admitted", "fail"))
actual2 = as.factor(ifelse(y==1, "admitted", "fail"))
confMatrix2 = confusionMatrix(predict2, actual2)
confMatrix2

yhat3 = fitted(base_model3)
predict3 = as.factor(ifelse(round(yhat3)==1, "admitted", "fail"))
actual3 = as.factor(ifelse(y==1, "admitted", "fail"))
confMatrix3 = confusionMatrix(predict3, actual3)
confMatrix3

yhat4 = fitted(base_model4)
predict4 = as.factor(ifelse(round(yhat4)==1, "admitted", "fail"))
actual4 = as.factor(ifelse(y==1, "admitted", "fail"))
confMatrix4 = confusionMatrix(predict4, actual4)
confMatrix4

ModelName <- c("base_model", "base_model2", "base_model3")
acc1 = confMatrix$overall['Accuracy']
acc2 = confMatrix2$overall['Accuracy']
acc3 = confMatrix3$overall['Accuracy']
Accuracy <- c(acc1, acc2, acc3)

sens1 = confMatrix$byClass['Sensitivity']
sens2 = confMatrix2$byClass['Sensitivity']
sens3 = confMatrix3$byClass['Sensitivity']
Sensitivity <- c(sens1, sens2, sens3)

spec1 = confMatrix$byClass['Specificity']
spec2 = confMatrix2$byClass['Specificity']
spec3 = confMatrix3$byClass['Specificity']
Specificity <- c(spec1, spec2, spec3)

prec1 = confMatrix$byClass['Precision']
prec2 = confMatrix2$byClass['Precision']
prec3 = confMatrix3$byClass['Precision']
Precision <- c(prec1, prec2, prec3)

f11 = confMatrix$byClass['F1']
f12 = confMatrix2$byClass['F1']
f13 = confMatrix3$byClass['F1']
F1_Score <- c(f11, f12, f13)

confMatrixCompare <- data.frame(ModelName, Accuracy, Sensitivity, Specificity, Precision, F1_Score)
confMatrixCompare

library(performance)
c1 <- r2_coxsnell(base_model)
c2 <- r2_coxsnell(base_model2)
c3 <- r2_coxsnell(base_model3)
CoxnSnell_R2 <- c(c1, c2, c3)

library(fmsb)
n1 <- NagelkerkeR2(base_model)$R2
n2 <- NagelkerkeR2(base_model2)$R2
n3 <- NagelkerkeR2(base_model3)$R2
NagelKerne_R2 <- c(n1, n2, n3)

library(pscl)
m1 <- pR2(base_model)['McFadden']
m2 <- pR2(base_model2)['McFadden']
m3 <- pR2(base_model3)['McFadden']
McFadden_R2 <- c(m1, m2, m3)

a1 = AIC(base_model)
a2 = AIC(base_model2)
a3 = AIC(base_model3)
AIC <- c(a1, a2, a3)

performanceCompare <- data.frame(ModelName, CoxnSnell_R2, NagelKerne_R2, McFadden_R2, AIC)
performanceCompare

#================
# Model BayesGLM
#================
library(arm)
modelBayes = bayesglm(Chance.of.Admit~., data = dataGraduate, family="binomial")
summary(modelBayes)
lrtest(modelBayes)
hoslem.test(dataGraduate$Chance.of.Admit, fitted(modelBayes))

modelBayes2 = bayesglm(Chance.of.Admit~LOR+CGPA, data = dataGraduate, family="binomial")
summary(modelBayes2)
lrtest(modelBayes2)
hoslem.test(dataGraduate$Chance.of.Admit, fitted(modelBayes2))

modelBayes3 = bayesglm(Chance.of.Admit~CGPA, data = dataGraduate, family="binomial")
summary(modelBayes3)
lrtest(modelBayes3)
hoslem.test(dataGraduate$Chance.of.Admit, fitted(modelBayes3))

#check multicollinearity
library(car)
vif(modelBayes)
vif(modelBayes2)

#Confusion Matrix
y = dataGraduate$Chance.of.Admit

yhat = fitted(modelBayes)
predict = as.factor(ifelse(round(yhat)==1, "admitted", "fail"))
actual = as.factor(ifelse(y==1, "admitted", "fail"))
confMatrix = confusionMatrix(predict, actual)
confMatrix

yhat2 = fitted(modelBayes2)
predict2 = as.factor(ifelse(round(yhat2)==1, "admitted", "fail"))
actual2 = as.factor(ifelse(y==1, "admitted", "fail"))
confMatrix2 = confusionMatrix(predict2, actual2)
confMatrix2

yhat3 = fitted(modelBayes3)
predict3 = as.factor(ifelse(round(yhat3)==1, "admitted", "fail"))
actual3 = as.factor(ifelse(y==1, "admitted", "fail"))
confMatrix3 = confusionMatrix(predict3, actual3)
confMatrix3

yhat4 = fitted(modelBayes4)
predict4 = as.factor(ifelse(round(yhat4)==1, "admitted", "fail"))
actual4 = as.factor(ifelse(y==1, "admitted", "fail"))
confMatrix4 = confusionMatrix(predict4, actual4)
confMatrix4

Model_Name <- c("ModelBayes", "ModelBayes2", "ModelBayes3")
acc1 = confMatrix$overall['Accuracy']
acc2 = confMatrix2$overall['Accuracy']
acc3 = confMatrix3$overall['Accuracy']
Accuracy <- c(acc1, acc2, acc3)

sens1 = confMatrix$byClass['Sensitivity']
sens2 = confMatrix2$byClass['Sensitivity']
sens3 = confMatrix3$byClass['Sensitivity']
Sensitivity <- c(sens1, sens2, sens3)

spec1 = confMatrix$byClass['Specificity']
spec2 = confMatrix2$byClass['Specificity']
spec3 = confMatrix3$byClass['Specificity']
Specificity <- c(spec1, spec2, spec3)

prec1 = confMatrix$byClass['Precision']
prec2 = confMatrix2$byClass['Precision']
prec3 = confMatrix3$byClass['Precision']
Precision <- c(prec1, prec2, prec3)

f11 = confMatrix$byClass['F1']
f12 = confMatrix2$byClass['F1']
f13 = confMatrix3$byClass['F1']
F1_Score <- c(f11, f12, f13)

confMatrixCompare <- data.frame(Model_Name, Accuracy, Sensitivity, Specificity, Precision, F1_Score)
confMatrixCompare

library(performance)
c1 <- r2_coxsnell(modelBayes)
c2 <- r2_coxsnell(modelBayes2)
c3 <- r2_coxsnell(modelBayes3)
CoxnSnell_R2 <- c(c1, c2, c3)

library(fmsb)
n1 <- NagelkerkeR2(modelBayes)$R2
n2 <- NagelkerkeR2(modelBayes2)$R2
n3 <- NagelkerkeR2(modelBayes3)$R2
NagelKerne_R2 <- c(n1, n2, n3)

library(pscl)
m1 <- pR2(modelBayes)['McFadden']
m2 <- pR2(modelBayes2)['McFadden']
m3 <- pR2(modelBayes3)['McFadden']
McFadden_R2 <- c(m1, m2, m3)

a1 = AIC(modelBayes)
a2 = AIC(modelBayes2)
a3 = AIC(modelBayes3)
AIC <- c(a1, a2, a3)

performanceCompare <- data.frame(Model_Name, CoxnSnell_R2, NagelKerne_R2, McFadden_R2, AIC)
performanceCompare

coef(modelBayes)
exp(coef(modelBayes))

BIC(base_model)
BIC(base_model2)
BIC(base_model3)
BIC(modelBayes)
BIC(modelBayes2)
BIC(modelBayes3)
