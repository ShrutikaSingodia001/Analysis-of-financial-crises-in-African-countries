#Data Preperation
africa <- read.csv("african_crises.csv")
View(africa)
dim(africa)
str(africa)
colnames(africa)
nrow(africa)
ncol(africa)
attach(africa)

#filter the top 5 countries in Africa where they have the highest number of cases
sort(table(africa$country), decreasing = TRUE)[1:5]  #Egypt, South Africa, Zimbabwe, Algeria, and Angola
library(dplyr)
Africa <- africa %>% select(country,year,exch_usd,domestic_debt_in_default,inflation_annual_cpi,banking_crisis) %>%
filter(country %in% c("Egypt","South Africa","Zimbabwe","Algeria","Angola"))
Africa <- filter(Africa, year > 1880)
View(Africa)
str(Africa)
detach(africa)

sort(table(Africa$country),decreasing=TRUE)
Africa$country <- droplevels(Africa$country)
anyNA(Africa)
table(Africa$year)

#install.packages("GGally")
Africa$exch_usd <- round(Africa$exch_usd,2)
Africa$inflation_annual_cpi <- round(Africa$inflation_annual_cpi,2)
summary(Africa$exch_usd)
boxplot(Africa$exch_usd, main = "exch_usd")
summary(Africa$inflation_annual_cpi)
boxplot(Africa$inflation_annual_cpi, main = "inflation_annual_cpi")

#outliers
outlier_iqr <- function(x){
  iqr <- IQR(x,na.rm = T,type = 7)
  q <- quantile(x)
  upper_bound = q[4]+(iqr*1.5)
  lower_bound = q[2]-(iqr*1.5)
  outliers <- which ((x > upper_bound) | (x < lower_bound))
  return(outliers)
}
print(outlier_iqr(Africa$inflation_annual_cpi))
print(outlier_iqr(Africa$exch_usd))

#remove significant outliers

Africa_data <- Africa[-c(129,130,131,132,133,137,151,479,481,482,483,484,484,485,486,487,488,489),]
View(Africa_data)
str(Africa_data)
summary(Africa_data)
boxplot(Africa_data$inflation_annual_cpi)
boxplot(Africa_data$exch_usd)
detach(Africa)
attach(Africa_data)
colnames(Africa_data)

#save Africa_data
getwd()
write.csv(Africa_data,'Africa_dateset.csv')

library(aod)
library(stats)
library(pROC)

#Banking crisis VS Domestic Debt

tt<-table(banking=Africa_data$banking_crisis, debt=Africa_data$domestic_debt_in_default)
prop.test(c(22,6),c(446,26),conf.level = 0.95,correct = FALSE)
prop.debt.banking<-tt[1,1]/colSums(tt)[1]
prop.nodebt.banking<-tt[1,2]/colSums(tt)[2]

#Risk difference
risk_diff <- abs(prop.debt.banking-prop.nodebt.banking)*100
#The risk of having a banking crisis is 18.14% higher among no domestic debt as among domestic debt
#Is "domestic_debt_in_default" a good predictor on causing a baking crisis? 

model<-glm(as.factor(banking_crisis)~domestic_debt_in_default, family = binomial)
summary(model)

#Domestic_debt_in_default is a significant value since its p-value(0.000645) less than 0.05.

#Odds ratio and 95% confident interval
exp (cbind (OR = coef (model), confint.default (model)))

#C-statistic
Africa_data$prob.model<-predict(model,  type=c("response"))
(g <-roc(Africa_data$banking_crisis ~ Africa_data$prob.model)) 
roc(Africa_data$banking_crisis ~ Africa_data$prob.model, plot=TRUE, legacy.axes=T, percent=T, 
    xlab="False Positive (%)", ylab="True Positive (%)", col="blue", lwd=4,  print.auc=T, print.auc.x=45, main = "ROC curve for model")

#Are the variables inflation_annual_cpi, exch_usd, domestic_debt_in_default and together good predictor for banking crisis?
model2<-glm(as.factor(banking_crisis)~Africa_data$domestic_debt_in_default+Africa_data$inflation_annual_cpi+Africa_data$exch_usd,family = binomial)
summary(model2)


#only inflation_annual_cpi variable id significant because the p-value is less than 0.05 but domestic_debt_in_default is no longer significant in predicting banking crisis along with the exch_usd variable.
#Odds ratio and 95% confident interval

exp (cbind (OR = coef (model2), confint.default (model2)))
#C-statistic

Africa_data$prob.model2<-predict(model2,  type=c("response"))
(g <-roc(Africa_data$banking_crisis ~ Africa_data$prob.model2)) 
roc(Africa_data$banking_crisis ~ Africa_data$prob.model2, plot=TRUE, legacy.axes=T, percent=T, 
    xlab="False Positive (%)", ylab="True Positive (%)", col="blue", lwd=2,  print.auc=T, print.auc.x=40, main = "ROC curve for model")

