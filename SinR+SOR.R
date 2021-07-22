library(tidyverse)
library(ModelMetrics)
library(openxlsx)
library(readr)
library(gvlma)
Base <- read_csv("D:/Проекты/Язык R/Заметки на habr/З4 - PSpline Regression/acs2017_census_tract_data.csv")
Base_add <- as.data.frame(na.omit(Base[,c(5:37)])) 
glimpse(Base_add) # 11 - 14 переменные: зависимые
tr.index <- sample(1:74001, 74001*0.8) 
# Отбираем переменные
mod <- 20
library("FSinR")
evaluator <- filterEvaluator('determinationCoefficient') # Чем мы мерим
woa_search <- whaleOptimization()
set.seed(100)
Res<-as.data.frame(matrix(0, nrow = mod, ncol = 7))
colnames(Res) <- c("R2", "MAE", "MSE", "Approximation", "Appr_5_perc","Appr_15_perc", "Time")
Res_per_1<-as.data.frame(matrix(0, nrow = mod, ncol = 29))
for(k in 1:mod) {
print(k)
  tr.index <- sample(1:74001, 74001*0.8) 
  Base_tr <- Base_add[tr.index, c(1:10,14:33)]
  Base_test <- Base_add[-tr.index, c(1:10,14:33)]
  model_1<-woa_search(Base_tr, 'Income', evaluator)
  Res_per_1[k,] <- model_1$bestFeatures
}
colnames(model_1$bestFeatures)[order(apply(Res_per_1,2,sum), decreasing = TRUE)]
# Модели
library(SOP)
tr.index <- sample(1:74001, 74001*0.8)
Base_tr_1 <- Base_add[tr.index, c(1:10,14:33)]
Base_test_1 <- Base_add[-tr.index, c(1:10,14:33)]
m1_lm <- lm(Income ~ Poverty, data = Base_tr_1)
summary(m1_lm)
gvlma(m1_lm)
Z_test <- predict(m1_lm, newdata = Base_test_1)
res1<-c(cor(Z_test, Base_test_1$Income)^2,mae(Z_test, Base_test_1$Income),
  median(abs((Z_test-Base_test_1$Income)/Base_test_1$Income))*100,
  sum(abs((Z_test-Base_test_1$Income)/Base_test_1$Income*100) < 5)/19457*100, 
  sum(abs((Z_test-Base_test_1$Income)/Base_test_1$Income*100) < 15)/19457*100)
m1_f <- sop(Income ~ f(Poverty), data = Base_tr_1)
summary(m1_f)
Z_test <- predict(m1_f, newdata = Base_test_1)
res2<-c(cor(Z_test, Base_test_1$Income)^2,mae(Z_test, Base_test_1$Income),
  median(abs((Z_test-Base_test_1$Income)/Base_test_1$Income))*100,
  sum(abs((Z_test-Base_test_1$Income)/Base_test_1$Income*100) < 5)/19457*100, 
  sum(abs((Z_test-Base_test_1$Income)/Base_test_1$Income*100) < 15)/19457*100)
m1_ad <- sop(Income ~ ad(Poverty, pord = 4, degree = 5), data = Base_tr_1)
summary(m1_ad)
Z_test <- predict(m1_ad, newdata = Base_test_1)
res3<-c(cor(Z_test, Base_test_1$Income)^2,mae(Z_test, Base_test_1$Income),
  median(abs((Z_test-Base_test_1$Income)/Base_test_1$Income))*100,
  sum(abs((Z_test-Base_test_1$Income)/Base_test_1$Income*100) < 5)/19457*100, 
  sum(abs((Z_test-Base_test_1$Income)/Base_test_1$Income*100) < 15)/19457*100)
m2_lm <- lm(Income ~ Poverty+Professional, data = Base_tr_1)
summary(m2_lm)
gvlma(m2_lm)
Z_test <- predict(m2_lm, newdata = Base_test_1)
res4<-c(cor(Z_test, Base_test_1$Income)^2,mae(Z_test, Base_test_1$Income),
        median(abs((Z_test-Base_test_1$Income)/Base_test_1$Income))*100,
        sum(abs((Z_test-Base_test_1$Income)/Base_test_1$Income*100) < 5)/19457*100, 
        sum(abs((Z_test-Base_test_1$Income)/Base_test_1$Income*100) < 15)/19457*100)
m2_f <- sop(Income ~ f(Poverty,Professional), data = Base_tr_1)
summary(m2_f)
Z_test <- predict(m2_f, newdata = Base_test_1)
res5<-c(cor(Z_test, Base_test_1$Income)^2,mae(Z_test, Base_test_1$Income),
        median(abs((Z_test-Base_test_1$Income)/Base_test_1$Income))*100,
        sum(abs((Z_test-Base_test_1$Income)/Base_test_1$Income*100) < 5)/19457*100, 
        sum(abs((Z_test-Base_test_1$Income)/Base_test_1$Income*100) < 15)/19457*100)
m2_f_1 <- sop(Income ~ Poverty + f(Professional), data = Base_tr_1)
summary(m2_f_1 )
Z_test <- predict(m2_f_1, newdata = Base_test_1)
res7<-c(cor(Z_test, Base_test_1$Income)^2,mae(Z_test, Base_test_1$Income),
        median(abs((Z_test-Base_test_1$Income)/Base_test_1$Income))*100,
        sum(abs((Z_test-Base_test_1$Income)/Base_test_1$Income*100) < 5)/19457*100, 
        sum(abs((Z_test-Base_test_1$Income)/Base_test_1$Income*100) < 15)/19457*100)
m2_ad <- sop(Income ~ ad(Poverty, pord = 4, degree = 5)+ad(Professional, pord = 3, degree = 4), data = Base_tr_1)
summary(m2_ad)
Z_test <- predict(m2_ad, newdata = Base_test_1)
res6<-c(cor(Z_test, Base_test_1$Income)^2,mae(Z_test, Base_test_1$Income),
        median(abs((Z_test-Base_test_1$Income)/Base_test_1$Income))*100,
        sum(abs((Z_test-Base_test_1$Income)/Base_test_1$Income*100) < 5)/19457*100, 
        sum(abs((Z_test-Base_test_1$Income)/Base_test_1$Income*100) < 15)/19457*100)
Res <- rbind(res1,res2,res3,res4,res5,res7,res6)
colnames(Res) <- c("R2","MAE","Median error", "Appr5","Appr15")
rownames(Res) <- c("m1_lm","m1_f","m1_ad","m2_lm","m2_f","m2_f_1","m2_ad" )
Res
plot(m1_ad)
