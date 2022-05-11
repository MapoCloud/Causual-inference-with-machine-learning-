library(grf)
library(ggplot2)
#导入数据
setwd("C:/Users/王赫/Desktop/4月：东西结对帮扶/数据/衍生数据/合并数据")
df=read.csv("testdata.csv")
View(df)
#将样本分成训练集和测试集
set.seed(1839)
cases <- sample(seq_len(nrow(df)), round(nrow(df) * .5))
df.train <- df[cases, ]
df.test <- df[-cases, ]
#提取出处理变量
help <- as.matrix(df.train$help)
#提取出输出变量
reilience <- as.matrix(df.train$rho1)
#提取控制变量
X <- model.matrix(lm(reilience ~ -1+members+age+kids+labors+unhealth+employeds+gender+edu+avgedu+poverty+lnavgasset, data = df.train))
#估计因果森林
cf<-causal_forest(X,reilience,help)
#得到因果森林估计结果
effects <- predict(cf)$predictions
hist(effects)
#使用训练集检验
X.test <- model.matrix(lm(rho1 ~ -1+members+age+kids+labors+unhealth+employeds+gender+edu+avgedu+poverty+lnavgasset,data = df.test))
effects.hold <- predict(cf, X.test)$predictions
SEs <- sqrt(predict(cf, X.test, estimate.variance = TRUE)$variance.estimates)
hist(effects.hold)
#cate
average_treatment_effect(cf, target.sample = "all")
#catt
average_treatment_effect(cf, target.sample ="treated")
#rate
rank_average_treatment_effect(cf, effects)
#模型的有效性
test_calibration(cf)
#变量重要性(暂时不用)
variable_importance(cf, decay.exponent = 2, max.depth = 4)
#异质性检验
#是否存在异质性
plot_htes <- function(cf_preds, ci = FALSE, z = 1.96) {
  
  out <- ggplot(
    mapping = aes(
      x = rank(cf_preds$predictions), 
      y = cf_preds$predictions
    )
  ) +
    geom_point() +
    labs(x = "Rank", y = "Estimated Treatment Effect") +
    theme_light()
  
  if (ci > 0) {
    out <- out +
      geom_errorbar(
        mapping = aes(
          ymin = cf_preds$predictions + z * sqrt(cf_preds$variance.estimates),
          ymax = cf_preds$predictions - z * sqrt(cf_preds$variance.estimates)
        )
      )
  }    
  return(out)
}
cf_preds <- predict(cf, X.test,estimate.variance=TRUE)
cf_preds
plot_htes(cf_preds,ci=TRUE,z=1.96)
#年龄异质性
ggplot_data<-data.frame(X.test)
p1 <- ggplot(ggplot_data, aes(x = edu, y = effects.hold)) +
  geom_smooth(method = "loess", span = 1) +
  geom_point() +
  theme_light()
p1
#平均受教育程度异质性
p2 <- ggplot(ggplot_data, aes(x =avgedu , y = effects.hold)) +
  geom_smooth(method = "loess", span = 1) +
  geom_point() +
  theme_light()
p2
#资产异质性
p3 <- ggplot(ggplot_data, aes(x = lnavgasset, y = effects.hold)) +
  geom_smooth(method = "loess", span = 1) +
  geom_point() +
  theme_light()
p3
