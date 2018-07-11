# ClinicalParser
A set of analysis techniques and functions intended to aid clinical trial data analysis.

### Current Contents:

*Forest Plots for both univariate and multivariate cox regression

library(survival)
df<-aml
colnames(df)<-c("TIME","OUTCOME","x")
univariate_forest(my_data,columns_to_test=colnames("x"),time="TIME",outcome="OUTCOME")
multivariate_forest(my_data,columns_to_test=colnames("x"),time="TIME",outcome="OUTCOME")

