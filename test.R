library(survival)
library(clinicalParser)
df<-aml
colnames(df)<-c("TIME","OUTCOME","x")
df[,"y"]<-df[,"x"]
univariate_forest(df,columns_to_test=c("x","y"),time="TIME",outcome="OUTCOME")
multivariate_forest(df,columns_to_test=c("x","y"),time="TIME",outcome="OUTCOME")
