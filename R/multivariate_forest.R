cox_multivariate_plotter<-function(cox_data,df=data,columns_to_test,show_only,keep_insignificant,title,verbose=T){
  #Verify type coxph
  if(class(cox_data)!="coxph"){
    print("Class not of type coxph for at least one item in list") 
    print(class(cox_list))
    return(NULL)
  }
  #Build summary statistics
  df_summ<-data.frame(
    Y_VAL<-numeric(),
    Variable<-character(),
    Sub_Variables<-character(),
    Population<-numeric(),
    Neg_Population<-numeric(),
    hazard<-numeric(),
    hazard_05<-numeric(),
    hazard_95<-numeric(),
    p_value<-numeric(),
    stars<-numeric(),
    stringsAsFactors = F
  )
  colnames(df_summ)<-c("Y_VAL","Variable","Sub_Variables","Population","Neg_Population","hazard","hazard_05","hazard_95","p_value","stars")
  count<-0
  print("analyzing....")
  tmp_df<-eval(cox_data$call$data)
  tmp_cox<-summary(cox_data)
  if(length(show_only)!=0){
    columns_to_test<-columns_to_test[columns_to_test%in%show_only]
  }
  for(column in columns_to_test){
    
    print(paste("     ",column))
    if(!is.null(cox_data$xlevels[column][[1]])){#categorical
      #count<-count-1
      #df_summ[nrow(df_summ)+1,]<-list(
      #    count,
      #    strsplit(as.character(cox_data$formula),"~")[[3]],#Variable
      #    NA,#Sub_Vatiable
      #    NA,#Population
      #    NA,#NegPopulation
      #    #coxout<-summary(coxph(Surv(TIME,CENS2) ~ .,df[colnames(df)%in%c(name,"DURATION","CENS2","TIME")]))$conf.int
      #    NA,#hazard
      #    NA,#hazard_05
      #    NA,#hazard_95
      #    NA,#p_value
      #    NA#stars
      #  )
      for(var_factor in 1:length(cox_data$xlevels[column][[1]])){
        #print(var_factor)
        #print(length(cox_data$xlevels[column]))
        count<-count-1
        
        #print("Factors...")
        factor_set<-cox_data$xlevels[column][[1]]
        #print(factor_set)
        #print(nrow(tmp_df[tmp_df[,column]==factor_set[var_factor],]))
        #print(nrow(tmp_df[tmp_df[,column]==factor_set[var_factor]&tmp_df[,"OUTCOME"]==TRUE,]))
        #print(tmp_cox$coefficients)
        n_event<-nrow(tmp_df[tmp_df[,column]==factor_set[var_factor]&tmp_df[,"OUTCOME"]==TRUE,])
        n_sub<-nrow(tmp_df[tmp_df[,column]==factor_set[var_factor],])
        
        #print("Building row...")
        #print(paste(column,factor_set[var_factor],sep=""))
        #if(var_factor!=1)print(tmp_cox$conf.int[paste(column,factor_set[var_factor],sep=""),1])
        df_summ[nrow(df_summ)+1,]<-list(
          count,
          ifelse(var_factor==1,column,NA),#Variable
          factor_set[var_factor],#Sub_Vatiable
          n_sub,#Population
          n_event,#NegPopulation
          #coxout<-summary(coxph(Surv(TIME,CENS2) ~ .,df[colnames(df)%in%c(name,"DURATION","CENS2","TIME")]))$conf.int
          if(var_factor==1) NA else tmp_cox$conf.int[paste(column,factor_set[var_factor],sep=""),1],#hazard
          if(var_factor==1) NA else tmp_cox$conf.int[paste(column,factor_set[var_factor],sep=""),3],#hazard_05
          if(var_factor==1) NA else tmp_cox$conf.int[paste(column,factor_set[var_factor],sep=""),4],#hazard_95
          ifelse(var_factor==1,NA, 
                 tmp_cox$coefficients[paste(column,factor_set[var_factor],sep=""),'Pr(>|z|)']),#stars,
          ifelse(var_factor==1,NA,
                 ifelse(tmp_cox$coefficients[paste(column,factor_set[var_factor],sep=""),'Pr(>|z|)']<0.001,'***',
                        ifelse(tmp_cox$coefficients[paste(column,factor_set[var_factor],sep=""),'Pr(>|z|)']<0.01,'**',
                               ifelse(tmp_cox$coefficients[paste(column,factor_set[var_factor],sep=""),'Pr(>|z|)']<0.1,'*',''))))#stars
        )
        #print(df_summ)
        #if(var_factor==2) stop()
      }
    }else{#continuous
      count<-count-1
      n_event<-nrow(tmp_df[!is.na(tmp_df[,column])&tmp_df[,"OUTCOME"]==TRUE,])
      n_sub<-nrow(tmp_df[!is.na(tmp_df[,column]),])
      #print(column)
      indx <- grepl(column, rownames(tmp_cox$conf.int),fixed=TRUE)
      indx<-rownames(tmp_cox$conf.int)[indx]
      #print(indx)
      #print("row names?")
      #print(rownames(tmp_cox$conf.int))
      tmp_list<-list(
        count,
        column,#Variable
        NA,#Sub_Vatiable
        n_sub,#Population
        n_event,#NegPopulation
        #coxout<-summary(coxph(Surv(TIME,CENS2) ~ .,df[colnames(df)%in%c(name,"DURATION","CENS2","TIME")]))$conf.int
        tmp_cox$conf.int[indx,1],#hazard
        tmp_cox$conf.int[indx,3],#hazard_05
        tmp_cox$conf.int[indx,4],#hazard_95
        tmp_cox$coefficients[indx,'Pr(>|z|)'],
        ifelse(tmp_cox$coefficients[indx,'Pr(>|z|)']<0.001,'***',
               ifelse(tmp_cox$coefficients[indx,'Pr(>|z|)']<0.01,'**',
                      ifelse(tmp_cox$coefficients[indx,'Pr(>|z|)']<0.1,'*','')))#stars
      )
      #print(tmp_list)
      df_summ[nrow(df_summ)+1,]<-tmp_list
    }
  }
  if(verbose) print("Moving into plotting the graph from created table")
  lower_bound<-0.43
  upper_bound<-1000
  #initiate graph
  #forest_plot<-ggplot(df_summ)
  #df_summ$hazard_95[is.infinite(df_summ$hazard_95)]<-1e10
  
  #df_summ$hazard_05[df_summ$hazard_05==0]<-1e-10
  df_summ <- mutate(df_summ, Ubound_limit = ifelse(is.infinite(hazard_95), 6, NA), Lbound_limit = ifelse(hazard_05<=0., 0.43, NA))
  #df_summ <- mutate(df_summ, Ubound_limit = ifelse(hazard_95>1.0, 6, NA), Lbound_limit = ifelse(hazard_05<=0.4, 0.43, NA))
  df_summ$hazard_05[is.infinite(df_summ$hazard_95)]<-df_summ$hazard
  df_summ$hazard_95[is.infinite(df_summ$hazard_95)]<-df_summ$hazard
  #geom_segment(aes(x = formN - .12, xend = formN - .12, y = ALPHA, yend = ALPHA - LCL_l), arrow = arrow(length = unit(myData_m$LCL_l, "cm")))
  df_summ$stars[!is.na(df_summ$p_value)]<-paste(format(ifelse(df_summ$p_value[!is.na(df_summ$p_value)]<0.001,0.001,round(df_summ$p_value[!is.na(df_summ$p_value)],3)),scientific=F,digits=2),df_summ$stars[!is.na(df_summ$p_value)],sep="")
  df_summ$stars[!is.na(df_summ$p_value)&df_summ$p_value<0.001]<-paste("<",df_summ$stars[!is.na(df_summ$p_value)&df_summ$p_value<0.001],sep="")
  df_summ[,'colorodd']=NA
  df_summ$colorodd[df_summ$Y_VAL%in%seq(-2,-nrow(df_summ),-2)]=df_summ$Y_VAL[df_summ$Y_VAL%in%seq(-2,-nrow(df_summ),-2)]
  df_summ[,'coloreven']=NA
  df_summ$coloreven[df_summ$Y_VAL%in%seq(-1,-nrow(df_summ),-2)]=df_summ$Y_VAL[df_summ$Y_VAL%in%seq(-1,-nrow(df_summ),-2)]
  #df_summ$df[is.na(df_summ$hazard)],NA,ifelse(is.na(hazard),"reference",
  t_size<-3
  ticks<-c(0.5,0.8,1,1.2,1.5,2,3,5,10,30,60,100)
  if(1.5*max(df_summ$hazard_95[!is.na(df_summ$hazard_95)])>20) ticks<-c(0.5,1,2,5,10,30,60,100) 
  surv_plot<-ggplot(df_summ)+
    geom_tile(aes(y=colorodd,x=1.0,alpha=0.1),fill="gray95",width=30,height=1.0)+
    geom_tile(aes(y=coloreven,x=1.0,alpha=0.1),fill="gray70",width=30,height=1.0)+
    
    scale_x_log10(labels=ticks,breaks=ticks)+
    geom_vline(size=0.1,xintercept=ticks[ticks!=1],alpha=0.6)+
    coord_cartesian(xlim=c(0.045/max(df_summ$hazard_95[!is.na(df_summ$hazard_95)]),1.5*max(df_summ$hazard_95[!is.na(df_summ$hazard_95)])),#1.3*max(df_summ$hazard_95[!is.na(df_summ$hazard_95) & df_summ$hazard_95<1e9])),
                    ylim=c(min(df_summ$Y_VAL)-0.45,max(df_summ$Y_VAL)+1.5),expand=FALSE)+
    theme(
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.x=element_blank(),
      #axis.text.x=element_blank(),
      #axis.ticks.x=element_blank(),
      legend.position="none",
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_blank(), 
      #axis.text.x = element_text(angle = 60, hjust = 1),
      axis.line = element_line(colour = "white"))+
    geom_vline(xintercept=1.0,linetype=8)+
    geom_point(aes(x=ifelse(is.na(hazard),1,ifelse(hazard<lower_bound,lower_bound,hazard)),y=Y_VAL,size=Population/max(df_summ$Population)),shape=15)+
    geom_errorbarh(aes(x=ifelse(hazard<lower_bound,lower_bound,hazard),
                       xmin=ifelse(hazard_05<lower_bound,lower_bound,hazard_05), 
                       xmax=ifelse(hazard_95>upper_bound,upper_bound,hazard_95),y=Y_VAL),height=0.3)+
    geom_segment(aes(x = Ubound_limit, xend = Lbound_limit, y = Y_VAL, yend = Y_VAL), arrow = arrow(length = unit(0.3, "cm")))+
    geom_segment(aes(x = Lbound_limit, xend = Ubound_limit, y = Y_VAL, yend = Y_VAL), arrow = arrow(length = unit(0.3, "cm")))+
    
    geom_text(aes(label=Variable,x=0.05/max(df_summ$hazard_95[!is.na(df_summ$hazard_95)]),y=Y_VAL),fontface="bold",hjust = 0,size=t_size)+
    geom_text(aes(label=Sub_Variables,
                  x=10**((log10(0.06/max(df_summ$hazard_95[!is.na(df_summ$hazard_95)]))+log10(0.5))*7/10),y=Y_VAL),hjust = 0,size=t_size)+
    geom_text(aes(label=ifelse(!is.na(Neg_Population), paste(Neg_Population,"/",Population,"\n(",format(Neg_Population/Population,digits=2),")"), NA),
                  x=10**((log10(0.06/max(df_summ$hazard_95[!is.na(df_summ$hazard_95)]))+log10(0.5))*4.5/10),y=Y_VAL),size=t_size)+
    geom_text(aes(label=ifelse(is.na(Neg_Population) & is.na(hazard),NA,ifelse(is.na(hazard),"reference",
                                                                               paste(format(hazard,digits=2),"\n(",format(hazard_05,digits=2),"-",format(hazard_95,digits=2),")"))),
                  x=10**((log10(0.06/max(df_summ$hazard_95[!is.na(df_summ$hazard_95)]))+log10(0.5))*2.5/10),y=Y_VAL),size=t_size)+
    geom_text(aes(label=stars,x=1.05*max(df_summ$hazard_95[!is.na(df_summ$hazard_95)]),y=Y_VAL-0.15),hjust = 0,size=t_size)+
    ggtitle(title)+
    geom_tile(aes(x=1,y=0.5),fill="white",width=30,height=2.0)+
    geom_text(aes(label="Variable",
                  x=0.05/max(df_summ$hazard_95[!is.na(df_summ$hazard_95)]),y=0),fontface="bold",hjust=0,size=3)+
    geom_text(aes(label="Unfavorable outcomes/\nStudy participants (%)",
                  x=10**((log10(0.06/max(df_summ$hazard_95[!is.na(df_summ$hazard_95)]))+log10(0.5))*4.5/10),y=-0.1),fontface="bold",size=2.2)+
    geom_text(aes(label="Hazard Ratio (95% CI)",
                  x=10**((log10(0.06/max(df_summ$hazard_95[!is.na(df_summ$hazard_95)]))+log10(0.5))*1.5/10),y=0),fontface="bold",size=3)+
    
    geom_text(aes(label="Pr(>|z|)",x=1.05*max(df_summ$hazard_95[!is.na(df_summ$hazard_95)]),y=0),fontface="bold",hjust=0.3,size=3)
  
  
  png("multivariate_surv_out.png",width = 10,height = nrow(df_summ)/2,units="in",res=400)
  print(surv_plot)
  dev.off()
  
  
  #print(df_summ)
  print(surv_plot)
  return(df_summ)
}


multivariate_forest<-function(data,columns_to_test=c(),time="TIME",outcome="OUTCOME",title="Forest Plot for Hazard",
                                           keep_insignificant=TRUE,show_only=c(),verbose=T){
  if(verbose) print("Generate coxph list started....")
  cox_list<-list()
  if(time!="TIME"){
    data[,"TIME"]<-data[,time]
    data[,time]<-NULL
  }
  if(outcome!="OUTCOME"){
    data[,"OUTCOME"]<-0
    data[,"OUTCOME"]<-data[,outcome]
    data[,outcome]<-NULL
    
  }
  rm(outcome)
  if(length(columns_to_test)==0) columns_to_test<-colnames(data[,!(colnames(data)%in%c("TIME","OUTCOME"))])
  #print(outcome)
  #print(nrow(data))
  #print(length(data$OUTCOME))
  
  #print(length(data$TIME))
  #print(table(data$OUTCOME,useNA = "a"))
  
  df<-data
  #for(i in 1:length(columns_to_test)){
  #  cox_list[[i]]<-coxph(Surv(TIME,OUTCOME)~., data =df[,colnames(df)%in%
  #                                                        c(columns_to_test[i],
  #                                                          "TIME",
  #                                                          "OUTCOME")])
  #}
  cox_data<-coxph(Surv(TIME,OUTCOME)~., data =df[,colnames(df)%in%
                                                        c(columns_to_test,
                                                        "TIME",
                                                        "OUTCOME")])
  if(verbose) print("Generated coxph list.... Moving onto univariate table build")
  df_summ<-cox_multivariate_plotter(cox_data,data,columns_to_test,show_only,keep_insignificant,title=title)
  return(df_summ)
}
