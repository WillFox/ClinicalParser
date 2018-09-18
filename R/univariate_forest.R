
#' Univariate Plotter for Cox Regression
#'
#' This function takes a list of coxph regressions completed in univariate. Takes both categorical and continuous coxph analysis
#' @param cox_list a list of coxph regressions
#' @param df the data that was utilized to build the coxph
#' @param columns_to_test a vector of strings that match column names to include in output
#' @param title the title shown at the top of the plot
#' @param verbose the amount of status log output provided
#' @keywords cox 
#' @keywords regression
#' @export
#' @examples 
#' cox_univariate_plotter()

cox_univariate_plotter<-function(cox_list,df=data,columns_to_test,title,verbose=T){
  #Currently, must be of type list input
  if(!is.list(cox_list)) cox_list<-list(cox_list) else(cox_list<-cox_list)
  #Verify all are type coxph
  for(i in 1:length(cox_list)){
    if(class(cox_list[[i]])!="coxph"){
      print("Class not of type coxph for at least one item in list") 
      print(class(cox_list[[i]]))
      return(NULL)
    }
    print(cox_list[[i]])
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
  for(i in 1:length(cox_list)){
    
    print(paste("analyzing(",i,")"))
    tmp_df<-eval(cox_list[[i]]$call$data)
    if(!is.null(cox_list[[i]]$xlevels)){#categorical
      #count<-count-1
      #df_summ[nrow(df_summ)+1,]<-list(
      #    count,
      #    strsplit(as.character(cox_list[[i]]$formula),"~")[[3]],#Variable
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
      for(var_factor in 1:length(cox_list[[i]]$xlevels[[1]])){
        
        count<-count-1
        #print(cox_list[[i]]$call$data)
        #print(cox_list[[i]]$xlevels[[1]][var_factor])
        tmp_cox<-summary(cox_list[[i]])
        #print(tmp_cox$coefficients)
        n_event<-nrow(tmp_df[tmp_df[,strsplit(as.character(cox_list[[i]]$formula),"~")[[3]]]==cox_list[[i]]$xlevels[[1]][var_factor]&tmp_df[,"OUTCOME"]==TRUE,])
        n_sub<-nrow(tmp_df[tmp_df[,strsplit(as.character(cox_list[[i]]$formula),"~")[[3]]]==cox_list[[i]]$xlevels[[1]][var_factor],])
        df_summ[nrow(df_summ)+1,]<-list(
          count,
          ifelse(var_factor==1,strsplit(as.character(cox_list[[i]]$formula),"~")[[3]],NA),#Variable
          cox_list[[i]]$xlevels[[1]][var_factor],#Sub_Vatiable
          n_sub,#Population
          n_event,#NegPopulation
          #coxout<-summary(coxph(Surv(TIME,CENS2) ~ .,df[colnames(df)%in%c(name,"DURATION","CENS2","TIME")]))$conf.int
          if(var_factor==1) NA else tmp_cox$conf.int[rownames(tmp_cox$conf.int)[var_factor-1],1],#hazard
          if(var_factor==1) NA else tmp_cox$conf.int[rownames(tmp_cox$conf.int)[var_factor-1],3],#hazard_05
          if(var_factor==1) NA else tmp_cox$conf.int[rownames(tmp_cox$conf.int)[var_factor-1],4],#hazard_95
          ifelse(var_factor==1,NA, 
                 tmp_cox$coefficients[rownames(tmp_cox$coefficients)[var_factor-1],'Pr(>|z|)']),#stars,
          ifelse(var_factor==1,NA,
                 ifelse(tmp_cox$coefficients[rownames(tmp_cox$coefficients)[var_factor-1],'Pr(>|z|)']<0.001,'***',
                        ifelse(tmp_cox$coefficients[rownames(tmp_cox$coefficients)[var_factor-1],'Pr(>|z|)']<0.01,'**',
                               ifelse(tmp_cox$coefficients[rownames(tmp_cox$coefficients)[var_factor-1],'Pr(>|z|)']<0.1,'*',''))))#stars
        )
      }
    }else{#continuous
      count<-count-1
      tmp_cox<-summary(cox_list[[i]])
      n_event<-nrow(tmp_df[!is.na(tmp_df[,colnames(tmp_df)%in%c(rownames(tmp_cox$conf.int[1]))])&tmp_df[,"OUTCOME"]==TRUE,])
      #n_sub<-nrow(tmp_df[!is.na(tmp_df[,rownames(tmp_cox$conf.int)[1]]),])
      n_sub<-nrow(tmp_df[!is.na(tmp_df[,colnames(tmp_df)%in%c(rownames(tmp_cox$conf.int[1]))]),])
      df_summ[nrow(df_summ)+1,]<-list(
        count,
        rownames(tmp_cox$conf.int)[1],#Variable
        NA,#Sub_Vatiable
        cox_list[[i]]$n,#Population
        cox_list[[i]]$nevent,#NegPopulation
        #coxout<-summary(coxph(Surv(TIME,CENS2) ~ .,df[colnames(df)%in%c(name,"DURATION","CENS2","TIME")]))$conf.int
        tmp_cox$conf.int[rownames(tmp_cox$conf.int)[1],1],#hazard
        tmp_cox$conf.int[rownames(tmp_cox$conf.int)[1],3],#hazard_05
        tmp_cox$conf.int[rownames(tmp_cox$conf.int)[1],4],#hazard_95
        tmp_cox$coefficients[rownames(tmp_cox$coefficients)[1],'Pr(>|z|)'],
        ifelse(tmp_cox$coefficients[rownames(tmp_cox$coefficients)[1],'Pr(>|z|)']<0.001,'***',
               ifelse(tmp_cox$coefficients[rownames(tmp_cox$coefficients)[1],'Pr(>|z|)']<0.01,'**',
                      ifelse(tmp_cox$coefficients[rownames(tmp_cox$coefficients)[1],'Pr(>|z|)']<0.1,'*','')))#stars
      )
    }
  }
  if(verbose) print("Moving into plotting the graph from created table")
  #initiate graph
  #forest_plot<-ggplot(df_summ)
  #df_summ$hazard_95[is.infinite(df_summ$hazard_95)]<-1e10
  
  #df_summ$hazard_05[df_summ$hazard_05==0]<-1e-10
  lower_bound<-0.43
  upper_bound<-1000
  #df_summ <- mutate(df_summ, Ubound_limit = ifelse(is.infinite(hazard_95), 6, NA), Lbound_limit = ifelse(hazard_05==0., 0.43, NA))
  df_summ <- mutate(df_summ, Ubound_limit = ifelse(hazard_95>upper_bound, 6, NA), Lbound_limit = ifelse(hazard_05<=lower_bound, lower_bound, NA))
  #df_summ$Ubound_limit[!is.na(df_summ$Lbound_limit)]<-df_summ$hazard_95[!is.na(df_summ$Lbound_limit)] 
  #df_summ$hazard_05[!is.na(df_summ$Lbound_limit) & df_summ$hazard_05<=lower_bound]<-lower_bound
  #df_summ$hazard[!is.na(df_summ$Lbound_limit) & df_summ$hazard<=lower_bound]<-(lower_bound*2+df_summ$hazard_95[!is.na(df_summ$Lbound_limit) & df_summ$hazard<=lower_bound])/3
  
  #df_summ$hazard_95[!is.na(df_summ$Ubound_limit) & df_summ$hazard_95>=lower_bound]<-max(df_summ$hazard_95[!is.na(df_summ$hazard_95)])
  
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
                    ylim=c(min(df_summ$Y_VAL)-0.5,max(df_summ$Y_VAL)+1.5),expand=FALSE)+
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
  
  
  png("surv_out.png",width = 10,height = (nrow(df_summ)/2)+1,units="in",res=400)
  print(surv_plot)
  dev.off()
  
  
  print(df_summ)
  print(surv_plot)
  return(df_summ)
}

#' Univariate Plotter for Cox Regression
#'
#' This function takes a data frame and iteratively performs univariate cox regression, prints a forrest plot, and returns a table that can use code from cox_univariate_plotter to customize the plot. 
#' @param data A data frame that contains an outcome column, time column, and a column name within columns_to_test.
#' @param columns_to_test The columns to test from the data
#' @param time The continuous time to event column for the regression analysis. Default: "TIME"
#' @param outcome The binary outcome column of the data. Default: "OUTCOME"
#' @param title Title at top of plot. Default: "Forest Plot for Hazard"
#' @param verbose the amount of status log output provided. Default: TRUE
#' @keywords cox regression univariate
#' @export
#' @examples 
#' univariate_forest()

univariate_forest<-function(data,columns_to_test,time="TIME",outcome="OUTCOME",title="Forest Plot for Hazard",verbose=TRUE){
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
  #rm(outcome)
  #print(outcome)
  #print(nrow(data))
  #print(length(data$OUTCOME))
  
  #print(length(data$TIME))
  #print(table(data$OUTCOME,useNA = "a"))
  
  df<-data
  if(verbose) print("Coxph list generating...")
  for(i in 1:length(columns_to_test)){
    cox_list[[i]]<-coxph(Surv(TIME,OUTCOME)~., data =df[,colnames(df)%in%
                                                          c(columns_to_test[i],
                                                            "TIME",
                                                            "OUTCOME")])
  }
  if(verbose) print("Generated coxph list.... Moving onto univariate table build")
  df_summ<-cox_univariate_plotter(cox_list,data,columns_to_test,title=title)
  return(df_summ)
}