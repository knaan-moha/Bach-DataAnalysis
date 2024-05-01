  
  library(readxl)
  library(car)
  library(WRS2)
  

  react_performance <-React_Performance_UserFlow_NonDeployed <- read_excel("BachelorDocumantion /UserFlowDataSets /React_Performance_UserFlow_NonDeployed.xlsx")
  blazor_performance <- Blazor_Performance_UserFlow_NonDeployed <- read_excel("BachelorDocumantion /UserFlowDataSets /Blazor_Performance_UserFlow_NonDeployed.xlsx")
  
  dimensions <- dim(react_performance) # Returns a vector: [number of rows, number of columns]
  num_columns <- dimensions[2]
  test_variance<- function(blazor_p_value, react_p_value, blazor_data, react_data, x, data_frame){
    
    
    if ((x - 1) %% 3 == 0){
      if ((x - 1) %% 9 == 0) {
        print(" ")
        print(" ")
        print(sprintf("%s ", colnames(data_frame)[x]))
      }
      
      print(sprintf("===================  %s=======================", data_frame[1, x]))
      
    }
    
    
    if(is.na(blazor_p_value)==FALSE && is.na(react_p_value)==FALSE){
      
      data <- data.frame(
        performance = c(blazor_data, react_data),
        framework = factor(rep(c("Blazor", "React"), each = 20))
        
      )
      levene_result <- leveneTest(performance ~ framework, data = data)
      p_val_levenes=levene_result$`Pr(>F)`[1] 
      
      print(sprintf("%s: ", data_frame[2, x]))
      
      yuen_result <- yuen(formula=performance ~ framework, data = data)
      yuen_result_bootstrapped <- yuenbt(formula=performance ~ framework, data = data,  nboot=1000)
      if (p_val_levenes<0.05){
        print("The variances are not equal")
      }
      else {
        print("The variances are equal")
        
      }
      
      print(sprintf("Levenes test= %.3e", p_val_levenes))
      print(sprintf("Robust t test= %.3e", yuen_result$p.value))
      print(sprintf("Bootstrapped t test= %s", yuen_result_bootstrapped$p.value))
      print(" ")
      
      
    }
    else{
      print(sprintf("%s: not available", data_frame[2, x]))
      
    }
    
    
  }
  
  
  
  p_val_normality<-function(data_frame, x){
    
    column_with_metic <-data_frame[3:22, x]
    numeric_column_with_metic<-as.numeric(unlist(column_with_metic))
    are_identical <- all(numeric_column_with_metic[3:22] == numeric_column_with_metic[3])
    
    
    if (is.na(are_identical)==TRUE) {
      print(sprintf("%s: not available", data_frame[2, x]))
      not_available_count=not_available_count+1
      return(NA)
      
    }
    else if(are_identical==TRUE){
      print(sprintf("%s: identical", data_frame[2, x]))
      identical_count=identical_count+1
      return(NA)
      
    }
    else {
      p_value<-shapiro.test(numeric_column_with_metic)$p.value
      if (p_value<0.05) {
        #print(sprintf("%s: p value= %s, not normal", data_frame[2, x], p_value))
        not_normal_count=not_normal_count+1
        return(p_value)
        
        
      } else {
        #print(sprintf("%s: p value= %s, normal", data_frame[2, x], p_value))
        normal_count=normal_count+
          return(p_value)
      }
      
    }
    
    
  }
  
  test_normality<-function(data_frame, data_frame2){
    dimensions<-dim(data_frame)
    num_columns <- dimensions[2]
    
    empty_character_vector <- character() 
    empty_character_vector_2 <- character() 
    print("BLAZOR")
    for (x in 1:num_columns){
      
      
      
      if ((x - 1) %% 3 == 0){
        if ((x - 1) %% 9 == 0) {
          print(" ")
          print(" ")
          print(sprintf("%s ", colnames(data_frame)[x]))
        }
        
        print(sprintf("===================  %s=======================", data_frame[1, x]))
        
      }
      
      
      p_val1=p_val_normality(data_frame, x)
      empty_character_vector<- append(empty_character_vector, p_val1)
      
      
      
      
      
      
    }
    print("React")
    for (x in 1:num_columns){
      
      
      
      if ((x - 1) %% 3 == 0){
        if ((x - 1) %% 9 == 0) {
          print(" ")
          print(" ")
          print(sprintf("%s ", colnames(data_frame2)[x]))
        }
        
        print(sprintf("===================  %s=======================", data_frame2[1, x]))
        
      }
      
      
      
      p_val2=p_val_normality(data_frame2, x)
      
      empty_character_vector_2<- append(empty_character_vector_2, p_val2)
      
      
      
    }
    
    
    #print(empty_character_vector)
    #print(empty_character_vector_2)
    
    for(x in 1:length(empty_character_vector)){
      numeric_data_frame<-as.numeric(unlist(data_frame[3:22, x]))
      numeric_data_frame2<-as.numeric(unlist(data_frame2[3:22,x]))
      test_variance(empty_character_vector[x], empty_character_vector_2[x], numeric_data_frame, numeric_data_frame2, x, data_frame)
      
      
    }
    
    
    
    
  }
  
  
  test_normality(blazor_performance, react_performance)
  
  
  
  print("*****************************Summary********************************")
  print(sprintf("Number of not normally distributed: %s", not_normal_count))
  print(sprintf("Number of normally distributed: %s", normal_count))
  print(sprintf("Number of identical columns %s", identical_count))
  print(sprintf("Number of unavailable columns %s", not_available_count))
  print(sprintf("Total number of columns: %s", num_columns))