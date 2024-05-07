  
  library(readxl)
  library(car)
  library(WRS2)
  

  react_performance <-React_Performance_UserFlow_NonDeployed <- read_excel("BachelorDocumantion /UserFlowDataSets /React_Performance_UserFlow_NonDeployed.xlsx")
  blazor_performance <- Blazor_Performance_UserFlow_NonDeployed <- read_excel("BachelorDocumantion /UserFlowDataSets /Blazor_Performance_UserFlow_NonDeployed.xlsx")
  
  dimensions <- dim(react_performance)  
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
        tech = factor(rep(c("Blazor", "React"), each = 20))
        
      )
      levene_result <- leveneTest(performance ~ tech, data = data, center = median)
      p_val_levenes=levene_result$`Pr(>F)`[1] 
      
      print(sprintf("%s: ", data_frame[2, x])) 
      
      yuen_result <- yuen(formula=performance ~ tech, data = data)
      yuen_result_bootstrapped <- yuenbt(formula=performance ~ tech, data = data,  nboot=1000)
    
      
      print(sprintf("Levenes test= %.3e", p_val_levenes))
      print(sprintf("Robust t test= %.3e", yuen_result$p.value))
      print(sprintf("Bootstrapped t test= %s", yuen_result_bootstrapped$p.value))
      print(" ")
      
      
    }
    else{
      print(sprintf("%s: not available", data_frame[2, x]))
      
    }
    
    
  }
  
  
  

  
Variance_trimmed_Analysis<-function(data_frame, data_frame2){
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
    
    
  
    
    for(x in 1:length(empty_character_vector)){
      numeric_data_frame<-as.numeric(unlist(data_frame[3:22, x]))
      numeric_data_frame2<-as.numeric(unlist(data_frame2[3:22,x]))
      test_variance(empty_character_vector[x], empty_character_vector_2[x], numeric_data_frame, numeric_data_frame2, x, data_frame)
      
      
    }
    
    
    
    
  }
  
  
Variance_trimmed_Analysis(blazor_performance, react_performance)
  
  
