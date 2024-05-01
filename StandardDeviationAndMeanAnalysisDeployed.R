library(readxl)

react_data_frame <-React_Performance_UserFlow_Deployed <- read_excel("BachelorDocumantion /UserFlowDataSets /React_Performance_UserFlow_Deployed.xlsx")
blazor_data_frame <- Blazor_Performance_UserFlow_Deployed <- read_excel("BachelorDocumantion /UserFlowDataSets /Blazor_Performance_UserFlow_Deployed.xlsx")
check_standard_deviation <- function(react_data_frame, blazor_data_frame) {
  dimensions <- dim(react_data_frame)
  num_columns <- dimensions[2]
  
  print("BLAZOR")
  
  for (x in 1:num_columns) {
    
    if ((x - 1) %% 3 == 0) {
      if ((x - 1) %% 9 == 0) {
        print(" ")
        print(" ")
        
        header_info <- react_data_frame[1, x]
        print(header_info)
      }
      print(sprintf("===================  %s =======================", react_data_frame[1, x]))
    }
    
    
    react_values <- as.numeric(unlist(react_data_frame[3:22, x]))
    blazor_values <- as.numeric(unlist(blazor_data_frame[3:22, x]))
    
    
    if (any(is.na(react_values))) {
      react_values <- ifelse(is.na(react_values), mean(react_values, na.rm = TRUE), react_values)
    }
    if (any(is.na(blazor_values))) {
      blazor_values <- ifelse(is.na(blazor_values), mean(blazor_values, na.rm = TRUE), blazor_values)
    }
    
    
    react_sd <- sd(react_values, na.rm = TRUE)
    blazor_sd <- sd(blazor_values, na.rm = TRUE)
    react_mean <- mean(react_values, na.rm = TRUE)
    blazor_mean <- mean(blazor_values, na.rm = TRUE)
    
    print("Standard Deviation")
    print(sprintf("React %s: %.3f; Blazor: %.3f", react_data_frame[2, x], react_sd, blazor_sd))
    print("Means")
    print(sprintf("React %s: %.3f; Blazor: %.3f", react_data_frame[2, x], react_mean, blazor_mean))
  }
}


check_standard_deviation(react_data_frame, blazor_data_frame)