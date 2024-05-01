library(readxl)



react_performance <-React_Performance_UserFlow_NonDeployed <- read_excel("BachelorDocumantion /UserFlowDataSets /React_Performance_UserFlow_NonDeployed.xlsx")
blazor_performance<- Blazor_Performance_UserFlow_NonDeployed <- read_excel("BachelorDocumantion /UserFlowDataSets /Blazor_Performance_UserFlow_NonDeployed.xlsx")


analyze_data_frame <- function(data_frame, framework_name) {
  dimensions <- dim(data_frame) # Returns a vector: [number of rows, number of columns]
  
  num_columns <- dimensions[2]
  print(sprintf("%s data frame has %s columns", framework_name, num_columns))
  
  print(data_frame[3:22, 1])
  not_normal_count=0
  not_available_count=0
  normal_count=0
  identical_count=0
  
  for (x in 1:num_columns) {
    if ((x - 1) %% 3 == 0) {
      if ((x - 1) %% 9 == 0) {
        print(" ")
        print(" ")
        print(sprintf("%s ", colnames(data_frame)[x]))
      }
      print(sprintf("===================  %s =======================", data_frame[1, x]))
    }
    
    column_with_metric <- data_frame[3:22, x]
    numeric_column_with_metric <- as.numeric(unlist(column_with_metric))
    are_identical <- all(numeric_column_with_metric[3:22] == numeric_column_with_metric[3])
    
    if (is.na(are_identical)) {
      print(sprintf("%s: not available", data_frame[2, x]))
      not_available_count = not_available_count + 1
    } else if (are_identical) {
      print(sprintf("%s: identical", data_frame[2, x]))
      identical_count = identical_count + 1
    } else {
      p_value <- shapiro.test(numeric_column_with_metric)$p.value
      if (p_value < 0.5) {
        formatted_p_value <- sprintf("%.3e", p_value)
        print(sprintf("%s: p value = %s not normal", data_frame[2, x], formatted_p_value ))
        not_normal_count = not_normal_count + 1
      } else {
        print(sprintf("%s: p value = %s normal", data_frame[2, x],  formatted_p_value))
        normal_count = normal_count + 1
      }
    }
  }
  
  print("***************************** Summary ********************************")
  print(sprintf("Number of not normally distributed columns: %s", not_normal_count))
  print(sprintf("Number of normally distributed columns: %s", normal_count))
  print(sprintf("Number of identical columns: %s", identical_count))
  print(sprintf("Number of unavailable columns: %s", not_available_count))
  print(sprintf("Total number of columns: %s", num_columns))
}


analyze_data_frame(react_performance, "React")


analyze_data_frame(blazor_performance, "Blazor")
