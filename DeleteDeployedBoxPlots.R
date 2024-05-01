library(readxl)
react_performance_final <-react_Delete_UserFlow_Deployed <- read_excel("BachelorDocumantion /UserFlowDataSets /react_Delete_UserFlow_Deployed.xlsx")
blazor_performance_final <- blazor_Delete_UserFlow_Deployed <- read_excel("BachelorDocumantion /UserFlowDataSets /blazor_Delete_UserFlow_Deployed.xlsx")
#print(react_performance_final[3:22, 5])
react_dataEdge<-as.numeric(unlist(react_performance_final[3:22, 3]))
blazor_dataEdge<-as.numeric(unlist(blazor_performance_final[3:22, 3]))
data <- data.frame(
  performanceEdge = c(blazor_dataEdge, react_dataEdge),
  frameworkEdge = factor(rep(c("Blazor", "React"), each = 20))
  
)
print(data)


#print(react_performance_final[3:22, 10])
react_dataChrome<-as.numeric(unlist(react_performance_final[3:22, 6]))
blazor_dataChrome<-as.numeric(unlist(blazor_performance_final[3:22, 6]))
dataChrome <- data.frame(
  performanceChrome = c(blazor_dataChrome, react_dataChrome),
  frameworkChrome = factor(rep(c("Blazor", "React"), each = 20))
  
)
print(dataChrome)



#print(react_performance_final[3:22, 15])
react_dataBrave<-as.numeric(unlist(react_performance_final[3:22, 9]))
blazor_dataBrave<-as.numeric(unlist(blazor_performance_final[3:22, 9]))
dataBrave <- data.frame(
  performanceBrave = c(blazor_dataBrave, react_dataBrave),
  frameworkBrave = factor(rep(c("Blazor", "React"), each = 20))
  
)
print(dataBrave)


layout(matrix(c(1, 2, 3), 1, 3, byrow = TRUE))




col_fill <- c(rgb(92/255, 45/255, 145/255, alpha = 0.7),  # Purple with 50% opacity
              rgb(97/255, 219/255, 251/255, alpha = 0.7))  # Light blue with 50% opacity
col_fill2 <- c(rgb(92/255, 45/255, 145/255, alpha = 1),  # Purple with 50% opacity
               rgb(0/255, 91/255, 150/255, alpha = 0.8))  


par(col.axis="black", cex.axis=1.1, font.axis=1.9, cex.lab=1.3)
boxplot(performanceEdge~ frameworkEdge,
        data=data,
        
        main=expression(bold("Edge")),
        
        
        ylab="CLS",
        col=col_fill, 
        border = col_fill2, 
        xlab = "", 
        boxwex = 0.7,
        whisklwd = 2 , 
        whiskcol = c("black", "black"),
        staplecol =  c("black", "black"),
        lwd =1.2
        
        
        
)

par(col.axis="black", cex.axis=1.1, font.axis=1.9, cex.lab=1.3)
boxplot(performanceChrome~frameworkChrome,
        data=dataChrome,
        
        main=expression(bold("Chrome")),
        
        
        ylab = "CLS",
        col=col_fill, 
        border = col_fill2, 
        xlab = "", 
        boxwex = 0.7,
        whisklwd = 2 , 
        whiskcol = c("black", "black"),
        staplecol =  c("black", "black"),
        lwd =1.2
        
        
        
)



par(col.axis="black", cex.axis=1.1, font.axis=1.9, cex.lab=1.3)

boxplot(performanceBrave~frameworkBrave,
        data=dataBrave,
        main=expression(bold("Brave")),
        
        ylab="CLS",
        col= col_fill, 
        border= col_fill2,
        xlab = "", 
        boxwex = 0.7,
        whiskcol = c("black", "black"),
        staplecol =  c("black", "black"),
        whisklwd = 2,
        lwd =1.2
)
## conti
