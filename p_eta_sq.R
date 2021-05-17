# calculate partial eta squared
# 'model' argument takes an aov object
# Joel Le Foresiter
# joel.leforestier@mail.utoronto.ca
# May 17, 2021

p_eta_sq <- function(model) {
  output <- summary(model)
  var <- names(coef(model))[2:length(names(coef(model)))]
  petasq <- vector()
  
  for(p in 1:length(var)) { # calculate partial eta square for each predictor in the model
    petasq <- c(petasq, round(output[[1]]$`Sum Sq`[p] / (output[[1]]$`Sum Sq`[p] + output[[1]]$`Sum Sq`[length(var) + 1]), 2))
  }
  
  result <- data.frame(var, petasq)
  return(result)
}