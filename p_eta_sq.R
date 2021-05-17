# calculate partial eta squared
# 'model' argument takes an aov object
# Joel Le Foresiter
# joel.leforestier@mail.utoronto.ca
# May 17, 2021

p_eta_sq <- function(model) {
  output <- data.frame(summary(model)[[1]])
  var <- row.names(output)[1:(nrow(output) - 1)]
  petasq <- vector()
  
  for(p in 1:length(var)) { # calculate partial eta square for each predictor in the model
    petasq <- c(petasq, round(output$Sum.Sq[p] / (output$Sum.Sq[p] + output$Sum.Sq[nrow(output)]), 2))
  }
  
  result <- data.frame(var, petasq)
  return(result)
}
