
conjunctionToConstraint <- function (conjunction){
  
  constraint <- paste(paste("Pr[(", conjunction[1] ,  ")|\\[Not](" , conjunction[2], "\\[And]",
                            conjunction[3] ,")] == 0", sep = ""), ", \n ",
                      paste("Pr[(", conjunction[1] ,  ")|(" , conjunction[2], "\\[And]", conjunction[3] ,")] == 1",
                            sep = ""), sep = ""
  )
  return(constraint)
}

#cat(conjunctionToConstraint(c("D", "A", "a")))



conjunctionsToConstraints <- function ( listOfConjunctions){
  constraints <- list()
  
  for(i in 1:length(listOfConjunctions)){
    constraints[[2*i]] <-   conjunctionToConstraint(listOfConjunctions[[i]])
  }
  
  for(i in 1:length(listOfConjunctions)){
    constraints[2*i -1] <- ",\n"
  }
  
  constraints[1] <- NULL
  return(constraints)
}
