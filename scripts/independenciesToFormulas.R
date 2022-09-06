independenciesToFormulas <- function(independencies){
  formulas <- list()
  for( i in 1:length(independencies)){
    triple <- independencies[[i]]
    
    if(length(triple$Z)>0){
      triple$Xs <- paste(unlist(strsplit(triple$X, split = "")),
                         collapse = "\\[And]")
      triple$Ys <- paste(unlist(strsplit(triple$Y, split = "")), 
                         collapse = "\\[And]")
      triple$Zs <- paste(unlist(strsplit(triple$Z, split = "")), 
                         collapse = "\\[And]")
      
      # a <- c("a","na")
      # b <- c("b","nb")
      # c <- c("c","nc")
      # expand.grid(a =a, b = b, c= c)
      
      abc <- paste("Pr[(", triple$Xs,  ")|(" , triple$Ys, ")\\[And](",  triple$Zs,
                   ")] == Pr[(", triple$Xs,  ")|(" , triple$Zs, ")]", sep = "")
      
      nabc <- paste("Pr[\\[Not](", triple$Xs,  ")|(" , triple$Ys, ")\\[And](", 
                    triple$Zs,")] == Pr[\\[Not](", triple$Xs,  
                    ")|(" , triple$Zs, ")]", sep = "")
      
      anbc <- paste("Pr[(", triple$Xs,  ")|\\[Not](" , triple$Ys, ")\\[And](", 
                    triple$Zs, ")] == Pr[(", triple$Xs,  ")|(" ,
                    triple$Zs, ")]", sep = "")
      
      
      nanbc <- paste("Pr[\\[Not](", triple$Xs,  ")|\\[Not](" , triple$Ys,
                     ")\\[And](",  triple$Zs,
                     ")] == Pr[\\[Not](", triple$Xs,  ")|(" , triple$Zs, ")]", sep = "")
      
      
      abnc <- paste("Pr[(", triple$Xs,  ")|(" , triple$Ys, ")\\[And] \\[Not](",
                    triple$Zs,")] == Pr[(", triple$Xs,  ")|\\[Not](", 
                    triple$Zs, ")]", sep = "")
      
      nabnc <- paste("Pr[\\[Not](", triple$Xs,  ")|(" , triple$Ys, ")\\[And] \\[Not](",
                     triple$Zs,")] == Pr[\\[Not](", triple$Xs,  ")|\\[Not](", 
                     triple$Zs, ")]", sep = "")
      
      anbnc <- paste("Pr[(", triple$Xs,  ")|\\[Not](" , triple$Ys, ")\\[And] \\[Not](",
                     triple$Zs,")] == Pr[(", triple$Xs,  ")|\\[Not](", 
                     triple$Zs, ")]", sep = "")
      
      nanbnc <- paste("Pr[\\[Not](", triple$Xs,  ")|\\[Not](" , triple$Ys, ")\\[And] \\[Not](",
                      triple$Zs,")] == Pr[\\[Not](", triple$Xs,  ")|\\[Not](", 
                      triple$Zs, ")]", sep = "")
      
      
      formulas[[2*i]] <- paste(c(abc , nabc, anbc, nanbc, abnc, nabnc, 
                                 anbnc, nanbnc), collapse = ", \n ")
    } else {
      
      triple$Xs <- paste(unlist(strsplit(triple$X, split = "")),
                         collapse = "\\[And]")
      triple$Ys <- paste(unlist(strsplit(triple$Y, split = "")), 
                         collapse = "\\[And]")
      
      ab <- paste(
        "Pr[(", triple$Xs,  ")|(" , triple$Ys,")] == Pr[(", triple$Xs, ")]", sep = ""
      )
      
      nab <- paste(
        "Pr[\\[Not](", triple$Xs,  ")|(" , triple$Ys,")] == Pr[\\[Not](", triple$Xs, ")]",
        sep = "")
      
      anb <- paste(
        "Pr[(", triple$Xs,  ")|\\[Not](" , triple$Ys,")] == Pr[(", triple$Xs, ")]", 
        sep = "")
      
      nanb <- paste("Pr[\\[Not](", triple$Xs,  ")|\\[Not](" , triple$Ys,")] == Pr[\\[Not](", triple$Xs, ")]",
                    sep = "")
      
      formulas[[2*i]] <- paste(c(ab , nab, anb, nanb), collapse = ", \n ")
    }
    
  }
  
  for(i in 1:length(independencies)){
    formulas[2*i -1] <- ",\n"
  }
  formulas[1] <- NULL
  
  return(formulas)
}
