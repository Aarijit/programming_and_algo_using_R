## Excercise ##


## Exercise 1: Write a program using a for statement to compute the 1-norm of a given 
vector v ∈ R n . Apply the program to an example vector as Apply the program 
to an example vector as onenorm_for(matrix(c(4,5,4,3,-1,3,4,5,-4,2),ncol=1)) ###


## SOLUTION: ##

onenorm_for<- function(v){
  sumvalue=0
  for(i in 1:length(v)){
    sumvalue=sumvalue+abs(v[i])
  }
  return(sumvalue)
}

onenorm_for(matrix(c(4,5,4,3,-1,3,4,5,-4,2),ncol=1))




