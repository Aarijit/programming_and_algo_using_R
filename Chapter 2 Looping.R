## Excercise ##



## Exercise 1: Write a program using a for statement to compute the 1-norm of a given vector v ∈ R n . Apply the program to an example vector as Apply the program to an example vector as onenorm_for(matrix(c(4,5,4,3,-1,3,4,5,-4,2),ncol=1)) ###


## SOLUTION: ##

onenorm_for<- function(v){
  sumvalue=0
  for(i in 1:length(v)){
    sumvalue=sumvalue+abs(v[i])
  }
  return(sumvalue)
}

onenorm_for(matrix(c(4,5,4,3,-1,3,4,5,-4,2),ncol=1))




### EXECISE 2: Consider the original program for the geometric series. How the program can be changed in order to avoid infinite loop for faulty values of x? ##

## EXAMPLE: ##

numberoftermsingeo <- function(x,desirederror){
  refvalue = 1/(1-x)
  term =1
  sumvalue = x^(term-1)
  while (abs((refvalue-sumvalue)/refvalue)>desirederror){
    term=term+1
    sumvalue=sumvalue+x^(term-1)
  }
  return(term)
}
numberoftermsingeo(0.5,0.01)

## SOLUTION: ##

numberoftermsingeo <- function(x,desirederror){
  refvalue = 1/(1-x)
  term =1
  sumvalue = x^(term-1)
  if ( x > 1){
    return(print("The Series will not Converge"))
  }
  else if ( x < 1) {
    while (abs((refvalue-sumvalue)/refvalue)>desirederror){
      term=term+1
      sumvalue=sumvalue+x^(term-1)
  }
  
  }
  return(term)
}
numberoftermsingeo(1.1,0.01)
