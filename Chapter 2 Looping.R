### LOOPS

## EXAMPLE 1:  Calculation of 1-Norm Using For 

oneNorm_for<-function(v){
  sumVal=0
  for(i in 1:length(v)){
    sumVal=sumVal+abs(v[i])
  }
  return(sumVal)
}


```


```{r}

## EXAMPLE 2: Calculation of 1-Norm Using While
oneNorm_while<-function(v){
  sumVal=0
  n=length(v)
  while(i<=n){
    sumVal=sumVal+abs(v[i])
    i=i+1
  }
  return(sumVal)
}


```



```{r}
## EXAMPLE 3:  Finding the First Zero Using For 

findZero_for<-function(v){
  n=length(v)
  for(i in 1:n){
    if (v[i]==0){
      return(i)
    }
  }
  return("Vector does not contain Zero!")
}


```


```{r}
## EXAMPLE 4:  Calculation of Inﬁnity-Norm 

infinityNorm<-function(v){
  maxVal=0
  for(i in 1:length(v)){
    if(abs(v[i]>maxVal)){
      maxVal=abs(v[i])
    }
    return(maxVal)
  }
}




```




```{r}
## EXAMPLE 5:  Matrix–Vector Multiplication 

matVecMult<-function(A,x){
  m=nrow(A)
  n=ncol(A)
  y=matrix(0,nrow = m)
  for(i in 1:m){
    for(j in 1:n){
      
      y[i]=y[i]+A[i,j]*x[j]
      
    }
  }
  return(y)
}


matVecMult(matrix(c(2, 4, 3, 1, 5, 7),nrow = 2,ncol = 3),c(2,3,4))
```



```{r}
## EXAMPLE 6: Finding the Closest Pair 

findClosest<-function(x,y){
  n=length(x)
  minDistance=sqrt((x[1]-x[2])^2+(y[1]-y[2])^2)
  iBackUp=1
  jBackUp=2
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      distance=sqrt((x[i]-x[j])^2+(y[i]-y[j])^2)
      if(distance<minDistance){
        minDistance=distance
        iBackUp=i
        jBackUp=j
      }
    }
  }
  list(minDistance,iBackUp,jBackUp)
}

findClosest(c(2,3,4),c(3,5,6))


```




```{r}
## EXAMPLE 7: Finding the Number of Terms for e 

numberOfTermsForE<-function(desiredError){
  refValue=exp(1)
  term=1
  sumVal=1/factorial(term-1)
  while(abs((refValue-sumVal)/refValue)>desiredError){
    term=term+1
    sumVal=sumVal+1/factorial(term-1)
  }
  return(term)
}


```



```{r}
## EXAMPLE 8: Finding the Number of Terms in the Geometric Series

numberOfTermsinGeo<-function(x,desiredError){
  refValue=1/(1-x)
  term=1
  sumVal=x^(term-1)
  while(abs((refValue-sumVal)/refValue)>desiredError){
    term=term+1
    sumVal=sumVal+x^(term-1)
  }
  return(term)
}

```


```{r}
## EXAMPLE 9:  Babylonian Method for Square-Root of 5 

BabylonianForSqrtFive<-function(){
  xold=2
  xnew=0.5*(xold+5/xold)
  while(abs(xold-xnew)>0.001){
    print(xold)
    xold=xnew
    xnew=0.5*(xold+5/xold)
  }
  return(xnew)
}


```




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


## EXERCISE 3 ##

## Write a program to calculate the 2-norm of a given vector v ∈ R n , i.e., v 2 = 	(v[i]) 2 , using a for or while loop. Apply it to an example vector as twonorm(matrix(c(5,4,1,6,7,8,-4,15,-2,4),ncol=1)) Compare your result with the value given by the built-in function of R, i.e., norm(matrix(c(5,4,1,6,7,8,-4,15,-2,4),ncol=1),"E") ##

## SOLUTION ##

twoNorm<-function(v){
  sum_prod=0
  for(i in 1:length(v)){
    sum_prod=sum_prod+(v[i])^2
  }
  return((sum_prod)^1/2)
}

twoNorm(matrix(c(5,4,1,6,7,8,-4,15,-2,4),ncol=1))


norm(matrix(c(5,4,1,6,7,8,-4,15,-2,4),ncol=1),"E")


## EXERCISE 4 ##

## Write a program that calculates the sum of cubes of positive integers from 1 to n for a given value of n ##

## SOLUTION ##

sumOfCubes<- function(n){
  sumValue=0
  for ( i in 1 : n){
    sumValue=sumValue+i^3
    
  }
  return(sumValue)
}

sumOfCubes(10)

((10*(10+1))/2)^2




