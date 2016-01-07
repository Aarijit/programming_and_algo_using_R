### CHAPTER 4: COMPLEXITY OF PROGRAMS AND ALGORITHMS  ###

## EXAMPLE 1: INNER PRODUCT ### 


innerprod<-function(v,w){
  sum_prod=0
  for(i in 1:length(v)){
    sum_prod=sum_prod+v[i]*w[i]
  }
  return(sum_prod)
}



```


```{r}
## EXAMPLE 2: INNER PRODUCT: 2ND METHOD


innerProductWithCheck<-function(v,w){
  if(length(v)==length(w)){
    sumVal=0
    for (i in 1:lenght(v)){
      sumVal=sumVal+w[i]*v[i]
    }
    return(sumVal)
  }
  else{
    print("Vectors must have same lenght!")
  }
}


```



```{r}
### CHAPTER 4: ACCURACY ISSUES ###

## EXAMPLE 1: CREATING PLOT OF MATHEMATICAL FUNCTION


evalFunction1<-function(xMin,xMax,n){
  x=c(0)
  f=c(0)
  for (i in 0:n){
    x[i+1]=xMin+i*(xMax-xMin)/n
    f[i+1]=(1-cos(x[i+1]))/(x[i+1])^2
  }
  plot(x,f,type="l",col="blue",xlab = "x",ylab = "function")
  
}

evalFunction1(-1,1,100)
evalFunction1(-10^-7,10^-7,200) 
## ALTERNATE METHOD USING R's "CURVE" FUNCTION

curve((1-cos(x))/(x)∧2,from=-1,to=1) 

```


```{r}
### EXAMPLE 2: CREATING PLOT OF MATHEMATICAL FUNCTION

evalFunction2<-function(xMin,xMax,n){
  x=c(0)
  f=c(0)
  for (i in 0:n){
    x[i+1]=xMin+i*(xMax-xMin)/n
    f[i+1]=(sin(x[i+1]/2))^2/((x[i+1])^2/2)
  }
  plot(x,f,type="l",col="blue",xlab = "x",ylab = "function")
  
}
evalFunction2(-1,1,100)
evalFunction2(-10^-7,10^-7,200) 




```


```{r}
### EXAMPLE 2: CREATING PLOT OF MATHEMATICAL FUNCTION
evalFunctionWithCheck<-function(xMin,xMax,n,epsilon){
  x=c(0)
  f=c(0)
  for(i in 0:n){
    x[i+1]=xMin+i*(xMax+xMin)/n
    if (abs(x[i+1])>epsilon){
      f[i+1]=(sin(x[i+1]/2))^2/((x[i+1])^2/2)
    }
    else{
      f[i+1]=1/2
    }
  }
  plot(x,f,type="l",col="blue",xlab = "x",ylab = "function")
}
evalFunctionWithCheck(-1,1,100,10^-10)
evalFunctionWithCheck(-10^-7,10^-7,200,10^-10) 


```


```{r}
## EXAMPLE 3: Direct Polynomial Evaluation 

polyeval<-function(a,x0){
  n=length(a)-1
  polyValue=a[1]
  for(i in 1:n){
    polyValue=polyValue+a[j+1]*(x0)^j
  }
  return(polyValue)
}



```



```{r}
## EXAMPLE 4: Improved Direct Polynomial Evaluation
polyEvalImproved<-function(a,x0){
  n=length(a)-1
  polyValue=a[1]
  powers0fx0=1
  for(j in 1:n){
    powers0fx0=x0*powers0fx0
    polyValue=polyValue+a[j+1]*powers0fx0
  }
  return(polyValue)
}

```



```{r}

##EXAMPLE 5:  Polynomial Evaluation With Horner’s Algorithm 

polyEvalHorner<-function(a,x0){
  n=length(a)-1
  polyValue=a[n+1]
  for(j in 1:n){
    polyValue=a[n+1-j]+x0*polyValue
  }
  return(polyValue)
}

```



```{r}
## EXAMPLE 6:  Using Horner’s Algorithm At Multiple Points 

polyEvalHornerMultiple<-function(a,xMin,xMax,n){
  x=c(0)
  y=c(0)
  for(i in 0:n){
    x[i+1]=xMin+i*(xMax-xMin)/n
    y[i+1]=polyEvalHorner(a,x[i+1])
  }
  plot(x,y,type="l",col="blue",xlab = "x",ylab = "function")
}
polyEvalHornerMultiple(c(1,-3,3,-1),0.99999,1.00001,200)


```



```{r}



## SOLUTIONS ##

## PROBLEM 1 :  Write a program that calculates the inner product of two given vectors v and w ∈ R n using a loop. Test your program for various small vectors. Then, use your program and measure the processing time for vectors of different sizes ##

## SOLUTION ##

innerProduct<-function(c, w ){
  sumProduct=0
  n=length(c)
  i=1
  while( i<=n ){
    sumProduct = sumProduct+c[i]*w[i]
    i = i +1
  }
  return(sumProduct)
  
} 
system.time(innerProduct(matrix(1,nrow=10000),matrix(1,nrow=10000)))
system.time(innerProduct(matrix(1,nrow=40000),matrix(1,nrow=40000)))
system.time(innerProduct(matrix(1,nrow=160000),matrix(1,nrow=160000)))
system.time(innerProduct(matrix(1,nrow=640000),matrix(1,nrow=640000)))



## PROBLEM 2 : Using your time measurement for n = 640, 000 in Question 1, estimate how many floating-point operations can be performed per second, assuming that a summation and multiplication is one floating-point operation. This will give a rough idea on how powerful the computer that you employ your program on. ##

## SOLUTION: CAN'T SOLVE THIS PROBLEM ##



## PROBLEM 3 : Write a program that multiplies a matrix A ∈ R m×n with a vector x ∈ R n . Test your program for various small matrices and vectors. Then, use your program and measure the processing time for matrices and vectors of different sizes ##

## SOLUTION ##

matrixMultiplication<-function(A,x){
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

system.time(matrixMultiplication(matrix(1,nrow=100,ncol=100),matrix(1,nrow=100)))
system.time(matrixMultiplication(matrix(1,nrow=200,ncol=200),matrix(1,nrow=200)))
system.time(matrixMultiplication(matrix(1,nrow=400,ncol=400),matrix(1,nrow=400)))
system.time(matrixMultiplication(matrix(1,nrow=800,ncol=800),matrix(1,nrow=800)))


## PROBLEM 4 : Write a program that multiplies two matrices A ∈ R m×n and B ∈ R n×p . Test your program for various small matrices. Then, use your program and measure the processing time for 50 × 50 and 100 × 100 matrices ##

## SOLUTION ##

matMatMult<-function(A,B){
  m = nrow(A)
  n = ncol(A)
  p = ncol(B)
  C = matrix(0,nrow=m,ncol=p)
  for (i in 1:m){
    for (j in 1:p){
      sumvalue = 0
      for (k in 1:n){
        sumvalue = sumvalue + A[i,k]*B[k,j]
      }
      C[i,j] = sumvalue
    }
  }
  return(C)
}

A=matrix(1,nrow=50,ncol=50)
B=matrix(1,nrow=50,ncol=50)
system.time(matMatMult(A,B))

A=matrix(1,nrow=100,ncol=100)
B=matrix(1,nrow=100,ncol=100)
system.time(matMatMult(A,B))

## COULD NOT DO 5, 6 , 7 AND 8 ##


      
    }
  }
}