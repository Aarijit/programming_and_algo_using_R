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
