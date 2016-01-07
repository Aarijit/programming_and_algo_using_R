### SOLUTIONS OF LINEAR SYSTEM OF EQUATION

## EXAMPLE 1:  Forward Substitution 

forwardSub<-function(L,b){
  x=c(0)
  n=nrow(L)
  for(i in (1:n)){
    x[i]=b[i]
    if(i>1){
      for(j in (1:(i-1))){
        x[i]=x[i]-L[i,j]*x[j]
      }
    }
    x[i]=x[i]/L[i,j]
  }
  return(cbind(x))
}



```



```{r}
## EXAMPLE 2:  Backward Substitution 

backwardSub<-function(U,b){
  x=c(0)
  n=nrow(U)
  for(i in (n:1)){
    x[i]=b[i]
    if(i<n){
      for(j in ((i+1):n)){
        x[i]=x[i]-U[i,j]*x[j]
      }
    }
    x[i]=x[i]/U[i,i]
  }
  return(cbind(x))
}

```



```{r}
## ## EXAMPLE 3:  Gaussian Substitution 

gaussianElimination<-function(Ab){
  n=nrow(Ab)
  for(k in (1:(n-1))){
    for (i in ((k+1):n)){
      mik=Ab[i,k]/Ab[k,k]
      Ab[i,k]=0
      for(j in ((k+1):(n+1))){
        Ab[i,j]=Ab[i,j]-mik*Ab[k,j]
      }
    }
  }
  return(Ab)
}


```



```{r}
## EXAMPLE 4: LU FACTORIZATION

luFactorization<-function(A){
  n=nrow(A)
  L=matrix(0,nrow=n,ncol=n)
  for(k in (1:(n-1))){
    for(i in ((k+1):n)){
      L[i,k]=A[i,k]/A[k,k]
      A[i,k]=0
      for(j in (k+1):n)){
        A[i,j]=A[i,j]-L[i,k]*Ab[k,j]
      }
    }
  }
  for(k in (1:n)){
    L[k,k]=1
  }
  return(cbind(L,A))
}


```



```{r}
## EXAMPLE 5:  Gaussian Elimination With Partial Pivoting (ISSUE WITH THE CODE)

gaussianEliminationPartial<-function(Ab){
  n=nrow(Ab)
  for(k in (1:(n-1))){
    pivotIndex=k
    for(i in ((k+1):n)){
      if((abs[Ab(i,k)])>abs(Ab[pivotIndex,k])){
        pivotIndex=i
      }
      
    }
    If (pivotIndex !=k){
      for(j in (k:(n+1))){
        buffer=Ab[k,j]
        Ab[k,j]=Ab[pivotIndex,j]
        Ab[pivotIndex,j]=buffer
        
      }
      
      
    }
    
    for(i in ((k+1):n)){
      mik=Ab[i,k]/Ab[k,k]
      Ab[i,k]=0
      for(j in ((k+1):(n+1))){
        Ab[i,j]=Ab[i,j]-mik*Ab[k,j]
      }
    }
  }
  return(Ab)
}

```



```{r}
## EXAMPLE 6: Gaussian Elimination for Tridiagonal Matrices

gaussianEliminationTridigonal<-function(Ab){
  n=nrow(Ab)
  for(k in (1:(n-1))){
    multiplier=Ab[k+1,k]/Ab[k,k]
    Ab[k+1,k]=0
    Ab[k+1,k+1]=Ab[k+1,k+1]-multiplier*Ab[k,k+1]
    Ab[k+1,n+1]=Ab[k+1,n+1]-multiplier*Ab[k,n+1]
  }
  return(Ab)
}



```



```{r}
## EXAMPLE 7: Backward Substitution for Tridiagonal Matrices 

backwardSubTridiagonal<-function(U,b){
  x=c(0)
  n=nrow(U)
  for(i in (n:1)){
    x[i]=b[i]
    if(i<n){
      x[i]=x[i]-U[i,i+1]*x[i+1]
    }
    x[i]=x[i]/U[i,i]
  }
  return(cbind(x))
}


```



```{r}
# EXAMPLE 8:  Cholesky Factorization 

choleskyfactorization = function(A){
  n = nrow(A)
  L = matrix(0,nrow=n,ncol=n)
  for (i in (1:n)){
    L[i,i] = A[i,i]
    if (i > 1){
      for (k in (1:(i-1))){
        L[i,i] = L[i,i] - L[i,k]*L[i,k]
      }
    }
    L[i,i] = (L[i,i])^(1/2)
    if (i < n){
      for (j in ((i+1):n)){
        L[j,i] = A[j,i]
        if (i > 1){
          for (k in (1:(i-1))){
            L[j,i] = L[j,i] - L[j,k]*L[i,k]
            
          }
        }
        L[j,i] = L[j,i]/L[i,i]
      }
    }
  }
  return(L)
}




```





```{r}
### MISSED AN EXAMPLE ###

```



```{r}

## PROBLEM 1: 
