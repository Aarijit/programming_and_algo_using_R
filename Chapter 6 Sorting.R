```{r}
### SORTING

## EXAMPLE 1:  Bubble Sort 

bubbleSort<-function(a){
  n=length(a)
  swap=1
  while(swap==1){
    swap=0
    for(i in (1:(n-1))){
      if(a[i]>a[i+1]){
        aBackUp=a[i]
        a[i]=a[i+1]
        a[i+1]=aBackUp
        swap=1
      }
    }
  }
  return(a)
}
bubbleSort(c(5,−3,4,6,3,1))


```


```{r}
### EXAMPLE 2:  Improved Bubble Sort

bubbleSortImproved<-function(a){
  n=length(a)
  swap=1
  step=1
  while(swap==1){
    swap=0
    if(step<n){
      for(i in (1:(n-step))){
        if(a[i]>a[i+1]){
          aBackUp=a[i]
          a[i]=a[i+1]
          a[i]=aBackUp
          swap=1
        }
      }
    }
    step=step+1
  }
  return(a)
}
bubbleSortImproved(c(5,−3,4,6,3,1))

```



```{r}
### EXAMPLE 3: Insertion Sort 

insertionSort<-function(a){
  n=length(a)
  for(i in (2:n)){
    for(j in (1:(i-1))){
      if(a[i-j]>a[i-j+1]){
        aBackUp=a[i-j]
        a[i-j]=a[i-j+1]
        a[i-j+1]=aBackUp
      }
      else{
        break
      }
    }
  }
  return(a)
}
insertionSort(c(5,−3,4,6,3,1))

```



```{r}
## EXAMPLE 4: QUICK SORT

quickSort<-function(a){
  n=length(a)
  if(n>1){
    p=floor(n/2)
    pivot=a[p]
    aSmaller=c()
    aLarger=c()
    for(i in (1:n)){
      if(i!=p){
        if(a[i]<pivot){
          aSmaller=rbind(aSmaller,a[i])
        }
        else{
          aLarger=rbind(a[i],aLarger)
        }
      }
    }
    aSmaller=quickSort(aSmaller)
    aLarger=quickSort(aLarger)
    a=rbind(aSmaller,a[p],aLarger)
  }
  return(a)
}
quickSort(c(5,−3,4,6,3,1))

```



```{r}


## PROBLEM 1 : Write a program that sorts a given vector using the improved bubble sort algo-rithm. The input should be a vector of n elements. The output should be the same vector with sorted elements.##

## SOLUTION : ##

bubblesortimproved<-function(a){
  n=length(a)
  swap=1
  step=1
  while(swap==1){
    swap=0
    if(step<n){
      for(i in (1:(n-step))){
        if(a[i]>a[i+1]){
          aBackUp=a[i]
          a[i]=a[i+1]
          a[i]=aBackUp
          swap=1
        }
      }
    }
    step=step+1
  }
  return(a)
}
bubblesortimproved(matrix(c(50,-3,1,6,12,10,0,-40,1,5,8,1),ncol=1))


## PROBLEM 2 : Write a program that sorts a given vector using the insertion sort algorithm. The input should be a vector of n elements. The output should be the same vector with sorted elements ##

## SOLITION : ##

insertionSort<-function(a){
  n=length(a)
  for(i in (2:n)){
    for(j in (1:(i-1))){
      if(a[i-j]>a[i-j+1]){
        aBackUp=a[i-j]
        a[i-j]=a[i-j+1]
        a[i-j+1]=aBackUp
      }
      else{
        break
      }
    }
  }
  return(a)
}

insertionSort(matrix(c(50,-3,1,6,12,10,0,-40,1,5,8,1),ncol=1))


  ## SKIPPING PROBLEM 3 ##

## EXERCISE 4 : Write a program that sorts a given vector using the quick sort algorithm with p = n/2 pivoting. The input should be a vector of n elements. The output should be the same vector with sorted elements. ##

## SOLUTION ##

quickSort<-function(a){
n=length(a)
if(n>1){
  p=floor(n/2)
  pivot=a[p]
  aSmaller=c()
  aLarger=c()
  for(i in (1:n)){
    if(i!=p){
      if(a[i]<pivot){
        aSmaller=rbind(aSmaller,a[i])
      }
      else{
        aLarger=rbind(a[i],aLarger)
      }
    }
  }
  aSmaller=quickSort(aSmaller)
  aLarger=quickSort(aLarger)
  a=rbind(aSmaller,a[p],aLarger)
}
return(a)
}

quickSort(matrix(c(50,-3,1,6,12,10,0,-40,1,5,8,1),ncol=1))


## SKIPPING PROBLEM 5 ##





