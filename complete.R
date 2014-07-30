complete <- function(directory, id = 1:332)
{
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  path = getwd()
  p2 = paste(path,"/",directory,sep="")
  setwd(p2)
  
  first = T
  for (i in id)
  {
    if(i<10)
    {
      t1 = "00"
    }
    else if(i>9 & i<100)
    {
      t1 = "0"
    }
    else
    {
      t1 = ""
    }
    tfn = paste(t1,i,".csv",sep="")
    temp = read.csv(tfn)
    num = nrow(na.omit(temp))
    if(first)
    {
      res = matrix(c(i,num),1,2)
      first = F
    }
    else
    {
      res = rbind(res,c(i,num))
    }
    
  }
  print (res)   
}
