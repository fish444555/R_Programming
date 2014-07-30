pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  
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
    if(first)
    {    
      total = temp
      first = F
    }
    else
    {
      total = rbind(total,temp)
    }  
  }
  res = apply(na.omit(total[,2:3]),2,mean)
  print(res)
  if(pollutant=="sulfate")
  {
    print(res[[1]]) 
  }
  else
  {
    print(res[[2]])   
  }
  


}
