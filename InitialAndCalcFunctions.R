#Create A Martix For Network And Has To Cover All K Coverage
CreateNetworkMatrix <- function (networkModel,iTagsCount,iReadersCount,ipt,imaxCapacity)
{
  myNetwork <<- matrix(networkModel, nr = iTagsCount, nc = iReadersCount,TRUE)
  TagsCount<<-iTagsCount
  ReadersCount<<-iReadersCount
  ptNumber<<-ipt
  maxCapacity<<-imaxCapacity
  returnValue(myNetwork) 
} 
#Calculate Matrix For Coverage in K Coverage
CalcFcov <- function ()
{
  sum<-0
  min<-1000;
  for (i in 1:TagsCount)
  {
    sum<-0
    for (j in 1:ReadersCount)
    {
      if (myNetwork[i,j]-ptNumber>0)
      {
        sum<-sum+myNetwork[i,j]-ptNumber
      }
    }
    if (sum<min) 
    {
      min<-sum
    }
  }
  returnValue(min) 
}
#Calculate Interferiance Function And Get Minumum Data
CalcFIntr<- function()
{
  sum<-0
  min<-1000;
  for (i in 1:TagsCount)
  {
    sum<-0
    for (j in 1:ReadersCount)
    {
      if (myNetwork[i,j]-ptNumber>0)
      {
        sum<-sum+1
      }
    }
    if (sum<min) 
    {
      min<-sum
    }
  }
  returnValue(min) 
}

#Calculate Load Balancing Function And Get Minumum Data
CalcFLoadBalance<- function()
{
  sum<-0
  lstLoad<-list()
  for (j in 1:ReadersCount)
  {
    sum<-0
    for (i in 1:TagsCount)
    {
      if (myNetwork[i,j]-ptNumber>0)
      {
        sum<-sum+1
      }
    }
    loadBL<-sum/maxCapacity
    lstLoad[[length(lstLoad)+1]]<-loadBL
  }
  output <- matrix(unlist(lstLoad), ncol = 1, byrow = TRUE)
  resultVar<-var(output)
  returnValue(resultVar) 
}

#Calculate Agregate Function And Get Minumum Data
CalcFAgregate<- function()
{
  sum<-0
  min<-1000;
  for (i in 1:TagsCount)
  {
    sum<-0
    for (j in 1:ReadersCount)
    {
      if (myNetwork[i,j]-ptNumber>0)
      {
        sum<-sum+1
      }
    }
    if (sum<min) 
    {
      min<-sum
    }
  }
  returnValue(min) 
}
