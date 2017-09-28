#Create A Martix For Network And Has To Cover All K Coverage
CreateNetworkMatrix <- function (networkModel)
{
  myNetwork <<- matrix(networkModel, nr = TagsCount, nc = ReadersCount,TRUE)
  
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
      if (is.na(myNetwork[i,j]))
      {
        
      }
      else{
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
      if (is.na(myNetwork[i,j]))
      {
        
      }
      else{
      if (myNetwork[i,j]-ptNumber>0)
      {
        sum<-sum+1
      }
    }
    if (sum<min) 
    {
      min<-sum
    }}
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
      if (is.na(myNetwork[i,j]))
      {
        
      }
      else{
      if (myNetwork[i,j]-ptNumber>0)
      {
        sum<-sum+1
      }
    }
    loadBL<-sum/maxCapacity
    lstLoad[[length(lstLoad)+1]]<-loadBL
    }
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
      if (is.na(myNetwork[i,j]))
      {
        
      }
      else{
      if (myNetwork[i,j]-ptNumber>0)
      {
        sum<-sum+1
      }
    }
    if (sum<min) 
    {
      min<-sum
    }}
  }
  returnValue(min) 
} 
#Create A List For Making Decistion Based on them
CreateLstAllNetworks <- function ()
{
  
  decisionFuzzyModel<-list(list()) # To Find The Fuzzy Descition Table
  modelCounter<-0
 
    coverageResult<-CalcFcov()
    coverFuzzyTable<-CoverageFuzzyTable()
    fuzzyResultCover<-SearchFuzzyResult(coverFuzzyTable,coverageResult)
    decisionFuzzyModel[[1]]$FCover<-fuzzyResultCover
    
    interferianceResult<-CalcFIntr()
    interferianceFuzzyTable<-InterferianceFuzzyTable()
    fuzzyResultInterferiance<-SearchFuzzyResult(interferianceFuzzyTable,interferianceResult)
    decisionFuzzyModel[[1]]$FInterFeri<-fuzzyResultInterferiance
    
    loadbalanceResult<-CalcFLoadBalance()
    loadbalanceFuzzyTable<-LoadBalancingFuzzyTable()
    fuzzyResultLoadbalance<-SearchFuzzyResult(loadbalanceFuzzyTable,loadbalanceResult)
    decisionFuzzyModel[[1]]$FLoadBalance<-fuzzyResultLoadbalance
    
    aggregationResult<-CalcFAgregate()
    aggregationFuzzyTable<-AggregationFuzzyTable()
    fuzzyResultaggregation<-SearchFuzzyResult(aggregationFuzzyTable,aggregationResult)
    decisionFuzzyModel[[1]]$FAggrigate<-fuzzyResultaggregation
  
    returnValue(decisionFuzzyModel)
 
}
#Check If The Network has minimum Reqirements For Coverage
CheckValidNetwork<-function(enteredNetwork,kCoverage)
{
  validate<-TRUE
  for (i in 1:TagsCount)
  {
    sum<-0
    for (j in 1:ReadersCount)
    {
      if (is.na(enteredNetwork[i,j]))
      {
        
      }
      else{
        if (enteredNetwork[i,j]-ptNumber>0)
        {
          sum<-sum+1
        }
      }
    }
    if (sum<kCoverage) 
    {
      validate<-FALSE
    }
  }
  returnValue(validate)
}
#Search Fuzzy Table For Selected Result in Model
SearchFuzzyResult<-function(fuzzyTable,funcResult)
{
  countlst<-length(fuzzyTable$Value)
  selectedIndex<-0
  for (i in 1:countlst)
  {
     if (funcResult>=fuzzyTable$Value[[i]])
     {
       if (i==countlst)
       {
         selectedIndex<-i
         break
       }
       if (funcResult<fuzzyTable$Value[[i+1]]){
         selectedIndex<-i
         break
       }
       
     }else{
       selectedIndex<-i
       break
     }
  }
  returnValue(fuzzyTable$FuzzyResult[[selectedIndex]])
  
}
#Make Fuzzy Data For Each Result in Coverage
CoverageFuzzyTable<-function()
{
  
  fuzzyArray<-list()
  #Worst Stuation
  fuzzyArray$Value[[1]]=-1000
  fuzzyArray$FuzzyResult[[1]]=c(0,0,1)
  
  #Bad Situation
  fuzzyArray$Value[[2]]=0
  fuzzyArray$FuzzyResult[[2]]=c(0,1,3)
  
  #Medium Situation
  fuzzyArray$Value[[3]]=70
  fuzzyArray$FuzzyResult[[3]]=c(3,5,7)
  
  #Good Situation
  fuzzyArray$Value[[4]]=90
  fuzzyArray$FuzzyResult[[4]]=c(7,9,10)
  
  #Best Situation
  fuzzyArray$Value[[5]]=1000
  fuzzyArray$FuzzyResult[[5]]=c(9,10,10)
  
  returnValue(fuzzyArray)
}
#Make Fuzzy Data For Each Result in Interferiance
InterferianceFuzzyTable<-function()
{
  fuzzyArray<-list()
  #Worst Stuation
  fuzzyArray$Value[[1]]=-1000
  fuzzyArray$FuzzyResult[[1]]=c(0,0,1)
  
  #Bad Situation
  fuzzyArray$Value[[2]]=-1000
  fuzzyArray$FuzzyResult[[2]]=c(0,1,3)
  
  #Medium Situation
  fuzzyArray$Value[[3]]=-1000
  fuzzyArray$FuzzyResult[[3]]=c(3,5,7)
  
  #Good Situation
  fuzzyArray$Value[[4]]=-1000
  fuzzyArray$FuzzyResult[[4]]=c(7,9,10)
  
  #Best Situation
  fuzzyArray$Value[[5]]=-1000
  fuzzyArray$FuzzyResult[[5]]=c(9,10,10)
  
  returnValue(fuzzyArray)
}
#Make Fuzzy Data For Each Result in Load Balancing
LoadBalancingFuzzyTable<-function()
{
  fuzzyArray<-list()
  #Worst Stuation
  fuzzyArray$Value[[1]]=-1000
  fuzzyArray$FuzzyResult[[1]]=c(0,0,1)
  
  #Bad Situation
  fuzzyArray$Value[[2]]=-1000
  fuzzyArray$FuzzyResult[[2]]=c(0,1,3)
  
  #Medium Situation
  fuzzyArray$Value[[3]]=-1000
  fuzzyArray$FuzzyResult[[3]]=c(3,5,7)
  
  #Good Situation
  fuzzyArray$Value[[4]]=-1000
  fuzzyArray$FuzzyResult[[4]]=c(7,9,10)
  
  #Best Situation
  fuzzyArray$Value[[5]]=-1000
  fuzzyArray$FuzzyResult[[5]]=c(9,10,10)
  
  returnValue(fuzzyArray)
}
#Make Fuzzy Data For Each Result in Aggregation
AggregationFuzzyTable<-function()
{
  fuzzyArray<-list()
  #Worst Stuation
  fuzzyArray$Value[[1]]=-1000
  fuzzyArray$FuzzyResult[[1]]=c(0,0,1)
  
  #Bad Situation
  fuzzyArray$Value[[2]]=-1000
  fuzzyArray$FuzzyResult[[2]]=c(0,1,3)
  
  #Medium Situation
  fuzzyArray$Value[[3]]=-1000
  fuzzyArray$FuzzyResult[[3]]=c(3,5,7)
  
  #Good Situation
  fuzzyArray$Value[[4]]=-1000
  fuzzyArray$FuzzyResult[[4]]=c(7,9,10)
  
  #Best Situation
  fuzzyArray$Value[[5]]=-1000
  fuzzyArray$FuzzyResult[[5]]=c(9,10,10)
  
  returnValue(fuzzyArray)
}
#Make A Network By giving X and Y of Space And put Position On Page
CreateNetworkPositions<-function (XNumberPage,YNumberPage,iTagCount,jReaderCount,positionTagReaderRef)
{

  positionTagArray<-list()
  positionReaderArray<-list()
  for (counterTags in 1:iTagCount)
  {
    positionTagArray[counterTags]<-list(list(Xpoint=runif(1, 0, XNumberPage),Ypoint=runif(1, 0, YNumberPage)))
    
  }
  for (counterTags in 1:jReaderCount)
  {
    positionReaderArray[counterTags]<-list(list(Xpoint=runif(1, 0, XNumberPage),Ypoint=runif(1, 0, YNumberPage)))
  }
  eval.parent(substitute(positionTagReaderRef<-positionReaderArray))
  returnValue(positionTagArray)
  
  
}
# Find A Valid Network To Start Finding Best Choices
FindValidNewtworkToCalculate<-function(XNumberPage,YNumberPage,iTagCount,jReaderCount,KCoverage,ipt,imaxCapacity)
{
  TagsCount<<-iTagCount
  ReadersCount<<-jReaderCount
  ptNumber<<-ipt #Is Tresh hold Number
  maxCapacity<<-imaxCapacity # Capacity Of each reader
  sampleCounnt<-200
  validindex<-1
  SampleNetworks<<-list()
  GeneralFuzzyReslt<<-list(list())
  for (sample in 1:sampleCounnt)
  {
    lstReaders<-list()
    lstTags<-CreateNetworkPositions(XNumberPage,YNumberPage,iTagCount,jReaderCount,lstReaders)
    myNetwork <<- CreateNetworkMatrixBasedOnPositions(lstReaders,lstTags,iTagCount,jReaderCount)
    resultValidation<-CheckValidNetwork (myNetwork,KCoverage)
    if (resultValidation){
       SampleNetworks[[validindex]]<<-lstTags
       resultFuzzyNetwork<- CreateLstAllNetworks()
       GeneralFuzzyReslt[[validindex]]<<-resultFuzzyNetwork
       validindex<-validindex+1
    }
  
  }
  # ToDo , We have our decition Making Table and Networks
  #Now It's time to use Topsis fuzzy to find the ideal soltion
}
#To Find Matrix of Network
CreateNetworkMatrixBasedOnPositions<-function (lstReaders,lstTags,iTagCount,jReaderCount)
{
  mySelectedNetwork<-matrix(NA , nr = iTagCount, nc = jReaderCount,TRUE)
  for (reader in 1:jReaderCount)
  {
    xReader<-lstReaders[[reader]]$Xpoint
    yReader<-lstReaders[[reader]]$Ypoint
    for (tag in 1:iTagCount)
    {
      radiatedPower<--20
      xtag<-lstTags[[tag]]$Xpoint
      ytag<-lstTags[[tag]]$Ypoint
      resultSum<-((xReader-xtag)^2)-((yReader-ytag)^2)
      if (resultSum<0){
          resultSum<-resultSum*-1
      }
      euclidosResult<-sqrt(resultSum)
      if (euclidosResult<=2.5){
        radiatedPower<-1
      }
      mySelectedNetwork[tag,reader]<-radiatedPower
    }
  }
  returnValue(mySelectedNetwork)
}

