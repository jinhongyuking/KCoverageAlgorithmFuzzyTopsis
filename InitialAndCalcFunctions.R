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
  lstdecition<-ConvertFuzzyNumbersToPureNumbers(GeneralFuzzyReslt,validindex-1)
  matrixDecition<-matrix(lstdecition,nrow=validindex-1,ncol=4)
  w <- c(1,1,1,1)
  cb <- c()#c('max','max','max','max','max')
  numberofcb=ncol(matrixDecition)/(validindex-1)
  for(cbcounter in 1:numberofcb)
  {
    cb[cbcounter]<-'max'
  }
  if (validindex>1){
  resultTopsisFuzzy<<-FuzzyTOPSISLinear(matrixDecition,w,cb,validindex-1)
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
#Create A List To Pass to a topsis function
ConvertFuzzyNumbersToPureNumbers<-function(lstFuzzyNumbers,countResults)
  {
 lstResult<-c()
 indexlst<-0
   for (sample in 1:countResults)
  {
     sumdata<-0
    sumdata<-sumdata+lstFuzzyNumbers[[sample]][[1]]$FCover[1]
    sumdata<-sumdata+lstFuzzyNumbers[[sample]][[1]]$FCover[2]
    sumdata<-sumdata+lstFuzzyNumbers[[sample]][[1]]$FCover[3]
    resultFCover<-sumdata/3
    indexlst<-indexlst+1
    lstResult[indexlst]<-resultFCover
    sumdata<-0
    sumdata<-sumdata+lstFuzzyNumbers[[sample]][[1]]$FInterFeri[1]
    sumdata<-sumdata+lstFuzzyNumbers[[sample]][[1]]$FInterFeri[2]
    sumdata<-sumdata+lstFuzzyNumbers[[sample]][[1]]$FInterFeri[3]
    resultFInterFer<-sumdata/3
    indexlst<-indexlst+1
    lstResult[indexlst]<-resultFInterFer
    sumdata<-0
    sumdata<-sumdata+lstFuzzyNumbers[[sample]][[1]]$FLoadBalance[1]
    sumdata<-sumdata+lstFuzzyNumbers[[sample]][[1]]$FLoadBalance[2]
    sumdata<-sumdata+lstFuzzyNumbers[[sample]][[1]]$FLoadBalance[3]
    resultFLoadBalance<-sumdata/3
    indexlst<-indexlst+1
    lstResult[indexlst]<-resultFLoadBalance
    sumdata<-0
    sumdata<-sumdata+lstFuzzyNumbers[[sample]][[1]]$FAggrigate[1]
    sumdata<-sumdata+lstFuzzyNumbers[[sample]][[1]]$FAggrigate[2]
    sumdata<-sumdata+lstFuzzyNumbers[[sample]][[1]]$FAggrigate[3]
    resultFAggrigate<-sumdata/3
    indexlst<-indexlst+1
    lstResult[indexlst]<-resultFAggrigate
  }
 returnValue(lstResult)
}
#Calculating Fuzzy Result Of Topsis In Network
              #d <- matrix(c(5.7,6.3,6.3,7.7,8.3,8,9.3,9.7,9,5,9,7,7,10,9,9,10,10,5.7,8.3,7,7.7,9.7,9,
              #'  9,10,10,8.33,9,7,9.67,10,9,10,10,10,3,7,6.3,5,9,8.3,7,10,9.7),nrow=3,ncol=15)
              #'  w <- c(0.7,0.9,1,0.9,1,1,0.77,0.93,1,0.9,1,1,0.43,0.63,0.83)
              #'  cb <- c('max','max','max','max','max')
              #'  FuzzyTOPSISLinear(d,w,cb)
FuzzyTOPSISLinear <- function(decision, #matrix with all the alternatives
                              weights,  #vector with the numeric values of the weights
                              cb ,       #vector with the "type" of the criteria (benefit = "max", cost = "min")
                              numberofAlternates
)
{
  #Checking the arguments
  if(! is.matrix(decision))
    stop("'decision' must be a matrix with the values of the alternatives")
  if(missing(weights))
    stop("a vector containing n weigths, adding up to 1, should be provided")
  #   if(sum(weights[seq(2, length(weights), numberofAlternates)]) != 1)
  #     stop("The sum of 'weights' is not equal to 1")
  if(! is.character(cb))
    stop("'cb' must be a character vector with the type of the criteria")
  if(! all(cb == "max" | cb == "min"))
    stop("'cb' should contain only 'max' or 'min'")
  if(length(weights) != ncol(decision))
    stop("length of 'weights' does not match the number of the criteria")
  if(length(cb) != ncol(decision)/numberofAlternates)
    stop("length of 'cb' does not match the number of the criteria")
  
  
  #TOPSIS method
  
  # Conversion of cb in "fuzzy" values
  new_cb <- c(1:ncol(decision))
  k=1
  for(j in seq(1, ncol(decision), numberofAlternates)){
    if (cb[k] == 'max'){
      new_cb[j] <- 'max'
      new_cb[j+1] <- 'max'
      new_cb[j+2] <- 'max'
    }
    else{
      new_cb[j] <- 'min'
      new_cb[j+1] <- 'min'
      new_cb[j+2] <- 'min'
    }
    k=k+1
  }
  
  #1. Normalization and weighting
  N <- matrix(nrow = nrow(decision), ncol = ncol(decision))
  
  for(i in seq(1, ncol(decision), 1)){
    if (new_cb[i] == 'max'){
      denominator = max(decision[,i])
      N[,i] = decision[,i]/denominator
    #  N[,i+1] = decision[,i+1]/denominator
     # N[,i+2] = decision[,i+2]/denominator
    }
    else{
      denominator = min(decision[,i])
      N[,i] = denominator/decision[,i+2]
      N[,i+1] = denominator/decision[,i+1]
      N[,i+2] = denominator/decision[,i]
    }
  }
  
  W <- diag(weights)
  NW <- N%*%W
  
  #2. Ideal solutions
  posI <- rep(1,ncol(decision))
  negI <- rep(0,ncol(decision))
  
  #3. Distances to the ideal solutions
  distance_pos = matrix(0,nrow = nrow(decision), ncol = ncol(decision))
  distance_neg = matrix(0,nrow = nrow(decision), ncol = ncol(decision))
  for(j in seq(1, ncol(decision), 3)){
    distance_pos[,j] = ((1/3)*((NW[,j]-posI[j])^2+(NW[,j+1]-posI[j+1])^2+(NW[,j+2]-posI[j+2])^2))^(1/2)
    distance_neg[,j] = ((1/3)*((NW[,j]-negI[j])^2+(NW[,j+1]-negI[j+1])^2+(NW[,j+2]-negI[j+2])^2))^(1/2)
  }
  
  posDis <- apply(distance_pos, 1, sum)
  negDis <- apply(distance_neg, 1, sum)
  
  #4. R index
  R <- negDis/(negDis+posDis)
  
  #5. Rank the alternatives
  return(data.frame(Alternatives = 1:nrow(decision), R = R, Ranking = rank(-R, ties.method= "first")))
  
}

