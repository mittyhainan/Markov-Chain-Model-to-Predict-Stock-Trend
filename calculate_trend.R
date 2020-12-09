calculate_trend = function(data){
  num_increase <<- 0
  num_decrease <<- 0
  num_stagnant <<- 2
  increase_index <<- c()
  decrease_index <<- c()
  stagnant_index <<- c(1,2)
  index <<- rep(0,length(data))
  increase2increase <<- 0
  increase2decrease <<- 0
  increase2stagnant <<- 0
  decrease2increase <<- 0
  decrease2decrease <<- 0
  decrease2stagnant <<- 0
  stagnant2increase <<- 0
  stagnant2decrease <<- 0
  stagnant2stagnant <<- 0
  for(i in 3:length(data)){
    if(data[i] > data[i-1] && data[i-1] > data[i-2]){
      num_increase <<- num_increase +1
      increase_index <<- c(increase_index,i)
      index[i] <<- 1
    }
    else if(data[i] < data[i-1] && data[i-1] < data[i-2]){
      num_decrease <<- num_decrease + 1
      decrease_index <<- c(decrease_index,i)
      index[i] <<- -1
    }
    else{
      num_stagnant <<- num_stagnant +1
      stagnant_index <<- c(stagnant_index,i)
    }
  }
  #increase
  for(i in 1:length(increase_index)){
    if(increase_index[i]!= length(data)){
      if(index[increase_index[i]+1] == 1){
        increase2increase <<- increase2increase +1
      }
      else if(index[increase_index[i]+1] == -1){
        increase2decrease <<- increase2decrease +1
      }
      else{
        increase2stagnant <<- increase2stagnant+1
      }
    }
  } 
  #decrease
  for(i in 1:length(decrease_index)){
    if(decrease_index[i] != length(data)){
      if(index[decrease_index[i]+1] == 1){
        decrease2increase <<- decrease2increase+1
      }
      if(index[decrease_index[i]+1] == -1){
        decrease2decrease <<- decrease2decrease+1
      }
      else{
        decrease2stagnant <<- decrease2stagnant+1
      }
    }
  }
  #stagnant
  for(i in 1:length(stagnant_index)){
    if(stagnant_index[i] != length(data)){
      if(index[stagnant_index[i]+1] == 1){
        stagnant2increase <<- stagnant2increase+1
      }
      else if(index[stagnant_index[i]+1] == -1){
        stagnant2decrease <<- stagnant2decrease+1
      }
      else{
        stagnant2stagnant <<- stagnant2stagnant+1
      }
    }
  }
  
}
