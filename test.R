test_index = c()
prediction_markov_list = c()
for(i in 1:12){
  a = 1+(i-1)*250
  b = 200 + (i-1)*250
  c = 201+(i-1)*250
  d = 250+(i-1)*250
  if(i < 13){
    trainset = apple_daily$PRC[a:b]
    testset = apple_daily$PRC[c:d]
  }
  else{
    trainset = apple_daily$PRC[a:b]
    testset = apple_daily$PRC[c:3272]
  }
  calculate_trend(apple_daily$PRC[a:d])
  test_index = index[(length(index)-49):length(index)]
  #print(test_index)
  calculate_trend(trainset)
  apple_transition_matrix = matrix(c(increase2increase/num_increase,increase2decrease/num_increase,increase2stagnant/num_increase,decrease2increase/num_decrease,decrease2decrease/num_decrease,decrease2stagnant/num_decrease,stagnant2increase/num_stagnant,stagnant2decrease/num_stagnant,stagnant2stagnant/num_stagnant),nrow = 3,ncol = 3)
  apple_transition_matrix = t(apple_transition_matrix)
  #print(apple_transition_matrix)
  initial_state = c()
  if(index[length(trainset)]==1){
    initial_state = matrix(c(1,0,0),nrow = 1,ncol = 3)
  }
  else if(trainset[length(trainset)] == -1){
    initial_state = matrix(c(0,1,0),nrow = 1,ncol = 3)
  }
  else{
    initial_state = matrix(c(0,0,1),nrow = 1,ncol = 3)
  }
  #run on testset
  prediction_index = c()
  for(j in 1:length(testset)){
    iteration = (initial_state%*%(apple_transition_matrix))
    if(which.max(iteration) == 1){
      prediction_index = c(prediction_index,1)
    }
    else if(which.max(iteration) == 2){
      prediction_index = c(prediction_index,-1)
    }
    else{
      prediction_index = c(prediction_index,0)
    }
  if(test_index[j]==1){
    initial_state = matrix(c(1,0,0),nrow = 1,ncol = 3)
  }
  else if(test_index[j] == -1){
    initial_state = matrix(c(0,1,0),nrow = 1,ncol = 3)
  }
  else{
    initial_state = matrix(c(0,0,1),nrow = 1,ncol = 3)
  }
  }
  print(mean(prediction_index == test_index))
  print(prediction_index)
  print(apple_transition_matrix)
  print(test_index)
  prediction_markov_list = c(prediction_markov_list, mean(prediction_index == test_index))
