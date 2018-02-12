## ------------------------------------------------------------------------
## Simulation for active question answering in multi-class prediction.
## Cong Liu.
## Last updated: 01/26/2018.
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
## load packages.
## ------------------------------------------------------------------------

library("dplyr")    # for some data preperation.
library("entropy")  # calculate entropy.
library("caret")    # for machine learning in R.
library("xgboost")  # using xgboost to train.
library("tidyr")    # data wrangling.
library("ggplot2")  # plot.

## ------------------------------------------------------------------------
## function definition.
## ------------------------------------------------------------------------

softmax <- function(W,x){
  # define softmax function.
  # W: weight matrix
  # x: sample matrix
  # z: vector of net input.
  z = W %*% x
  e_z = exp(z)
  sum_e_z = sum(e_z)
  
  # p: vector of probability for each class
  p = e_z/sum_e_z
  return (p)
}

testEval <- function(predict_prob,true_class,top_n){
  # define testing evaluation methods.
  # predict_prob: testing_num * K matrix
  # true_class: testing_num vector
  # top_n: count true postitive if trueClass ranked topN in predict_prob.
  # a vector or a number.
  
  for_test <- cbind(true_class,predict_prob)
  position_for_true <- apply(for_test,MARGIN = 1, function(x) {
    which(names(sort(x[-1],decreasing = T)) %in% 
            paste("X",as.character(unlist(x[1])),sep="")
    )
  })
  # Add na.rm=TRUE in case the testing class is not covered in training, though it is unlikely.
  top_rate <- function(x) sum(as.numeric(position_for_true) <= x,na.rm = TRUE)/length(position_for_true)
  return (data.frame(top_n=top_n,rate=mapply(top_rate,top_n)))
}

maskUpdator <- function(mask,feature_sort,size=NULL,ans_rate){
  # define method to update mask.
  # mask: a matrix num_test*P containig 1 or NA.
  # feature: a vector containing the sorted features.
  # ans_rate: a scalar indicating the ratio of NA should be filled for each sample.
  # size: a scalar indicating the number of NA should be filled.
  
  if(is.null(dim(mask))){
    # update mask for individual.
    mask = matrix(mask,nrow = 1)
  }
  if (is.null(size)){
    mask_after <- mask
    mask_after <- apply(mask_after, MARGIN = 1, function(x){
      y = x
      na_index = which(is.na(x))
      size = ceiling(sum(is.na(x)) * ans_rate)
      na_sort = na_index[order(match(na_index,feature_sort))]
      num_ans_index = na_sort[1:size]
      y[num_ans_index] = 1
      return(y)
    }) %>% t()
  }else{
    mask_after <- mask
    mask_after <- apply(mask_after, MARGIN = 1, function(x){
      y = x
      na_index = which(is.na(x))
      na_sort = na_index[order(match(na_index,feature_sort))]
      num_ans_index = na_sort[1:size]
      y[num_ans_index] = 1
      return(y)
    }) %>% t()
  }
}

entropyHelper <- function(x, unit = "log2") {
  # methods from FSelector package.
  # x: a vector containing the class label.
  # unit: "log2" or other entropy function related log type.
  
  return(entropy(table(x, useNA="always"), unit = unit))
}

infoGain <- function(data,unit = "log2"){
  # method modified from FSelector package.
  # data: a data frame. each row is a sample, with first column as lables.
  # unit "log2" or other entropy function compatible log type.
  attr_entropies = sapply(data, entropyHelper, "log2")
  class_entropy = attr_entropies[1]
  attr_entropies = attr_entropies[-1]
  joint_entropies = sapply(data[-1], function(t) {
    entropyHelper(data.frame(cbind(data[[1]], t)), "log2")
  })
  results = class_entropy + attr_entropies - joint_entropies
  return (results)
}

reduceTrain <- function(train_data, test_prediction,K_reduced){
  # define method to reduce the training classes.
  # train_data: a data frame. Original train_data, the first_colname should be train_label.
  # test_prediction: a vector contain test features.
  # K_reduced: a scalar. number of reduced classes.
  
  reduced_class = order(test_prediction,decreasing = TRUE)[1:K_reduced] - 1
  train_index_2 = which(train_data$train_label %in% reduced_class)
  train_reduced = train_data[train_index_2,]
  return (train_reduced)
}

BigSetSimulate <- function(N,P,K,missing_rate,num_train, entropy_threshold = 2){
  # simulation for test, train, xgb_model, mask and numberOfClasses.
  e = -1
  parameter <- list(N=N,P=P,K=K,missing_rate=missing_rate,num_train=num_train)
  while(e < entropy_threshold){
    complete_X = replicate(N,rbinom(n = P,size = 1,prob = 0.5)) # P*N
    W = replicate(P,runif(min = -0.5,max = 0.5,n = K)) # K*P
    prob = apply(X = complete_X,MARGIN = 2,softmax,W=W)
    y = apply(prob,MARGIN = 2,function(x) sample(x = (1:K),size = 1,prob = x))
    # hard-encode y for xgboost
    y = as.factor(paste("c",y,sep = ""))
    
    e = entropy(table(y))
  }
  
  dat <- data.frame(Class=y,t(complete_X))
  dat$Class <- as.numeric(dat$Class)
  dat <- dat %>% mutate(Class = Class - 1)
  # Make split index
  train_index <- sample(1:nrow(dat), num_train)
  # Full data set
  data_variables <- data.matrix(data.frame(lapply(dat[,-1], as.numeric)))
  data_label <- dat[,"Class"]
  data_matrix <- xgb.DMatrix(data = as.matrix(dat), label = data_label)
  # split train data and make xgb.DMatrix
  train_data   <- data_variables[train_index,]
  train_label  <- data_label[train_index]
  train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
  train_origin <- data.frame(train_label,train_data)
  # split test data and make xgb.DMatrix
  test_data  <- data_variables[-train_index,]
  test_label <- data_label[-train_index]
  # add na by applying a mask.
  mask = replicate(n = num_train,sample(x = c(NA,1),size = P,replace = T,prob = c(missing_rate,1 - missing_rate))) %>% t()
  # train the model.
  number_of_classes <- length(unique(data_label))
  xgb_params <- list("objective" = "multi:softprob",
                     "eval_metric" = "mlogloss",
                     "num_class" = number_of_classes)
  nround    <- 50 # number of XGBoost rounds
  bst_model <- xgb.train(params = xgb_params,
                         data = train_matrix,
                         nrounds = nround)
  #importance_matrix = xgb.importance(model = bst_model)
  big_set = list(train_origin = train_origin,
                 test_label = test_label,
                 test_data = test_data,
                 mask = mask,
                 bst_model = bst_model,
                 number_of_classes = number_of_classes,
                 parameter = parameter)
  return(big_set)
}

xgbPredict <- function(bst_model, test_label, test_data, number_of_classes){
  # define method for xgb based prediction.
  test_matrix <- xgb.DMatrix(data = test_data, label = test_label)
  test_pred <- predict(bst_model, newdata = test_matrix)
  test_prediction <- matrix(test_pred, nrow = number_of_classes,
                              ncol=length(test_pred)/number_of_classes) %>%
    t() %>%
    data.frame()
  return(test_prediction)
}

method1Na <- function(big_set){
  # define simulate for 1-NA.
  test_label = big_set$test_label
  mask = big_set$mask
  test_data = big_set$test_data * mask
  bst_model = big_set$bst_model
  number_of_classes = big_set$number_of_classes
  test_prediction <- xgbPredict(bst_model = bst_model, test_data = test_data, test_label = test_label, number_of_classes = number_of_classes)
  return(list(test_prediction=test_prediction,mask=mask))
}

method2Rand <- function(big_set, ans_rate = 0.5){
  # define simulate for 2-rand
  test_label = big_set$test_label
  mask = big_set$mask
  feature_random_sort <- sample(1:big_set$parameter$P,size = P,replace = F)
  mask <- maskUpdator(mask = mask,feature_sort = feature_random_sort,ans_rate=ans_rate)
  test_data = big_set$test_data * mask
  bst_model = big_set$bst_model
  number_of_classes = big_set$number_of_classes
  test_prediction <- xgb_predict(bst_model = bst_model, test_data = test_data, test_label = test_label, number_of_classes = number_of_classes)
  return(list(test_prediction=test_prediction,mask=mask))
}

method3Glb <- function(big_set, ans_rate = 0.5){
  # define simulate for 3-global
  train_origin <- big_set$train_origin
  test_label = big_set$test_label
  mask = big_set$mask
  feature_importance <- infoGain(train_origin)
  feature_complete_sort <- order(feature_importance, decreasing = TRUE)
  mask <- maskUpdator(mask = mask, feature_sort = feature_complete_sort, ans_rate=ans_rate)
  test_data = big_set$test_data * mask
  bst_model = big_set$bst_model
  number_of_classes = big_set$number_of_classes
  test_prediction <- xgb_predict(bst_model = bst_model, test_data = test_data, test_label = test_label, number_of_classes = number_of_classes)
  return(list(test_prediction=test_prediction,mask=mask))
}

method4Cust <- function(big_set, reduce_ratio_1shot = 0.8, ans_rate = 0.5){
  # define simulate for 4-cust.
  train_origin <- big_set$train_origin
  res <- method1Na(big_set)
  test_prediction_init <- res$test_prediction 
  K_reduced = ceiling(reduce_ratio_1shot * big_set$parameter$K)
  mask = big_set$mask
  mask_after_qa_cust = mask
  for(i in 1:dim(test_prediction_init)[1]){
    size = ceiling(sum(is.na(mask[i,])) * ans_rate)
    test_prediction = test_prediction_init[i,]
    reduced_train_data <- reduceTrain(train_data = train_origin, test_prediction = test_prediction,K_reduced = K_reduced)
    new_feature_importance <- infoGain(reduced_train_data)
    feature_cust_sort <- order(new_feature_importance,decreasing = TRUE)
    # update mask.
    mask_after_qa_cust[i,] = maskUpdator(mask = mask[i,],feature_sort = feature_cust_sort,size = size)
  }
  
  test_label = big_set$test_label
  mask = big_set$mask
  mask <- mask_after_qa_cust
  test_data = big_set$test_data * mask
  bst_model = big_set$bst_model
  number_of_classes = big_set$number_of_classes
  test_prediction <- xgb_predict(bst_model = bst_model, test_data = test_data, test_label = test_label, number_of_classes = number_of_classes)
  return(list(test_prediction=test_prediction,mask=mask))
}

method5Dyn <- function(big_set, reduce_ratio_step = 0.9, ans_rate = 0.5, update_size = 3){
  # define simulate for 5-dynam.
  train_origin <- big_set$train_origin
  res <- method1Na(big_set)
  test_prediction_init <- res$test_prediction
  test_label = big_set$test_label
  test_prediction <- test_prediction_init
  
  K_reduced = big_set$parameter$K
  mask = big_set$mask
  test_data = big_set$test_data
  mask_after_qa_sw = mask
  feature_importance <- infoGain(train_origin)
  feature_complete_sort <- order(feature_importance, decreasing = TRUE)
  
  for(i in 1:dim(test_prediction_init)[1]){
    size = ceiling(sum(is.na(mask[i,])) * ans_rate)
    step_size = update_size
    # init.
    # init the remaining size of NA to fill out. 
    size_working = size
    # init the mask.
    mask_working = mask_after_qa_sw[i,]
    # init the class number.
    K_working = K
    # init the training set
    training_working = train_origin
    # init the fature importance vector
    feature_sort_working = feature_complete_sort
    # init the test data.
    test_data_working = test_data[i,]
    # init the pred result.
    pred_result_working = test_prediction_init[i,]
    # init test label. It won't change.
    test_label_working = test_label[i]
    
    while(size_working > 0){
      
      # update size
      size_working = size_working - step_size
      # update training set.
      training_working = reduceTrain(training_working,pred_result_working,K_working)
      # update mask
      feature_sort_tmp = infoGain(training_working)
      feature_order_tmp <- order(feature_sort_tmp,decreasing = TRUE)
      if(size_working < 0){
        step_size = update_size + size_working
      }
      mask_working <- maskUpdator(mask = mask_working,feature_sort = feature_order_tmp,size = step_size)
      # update test data
      test_data_working <- mask_working * test_data[i,]
      # update prediction result
      bst_model = big_set$bst_model
      number_of_classes = big_set$number_of_classes
      pred_result_working <- xgb_predict(bst_model = bst_model, test_data = test_data_working, test_label = test_label_working, number_of_classes = number_of_classes)
      # update class number. Using floor() to make sure the update is moving.
      K_working = floor(K_working * reduce_ratio_step)
      
    }
    # summarize the result for one sample.
    mask_after_qa_sw[i,] = mask_working
    test_prediction[i,] = pred_result_working
  }
  return(list(test_prediction=test_prediction,mask=mask_after_qa_sw))
}

method6Comp <- function(big_set){
  # define simulate for 6-complete.
  test_label = big_set$test_label
  test_data = big_set$test_data
  bst_model = big_set$bst_model
  number_of_classes = big_set$number_of_classes
  test_prediction <- xgbPredict(bst_model = bst_model, test_data = test_data, test_label = test_label, number_of_classes = number_of_classes)
  return(list(test_prediction=test_prediction,mask=NA))
}

## ------------------------------------------------------------------------
## parameter setting.
## ------------------------------------------------------------------------

N = 1000 # number of samples.
num_train = ceiling(N/2) # number of training samples.
P = 50 # number of features.
K = 10 # number of classes.
missing_rate = 0.5 # ratio of NAs.
ans_rate = 0.5 # ratio of NAs to be completed. e.g. if 10 features are missing. then 5 questions could be answered.
reduce_ratio_1shot = 0.8
reduce_ratio_step = 0.9
update_size = 3
simulate_result_dir = "~/Project/phenolyzer_QA"
setwd(simulate_result_dir)
## ------------------------------------------------------------------------
## simulation control
## ------------------------------------------------------------------------

for(i in 1:100){
  print(i)
  set.seed(i)
  big_set <- BigSetSimulate(N = N, P = P, K = K, missing_rate = missing_rate, num_train = num_train, entropy_threshold = 2)
  result_6Comp <- method6Comp(big_set = big_set)
  result_1Na <- method1Na(big_set = big_set)
  result_2Rand <- method2Rand(big_set = big_set,ans_rate = ans_rate)
  result_3Glb <- method3Glb(big_set = big_set,ans_rate = ans_rate)
  result_4Cust <- method4Cust(big_set = big_set, reduce_ratio_1shot = reduce_ratio_1shot, ans_rate = ans_rate)
  result_5Dyn <- method5Dyn(big_set = big_set,reduce_ratio_step = reduce_ratio_step, ans_rate = ans_rate, update_size = update_size)
  
  top_n <- c(1,2,3,4,5)
  
  eval_matrix_1 <- testEval(result_6Comp$test_prediction, true_class = big_set$test_label + 1,top_n)
  eval_matrix_2 <- testEval(result_1Na$test_prediction, true_class = big_set$test_label + 1,top_n)
  eval_matrix_3 <- testEval(result_2Rand$test_prediction, true_class = big_set$test_label + 1,top_n)
  eval_matrix_4 <- testEval(result_3Glb$test_prediction, true_class = big_set$test_label + 1,top_n)
  eval_matrix_5 <- testEval(result_4Cust$test_prediction, true_class = big_set$test_label + 1,top_n)
  eval_matrix_6 <- testEval(result_5Dyn$test_prediction, true_class = big_set$test_label + 1,top_n)
  df <- data.frame(eval_matrix_1,eval_matrix_2[,-1],eval_matrix_3[,-1],
                   eval_matrix_4[,-1],eval_matrix_5[,-1],eval_matrix_6[,-1])
  colnames(df)[-1] <- c("6-complete","1-NA","2-rand","3-global","4-cust","5-dynam")
  file_name = paste("simu",i,"txt",sep = ".")
  write.table(df,file = file_name,sep = "\t",row.names = F,col.names = TRUE)
}  
  

## ------------------------------------------------------------------------

# plot results.
df = NULL
for(i in 1:100){
  file_name = paste("simu",i,"txt",sep = ".")
  dt = read.table(file = file_name,header = TRUE,sep = "\t")
  colnames(dt)[-1] <- c("6-complete","1-NA","2-rand","3-global","4-cust","5-dynam")
  dt = gather(dt, "class", "rate", 2:7)
  df = rbind(df,dt)
}
df %>% 
  ggplot(aes(x = as.factor(top_n),y = rate,fill=class)) +
  geom_boxplot() +
  theme_bw() + scale_x_discrete(limits = c(1,2,3,4,5)) +
  ggtitle("Comparison between different methods") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Top N") + 
  ylab("Percentage of Cases (500) within Rank %")