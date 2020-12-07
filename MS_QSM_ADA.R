# create empty matrices
{predict_test_rf_class_outer<-NULL
multiResultClass <- function(result1=NULL,result2=NULL,result3=NULL,result4=NULL,
                             result5=NULL,result6=NULL,result7=NULL,result8=NULL){me <- list(
                               result1 = result1,
                               result2 = result2,
                               result3 = result3,
                               result4 = result4,
                               result5 = result5,
                               result6 = result6,
                               result7 = result7,
                               result8 = result8)
                             #Set the name for the class
                             class(me) <- append(class(me),"multiResultClass")
                             return(me)}

predict_all_outer<-NULL;predict_all_connect_all_outer<-NULL;predict_all_ensemble_all_outer<-NULL
varImp_all_outer<-NULL;varImp_SC_FC_all_outer<-NULL;varImp_connect_all_outer<-NULL
auc_outer_outerloop<-NULL; outer_auc_connect_all_outer<-NULL; outer_auc_ensemble_all_outer<-NULL
best_hyper_outer<-NULL; best_hyper_connect_outer<-NULL; best_hyper_ensemble_outer<-NULL
best_threshold_all_outer<-NULL;best_threshold_all_connect_all_outer<-NULL;best_threshold_all_ensemble_all_outer<-NULL
result_ConfMatrix_outer<-NULL;result_ConfMatrix_outer_connect_all_outer<-NULL;result_ConfMatrix_outer_ensemble_all_outer<-NULL
err_cv_outer<-NULL;err_cv_connect_outer<-NULL;err_cv_ensemble_outer<-NULL
dim_data_outer<-NULL;result_ConfMatrix_outer1<-NULL
auc_single_ensemble_outerloop<-NULL
result_ConfMatrix_outer05<-NULL
result_ConfMatrix_outer05_1<-NULL
result_ConfMatrix_outerAUC<-NULL
result_ConfMatrix_outerAUC_1<-NULL
aucpr_outer_outerloop<-NULL;brierscore_allmodels<-NULL;predicted_observed_brier_outerloop<-NULL
mean_pred_ensemble_outer12345_weighted<-NULL;predict_all_ensemble_all_outer_weighted<-NULL
predict_test_rf_confusion_outer<-NULL

OuterKfold<-5;InnerKfold<-5;InnerIterNumber<-5;connecnumberbegin<-2

cor_pred<-NULL
brierscoreouter<-NULL
}

# to compute in parallel
parallelnumber<-10
myCluster <- makeCluster(parallelnumber)
registerDoMC(parallelnumber)

minsplitinterval<-seq(10,50,10)
cpinterval<-c(0.1,0.01,0.001,0.0001)
numberofiter<-50
varImp_all_outer1<-list()

# Start outerloop ####
# Start parallel ####
ADA_results_cross_validation<-foreach(it=rep(1:parallelnumber,10), # 10 cpu x 10 repetition=100 results
.combine = rbind,.multicombine=TRUE,.packages=c("nnet","rminer","caret","AUC","e1071","randomForest")) %dopar% {
                                                                                                                       
for(outerloop in 1:OuterKfold){
  
  # cat(paste("outerloop=",outerloop),"\n")
  
  # Split the data in 10 partitions
  auc_outer_outerloop_onlyforthisouterloop<-NULL
  aucpr_outer_outerloop_onlyforthisouterloop<-NULL
  folds_outerloop<-createFolds(factor(as.data.frame(data_used1[[1]])$Output_class),k=OuterKfold,list = FALSE)  
  # for(model in 1:1){
  for(model in 1:length(data_used1)){
    
    cat(paste("model=",model))
    
    data_used<-data_used1[[model]]
    data_used<-as.data.frame(data_used)
    names1<-names(data_used)
    names(data_used)<-make.names(names1, unique = TRUE, allow_ = TRUE)
    data_used$Output_class<-as.factor(data_used$Output_class) 
    
    # Create a train dataset using 9 partitions over 10                                                           
    trainData <- data_used[folds_outerloop != outerloop, ]
    
    # Create a test dataset using 1 partition over 10                                                           
    testData <- data_used[folds_outerloop == outerloop, ]
    
    err_cv<-NULL
    # Start inner loop ####
    # Repeat CV 5 times for different partitions
    for (iterinner in 1:InnerIterNumber) {

      # Create partitions from train dataset
      folds_outerloop_inner<-createFolds(factor(trainData$Output_class),k=InnerKfold,list = FALSE)
      
      for(innerloop in 1:InnerKfold){
        
        trainingData <- trainData[folds_outerloop_inner != innerloop, ]
        validationData <- trainData[folds_outerloop_inner == innerloop, ]
        
        normParam_training <- preProcess(trainingData,method = c("center", "scale"))
        trainingData <- predict(normParam_training, trainingData)
        validationData <- predict(normParam_training, validationData)
        
        smote_trainingData <- SMOTE(Output_class ~ ., trainingData, perc.over = 100)
        smote_trainingData1<-na.omit(smote_trainingData)
        
        # Inner loop CV for demographic
        
        for (ntree in minsplitinterval) {
          for (mtry in cpinterval) {
            
            # Fit the model using a couple of the hyperparameters
            control1<-rpart.control(minsplit = ntree, minbucket = round(ntree/3), cp = mtry, 
                                    maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
                                    surrogatestyle = 0, maxdepth = 10)
            
            
            innermodel_rf_prob<-  adabag::boosting(Output_class ~ ., smote_trainingData1, boos = TRUE, mfinal = numberofiter, coeflearn = 'Breiman', 
                                                   na.action=na.roughfix,control=control1)
            
            
            # Predict the validation dataset
            predict_validation_rf_prob<-adabag::predict.boosting(innermodel_rf_prob,validationData,type="prob")$prob[,1]
            auc_inner <- ROSE::roc.curve(validationData$Output_class, predict_validation_rf_prob,plotit = FALSE)$auc
            err_cv<-rbind(err_cv,c(iterinner,innerloop,ntree,mtry,auc_inner,model))
            
          }
        }
        
      }
      
    }
    err_cv_outer<-rbind(err_cv_outer,err_cv)
    
    # End inner loop ####
    
    # Best hyperparam ####
    param_median_auc<-NULL
    for(ntreebest in levels(as.factor(err_cv[,3]))){
      for(mtrybest in levels(as.factor(err_cv[,4]))){
        row1<-which(err_cv[,3]==ntreebest)
        row2<-which(err_cv[,4]==mtrybest)
        param_median_auc<-rbind(param_median_auc,c(as.numeric(ntreebest),as.numeric(mtrybest),
                                                   as.numeric(median(err_cv[intersect(row1,row2),5]))))
      }
    }
    best_hyper<-c(param_median_auc[which.max(param_median_auc[,3]),1],
                  param_median_auc[which.max(param_median_auc[,3]),2])
    
    best_hyper_outer<-rbind(best_hyper_outer,best_hyper)
    
    # SMOTE for train dataset ####
    normParam_train <- preProcess(trainData,method = c("center", "scale"))
    trainData <- predict(normParam_train, trainData)
    testData <- predict(normParam_train, testData)
    
    smote_trainData <- SMOTE(Output_class ~ ., trainData, perc.over = 100)
    smote_trainData_new<-na.omit(smote_trainData)
    control1<-rpart.control(minsplit = best_hyper[1], minbucket = round(best_hyper[1]/3), cp = best_hyper[2], 
                            maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
                            surrogatestyle = 0, maxdepth = 10)
    
    outermodel_rf_prob<-  adabag::boosting(Output_class ~ ., smote_trainData_new, boos = TRUE, mfinal = numberofiter, coeflearn = 'Breiman', 
                                           na.action=na.roughfix,control=control1)
    
    # Predict the test dataset
    predict_test_rf_prob<-adabag::predict.boosting(outermodel_rf_prob,testData,type="prob")$prob[,1]
    predict_test_rf_class<-adabag::predict.boosting(outermodel_rf_prob,testData,type="prob")$class
    predict_test_rf_confusion<-adabag::predict.boosting(outermodel_rf_prob,testData,type="prob")$confusion
    
    
    predict_test_rf_class_outer<-list(predict_test_rf_class_outer,cbind(predict_test_rf_class,
                                                                        as.numeric(as.character(testData$Output_class)),
                                                                        rep(model,length(predict_test_rf_class)),
                                                                        rep(outerloop,length(predict_test_rf_class))))
    
    predict_test_rf_confusion_outer<-rbind(predict_test_rf_confusion_outer,c(predict_test_rf_confusion[1],
                                                                             predict_test_rf_confusion[2],
                                                                             predict_test_rf_confusion[3],
                                                                             predict_test_rf_confusion[4]))
    ## var imp demo ####
    varImp_all_outer<-list(varImp_all_outer,as.numeric(outermodel_rf_prob$importance))

    # Predict&Output Demo ####
    predict_all_outer<-rbind(predict_all_outer,cbind(predict_test_rf_prob,
                                                     as.numeric(as.character(testData$Output_class)),
                                                     rep(model,length(predict_test_rf_prob)),
                                                     rep(outerloop,length(predict_test_rf_prob))))   
    
    # AUC for demo
    auc_outer <- ROSE::roc.curve(testData$Output_class, predict_test_rf_prob,plotit = FALSE)$auc
    require(PRROC)
    fg <- predict_test_rf_prob[testData$Output_class == 1]
    bg <- predict_test_rf_prob[testData$Output_class == 0]
    # PR Curve
    auc_pr_outer1 <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)  
    
    fg <- predict_test_rf_prob[testData$Output_class == 0]
    bg <- predict_test_rf_prob[testData$Output_class == 1]
    # PR Curve
    auc_pr_outer2 <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)  
    
    auc_outer_outerloop_onlyforthisouterloop<-rbind(auc_outer_outerloop_onlyforthisouterloop,c(auc_outer,auc_pr_outer1,auc_pr_outer2,model=model,outerloop=outerloop))
    auc_outer_outerloop<-rbind(auc_outer_outerloop,c(auc_outer,auc_pr_outer1,auc_pr_outer2,model))
    
  } ## end of the model loop
  
  # AUC for ensemble models ####
  predict_all_outer0<-predict_all_outer[which(predict_all_outer[,4]==outerloop),]
  
  predict_all_outer1<-predict_all_outer0[which(predict_all_outer0[,3]==1),1] # 
  predict_all_outer2<-predict_all_outer0[which(predict_all_outer0[,3]==2),1] # 
  predict_all_outer3<-predict_all_outer0[which(predict_all_outer0[,3]==3),1] # 

# compute the sensitivity, specificity and balanced accuracy

    threshold_predict_binary_final_threshold05<-matrix(NA,ncol = 1,nrow =length(testData$Output_class))
    for(k in 1:length(testData$Output_class)){

      if(predict_test_rf_prob[k]>=0.5)
        threshold_predict_binary_final_threshold05[k]<-1
      else
        threshold_predict_binary_final_threshold05[k]<-0
    }
    result_ConfMatrix05<-caret::confusionMatrix(as.factor(threshold_predict_binary_final_threshold05),testData$Output_class,positive="1")
    result_ConfMatrix_outer05<-cbind(result_ConfMatrix_outer05,result_ConfMatrix05)

    result_ConfMatrix_outer05_1<-rbind(result_ConfMatrix_outer05_1,c(result_ConfMatrix05$byClass[1:11],model=i,outer=outerloop))


    # Find the threshold using ROC curve
    sens_spe_thre<-cbind(rowMeans(cbind(roc.curve(testData$Output_class, mean_pred_single_ensemble_outer12345[,i],plotit = FALSE)$false.positive.rate,
                                        1-roc.curve(testData$Output_class, mean_pred_single_ensemble_outer12345[,i],plotit = FALSE)$true.positive.rate)),
                         roc.curve(testData$Output_class, mean_pred_single_ensemble_outer12345[,i],plotit = FALSE)$threshold)

    Threshold<-sens_spe_thre[which.max(sens_spe_thre[,1]),2]


    threshold_predict_binary_final_threshold_AUC<-matrix(NA,ncol = 1,nrow =length(testData$Output_class))
    for(k in 1:length(testData$Output_class)){

      if(predict_test_rf_prob[k]>=Threshold)
        threshold_predict_binary_final_threshold_AUC[k]<-1
      else
        threshold_predict_binary_final_threshold_AUC[k]<-0
    }
    result_ConfMatrixAUC<-caret::confusionMatrix(as.factor(threshold_predict_binary_final_threshold_AUC),testData$Output_class,positive="1")
    result_ConfMatrix_outerAUC<-cbind(result_ConfMatrix_outerAUC,result_ConfMatrixAUC)

    result_ConfMatrix_outerAUC_1<-rbind(result_ConfMatrix_outerAUC_1,c(result_ConfMatrixAUC$byClass[1:11],model=i,outer=outerloop))
  
}
  
  # Data dimensions  
  dim_data<-c(dim(data_used)[1],
              dim(data_used)[2],
              length(which(data_used$Output_class==1)),
              length(which(data_used$Output_class==0)),
              length(which(testData$Output_class==1)),
              length(which(testData$Output_class==0)),
              length(which(trainData$Output_class==1)),
              length(which(trainData$Output_class==0)))
  dim_data_outer<-rbind(dim_data_outer,dim_data)
 ## end of the outer loop

# Save the classification results and variable importance in the same list
result <- multiResultClass()
# result$result1 <- list(predict_all_outer,predict_all_ensemble_all_outer,predict_test_rf_class_outer,predict_test_rf_confusion_outer)
result$result1 <- predict_all_outer

result$result2 <-best_threshold_all_outer
result$result3 <- list(auc_outer_outerloop,auc_single_ensemble_outerloop,brierscore_allmodels,predicted_observed_brier_outerloop)
result$result3 <- auc_outer_outerloop
result$result4 <- best_hyper_outer
result$result5 <- dim_data_outer
result$result6 <- list(err_cv_outer)
result$result7 <- varImp_all_outer

return(result) 
}# end of parallel loop
  
# save the results
  save(ADA_results_cross_validation,
       file = "~/ADA_results_cross_validation.RData")
  