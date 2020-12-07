# MS-QSM

You can find the R codes including 
1. the implementation of Adative Boosting (ADA) method in classifying the observations in two classes with a cross-validation technique
2. the codes for the violinplot figure that showes the classification performance of three different models
3. the codes to compute the variable importance of each input variable

ADA method was trained with two loops (outer and inner) of k-fold cross validation (k = 5) to optimize the hyperparameters and test the performance of the method. The outer loop provided a training set (4/5 folds) for model building and test set (1/5 folds) for model assessment. Thus, the training dataset included approximately 80% of patients while the testing dataset included 20% patients. The inner loop (repeated over 5 different partitions of the training dataset only) performed grid-search to find the set of model hyperparameters that maximized AUC for classification. A model was fitted using those optimal hyperparameters on the entire training dataset and assessed on the hold-out test set from the outer loop. The outer loop was repeated for 100 different random partitions of the data. The average test AUC,sensitivity, specificity, and balanced accuracy in the classification analysis (over all 5 folds x 100 iterations = 500 test sets) were calculated to assess the performance of the method.

---

Please contact Ceren Tozlu (cet2005@med.cornell.edu) for any questions about the repository.



