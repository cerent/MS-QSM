# variable importance ####

data_used_bas<-list(cbind(Output_class=as.factor(data_demo$Output_class),data_ChaCoT2minusRimpos,data_demo[,c("Age","Gender","Race","Treatment.Duration","Disease.Duration")])
                    ,cbind(Output_class=as.factor(data_demo$Output_class),data_ChaCoRimpos,data_demo[,c("Age","Gender","Race","Treatment.Duration","Disease.Duration")])
                    ,cbind(Output_class=as.factor(data_demo$Output_class),data_ChaCoT2minusRimpos,data_ChaCoRimpos,data_demo[,c("Age","Gender","Race","Treatment.Duration","Disease.Duration")])
)

results<-ADA_results_cross_validation
 # for longitudinal
  
  varimp_ADA_concat_t2_rim_demo<-NULL;varimp_ADA_single_demo<-NULL;varimp_ADA_single_t2<-NULL;varimp_ADA_single_rim<-NULL;varimp_ADA_concat_t2_rim<-NULL;varimp_ADA_concat_rim<-NULL;varimp_ADA_concat_t2<-NULL
  for(i in 1:(dim(results)[1])){
    
     varimp_ADA_concat_t2<-rbind(varimp_ADA_concat_t2,                   rbind(results[i,8][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[2]],
                                                                              results[i,8][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[2]],
                                                                              results[i,8][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[2]],
                                                                              results[i,8][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[2]],
                                                                              results[i,8][[1]][[1]][[1]][[2]]))
    
    varimp_ADA_concat_rim<-rbind(varimp_ADA_concat_rim,                 rbind(results[i,8][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[2]],
                                                                              results[i,8][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[2]],
                                                                              results[i,8][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[2]],
                                                                              results[i,8][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[2]],
                                                                              results[i,8][[1]][[1]][[2]]))
    
    varimp_ADA_concat_t2_rim_demo<-rbind(varimp_ADA_concat_t2_rim_demo, rbind(results[i,8][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[2]],
                                                                              results[i,8][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[2]],
                                                                              results[i,8][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[2]],
                                                                              results[i,8][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[1]][[2]],
                                                                              results[i,8][[1]][[2]]))
  }

dim(varimp_ADA_single_demo)

# VAR IMP - Concatenated Model with T2 ####
dim(varimp_ADA_concat_t2)
par(mfrow=c(1,1))
par(mar=c(10,4.1,2,2.1))

numberofchosen<-NULL
for(i in 1:(dim(varimp_ADA_concat_t2)[2])){
  numberofchosen<-rbind(numberofchosen,length(which(varimp_ADA_concat_t2[,i]!=0)))
}
plot(1:(dim(varimp_ADA_concat_t2)[2]),numberofchosen)

varimp_ADA_concat_t2_quantiles<-NULL
for(i in 1:(dim(varimp_ADA_concat_t2)[2])){
  varimp_ADA_concat_t2_quantiles<-rbind(varimp_ADA_concat_t2_quantiles,summary(varimp_ADA_concat_t2[,i])[5])
  
}
dim(varimp_ADA_concat_t2_quantiles)


# BASELINE
varimp_ADA_concat_t2_colMeans<-varimp_ADA_concat_t2_quantiles
varimp_ADA_concat_t2_colMeans_names<-data.frame(varimp_ADA_concat_t2_colMeans,names(data_used_bas[[1]])[-1]) # BAS
varimp_ADA_concat_t2_colMeans_names_86regions<-varimp_ADA_concat_t2_colMeans_names[1:86,]
varimp_ADA_concat_t2_colMeans_names_86regions_BASELINE<-varimp_ADA_concat_t2_colMeans_names_86regions[,1]/max(varimp_ADA_concat_t2_colMeans_names_86regions[,1])

# VAR IMP - Concatenated Model 2 with rim ####

numberofchosen<-NULL
for(i in 1:(dim(varimp_ADA_concat_rim)[2])){
  numberofchosen<-rbind(numberofchosen,length(which(varimp_ADA_concat_rim[,i]!=0)))
}
plot(1:(dim(varimp_ADA_concat_rim)[2]),numberofchosen)

varimp_ADA_concat_rim_3rdquantile<-NULL;varimp_ADA_concat_rim_3rdquantile_nonzero<-NULL;varimp_ADA_concat_rim_3rdquantile_dividedbymax<-NULL
for(i in 1:(dim(varimp_ADA_concat_rim)[2])){
  varimp_ADA_concat_rim_3rdquantile<-rbind(varimp_ADA_concat_rim_3rdquantile,summary(varimp_ADA_concat_rim[,i])[5])
  varimp_ADA_concat_rim_3rdquantile_nonzero<-rbind(varimp_ADA_concat_rim_3rdquantile_nonzero,summary(varimp_ADA_concat_rim[which(varimp_ADA_concat_rim[,i]!=0),i])[5])
  varimp_ADA_concat_rim_3rdquantile_dividedbymax<-rbind(varimp_ADA_concat_rim_3rdquantile_dividedbymax,summary((varimp_ADA_concat_rim[,i]/max(varimp_ADA_concat_rim[,i])))[5])
}
data.frame(varimp_ADA_concat_rim_3rdquantile,varimp_ADA_concat_rim_3rdquantile_nonzero,varimp_ADA_concat_rim_3rdquantile_dividedbymax)
par(mfrow=c(1,2))

varimp_ADA_concat_rim_colMeans_names<-varimp_ADA_concat_rim_3rdquantile
varimp_ADA_concat_rim_colMeans_names<-data.frame(varimp_ADA_concat_rim_colMeans,names(data_used_bas[[2]])[-1])
varimp_ADA_concat_rim_colMeans_names_86regions_BASELINE<-varimp_ADA_concat_rim_colMeans_names[1:86,1]/max(varimp_ADA_concat_rim_colMeans_names[1:86,1])


# VAR IMP - Concatenated Model 4 with t2, rim and demo 
varimp_ADA_concat_t2_rim_demo_colMeans<-NULL
for(i in 1:(dim(varimp_ADA_concat_t2_rim_demo)[2])){
  varimp_ADA_concat_t2_rim_demo_colMeans<-rbind(varimp_ADA_concat_t2_rim_demo_colMeans,summary(varimp_ADA_concat_t2_rim_demo[,i])[5])
}

## baseline
data_used_forvarimp_crosssectional<-cbind(data_ChaCoT2minusRimpos,data_ChaCoRimpos,data_demo[,c("Age","Gender","Race","Treatment.Duration","Disease.Duration")])
names1<-names(data_used_forvarimp_crosssectional) 
varimp_ADA_concat_t2_rim_demo_colMeans_names<-data.frame(varimp_ADA_concat_t2_rim_demo_colMeans,names=make.names(names1, unique = TRUE, allow_ = TRUE))
varimp_ADA_concat_t2_rim_demo_colMeans_names[order(varimp_ADA_concat_t2_rim_demo_colMeans_names[,1],decreasing=TRUE),]
max<-max(varimp_ADA_concat_t2_rim_demo_colMeans_names[1:86,1],varimp_ADA_concat_t2_rim_demo_colMeans_names[87:172,1])
#T2
varimp_ADA_concmodel7_T2_colMedians_relative_BASELINE<-varimp_ADA_concat_t2_rim_demo_colMeans[1:86]/max
# rim
varimp_ADA_concmodel7_RIM_colMedians_relative_BASELINE<-varimp_ADA_concat_t2_rim_demo_colMeans[87:172]/max
