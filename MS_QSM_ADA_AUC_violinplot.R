
auc<- ADA_results_cross_validation[,3]

auc_models3_outer<-NULL
for(i in 1:dim(results)[1]){
  auc_simple_ensemble<-auc[[i]][[2]]
  
  dim(auc_models);head(auc_models)
  
  auc_models1<-auc_models[,-10] # remove the 10. column to write a new one
  numberofmodels<-length(which(auc_models1[,10]==1))
  auc_models2<-cbind(auc_models1,model=rep(1:as.numeric(numberofmodels),5))
  dim(auc_models2)
  
  auc_models3<-NULL
for(model in 1:3){
    auc_models3<-cbind(auc_models3,as.matrix(as.numeric(auc_models2[which(auc_models2[,"model"]==model),1])))
}
auc_models3_outer<-rbind(auc_models3_outer,auc_models3)
}
dim(auc_models3_outer)

# Plot AUC ####
par(mar=c(12,6,6,2.1))
#sets the bottom, left, top and right margins respectively of the plot region in number of lines of text.


library(vioplot)
plot(0,0,type="n",xlim=c(0,4), ylim=c(0.5,1.1), 
       xaxt = 'n',yaxt = 'n', xlab ="", ylab = "AUC",  main ="Baseline disability \n classification task",
     cex.axis=4,cex.main=2,cex.lab=2)

axis(2,at=seq(0,1,0.05),labels=seq(0,1,0.05),lwd.ticks=1,cex.axis=2)
for(hline in seq(0.5,1,0.05)){
  abline(h = hline,lty=2)}

for (i in 1:3) {
  
  vioplot(na.omit(auc_models3_outer[,i]), at = (i), add = T, 
          col=c("green", "blue", "palevioletred")[i],
          border=c("darkolivegreen4", "royalblue4", "violetred4")[i], 
          rectCol=c("darkblue", "darkblue", "palevioletred3")[i], 
          colMed=c("springgreen", "cyan", "magenta")[i]) }

axis(side=1,at=1:3,las=3,
         labels=c("Model I: \n rim- ChaCo","Model II: \n rim+ ChaCo","Model III: \n rim-/+ ChaCo"),cex.axis=2)
