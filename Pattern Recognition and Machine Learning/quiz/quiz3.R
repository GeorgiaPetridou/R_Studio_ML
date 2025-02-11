Class = c(1, 1, 0, 0, 1, 1, 0, 0, 1, 0)
 P_M1 = c(0.73, 0.69, 0.44, 0.55, 0.67, 0.47, 0.08, 0.15, 0.45, 0.35)
 P_M2 = c(0.61, 0.03, 0.68, 0.31, 0.45, 0.09, 0.38, 0.05, 0.01, 0.04)
 data = data.frame(Class, P_M1, P_M2)
 
 TP=sum((P_M1>0.5 & Class==1))
 FN=sum((P_M1<0.5 &Class==1))
 TPR=TP/(TP+FN)
 
 pred=ifelse(P_M1>0.5, 1, 0)
 ConfusionMatrix(Class, pred)
 
 TP=sum((P_M2>0.5 & Class==1))
 FN=sum((P_M2<0.5 &Class==1))
 
 pred=ifelse(P_M2>0.5, 1, 0)
 ConfusionMatrix(Class, pred)
 F1_Score(Class, pred,1)

 pred_obj = prediction(data[,2], Class,label.ordering = c("0", "1")) 
 ROCcurve <- performance(pred_obj, "tpr", "fpr")
 plot(ROCcurve, col = "blue")
 abline(0,1, col = "grey") 
 performance(pred_obj, "auc")
 unlist(performance(pred_obj, "auc")@y.values[1])
 
 
 pred_obj = prediction(data[,3], Class,label.ordering = c("0", "1")) 
 ROCcurve <- performance(pred_obj, "tpr", "fpr")
 plot(ROCcurve, col = "blue")
 abline(0,1, col = "grey") 
 performance(pred_obj, "auc")
 unlist(performance(pred_obj, "auc")@y.values[1])
 