mults_summary <- function(mult.reps,mult.out,base.comp){
  tempdata <- mapply(FUN=function(x){x[,mult.out]},mult.reps)
  tempdata.dif <- mapply(FUN=function(x){x[,mult.out]-base.comp[,mult.out]},mult.reps)
  out.mat <- cbind(mult.reps[[1]][,1:3],t(apply(tempdata,1,function(x){quantile(x,probs=c(0.5,0.025,0.975),na.rm=T)})),
                   apply(tempdata,1,function(x){(sum((x-mean(x,na.rm=T))^3,na.rm=T)/length(x[which(!is.na(x))]))/(sum((x-mean(x,na.rm=T))^2,na.rm=T)/(length(x[which(!is.na(x))])))^1.5}),
                   apply(tempdata,1,function(x){length(x[which(!is.na(x))])*sum((x-mean(x,na.rm=T))^4,na.rm=T)/(sum((x-mean(x,na.rm=T))^2,na.rm=T)^2)-3}),
                   apply(tempdata,1,function(x){sd(x,na.rm=T)/mean(x,na.rm=T)}),
                   apply(tempdata.dif,1,function(x){median(x,na.rm=T)}))
  colnames(out.mat)[4:10] <- c("med","lb","ub","skew","kurt","cv","dif")
  out.mat
}
