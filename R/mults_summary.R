mults_summary <- function(mult.reps,mult.out,base.comp){
  tempdata <- mapply(FUN=function(x){x[,mult.out]},mult.reps)
  tempdata.dif <- mapply(FUN=function(x){x[,mult.out]-base.comp[,mult.out]},mult.reps)
  out.mat <- cbind(mult.reps[[1]][,1:3],t(apply(tempdata,1,function(x){quantile(x,probs=c(0.5,0.025,0.975))})),
                   apply(tempdata,1,function(x){(sum((x-mean(x))^3)/length(x))/(sum((x-mean(x))^2)/(length(x)))^1.5}),
                   apply(tempdata,1,function(x){length(x)*sum((x-mean(x))^4)/(sum((x-mean(x))^2)^2)-3}),
                   apply(tempdata,1,function(x){sd(x)/mean(x)}),
                   apply(tempdata.dif,1,function(x){median(x)}))
  colnames(out.mat)[4:10] <- c("med","lb","ub","skew","kurt","cv","dif")
  out.mat
}
