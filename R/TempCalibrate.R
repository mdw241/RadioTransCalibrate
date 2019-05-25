#' Temperature Calibration of Radio Rransmitters.
#'
#' Requires a dataset that contains a column titled "period" and a column titled "temperature".
#' 
#' @param data a dataset that contains a column titled "period" and a column titled "temperature". 
#' @return Will generate  both a linear and quadratic equation for the relationship between temperature (Y variable) and period (X variable). A scatterplot illustrating both models will also be generated. The function anova() is used to compare the fit of both models. A statement will be also be generated that details which model is a better fit, and the p-value for the model comparison test will be provided.
#' @rdname TempCalibrate
#' @export

TempCalibrate <-  function(data) {
  
  plot1<-plot(data$period,data$temperature,xlab="period",ylab="temperature")
  m1<-lm(data$temperature~data$period)
  abline(m1)
  m2<-lm(data$temperature~I(data$period^2)+data$period)
  lines(fitted(m2)~data$period,col="blue")
  legend("topright",lty=c(1,1),c("linear","quadratic"),col=c(1,4),lwd=2)
  a<-anova(m1,m2)    # p<0.05 indicates more complex model (i.e., quadratic)
  (linear<-paste("temp= ",round(m1$coefficients[1],3)," + (",round(m1$coefficients[2],3)," * ","period)"))
  (quadratic<-paste("temp= ",round(m2$coefficients[1],3)," + (",round(m2$coefficients[3],3)," * ","period)"," + (",round(m2$coefficients[2],3)," * ","period^2)"))
  (better_fit<-ifelse(a$`Pr(>F)`[2]<0.05,"QUADRATIC","LINEAR"))
  (model_choice<-paste("the ",better_fit," model fits the data better (P-value= ",round(a$`Pr(>F)`[2],5),")")       )
  l<-list(plot1,linear,quadratic,model_choice)
  return(l)
  
}


