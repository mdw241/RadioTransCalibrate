#' Batch Radio Transmitter Calibratiion.
#' 
#' Calibrates one or more radio transmitters. Requires a dataset with 5 columns: "transmitter", "duration", "pulses", "period", and "temperature. BatchCalibrate() will generate a file named "batch_calibration.csv" with 1 row for each transmitter. For each transmitter, a linear equation, a quadratic equation, a statement that details which model is a better fit, and the p-value for the model comparison test will be provided. The function anova() is used as the model comparison test. The "batch_calibration.csv" file will be saved to the current working directory. Within the working directory must also exist a folder titled "calibration_images". A jpeg file of a scatterplot (temperature~period) for each transmitter with both linear and quadratic regression lines will be saved within the "calibration_images" folder.
#' 
#' @param data a dataset with 5 columns: "transmitter", "duration", "pulses", "period", and "temperature.
#' @return Will generate a table with both a linear and quadratic equation for the relationship between temperature (Y variable) and period (X variable). A scatterplot illustrating both models will also be generated and saved. The function anova() is used to compare the fit of both models. A statement will be also be generated that details which model is a better fit, and the p-value for the model comparison test will be provided.
#' @rdname BatchCalibrate
#' @export
#' 
BatchCalibrate <-  function(data) {
  # check datafile construction
  (colerror<-ifelse(ncol(data)==5,
                    "correct number of columns",
                    stop("number of columns is not divisible by 5")))
  (data$transmitter<-as.factor(data$transmitter))
  (split_datasets<-split(data,data$transmitter)  )

  for(split_data in split_datasets) {
      m1<-lm(split_data$temperature~split_data$period)
      m2<-lm(split_data$temperature~I(split_data$period^2)+split_data$period)
      a<-anova(m1,m2)    # p<0.05 indicates more complex model (i.e., quadratic)
      (linear<-paste("Linear: ","temp= ",round(m1$coefficients[1],3)," + (",round(m1$coefficients[2],3)," * ","period)"))
      (quadratic<-paste("Quadratic: ","temp= ",round(m2$coefficients[1],3)," + (",round(m2$coefficients[3],3)," * ","period)"," + (",round(m2$coefficients[2],3)," * ","period^2)"))
      (better_fit<-ifelse(a$`Pr(>F)`[2]<0.05,"QUADRATIC","LINEAR"))
      (model_choice<-paste("the ",better_fit," model fits the data better (P-value= ",round(a$`Pr(>F)`[2],5),")")       )
      
      
     
      freq<-split_data$transmitter[1]  
  
      batch_calibration<- data.frame(
        frequency = freq,
        linear_eqn<-linear,
        quadratic_eqn<-quadratic,
        best_model<-model_choice)
      
      write.table(batch_calibration, 
                  "batch_calibration.csv",
                  append = TRUE,
                  sep = ",",
                  row.names = FALSE,
                  col.names =  F)
     
    
    mypath <- file.path("calibration_images",paste(freq,"_calibration", ".jpg", sep = ""))    
    jpeg(file=mypath)
    mytitle = paste(freq)
    
    plot(split_data$period,split_data$temperature,xlab="period",ylab="temperature",main=freq)
    abline(m1)
    lines(fitted(m2)~split_data$period,col="blue")
    legend("topright",lty=c(1,1),c("linear","quadratic"),col=c(1,4,1,1),lwd=2)
    dev.off()
  }
}  


