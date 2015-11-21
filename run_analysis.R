run_analysis <- function() {
  
  features <- file(".\\features.txt")
  d <- read.csv(features,sep=" ",header=FALSE, as.is=TRUE)
  col_names <- d$V2
  ## col_names <- gsub("\\(\\)","",col_names)
  
  
  mean_idx  <-  grep("mean\\(\\)", col_names)
 # mean_names <- gsub("\\(\\)","",mean_names)
  
  
  std_idx    <- grep("std\\(\\)", col_names)
#  std_names  <- gsub("\\(\\)","",std_names)
  selection_columns <- c(mean_idx,std_idx)
  col_names <- col_names[selection_columns]
  new_names <- gsub("\\(\\)","",col_names)
 
  
  x_test        <- file(".\\test\\X_test.txt")
  x_train       <- file(".\\train\\X_train.txt")
  
  subject_test  <- file(".\\test\\subject_test.txt") 
  subject_train <- file(".\\train\\subject_train.txt")
  
  y_test       <- file(".\\test\\y_test.txt") 
  y_train      <- file(".\\train\\y_train.txt") 
  
  x_test_frame  <- read.csv(x_test,header=FALSE,sep= "")
  x_test_frame  <- x_test_frame[,selection_columns]
  x_test_frame$sequence <- c(seq(nrow(x_test_frame)))
  names(x_test_frame) <- c(new_names,"sequence")
  
#  print(dim(x_test_frame))
#  print(sum(complete.cases(x_test_frame)))
  
  
  x_train_frame  <- read.csv(x_train,header=FALSE,sep= "")
  x_train_frame  <- x_train_frame[,selection_columns]
  x_train_frame$sequence <- c(seq(nrow(x_train_frame)))
  names(x_train_frame) <- c(new_names,"sequence")
  
#  print(dim(x_train_frame))
#  print(sum(complete.cases(x_train_frame)))
  
  col_names <- c("subject_no")
  subject_test_frame <- read.csv(subject_test,header=FALSE,sep= "",col.names = col_names)
  subject_test_frame$sequence <- c(seq(nrow(subject_test_frame)))
  
  subject_train_frame <- read.csv(subject_train,header=FALSE,sep= "",col.names = col_names)
  subject_train_frame$sequence <- c(seq(nrow(subject_train_frame)))
  
  col_names <- c("activity_no")
  y_test_frame <- read.csv(y_test,header=FALSE,sep= "",col.names = col_names)
  y_test_frame$sequence <- c(seq(nrow(y_test_frame)))
  
  y_train_frame <- read.csv(y_train,header=FALSE,sep= "",col.names = col_names)
  y_train_frame$sequence <- c(seq(nrow(y_train_frame)))
  
  activity_labels <- file(".\\activity_labels.txt")
  col_names <- c("activity_no","activity_txt")
  
  activity_labels_frame <- read.csv(activity_labels,header=FALSE,sep= "",col.names = col_names)
  
  # print(head(activity_labels_frame))
  
  y_test_with_act <- merge(y_test_frame,activity_labels_frame,by=c("activity_no"))
  

  y_act_sub <- merge(y_test_with_act,subject_test_frame,by=c("sequence"))
  

  test_all <- merge(y_act_sub,x_test_frame,by=c("sequence"))    #### ?????
  
  y_train_with_act <- merge(y_train_frame,activity_labels_frame,by=c("activity_no"))
  
  y_act_sub <- merge(y_train_with_act,subject_train_frame,by=c("sequence"))
  
  train_all <- merge(y_act_sub,x_train_frame,by=c("sequence"))
  
  
 # print(head(test_all,1))
#  print(head(train_all,1))
  
  all_measurements <- rbind(train_all,test_all)
  
  all_measurements$sequence <- NULL
  all_measurements$activity_no <- NULL
  
  write.table(all_measurements,file = ".\\output4.txt")
  
  wynik <- aggregate(x=all_measurements[,-(1:2)],by = list(all_measurements$subject_no,all_measurements$activity_txt),FUN="mean")
  write.table(wynik,file = ".\\output5.txt",row.names=FALSE)
  return(wynik)
  
}