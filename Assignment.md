
> library(reshape2)
> library(plyr)
> data.set<-list()
> data.set$features <- read.table(paste("features.txt", sep="/"), col.names=c('id', 'name'), stringsAsFactors=FALSE)
> data.set$activity_labels <- read.table(paste("activity_labels.txt", sep="/"), col.names=c('id', 'Activity'))
> data.set$test <- cbind(subject=read.table(paste("test", "subject_test.txt", sep="/"), col.names="Subject"),
+ my_y=read.table(paste("test", "y_test.txt", sep="/"), col.names="Activity.ID"),
+ my_x=read.table(paste("test", "x_test.txt", sep="/")))
> data.set$train <- cbind(subject=read.table(paste("train", "subject_train.txt", sep="/"), col.names="Subject"),
+                         my_y=read.table(paste("train", "y_train.txt", sep="/"), col.names="Activity.ID"),
+                         my_x=read.table(paste("train", "X_train.txt", sep="/")))
> rename.features <- function(col) {
+ col <- gsub("tBody", "Time.Body", col)
+ col <- gsub("tGravity", "Time.Gravity", col)
+ 
+ col <- gsub("fBody", "FFT.Body", col)
+ col <- gsub("fGravity", "FFT.Gravity", col)
+ 
+ col <- gsub("\\-mean\\(\\)\\-", ".Mean.", col)
+ col <- gsub("\\-std\\(\\)\\-", ".Std.", col)
+ 
+ col <- gsub("\\-mean\\(\\)", ".Mean", col)
+ col <- gsub("\\-std\\(\\)", ".Std", col)
+ 
+ return(col)
+ }
> merged_file <- rbind(data.set$test, data.set$train)[,c(1, 2, grep("mean\\(|std\\(", data.set$features$name) + 2)]
> names(merged_file)<-c("Subject", "Activity.ID", rename.features(data.set$features$name[grep("mean\\(|std\\(", data.set$features$name)]))
> merged_file<-merge(merged_file, data.set$activity_labels, by.my_x="Activity.ID", by.my_y="id")
> my_merge.mean<-ddply(melt(merged_file, id.vars=c("Subject", "Activity")), .(Subject, Activity), summarise, MeanSamples=mean(value))
> write.csv(my_merge.mean, file = "my_mean.txt",row.names = FALSE)
> write.csv(merged_file, file = "merged files.txt",row.names = FALSE)
