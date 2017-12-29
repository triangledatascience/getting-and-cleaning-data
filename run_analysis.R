
#Getting and Cleaning Data Course Project

####################
#Part 0. Reads data from downloaded text files into data frames.
####################

filename_x_train = "X_train.txt"
filename_y_train = "y_train.txt"
filename_subject_train = "subject_train.txt"
filename_x_test = "X_test.txt"
filename_y_test = "y_test.txt"
filename_subject_test = "subject_test.txt"

df_x_train = read.table(paste0("./train/",filename_x_train))
df_y_train = read.table(paste0("./train/",filename_y_train))
df_subject_train = read.table(paste0("./train/",filename_subject_train))
df_x_test = read.table(paste0("./test/",filename_x_test))
df_y_test = read.table(paste0("./test/",filename_y_test))
df_subject_test = read.table(paste0("./test/",filename_subject_test))

####################
#Part 1. Merges the training and the test sets to create one data set.
####################

df_train = cbind(df_x_train, df_y_train, df_subject_train)
df_test = cbind(df_x_test, df_y_test, df_subject_test)
df = rbind(df_train,df_test)

####################
#Part 2. Extracts only the measurements on the mean and standard deviation 
#        for each measurement.
####################

filename_features = "features.txt"
names_x = read.table(filename_features)

loc_mean = grep("mean()", names_x[[2]])
loc_std = grep("std()", names_x[[2]])
loc_extracts = sort(c(loc_mean, loc_std, dim(df)[2]-1, dim(df)[2]))

df = df[,loc_extracts]

####################
#Part 3. Uses descriptive activity names to name the activities in the data set
####################

filename_labels = "activity_labels.txt"
labels_y = read.table(filename_labels)

df_labels_y = data.frame(act_num = labels_y[[1]], act_label = labels_y[[2]])

df = merge(x=df, y=df_labels_y
           , by.x=names(df)[length(names(df))-1], by.y = "act_num"
           , all.x = TRUE)
df = subset(df, select=-1)

####################
#Part 4. Appropriately labels the data set with descriptive variable names.
####################

loc_mean_std = sort(c(loc_mean, loc_std))
df_names_x = data.frame(name_num=names_x[[1]], name_label=names_x[[2]])

names_mean_std = df_names_x[loc_mean_std,2]
names(df) = c(as.character(names_mean_std), "subject", "activity")

####################
#Part 5. From the data set in step 4, creates a second, independent tidy data set 
#        with the average of each variable for each activity and each subject.
####################

library(dplyr)

df_summary = df %>% 
    group_by(activity, subject) %>%
    summarize_all(funs(mean(., na.rm=TRUE)))

write.table(df_summary, file="df_summary.txt", row.names = FALSE)

