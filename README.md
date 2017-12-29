### Getting and Cleaning Data Course Project Background

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

You should create one R script called run_analysis.R that does the following.

1.  Merges the training and the test sets to create one data set.
2.  Extracts only the measurements on the mean and standard deviation for each measurement.
3.  Uses descriptive activity names to name the activities in the data set
4.  Appropriately labels the data set with descriptive variable names.
5.  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

### File Descriptions

1.  `README.md`: provides the background of the project, descriptions of files in the repository, and explainations of how the R script works.
2.  `CODEBOOK.md`: describes the variables, the data, and any transformation work conducted to clean up the data.
3.  `run_analysis.R`: the R script used to perform the analysis.
4.  `df_summary.txt`: the tidy dataset created from step 5 of the instructions as above.

### Explanation of the R Script

Before running the script, complete the following steps.

1.  Download the data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
2.  Unzip the data downloaded into a folder
3.  Set the folder created in step 2 as the current working directory of the R process

Once completed the steps as above, you can run the `run_analysis.R` and it will create a text file called "df_summary.txt" in the directory created in step 2. 

The R script is composed of 6 parts:
*   Reads data from the text files downloaded into data frames.
*   Merges the training and the test sets to create one data set. The combined data set also includes the `activity` and `subject`.
*   Extracts only the measurements on the mean and standard deviation for each measurement. The `activity` and `subject` are also included in the resulting data set.
*   Uses descriptive activity names to name the activities in the data set.
*   Appropriately labels the data set with descriptive variable names.
*   From the data set created in the previous step, creates a second, independent tidy data set with the average of each variable for each activity and each subject. Save it as `df_summary.txt` in local computer.