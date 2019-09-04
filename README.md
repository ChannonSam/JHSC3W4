# JHSC3W4
Repo for week 4 assignment of Course 3 of the JH data science specialisation

This repository contains a copy of raw data downloaded from the following zipped document:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The zipped copy is named raw, and it has an unzipped equivalent directory. The main files in this repository are then:
 - This README file that explains what the repo contains
 - run_analysis.R -> an R script that tidies up the data, removing all non-mean or non-standard dev variables, and combining the observations for the test and training set. The tidying up also ensures the variables are appropriately named (utilising the metadata features.txt and activity_labels.txt from the raw directory). The data is then summarised for each activity and each sample in the experiment, providing a mean for each of the observed variables.
 - codebook -> explains in more details about the data collected
 - tidy_data.txt -> the tidied dataframe
 - tidy_sum.txt -> the summarised dataframe