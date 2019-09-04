url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
library(dplyr); library(tidyr)
# Get all data:
download.file(url, destfile = "raw.zip")

# Note - file was manually unzipped, as was unable to do this directly from R

# Ensure directories are saved appropriately:
home <- getwd()
setwd("raw")
setwd("UCI HAR Dataset")

# Get test data
path.test <- "test/"
d1 <- dir(path.test)
x_test <- tbl_df(read.table(paste0(path.test,"X_test.txt")))
y_test <- tbl_df(read.table(paste0(path.test,"y_test.txt")))
subject_test <- tbl_df(read.table(paste0(path.test,"subject_test.txt")))
path.is.test <- paste0("test/", "Inertial Signals/")
d2 <- dir(path.is.test)
body_acc_x_test <- tbl_df(read.table(paste0(path.is.test, "body_acc_x_test.txt")))
body_acc_y_test <- tbl_df(read.table(paste0(path.is.test, "body_acc_y_test.txt")))
body_acc_z_test <- tbl_df(read.table(paste0(path.is.test, "body_acc_z_test.txt")))
body_gyro_x_test <- tbl_df(read.table(paste0(path.is.test, "body_gyro_x_test.txt")))
body_gyro_y_test <- tbl_df(read.table(paste0(path.is.test, "body_gyro_y_test.txt")))
body_gyro_z_test <- tbl_df(read.table(paste0(path.is.test, "body_gyro_z_test.txt")))
total_acc_x_test <- tbl_df(read.table(paste0(path.is.test, "total_acc_x_test.txt")))
total_acc_y_test <- tbl_df(read.table(paste0(path.is.test, "total_acc_y_test.txt")))
total_acc_z_test <- tbl_df(read.table(paste0(path.is.test, "total_acc_z_test.txt")))

# Get training data
path.train <- "train/"

d3 <- dir(path.train)
x_train <- tbl_df(read.table(paste0(path.train,"X_train.txt")))
y_train <- tbl_df(read.table(paste0(path.train,"y_train.txt")))
subject_train <- tbl_df(read.table(paste0(path.train,"subject_train.txt")))
path.is.train <- paste0("train/", "Inertial Signals/")
d4 <- dir(path.is.train)
body_acc_x_train <- tbl_df(read.table(paste0(path.is.train, "body_acc_x_train.txt")))
body_acc_y_train <- tbl_df(read.table(paste0(path.is.train, "body_acc_y_train.txt")))
body_acc_z_train <- tbl_df(read.table(paste0(path.is.train, "body_acc_z_train.txt")))
body_gyro_x_train <- tbl_df(read.table(paste0(path.is.train, "body_gyro_x_train.txt")))
body_gyro_y_train <- tbl_df(read.table(paste0(path.is.train, "body_gyro_y_train.txt")))
body_gyro_z_train <- tbl_df(read.table(paste0(path.is.train, "body_gyro_z_train.txt")))
total_acc_x_train <- tbl_df(read.table(paste0(path.is.train, "total_acc_x_train.txt")))
total_acc_y_train <- tbl_df(read.table(paste0(path.is.train, "total_acc_y_train.txt")))
total_acc_z_train <- tbl_df(read.table(paste0(path.is.train, "total_acc_z_train.txt")))

# Get metadata:
activity_labels <- tbl_df(read.table("activity_labels.txt"))
features <- tbl_df(read.table("features.txt"))


# Basic summary of what we have:

# -- features contains 561 labels of variables
# -- activity_labels contains the 6 labels for each activity
# -- x_test and x_train contain 2947 and 7352 observations of the above
#    561 variables in features, respectively
# -- y_test and y_train contain 2947 and 7352 observations of one
#    variable, which is the activity label for each observation
# -- subject_test and subject_train contain the identity of the 2947 and
#    7352 observations seen in the testing and training set respectively

# Stage1 - Merging datasets:

x_all <- rbind(x_train, x_test)
y_all <- rbind(y_train, y_test)
colnames(y_all) <- "activity"
subject_all <- rbind(subject_train, subject_test)
colnames(subject_all) <- "id"

# NB - we merge these ALL together at the end of stage 4

# Stage2 - isolate the mean and std variables only:

features$V1 <- tolower(features$V1)
features$V2 <- tolower(features$V2)

cols.mean <- grep(x = features$V2, pattern = "mean")
cols.std <- grep(x = features$V2, pattern = "std")

View(features$V2[cols.mean])
View(features$V2[cols.std])
cols.int <- c(cols.mean, cols.std)
cols.int <- unique(cols.int)
cols.int <- cols.int[order(cols.int)]

x_all <- x_all[, cols.int]

# Stage 3 - Name the activities:

y_all <- mutate(y_all, activity = activity_labels$V2[activity])

# Stage 4 - label the variables appropriately:

cnames <- features[cols.int, 2, drop = TRUE]
colnames(x_all) <- cnames

# combining all together:
tidy_all <- cbind(subject_all, y_all, x_all)


# Stage 5 - From the data set in step 4, create a second,
# independent tidy data set with the average of each variable
# for each activity and each subject.

tidy_sum <- tidy_all %>% 
  group_by(id, activity) %>%
  summarise(`tbodyacc-mean()-x` = mean(`tbodyacc-mean()-x`, na.rm = TRUE),
            `tbodyacc-mean()-y` = mean(`tbodyacc-mean()-y`, na.rm = TRUE),
            `tbodyacc-mean()-z` = mean(`tbodyacc-mean()-z`, na.rm = TRUE),
            `tbodyacc-std()-x` = mean(`tbodyacc-std()-x`, na.rm = TRUE),
            `tbodyacc-std()-y` = mean(`tbodyacc-std()-y`, na.rm = TRUE),
            `tbodyacc-std()-z` = mean(`tbodyacc-std()-z`, na.rm = TRUE),
            `tgravityacc-mean()-x` = mean(`tgravityacc-mean()-x`, na.rm = TRUE),
            `tgravityacc-mean()-y` = mean(`tgravityacc-mean()-y`, na.rm = TRUE),
            `tgravityacc-mean()-z` = mean(`tgravityacc-mean()-z`, na.rm = TRUE),
            `tgravityacc-std()-x` = mean(`tgravityacc-std()-x`, na.rm = TRUE),
            `tgravityacc-std()-y` = mean(`tgravityacc-std()-y`, na.rm = TRUE),
            `tgravityacc-std()-z` = mean(`tgravityacc-std()-z`, na.rm = TRUE),
            `tbodyaccjerk-mean()-x` = mean(`tbodyaccjerk-mean()-x`, na.rm = TRUE),
            `tbodyaccjerk-mean()-y` = mean(`tbodyaccjerk-mean()-y`, na.rm = TRUE),
            `tbodyaccjerk-mean()-z` = mean(`tbodyaccjerk-mean()-z`, na.rm = TRUE),
            `tbodyaccjerk-std()-x` = mean(`tbodyaccjerk-std()-x`, na.rm = TRUE),
            `tbodyaccjerk-std()-y` = mean(`tbodyaccjerk-std()-y`, na.rm = TRUE),
            `tbodyaccjerk-std()-z` = mean(`tbodyaccjerk-std()-z`, na.rm = TRUE),
            `tbodygyro-mean()-x` = mean(`tbodygyro-mean()-x`, na.rm = TRUE),
            `tbodygyro-mean()-y` = mean(`tbodygyro-mean()-y`, na.rm = TRUE),
            `tbodygyro-mean()-z` = mean(`tbodygyro-mean()-z`, na.rm = TRUE),
            `tbodygyro-std()-x` = mean(`tbodygyro-std()-x`, na.rm = TRUE),
            `tbodygyro-std()-y` = mean(`tbodygyro-std()-y`, na.rm = TRUE),
            `tbodygyro-std()-z` = mean(`tbodygyro-std()-z`, na.rm = TRUE),
            `tbodygyrojerk-mean()-x` = mean(`tbodygyrojerk-mean()-x`, na.rm = TRUE),
            `tbodygyrojerk-mean()-y` = mean(`tbodygyrojerk-mean()-y`, na.rm = TRUE),
            `tbodygyrojerk-mean()-z` = mean(`tbodygyrojerk-mean()-z`, na.rm = TRUE),
            `tbodygyrojerk-std()-x` = mean(`tbodygyrojerk-std()-x`, na.rm = TRUE),
            `tbodygyrojerk-std()-y` = mean(`tbodygyrojerk-std()-y`, na.rm = TRUE),
            `tbodygyrojerk-std()-z` = mean(`tbodygyrojerk-std()-z`, na.rm = TRUE),
            `tbodyaccmag-mean()` = mean(`tbodyaccmag-mean()`, na.rm = TRUE),
            `tbodyaccmag-std()` = mean(`tbodyaccmag-std()`, na.rm = TRUE),
            `tgravityaccmag-mean()` = mean(`tgravityaccmag-mean()`, na.rm = TRUE),
            `tgravityaccmag-std()` = mean(`tgravityaccmag-std()`, na.rm = TRUE),
            `tbodyaccjerkmag-mean()` = mean(`tbodyaccjerk-mean()-x`, na.rm = TRUE),
            `tbodyaccjerkmag-std()` = mean(`tbodyaccjerkmag-std()`, na.rm = TRUE),
            `tbodygyromag-mean()` = mean(`tbodygyromag-mean()`, na.rm = TRUE),
            `tbodygyromag-std()` = mean(`tbodygyromag-std()`, na.rm = TRUE),
            `tbodygyrojerkmag-mean()` = mean(`tbodygyrojerkmag-mean()`, na.rm = TRUE),
            `fbodyacc-mean()-x` = mean(`fbodyacc-mean()-x`, na.rm = TRUE),
            `fbodyacc-mean()-y` = mean(`fbodyacc-mean()-y`, na.rm = TRUE),
            `fbodyacc-mean()-z` = mean(`fbodyacc-mean()-z`, na.rm = TRUE),
            `fbodyacc-std()-x` = mean(`fbodyacc-std()-x`, na.rm = TRUE),
            `fbodyacc-std()-y` = mean(`fbodyacc-std()-y`, na.rm = TRUE),
            `fbodyacc-std()-z` = mean(`fbodyacc-std()-z`, na.rm = TRUE),
            `fbodyacc-meanfreq()-x` = mean(`fbodyacc-meanfreq()-x`, na.rm = TRUE),
            `fbodyacc-meanfreq()-y` = mean(`fbodyacc-meanfreq()-y`, na.rm = TRUE),
            `fbodyacc-meanfreq()-z` = mean(`fbodyacc-meanfreq()-z`, na.rm = TRUE),
            `fbodyaccjerk-mean()-x` = mean(`fbodyaccjerk-mean()-x`, na.rm = TRUE),
            `fbodyaccjerk-mean()-y` = mean(`fbodyaccjerk-mean()-y`, na.rm = TRUE),
            `fbodyaccjerk-mean()-z` = mean(`fbodyaccjerk-mean()-z`, na.rm = TRUE),
            `fbodyaccjerk-std()-x` = mean(`fbodyaccjerk-std()-x`, na.rm = TRUE),
            `fbodyaccjerk-std()-y` = mean(`fbodyaccjerk-std()-y`, na.rm = TRUE),
            `fbodyaccjerk-std()-z` = mean(`fbodyaccjerk-std()-z`, na.rm = TRUE),
            `fbodyaccjerk-meanfreq()-x` = mean(`fbodyaccjerk-meanfreq()-x`, na.rm = TRUE),
            `fbodyaccjerk-meanfreq()-y` = mean(`fbodyaccjerk-meanfreq()-y`, na.rm = TRUE),
            `fbodyaccjerk-meanfreq()-z` = mean(`fbodyaccjerk-meanfreq()-z`, na.rm = TRUE),
            `fbodygyro-mean()-x` = mean(`fbodygyro-mean()-x`, na.rm = TRUE),
            `fbodygyro-mean()-y` = mean(`fbodygyro-mean()-y`, na.rm = TRUE),
            `fbodygyro-mean()-z` = mean(`fbodygyro-mean()-z`, na.rm = TRUE),
            `fbodygyro-std()-x` = mean(`fbodygyro-std()-x`, na.rm = TRUE),
            `fbodygyro-std()-y` = mean(`fbodygyro-std()-y`, na.rm = TRUE),
            `fbodygyro-std()-z` = mean(`fbodygyro-std()-z`, na.rm = TRUE),
            `fbodygyro-meanfreq()-x` = mean(`fbodygyro-meanfreq()-x`, na.rm = TRUE),
            `fbodygyro-meanfreq()-y` = mean(`fbodygyro-meanfreq()-y`, na.rm = TRUE),
            `fbodygyro-meanfreq()-z` = mean(`fbodygyro-meanfreq()-z`, na.rm = TRUE),
            `fbodyaccmag-mean()` = mean(`fbodyaccmag-mean()`, na.rm = TRUE),
            `fbodyaccmag-std()` = mean(`fbodyaccmag-std()`, na.rm = TRUE),
            `fbodyaccmag-meanfreq()` = mean(`fbodyaccmag-meanfreq()`, na.rm = TRUE),
            `fbodybodyaccjerkmag-mean()` = mean(`fbodybodyaccjerkmag-mean()`, na.rm = TRUE),
            `fbodybodyaccjerkmag-std()` = mean(`fbodybodyaccjerkmag-std()`, na.rm = TRUE),
            `fbodybodyaccjerkmag-meanfreq()` = mean(`fbodybodyaccjerkmag-meanfreq()`, na.rm = TRUE),
            `fbodybodygyromag-mean()` = mean(`fbodybodygyromag-mean()`, na.rm = TRUE),
            `fbodybodygyromag-std()` = mean(`fbodybodygyromag-std()`, na.rm = TRUE),
            `fbodybodygyromag-meanfreq()` = mean(`fbodybodygyromag-meanfreq()`, na.rm = TRUE),
            `fbodybodygyrojerkmag-mean()` = mean(`fbodybodygyrojerkmag-mean()`, na.rm = TRUE),
            `fbodybodygyrojerkmag-std()` = mean(`fbodybodygyrojerkmag-std()`, na.rm = TRUE),
            `fbodybodygyrojerkmag-meanfreq()` = mean(`fbodybodygyrojerkmag-meanfreq()`, na.rm = TRUE),
            `angle(tbodyaccmean,gravity)` = mean(`angle(tbodyaccmean,gravity)`, na.rm = TRUE),
            `angle(tbodyaccjerkmean),gravitymean)` = mean(`angle(tbodyaccjerkmean),gravitymean)`, na.rm = TRUE),
            `angle(tbodygyromean,gravitymean)` = mean(`angle(tbodygyromean,gravitymean)`, na.rm = TRUE),
            `angle(tbodygyrojerkmean,gravitymean)` = mean(`angle(tbodygyrojerkmean,gravitymean)`, na.rm = TRUE),
            `angle(x,gravitymean)` = mean(`angle(x,gravitymean)`, na.rm = TRUE),
            `angle(y,gravitymean)` = mean(`angle(y,gravitymean)`, na.rm = TRUE),
            `angle(z,gravitymean)` = mean(`angle(z,gravitymean)`, na.rm = TRUE))

# Write this to file:

write.table(tidy_all, paste0(home, "/tidy_data.txt"), row.names = FALSE)
write.table(tidy_sum, paste0(home, "/tidy_sum.txt"), row.names = FALSE)
