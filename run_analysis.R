##
## The functions
##

# Loads the activity code -> label array/table
#
loadActivityLabels <- function()
{
    labelsFile <- paste(workDir, "activity_labels.txt", sep = pathSep)
    if(! file.exists(labelsFile))
    {
        stop("The activity labels file [", labelsFile, "] not found.Exiting")
    }
    print(paste("loading", labelsFile))
    read.csv(labelsFile, header = FALSE, sep = " ")   
}

# Loads the coulmn index -> coulmn meaning array/table
#
loadFeatures <- function()
{
    featureFile <- paste(workDir, "features.txt", sep = pathSep)
    if(! file.exists(featureFile))
    {
        stop("The feature labels file [", featureFile, "] not found. Exiting")
    }
    print(paste("loading", featureFile))
    f <- read.csv(featureFile, header = FALSE, sep = " ")  

    #Make the vector of only second column which is a measure descr
    as.vector(f[ , 2])
}

# Loads the data and the subject and returns a data.frame that combines both
#
loadData <- function(setDir, dataFileArg, subjectFileArg, actFileArg)
{
    dataFile <- paste(workDir, setDir, dataFileArg, sep = pathSep)
    subjectFile <- paste(workDir,setDir, subjectFileArg, sep = pathSep)
    actFile <- paste(workDir, setDir, actFileArg, sep = pathSep)
    
    if(! file.exists(dataFile))
    {
        stop("The data file '", dataFile, "' does not exist", call. = FALSE)
    }
    print(paste("loading", dataFile))
    da <- read.table(dataFile, header = FALSE)
    d <- da[ , featureIdx ]
    
    if(! file.exists(subjectFile))
    {
        stop("The subject file '", subjectFile, "' does not exist", call. = FALSE)
    }
    print(paste("loading", subjectFile))
    s <- read.csv(subjectFile, header = FALSE)
    
    d$subject <- s$V1; # add the subject column
    
    if(! file.exists(actFile))
    {
        stop("The activity file '", actFile, "' does not exist", call. = FALSE)
    }
    print(paste("loading", actFile))
    tmpx <- read.csv(actFile, header = FALSE)
    a <- merge(tmpx, activityLabels, by.tmpx = tmpx$V1, activityLabels.y = activityLabels$V1)
    
    d$activity <- a$V2; # add the activity column
    
    d
}

# Cleans the column names
#
cleanColumnNames <- function(rawColumnNames)
{
    x1 <- gsub("\\(", "", rawColumnNames) # For Windows Vista
    x2 <- gsub("\\)", "", x1)             # I had to put double \
    x3 <- gsub("\\-", "", x2)             # as the escape character
    gsub("\\,", "", x3)
}

##
## The script starts here
##

workDir <- "projectData/UCI HAR Dataset"
pathSep = "/"

activityLabels <- loadActivityLabels()

features <- loadFeatures()

featurePattern <- "mean|std"
featureIdx <- grep(featurePattern, features, value = FALSE, ignore.case = TRUE)
featureNames <- grep(featurePattern, features, value = TRUE, ignore.case = TRUE)

testData <- loadData("test", "X_test.txt", "subject_test.txt", "y_test.txt")
trainData <- loadData("train","X_train.txt", "subject_train.txt", "y_train.txt")
allData <- merge(testData, trainData, all = TRUE)

names(allData) <- cleanColumnNames(c(featureNames, "subject", "activity"))
finalResult <- aggregate(. ~ subject + activity, data = allData, mean)
write.table(finalResult, "run_analysis.txt", row.names = FALSE)
finalResult
