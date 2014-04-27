# Load test and training data
getwd()
setwd("C:/2014/Data/UCI")
xTest <- read.table("./test/X_test.txt")
yTest <- read.table("./test/y_test.txt")
xTrain <- read.table("./train/X_train.txt")
yTrain <- read.table("./train/y_train.txt")

#Merge training and test set 
x = rbind(xTrain,xTest) 
y = rbind(yTrain,yTest) 

#correctly name the columns of the data
features = read.table("./features.txt", sep=""); features = features[,2];
activityLabels = read.table("./activity_labels.txt", sep=""); 
names(x) <- features

# which variables correspond to means 
variablesData = grep("[m]ean", features)

# correspondingly update x
xMatrixReduced = x[,variablesData]

# write it to a txt file
write.table(xMatrixReduced,file="./xMatrixReduced.txt", sep="")

yLabels=vector(mode="character", length=length(y))

# for each activity label, actually put in a factor variable with the name of the activity
for(a in 1:6){
  i = which(y==a)
  yLabels[i] <- as.character(activityLabels[a,2])
}

# loading subject IDs
subjectTrain = read.table("./train/subject_train.txt", sep="")
subjectTest = read.table("test/subject_test.txt", sep="")

## renaming IDs to more appropriate 
# first create a function to add the word "Subject" to the number ID
addSubject = function(x){
  return(paste("Subject", as.character(x), sep=""))
}

# apply said function to every element 
subjectNamesTest= sapply(subjectTest, FUN=addSubject)
subjectNamesTrain= sapply(subjectTrain, FUN=addSubject)

# also merge the test and train subject names
subjectNamesVector = rbind(subjectNamesTrain, subjectNamesTest)


### creating a second dataset that has the means of each variable for each subject
newData = matrix(ncol=length(names(xMatrixReduced)), nrow=length(unique(subjectNamesVector)))
rownames(newData) = unique(subjectNamesVector); 
colnames(newData) = names(xMatrixReduced);

# for each subject calculate the mean of all variables
for(s in unique(subjectNamesVector)){
  w = which(subjectNamesVector == s)
  cm = colMeans(xMatrixReduced[w,])
  newData[s,] = cm
}
write.table(newData,file="./newData.txt", sep="")
