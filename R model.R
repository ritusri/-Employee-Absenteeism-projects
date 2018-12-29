rm(list = ls())

#Getting Current working directory.
getwd()

#Setting working directory
setwd("C:/Users/anupr/Desktop/employee_sentism")

getwd()

x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','readxl')

lapply(x, require, character.only = TRUE)

## Read the data
file= read.csv("data.csv", header = T, na.strings = c(" ", "", "NA"))



#Getting the dimensions of data
dim(file)


# Fetting Structure Of data
str(file)

#Retrieving Column names of train and test data.
colnames(file)


#Removing Spaces between the column names
names(file) =  gsub(" ", "_", names(file))

############ Distribution pf Target Variable ##############

# For train data
library(ggplot2)
pl = ggplot(file ,aes(x = "Absenteeism time in hours")) + ggtitle("Absenteeism time in hours")
print(pl + geom_bar(fill = 'blue'))


########################################## Missing Value Analysis ##############################################

missing_val = data.frame(apply(file,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(file)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
write.csv(missing_val, "Miising_perc.csv", row.names = F)

# kNN Imputation
file = knnImputation(file, k = 3)
sum(is.na(file))


### Imputing Missing Values
require(DMwR)
cnames = colnames(file)
newdata = data.frame()
newdata = data.frame(file)
file = knnImputation(newdata, k = 3)
sum(is.na(file))
anyNA(file)

########################################### Data Visusalisation ####################################################

#### Box-Plot for data
boxplot(file[,c("ID", "Reason.for.absence","Month.of.absence","Day.of.the.week","Seasons","Transportation.expense","Distance.from.Residence.to.Work")])
boxplot(file[,c("Service.time", "Age","Work.load.Average.day","Hit.target","Disciplinary.failure" ,"Education" ,"Son")])
boxplot(file[,c("Social.drinker","Social.smoker","Pet", "Weight", "Height", "Body.mass.index","Absenteeism.time.in.hours")])

#### KDE plot 
library("kdensity")
plot(density(file$'ID'))
plot(density(file$'Reason.for.absence'))
plot(density(file$'Month.of.absence'))
plot(density(file$'Day.of.the.week'))
plot(density(file$'Seasons'))
plot(density(file$'Transportation.expense'))
plot(density(file$'Distance.from.Residence.to.Work'))
plot(density(file$'Service.time'))
plot(density(file$'Age'))
plot(density(file$'Work.load.Average.day'))
plot(density(file$'Hit.target'))
plot(density(file$'Absenteeism.time.in.hours'))
plot(density(file$'Body.mass.index'))
plot(density(file$'Height'))
plot(density(file$'Weight'))
plot(density(file$'Pet'))
plot(density(file$'Social.smoker'))
plot(density(file$'Social.drinker'))
plot(density(file$'Son'))
plot(density(file$'Education'))
plot(density(file$'Disciplinary.failure'))

## Normality Check
qqnorm(file$ID)
qqnorm(file$Reason.for.absence)
qqnorm(file$Month.of.absence)
qqnorm(file$Day.of.the.week)
qqnorm(file$Seasons)
qqnorm(file$Transportation.expense)
qqnorm(file$Distance.from.Residence.to.Work)
qqnorm(file$Service.time)
qqnorm(file$Age)
qqnorm(file$Work.load.Average.day)
qqnorm(file$Hit.target)
qqnorm(file$Absenteeism.time.in.hours)
qqnorm(file$Body.mass.index)
qqnorm(file$Height)
qqnorm(file$Weight)
qqnorm(file$Pet)
qqnorm(file$Social.drinker)
qqnorm(file$Social.smoker)
qqnorm(file$Son)
qqnorm(file$Education)
qqnorm(file$Disciplinary.failure)


#### PCA Visualisation
library(ggfortify)
autoplot(prcomp(file), data = file, colour = 'Absenteeism.time.in.hours')



###################### Outlier Analysis #######################

cnames =c('Service.time', 'Age', 'Work.load.Average.day', 'Transportation.expense','Hit.target', 'Height', 'Absenteeism.time.in.hours', 'Weight')
df = file

#Replace all outliers with NA and impute
for(i in cnames)
{
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  #print(length(val))
  df[,i][df[,i] %in% val] = NA
}

# Imputing missing values
df = knnImputation(df,k=3)
file = df
anyNA(file)


################################# CREATING DUMMIES FOR CATEGORICAL VARIABLES ####################################
file = fastDummies::dummy_cols(data , select_columns = "Seasons" , remove_first_dummy = TRUE)
file = fastDummies::dummy_cols(data , select_columns = "Month.of.absence" , remove_first_dummy = TRUE)
file = fastDummies::dummy_cols(data , select_columns = "Day.of.the.week" , remove_first_dummy = TRUE)
file = fastDummies::dummy_cols(data , select_columns = "Reason.for.absence" , remove_first_dummy = TRUE)
file = fastDummies::dummy_cols(data , select_columns = "ID" , remove_first_dummy = TRUE)
file = fastDummies::dummy_cols(data , select_columns = "Education" , remove_first_dummy = TRUE)
file = fastDummies::dummy_cols(data , select_columns = "Pet" , remove_first_dummy = TRUE)
file = fastDummies::dummy_cols(data , select_columns = "Son" , remove_first_dummy = TRUE)

knitr::kable(file)

# Deleting the columns for which dummies are created
file = subset(file, select = -c(Seasons,Month.of.absence, Day.of.the.week,Reason.for.absence,ID, Education, Pet, Son ))

############################################ Scaling the data #######################################

cnames1 =c('Service.time', 'Age', 'Work.load.Average.day', 'Transportation.expense','Hit.target', 'Height', 'Weight')


for(i in cnames1){
  print(i)
  file[,i] = (file[,i] - min(file[,i]))/
    (max(file[,i] - min(file[,i])))
}

# Normalization
for(i in cnames1)
{
  print(i)
  file[,i] = (file[,i] - min(file[,i]))/(max(file[,i])-min(file[,i]))
}

########################################## Feature Selection ######################################

##### Using Correlation plot

library(corrgram)
corrgram(file[,cnames] , order =F, upper.panel = panel.pie , text.panel = panel.txt , main = "Correlation Plot")

# Weight seems to be correlated, so Deleting weight from the data

file = subset(file , select = -c(Weight))




##### Splitting the data into train and test
n = nrow(file)
trainIndex = sample(1:n, size = round(0.8*n), replace=FALSE)
train = file[trainIndex ,]
test = file[-trainIndex ,]

X_train = subset(train,select = -c(Absenteeism.time.in.hours))
y_train = subset(train,select = c(Absenteeism.time.in.hours))

X_test = subset(test,select = -c(Absenteeism.time.in.hours))
y_test = subset(test,select = c(Absenteeism.time.in.hours))

##### Using PCA

#principal component analysis
prin_comp = prcomp(X_train)
prcomp(file[,-1])

#compute standard deviation of each principal component
std_dev = prin_comp$sdev

#compute variance
pr_var = std_dev^2

#proportion of variance explained
prop_varex = pr_var/sum(pr_var)

#cdf plot for principle components
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#add a training set with principal components
X_train.data = data.frame( prin_comp$x)

# From the above plot selecting 45 components since it explains almost 95+ % data variance
X_train.data =X_train.data[,1:45]

#transform test into PCA
X_test.data = predict(prin_comp, newdata = X_test)
X_test.data = as.data.frame(X_test.data)

#select the first 45 components
X_test.data=X_test.data[,1:45]

######################################## Machine learning model########################################

X_train.data$Absenteeism_time_in_hours = paste(y_train$Absenteeism_time_in_hours)
X_test.data$Absenteeism_time_in_hours = paste(y_test$Absenteeism_time_in_hours)


library(mlbench)
#### KNN
#Develop Model on training data
fit_LR = knnreg(Absenteeism_time_in_hours ~ ., data = X_train.data)
#Lets predict for testing data
pred_LR_test = predict(fit_LR,X_test.data)
# Results 
print(postResample(pred = pred_LR_test, obs =y_test$Absenteeism_time_in_hours))



###### Multiple Linear Regression
#Develop Model on training data
set.seed(100)
#Develop Model on training data
fit_LR = lm(Absenteeism.time.in.hours ~ ., data = X_train.data)
#Lets predict for testing data
pred_LR_test = predict(fit_LR,X_test.data)
# Results 
print(postResample(pred = pred_LR_test, obs =y_test$Absenteeism_time_in_hours))



###### Decision Tree

#Develop Model on training data

fit_DT = rpart(Absenteeism.time.in.hours ~., data = X_train.data, method = 'anova')
pred_DT_test = predict(fit_DT,X_test.data)
# for getting Results
print(postResample(pred = pred_DT_test, obs = y_test$Absenteeism.time.in.hours))



###### GBDT

#Develop Model on training data
fit_GBDT = gbm(Absenteeism_time_in_hours~., data = X_train.data, n.trees = 500, interaction.depth = 2)
#Lets predict for testing data
pred_GBDT_test = predict(fit_GBDT,X_test.data, n.trees = 500)
# For testing data 
print(postResample(pred = pred_GBDT_test, obs = y_test$Absenteeism.time.in.hours))





###### Random Forest
#Develop Model on training data
fit_DT = randomForest(Absenteeism_time_in_hours ~., data = X_train.data)
pred_DT_test = predict(fit_DT,X_test.data)
# for getting Results
print(postResample(pred = pred_DT_test, obs = y_test$Absenteeism.time.in.hours))



