#Clear Environment
rm(list=ls(all=T))

#Setting working directory
setwd("C:/Users/saksh/OneDrive/Desktop/Project/Predication-of-bike-rental")

#Load Libraries
x = c("ggplot2", "corrgram", "caret", "randomForest", "dummies", "e1071", "rpart", "gbm", 'DataCombine')

# Loading required libraries
lapply(x, require, character.only = TRUE)
#Remove list
rm(x)

######################## Functions used in the project ########################################
#This function will segregate the different data types present in the dataframe.
datatype_separator <- function(dataFrame, column_names){
  fact_columns = list()
  number_columns =list()
  logic_columns = list()
  int_columns = list()
  char_columns = list()
  unknown_columns = list()
  for(i in 1:ncol(dataFrame)){
    if(class(dataFrame[,i])=='factor'){
      fact_columns = c(fact_columns,column_names[i])
    } else if(class(dataFrame[,i])=='numeric'){
      number_columns = c(number_columns,column_names[i])
    } else if(class(dataFrame[,i])=='integer'){
      int_columns = c(int_columns,column_names[i])
    } else if(class(dataFrame[,i])=='character'){
      char_columns = c(char_columns,column_names[i])
    } else if(class(dataFrame[,i])=='logical'){
      logic_columns = c(logic_columns,column_names[i])
    } else{
      unknown_columns = c(unknown_columns,column_names[i])
    }
  }
  list(fact_columns,number_columns,int_columns,char_columns,logic_columns,unknown_columns)
}

outlier_dataFrame<- function(dataFrame,num_datatype){
  k = 1
  len_val = c()
  ## Total number of outliers
  for(i in num_datatype){
    print(i)
    print(k)
    val = dataFrame[,i][dataFrame[,i] %in% boxplot.stats(dataFrame[,i])$out]
    print(length(val))
    len_val[[k]] = length(val)
    k = k+1
  }
  outlier_analysis = data.frame(feature = num_datatype, total_outliers = len_val)
}

################################# Loading data #########################################

dataFrame = read.csv("day.csv", header = T, na.strings = c(" ", "", "NA"))

################################### Explore the data ##########################################

#Viewing dataframe
head(dataFrame)

#Display the Structure of dataframe
str(dataFrame)

################################################## Drop unwanted features ##################################################
#After viewing and analysing data we conclude that the sum of casual and registered column equals cnt column hence casual and
#regestered columns are droped. Also we are not doing doing any time series analysis so we do not require dteday.Since Instant field is
#only used to store indices of records we can drop that too.

drop_columns = c('dteday','casual','registered','instant')
dataFrame = dataFrame[,!names(dataFrame) %in% drop_columns]

# Calling function to seperate by datatype
datatype = datatype_separator(dataFrame,colnames(dataFrame))
num_datatype = datatype[[2]]
num_datatype = unlist(num_datatype, recursive=FALSE)
int_datatype = datatype[[3]]
int_datatype = unlist(int_datatype, recursive=FALSE)

# Converted to corrected data type
####################################
# It was noticed that some features where stored as the wrong data type and hence are converted to the right data type.

convert_obj = c('season', 'yr', 'mnth', 'holiday', 'weekday', 'workingday', 'weathersit')
for(i in 1:length(int_datatype)){
  if(int_datatype[i] %in% convert_obj){
    dataFrame[,int_datatype[i]] = factor(dataFrame[,int_datatype[i]])
  }
}
# Convert target variable to numeric data type
dataFrame[,'cnt'] = as.numeric(dataFrame[,'cnt'])

# Calling function to seperate by datatype
datatype = datatype_separator(dataFrame,colnames(dataFrame))
fact_datatype = datatype[[1]]
fact_datatype = unlist(fact_datatype, recursive=FALSE)
num_datatype = datatype[[2]]

num_datatype = unlist(num_datatype, recursive=FALSE)
int_datatype = datatype[[3]]
int_datatype = unlist(int_datatype, recursive=FALSE)



#########################################################################################################################
# Data Visualisation and Data Understanding 
#########################################################################################################################

#Plotting no of bikes rented with respect to the days of the week.
ggplot(data = dataFrame, aes(x = reorder(weekday,-cnt), y = cnt))+
  geom_bar(stat = "identity",fill = "white")+
  labs(title = "Number of bikes rented with respect to days", x = "Days of the week")+ 
  theme(panel.background = element_rect("cadetblue"))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#Plotting Bikes rented with respect to variation in temperature and hunidity
ggplot(dataFrame,aes(temp,cnt)) + 
  geom_point(aes(color=hum),alpha=0.5) +
  labs(title = "Bikes rented with respect to variation in temperature and hunidity", x = "Normalized temperature")+
  scale_color_gradientn(colors=c('dark blue','blue','light blue','light green','yellow','orange','red')) +
  theme_bw()

#Plotting Bikes rented with respect to temperature and weathersite
ggplot(data = dataFrame, aes(x = temp, y = cnt))+
  geom_point(aes(color=weathersit))+
  labs(title = "Bikes rented with respect to temperature and weathersite", x = "Normalized temperature")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme_bw()

#PLotting Bikes rented with respect to temperature and workingday
ggplot(data = dataFrame, aes(x = temp, y = cnt))+
  geom_point(aes(color=workingday))+
  labs(title = "Bikes rented with respect to temperature and workingday", x = "Normalized temperature")+
  #  theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme_bw()

########################################################################
# Distribution Plot
########################################################################

#Plot Temperature distribution (temp)
ggplot(data = dataFrame, aes(x = temp))+
  geom_histogram(bins = 30, fill = "white", col = "black")+
  labs(title = "Temperature distribution (temp)", x = "Normalized temperature")+ 
  theme(panel.background = element_rect("cadetblue"))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#PLot Real feel temperature distribution (atemp)
ggplot(data = dataFrame, aes(x = atemp))+
  geom_histogram(bins = 30, fill = "white", col = "black")+
  labs(title = "Real feel temperature distribution (atemp)", x = "Normalized temperature")+ 
  theme(panel.background = element_rect("cadetblue"))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#Plot Humidity distribution
ggplot(data = dataFrame, aes(x = hum))+
  geom_histogram(bins = 30, fill = "white", col = "black")+
  labs(title = "Humidity distribution", x = "Normalized humidity")+ 
  theme(panel.background = element_rect("cadetblue"))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#Plot Windspeed distribution
ggplot(data = dataFrame, aes(x = windspeed))+
  geom_histogram(bins = 30, fill = "white", col = "black")+
  labs(title = "Windspeed distribution", x = "Features")+ 
  theme(panel.background = element_rect("cadetblue"))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#Distribution of unique values in a Feature
for(i in 1:length(fact_datatype)){
  fact = fact_datatype[i]
  print(fact)
  print(table(dataFrame[fact]))
}


########################################################################################################################
# Missing Values Analysis
########################################################################################################################
missing_val = sum(is.na(dataFrame))
print(missing_val)
###########################################################################################################################
#### Outlier Analysis
###########################################################################################################################
#Plot outliers present in continous data set
for (i in 1:length(num_datatype))

  {
  assign(paste0("gn",i), ggplot(aes_string(y = (num_datatype[i])), data = subset(dataFrame))+ geom_boxplot()+
           labs(y=num_datatype[i])+
           theme(panel.background = element_rect(fill = "palegreen"))+
           ggtitle(paste("Outlier analysis for ",num_datatype[i])))
}
## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,ncol=4)


outlier_analysis = outlier_dataFrame(dataFrame,num_datatype)

ggplot(data = outlier_analysis, aes(x = reorder(feature,-total_outliers), y = total_outliers))+
  geom_bar(stat = "identity",fill = "white")+
  labs(title = "Outliers in each feature", x = "Features")+ 
  theme(panel.background = element_rect("cadetblue"))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

dataFrame_size = dim(dataFrame)
outliers_percentage = (sum(outlier_analysis$total_outliers)/dataFrame_size[1])*100
print(outliers_percentage)

#Removal of outliers
####################
# Since the percentage of outliers present in the data set is very less. it is decided to remove those records from
# the dataset.
for(i in num_datatype){
  val = dataFrame[,i][dataFrame[,i] %in% boxplot.stats(dataFrame[,i])$out]
  dataFrame = dataFrame[which(!dataFrame[,i] %in% val),]
}

#Plot outliers present in continous data set
for (i in 1:length(num_datatype))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (num_datatype[i])), data = subset(dataFrame))+ geom_boxplot()+
           labs(y=num_datatype[i])+
           theme(panel.background = element_rect(fill = "palegreen"))+
           ggtitle(paste("Outlier analysis for ",num_datatype[i])))
}
## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,ncol=4)

outlier_analysis = outlier_dataFrame(dataFrame,num_datatype)

dataFrame_size = dim(dataFrame)
outliers_percentage = (sum(outlier_analysis$total_outliers)/dataFrame_size[1])*100
print(outliers_percentage)

##################################Feature Selection################################################
###################################################################################################
## Correlation Plot 
#######################################
install.packages("GGally")
corrgram(dataFrame[,num_datatype], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
#Correlation plot and pair plot
library(GGally)
ggpairs(dataFrame[,num_datatype])

#######################################
## Anova Test
#######################################
anova = aov(cnt~ season, data = dataFrame)
summary(anova)
anova = aov(cnt~ yr, data = dataFrame)
summary(anova)
anova = aov(cnt~ mnth, data = dataFrame)
summary(anova)
anova = aov(cnt~ holiday, data = dataFrame)
summary(anova)
anova = aov(cnt~ weekday, data = dataFrame)
summary(anova)
anova = aov(cnt~ workingday, data = dataFrame)
summary(anova)
anova = aov(cnt~ weathersit, data = dataFrame)
summary(anova)

################################ Dimension Reduction ##############################

#Removal of less significiant features
dataFrame = subset(dataFrame, select = -c(atemp,holiday,weekday,workingday))

##########################################################################################################
#Creation of dummy variables
##########################################################################################################
#If package not installed
install.packages("fastDummies")

library(fastDummies)

# Function Creates the dummy variables and drops the first dummy variable
dataFrame <- fastDummies::dummy_cols(dataFrame, remove_first_dummy = TRUE)

## Dropping the original catergorical variable
dataFrame = subset(dataFrame, select = -c(season,yr,mnth,weathersit))

###########################################################################################################
# Model Development
###########################################################################################################
#Clean the environment
rmExcept("dataFrame")


#FUNCTION
#####################
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}
###########################################################################################################
#Divide data into train and test data set
###########################################################################################################
seed = 1234
set.seed(seed)
train.index = createDataPartition(dataFrame$cnt, p = .80, list = FALSE)
train = dataFrame[ train.index,]
test  = dataFrame[-train.index,]

#Rearranging data set
train = train[,c(5:21,1:4)]
test = test[,c(5:21,1:4)]


##########################################################
# Miltivarient Linear Regression to the Training set
###########################################################
#Building model
set.seed(seed)
linear_regressor = lm(formula = cnt ~.,data = train)
summary(linear_regressor)

# Predicting the Test data output
y_pred = predict(linear_regressor, newdata = test[,1:20])
install.packages("miscTools")
library(miscTools)

#Compute R^2
mlr_r_square <- rSquared(test[,21], test[,21] - y_pred)
print(mlr_r_square)
#Compute MSE
mlr_mse <- mean((test[,21] - y_pred)^2)
print(mlr_mse)
#Compute MAPE
mlr_mape = MAPE(test[,21], y_pred)
print(mlr_mape)

###################################################################################################################################
### Decission Tree Regression #####################################################################################################
###################################################################################################################################

set.seed(seed)
#Building model
fit = rpart(cnt ~ ., data = train, method = "anova")
#Variable importance
fit$variable.importance

# Predicting the Test data output
predictions_DT = predict(fit, test[,1:20])

#Compute R^2
dt_r2 <- rSquared(test[,21], test[,21] - predictions_DT)
print(dt_r2)
#Compute MSE
dt_mse <- mean((test[,21] - predictions_DT)^2)
print(dt_mse)
#Compute MAPE
dt_mape = MAPE(test[,21], predictions_DT)
print(dt_mape)

###################################################################################################################################
## Random Search ##################################################################################################################
###################################################################################################################################

# Create model with Random paramters 5 fold CV with 1 repeats
control = trainControl(method="repeatedcv", number=5, repeats=1,search='random')
set.seed(seed)
maxdepth = c(1:30)
tunegrid = expand.grid(.maxdepth=maxdepth)
dt_random = caret::train(cnt~., data=train, method="rpart2", metric="RMSE", tuneLength =10, trControl=control)

#print out summary of the model
print(dt_random)

#Best fit parameters
view_dt_rand_para = dt_random$bestTune
print(view_dt_rand_para)
#Build model based on best fit 
rand_dt_model = rpart(cnt ~ ., train[,0:21], method = "anova", maxdepth = 12)
#print out summary of the model
print(rand_dt_model)

#Presdict test data using random forest model
rand_dt_Predictions = predict(rand_dt_model, test[,1:20])

#Compute R^2
rand_dt_r2 = rSquared(test[,21], test[,21] - rand_dt_Predictions)
print(rand_dt_r2)
#Compute MSE
rand_dt_mse = mean((test[,21] - rand_dt_Predictions)^2)
print(rand_dt_mse)
#Compute MAPE
rand_dt_mape = MAPE(test[,21], rand_dt_Predictions)
print(rand_dt_mape)
###################################################################################################################################
# Grid Search #####################################################################################################################
###################################################################################################################################

# Create model based on paramters grid specificed with 5 fold CV with 1 repeats
control = trainControl(method="repeatedcv", number=5, repeats=1, search="grid")
set.seed(seed)
tunegrid = expand.grid(.maxdepth=c(6:18))
dt_grid = caret::train(cnt~., data=train, method="rpart2", metric="RMSE", tuneGrid=tunegrid, trControl=control)

#print out summary of the model
print(dt_grid)
#Plot RMSE Vs mtry values
#From this plot we can see that when tree depth is 10 RMSE is at its lowest
plot(dt_grid)

#Best fit parameters
view_dt_grid_pram = dt_grid$bestTune
print(view_dt_grid_pram)

#Build model based on best fir value
grid_dt_model = rpart(cnt ~ ., train[,0:21], method = "anova",maxdepth = 9)
#print out summary of the model
print(grid_dt_model)

#Presdict test data using model
grid_dt_Predictions = predict(grid_dt_model, test[,1:20])

#Compute R^2
grid_dt_r2 = rSquared(test[,21], test[,21] - grid_dt_Predictions)
print(grid_dt_r2)
#Compute MSE
grid_dt_mse = mean((test[,21] - grid_dt_Predictions)^2)
print(grid_dt_mse)
#Compute MAPE
grid_dt_mape = MAPE(test[,21], grid_dt_Predictions)
print(grid_dt_mape)


####################################################################################################################################
####################################
###Random Forest Regression
####################################
#Building model
RF_model = randomForest(cnt ~ ., train[,0:21], method = "anova",importance = TRUE)
#Prints out model information
print(RF_model)

#Predicting the Test data output
RF_Predictions = predict(RF_model, test[,1:20])

#Compute R^2
rf_r2 = rSquared(test[,21], test[,21] - RF_Predictions)
print(rf_r2)
#Compute MSE
rf_mse = mean((test[,21] - RF_Predictions)^2)
print(rf_mse)
#Compute MAPE
rf_mape = MAPE(test[,21], RF_Predictions)

print(rf_mape)

###################################################################################################################################
## Random Search ##################################################################################################################
###################################################################################################################################

# Create model with Random paramters 5 fold CV with 1 repeats
control = trainControl(method="repeatedcv", number=5, repeats=1,search='random')
set.seed(seed)
rf_random = caret::train(cnt~., data=train, method="rf", metric="RMSE", tuneLength =10, trControl=control)

#print out summary of the model
#here we see that when mtry is 5 RMSE is at its lowest
print(rf_random)

#Best fit parameters
view_rf_rand_para = rf_random$bestTune
print(view_rf_rand_para)

#Build model based on best fit 
rand_rf_model = randomForest(cnt ~ ., train[,0:21], method = "anova",importance = TRUE, mtry = 5)
#print out summary of the model
#from the summary it is seen that this model explains 86.88%of the variance
print(rand_rf_model)

#Presdict test data using random forest model
rand_rf_Predictions = predict(rand_rf_model, test[,1:20])

#Compute R^2
rand_rf_r2 <- rSquared(test[,21], test[,21] - rand_rf_Predictions)
print(rand_rf_r2)
#Compute MSE
rand_rf_mse <- mean((test[,21] - rand_rf_Predictions)^2)
print(rand_rf_mse)
#Compute MAPE
rand_rf_mape = MAPE(test[,21], rand_rf_Predictions)
print(rand_rf_mape)
###################################################################################################################################
# Grid Search #####################################################################################################################
###################################################################################################################################

# Create model based on paramters grid specificed with 5 fold CV with 1 repeats
control = trainControl(method="repeatedcv", number=5, repeats=1, search="grid")

set.seed(seed)
tunegrid = expand.grid(.mtry=c(3:7))
rf_grid = caret::train(cnt~., data=train, method="rf", metric="RMSE", tuneGrid=tunegrid, trControl=control)

#print out summary of the model
print(rf_grid)
#Plot RMSE Vs mtry values
#from the plot we can see that when the randomly selected predictor is 6 RMSE is at its lowest
plot(rf_grid)

#Best fit parameters
view_rf_grid_pram = rf_grid$bestTune
print(view_rf_grid_pram)

#Build model based on best fir value
grid_rf_model = randomForest(cnt ~ ., train[,0:21], method = "anova",importance = TRUE,mtry = 6)
#print out summary of the model
#from the summary we see that this model explains 87.55% of the variance
print(grid_rf_model)

#Presdict test data using model
grid_rf_Predictions = predict(grid_rf_model, test[,1:20])


#Compute R^2
grid_rf_r2 = rSquared(test[,21], test[,21] - grid_rf_Predictions)
print(grid_rf_r2)
#Compute MSE
grid_rf_mse = mean((test[,21] - grid_rf_Predictions)^2)
print(grid_rf_mse)
#Compute MAPE
grid_rf_mape = MAPE(test[,21], grid_rf_Predictions)
print(grid_rf_mape)
####################################################################################################################################
####################################
###Gradient Boosting
####################################
#Building model
gbm_base = gbm(cnt~., data = train,distribution = "gaussian",n.trees = 10000,
               shrinkage = 0.01, interaction.depth = 4)
#Print out summary of the model
summary(gbm_base)

#Presdict test data using random forest model
gbm_Predictions = predict(gbm_base, test[,1:20],n.trees = 10000)

#Compute R^2
gbm_r2 = rSquared(test[,21], test[,21] - gbm_Predictions)
print(gbm_r2)
#Compute MSE
gbm_mse = mean((test[,21] - gbm_Predictions)^2)
print(gbm_mse)
#Compute MAPE
gbm_mape = MAPE(test[,21], gbm_Predictions)
print(gbm_mape)
###################################################################################################################################
## Random Search ##################################################################################################################
###################################################################################################################################

# Create model with random paramters
control = trainControl(method="repeatedcv", number=5, repeats=1,search='random')
set.seed(seed)
gbm_random = caret::train(cnt~., data=train, method="gbm", metric="RMSE", tuneLength =10, trControl=control)
#print out summary of model
print(gbm_random)

#Best tune parameters
view_gbm_rand_param = gbm_random$bestTune
print(view_gbm_rand_param)

#Presdict test data using model
rand_gbm_Predictions = predict(gbm_random, test[,1:20],n.trees = 2571, interection.depth = 3, shrinkage = 0.02841639, n.minobsinnode = 21)

#Compute R^2
rand_gbm_r2 = r2 = rSquared(test[,21], test[,21] - rand_gbm_Predictions)
print(rand_gbm_r2)
#Compute MSE
rand_gbm_mse = mean((test[,21] - rand_gbm_Predictions)^2)
print(rand_gbm_mse)
#Compute MAPE
rand_gbm_mape = MAPE(test[,21], rand_gbm_Predictions)
print(rand_gbm_mape)
###################################################################################################################################
# Grid Search #####################################################################################################################
###################################################################################################################################
#Creat a model with 5 kfold and 1 repeat
control <- trainControl(method="repeatedcv", number=5, repeats=1, search="grid")
set.seed(seed)
tunegrid <- expand.grid(n.trees = seq(2565,2575, by = 2),
                        interaction.depth = c(2:4), 
                        shrinkage = c(0.01,0.02),
                        n.minobsinnode = seq(18,22, by = 2))

gbm_grid <- caret::train(cnt~., data=train, method="gbm", metric="RMSE", trControl=control, tuneGrid=tunegrid)

#Print out model summary
print(gbm_grid)
#PLot model 
#Boosting Iteration Vs RMSE for repeated cross validation is plotted for every combination
#of the grid parameter. It can be noted that the least RMSE is achived when the shrinkage is 0.01
#n.minobsinnode is 18 max tree depth.
plot(gbm_grid)

#Best tune parameters
view_gbm_grid_param = gbm_grid$bestTune
print(view_gbm_grid_param)

#Presdict test data using model
grid_gbm_Predictions = predict(gbm_grid, test[,1:20],n.trees = 2575, interection.depth = 3, shrinkage = 0.01, n.minobsinnode = 18)

#Compute R^2
grid_gbm_r2 = rSquared(test[,21], test[,21] - grid_gbm_Predictions)
print(grid_gbm_r2)
#Compute MSE
grid_gbm_mse = mean((test[,21] - grid_gbm_Predictions)^2)
print(grid_gbm_mse)
#Compute MAPE
grid_gbm_mape = MAPE(test[,21], grid_gbm_Predictions)
print(grid_gbm_mape)

####################################################################################################################################
####################################
###Xtra Gradient Boosting
####################################

library(xgboost)
set.seed(seed)
#Build model
XGB = xgboost(data = as.matrix(train[-21]), label = train$cnt, nrounds = 20)

#Presdict test data output using model
xgb_Predictions = predict(XGB, newdata = as.matrix(test[-21]))

#Compute R^2
xgb_r2 = rSquared(test[,21], test[,21] - xgb_Predictions)
print(xgb_r2)
#Compute MSE
xgb_mse = mean((test[,21] - xgb_Predictions)^2)
print(xgb_mse)
#Compute MAPE
xgb_mape = MAPE(test[,21], y_pred)
print(xgb_mape)
####################################################################################################################################
#Saving results to a dataframe for easy comprehension
######################################################

results = data.frame('Model name'=c('Multivarient linear regression', 'Decision tree default', 'Decision tree Random Search CV', 
                                'Decision tree Grid Search CV', 'Random Forest Default', 'Random Forest Random Search CV', 
                                'Random Forest Grid Search CV', 'Gradient Boosting Default', 'Gradient Boosting Random Search CV',
                                'Gradient Boosting Grid Search CV', 'XGBOOST'), 'MSE'=c(mlr_mse, dt_mse, rand_dt_mse, grid_dt_mse,
                                                                                        rf_mse, rand_rf_mse, grid_rf_mse, gbm_mse, 
                                                                                        rand_gbm_mse, grid_gbm_mse, xgb_mse),
                 'MAPE'=c(mlr_mape, dt_mape, rand_dt_mape, grid_dt_mape, rf_mape, rand_rf_mape, grid_rf_mape, gbm_mape, rand_gbm_mape, 
                          grid_gbm_mape, xgb_mape),
                 'R^2'=c(mlr_r_square, dt_r2, rand_dt_r2, grid_dt_r2, rf_r2, rand_rf_r2, grid_rf_r2, gbm_r2, rand_gbm_r2, 
                          grid_gbm_r2, xgb_r2))
View(results)
#################################################################################################################################################
## From the results dataframe we can see that Random forest default with no tunning has the lowest MAPE, but since it is not pruned
# it cause overfitting. Thus the next best model is chosen which is random forrest gridsearch cv with 5 fold cv and parameter mtry -6.
# The mape for this model in 13.05 and the r^2 value is 0.895 which means it explains approximately 90% of the variance. Hence 
# random forrest gridsearch cv with 5 fold cv is selected as the preferd model.
#################################################################################################################################################
