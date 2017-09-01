# Installation and loading packages
install.packages("h2o")
library(dplyr)
library(caTools)
library(mice)
library(h2o)
library(caret)
library(dplyr)
library(ggplot2)

########################################################################################################///
#                                                                                                      ///
#                                       Reading the data                                              ///
########################################################################################################


#Read the train data
setwd("C:/Users/Aoos/Documents/Uconn 2nd Sem/Data Analytics with R/Project/Prudential")
train<-read.csv("train.csv")

# creating a flag to identify training data
train$Training=1
#read the test data
test<-read.csv("test.csv")
#set the response to NA for test
test$Response=NA
#set a flag for test
test$Training=0

########################################################################################################///
#                                                                                                      ///
#                                       Feature Engineering                                           ///
########################################################################################################

# we merge train and test so that we can process the data together
glob<-rbind(train,test)

##########Missing value imputation #######################

#proportion of missing values
(length(glob[is.na(glob[!names(glob) %in% c("Response") ])]))/(ncol(glob)*nrow(glob))
# we have 5% missing data

#function to give the % of missing values
missing=function(df) 
{
  missing<-sapply(df[!names(df) %in% c("Response")], function(x) (sum(is.na(x))/(length(x)))*100  )
  missing_df<-data.frame(missing)
  missing_df<-subset(missing_df,missing_df$missing>0)
  missing_df<-cbind(rownames(missing_df),missing_df$missing)
  return(missing_df)
}
# computing missing values for our data set
missing(glob)

#Drop medical history 10,15,24 and 32 and Insurance_history_5 as they have many missing values
glob=glob[!names(glob) %in% c("Medical_History_10","Medical_History_15","Medical_History_24","Medical_History_32","Insurance_History_5") ]

#Family history 2 to 5 has many missing values. 
#create a single variable for Family_hist_2,3,4,5 

nas<-apply(temp,1,function(x) sum(is.na(x)))

# add the values in the four columns and divide the sum with the number of non missing values per row in family history
rem_nas<-(4-nas)
aa<-cbind(glob,(rem_nas))
oo<-apply(temp,1, sum,na.rm=T)
aa$combined_family_hist= oo/rem_nas

#drop the original variables from the data
histt_com_total<-aa[!names(aa)  %in% c("Family_Hist_2","Family_Hist_3","Family_Hist_4","Family_Hist_5")]
glob<-histt_com_total
missing(glob)

##################################################################################################
##################             converting our categorical variables to factors

##we will split the glob dataset into 2 parts such that one has categorical and other has numeric and discrete data
abc<-c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", "Employment_Info_6", "Insurance_History_5", 
       "combined_family_hist",
       "Medical_History_1",
       paste("Medical_Keyword_",1:48,sep=""))

new_data<-glob[!names(glob) %in% abc ]

#convert to factors and create data1 to hold categorical data
data1<-data.frame(apply(new_data,2,as.factor))
data1$Response<-NULL
#create data2 which has continuous data
data2<-glob[names(glob) %in% abc]


###############################################################################################################
###################              Missing value imputation

# Now we have Employment_Info_1, Employment_Info_4, Employment_Info_6, combined_family_hist 
# with missing values among continuous variables
# and Medical_History_1 as the discrete variable with missing values

# we will use the MICE package as it assumes that the values are missing completely at random and can hence be predicted 
# by using regression on other variable

# we will use Predictive mean matching function in R since 
# 1) some of our variables with missing values are discrete
# 2) we want to use regression to impute as we can see some correlation in the variables

imputed<- mice(data2,m=1,maxit=1,meth='pmm',seed=500)
completeddata2 <- complete(imputed,1)

# lets see if all the data has been imputed
missing(data2)
missing(completeddata2)

# now that we have imputed the missing vars we merge the two datasets data1 and completeddata2
total_data<-cbind(data1,completeddata2)

missing(total_data)
#this data has no missing values after impuatation


################################################################################################################
###################                      Feature selection

#drop the id column as it has no predictive value
total_data$Id=NULL

# we have limited computational power
# find the categorical variables which have more than 50 levels
for(i in 1:ncol(total_data))
{
  if(is.factor(total_data[,i])  & (length(unique(total_data[,i])) >50))
    cat(colnames(total_data[i]),i,'\n')
}

#Medical_History_2 has 628 levels hence we will drop it
total_data$Medical_History_2=NULL

# we will use our flag for getting the train data back after processing
# this the final model which we will use for training and validating our model
train_new=total_data[total_data$Training==1,]
train_new$Training=NULL

# add the response column back to train
train_new<-cbind(train_new,train$Response)
colnames(train_new)[ncol(train_new)]="Response"

########################################################################################################///
#                                                                                                       ///
#                                       Predictive Modeling using H2o                                   ///
########################################################################################################

#Number of threads -1 means use all cores on your machine ,max memory is 2gb
h2o.init(ip="localhost", port=54321 , max_mem_size = "2g",nthreads = -1) 

# To obtain the Info about the connection, run the below command
h2o.clusterInfo()
# Retrieve information on the status of the cluster running H2O.
h2o.clusterStatus()
# setting a random seed in order to reproduce split
set.seed(144)

#Split the data into train and validation in the ratio 70:30
split=sample.split(train_new$Response,SplitRatio=0.70)
df_train=subset(train_new,split==TRUE)
df_val=subset(train_new,split==FALSE)

str(df_train)
nrow(df_train)
ncol(df_train)
# Transferring the train and validation data frames to H2O JVM 
h2o_train= as.h2o(df_train,destination_frame = "prud_train.hex")
h2o_val= as.h2o(df_val,destination_frame =  "prud_val.hex")

# The h2o virtual machine can be shutted down by using the below command
# However, we would not be able to use the package command any further.
# Hence it is better to run this command when we would close the session.
#h2o.shutdown()

#conerting response variable to factor
h2o_train$Response=h2o.asfactor(h2o_train$Response)
## To obtain column names of a h2o frame, use either one of the below 2 commands
colnames(h2o_val)
names(h2o_train)
## Summary of the datasets
h2o.describe(h2o_train)
h2o.describe(h2o_val)

## h2o.rm("df_train.hex_sid_8d83_1")

# creating indices for response y and predictors 
# Our response is at 119 index of the data frame
x=c(1:118)
y=119  

#Estimating execution time
stime_1k = Sys.time()
#Creating a random forest model with mtries 10 and 500 trees
rf_model = h2o.randomForest(x = x,y = y,training_frame = h2o_train,
                            model_id = 'rf.for.1k', seed = 14,mtries = 10,ntrees = 500)
etime_1k = Sys.time()
runtime=etime_1k-stime_1k
paste("Time to run a 500 Tree Random Forest model: ", round(runtime,4),"minutes")
# 1st run: Time to run a 1000 Tree Random Forest model,mtries= 5:  9.513 minutes  
# 2nd run: Time to run a 500 Tree Random Forest model,mtries= 5 :  4.9426 minutes 
# 3rd run: Time to run a 500 Tree Random Forest model,mtries= 20:  6.5167 minutes 
# 4th run: Time to run a 500 Tree Random Forest model,mtries= 10 : 12.3376 minutes

# mtries:	Number of variables randomly sampled as candidates at each split. 
# If set to -1, defaults to sqrtp for classification and p/3 for regression 
# where p is the # of predictors Defaults to -1.

#Check the model performance on the training data
h2o.performance(model = rf_model)

#Evaluate the model performance on validation data
rand.for.val.perf = h2o.performance(model = rf_model,newdata = h2o_val)
rand.for.val.perf

#34556677888999--------------------------

# Hyparameter tuning using Grid Search and Model Selection with H2o
# Defining the tuning parameters - mtries
rf_pars = list(mtries=seq(5,15,1),ntrees=c(400),max_depth=c(10))
# Capture the start time
time_start_gr = Sys.time()
# Using the h2o.grid function, specify the hyper-parameters to be tuned
rf_grid = h2o.grid(algorithm = "randomForest",grid_id = "rf_grid1",x=x,y=y,training_frame=h2o_train,
                   validation_frame=h2o_val,seed=84,hyper_params = rf_pars)
# Capture the end time
time_end_gr = Sys.time()
runtime=time_end_gr-time_start_gr
paste("Time to run a Random Forest grid model: ", round(runtime,4),"minutes")
# Time to run a Random Forest grid model:  30.5611 minutes
rf_grid
# Sort the resulting models based on the lowest value of Accuracy
rf_perf_grid = h2o.getGrid(grid_id = "rf_grid1",sort_by = 'accuracy',decreasing = TRUE)
#Print all the models developed
print(rf_perf_grid)
# Hyper-Parameter Search Summary: ordered by decreasing accuracy ####
# max_depth mtries ntrees         model_ids           accuracy
# 1         10     15    400 rf_grid1_model_10 0.7012719652966694
# 2         10     14    400  rf_grid1_model_9  0.699727961179325
# 3         10     13    400  rf_grid1_model_8 0.6935519447099479
# 4         10     12    400  rf_grid1_model_7 0.6892140283802661
# 5         10     11    400  rf_grid1_model_6 0.6859789721344018
# 6         10     10    400  rf_grid1_model_5 0.6800970516873759
# 7         10      9    400  rf_grid1_model_4 0.6748768472906403
# 8         10      8    400  rf_grid1_model_3 0.6640688184692303
# 9         10      7    400  rf_grid1_model_2 0.6437761929269907
# 10        10      6    400  rf_grid1_model_1 0.6268656716417911
# 11        10      5    400  rf_grid1_model_0 0.5867215645908389 ####

# Select the best model from the various models developed by the grid search
rf_best_id = rf_perf_grid@model_ids[[1]]
h20.rf_best = h2o.getModel(rf_best_id)
# checking the performance for best model
h2o.performance(h20.rf_best)
#Obtain the Variable importance plot
h2o.varimp_plot(h20.rf_best,num_of_features = 20)

#########################################  Gradient Bossting Model  ##############################################
 
#The first GBM model is using default parameters
start_gbm=Sys.time()
gbm.model.1 <- h2o.gbm(y=y,x=x, training_frame = h2o_train,
                       model_id = "gbm.model.1",
                       seed = 1000)

end_gbm = Sys.time()
runtime=end_gbm-start_gbm
paste("Time to build GBM model1 : ", round(runtime,4),"seconds")
# Time to build GBM model1 :  36.0244 seconds

# checking performance of the model
gbm1_pr=h2o.performance(gbm.model.1,newdata = h2o_val)
gbm1_pr@metrics
# Confusion Matrix: vertical: actual; across: predicted ####
# 1   2 3 4    5    6    7    8  Error             Rate
# 1      285 233 2 1  200  363  226  552 0.8469 =  1,577 / 1,862
# 2      156 423 1 0  246  409  190  541 0.7848 =  1,543 / 1,966
# 3        8  14 3 0   97   92   24   66 0.9901 =    301 / 304
# 4        6   1 0 2    1   77   40  301 0.9953 =    426 / 428
# 5       43 126 0 0  899  290   75  197 0.4485 =    731 / 1,630
# 6      104 104 1 3  130 1235  474 1319 0.6335 =  2,135 / 3,370
# 7       25   5 0 0   15  549  815  999 0.6615 =  1,593 / 2,408
# 8       11   6 1 1   12  181  180 5455 0.0670 =    392 / 5,847
# Totals 638 912 8 7 1600 3196 2024 9430 0.4882 =  8,698 / 17,815 ######

# Hyperparameter tuning
# Specify a grid of parameters which will result in multiple models with all possible combinations of parameters
# Parameters grid-- Total 2*3*2*2= 24 models wil be created 
gbm_params_1 <- list(learn_rate = c(0.01, 0.001),
                     max_depth = c(4, 5, 6),
                     col_sample_rate = c(0.3, 0.5))

# Specified optimum number of trees = 1000 from above analysis
start_gbm_grid=Sys.time()
gbm_grid.1 <- h2o.grid("gbm", x = indep.var, y = dep.var,
                       grid_id = "gbm_grid.1",
                       training_frame = h2o_train,
                       validation_frame = h2o_val,
                       ntrees = 100,
                       seed = 1000,
                       sample_rate=0.7,
                       hyper_params = gbm_params_1)

end_gbm_grid = Sys.time()
runtime=end_gbm_grid-start_gbm_grid
paste("Time to build grid GBM models: ", round(runtime,3),"minutes")
# Run 1 : Time to build grid GBM models:  7.006 minutes
# Run 2 : Time to build grid GBM models:  7.983 minutes
# Run 3 : Time to build grid GBM models:  8.010 minutes
summary(gbm_grid.1)
# Get all model performance metrics in increasing order of the Accuracy
gbm_gridperf.1 <- h2o.getGrid(grid_id = "gbm_grid.1", 
                              sort_by = "accuracy", 
                              decreasing = T)
print(gbm_gridperf.1)
gbm_best_id = gbm_gridperf.1@model_ids[[1]]
h20.gbm_best = h2o.getModel(gbm_best_id)
h2o.performance(h20.gbm_best)
# Get best model from the list of all tried models
best_gbm_model <- gbm_gridperf.1@model_ids[[1]]
best_gbm <- h2o.getModel(best_gbm_model)
summary(best_gbm)
# creating a variable importance plot 
h2o.varimp_plot(best_gbm,num_of_features = 20)

# Confusion Matrix: vertical: actual; across: predicted ####
# 1       2   3 4 5    6    7    8  Error             Rate
# 1      237 222 0 0  214  357  246  586 0.8727 =  1,625 / 1,862
# 2      122 441 0 0  244  389  197  573 0.7757 =  1,525 / 1,966
# 3        5  13 0 0  100   89   24   73 1.0000 =      304 / 304
# 4        3   1 0 0    1   70   42  311 1.0000 =      428 / 428
# 5       29 128 0 0  900  299   74  200 0.4479 =    730 / 1,630
# 6       88  98 0 0  141 1239  435 1369 0.6323 =  2,131 / 3,370
# 7       17   1 0 0   17  591  738 1044 0.6935 =  1,670 / 2,408
# 8        2   2 0 0   13  184  141 5505 0.0585 =    342 / 5,847
# Totals 503 906 0 0 1630 3218 1897 9661 0.4914 = 8,755 / 17,815 ####

# We can see that the Random forest model outperforms(higher accruacy) the GBM model 
# so we choose it as the final model
# Best random model parameters:
#    max_depth mtries  ntrees         model_ids   accuracy
#          10     15    400  rf_grid1_model_10      0.701

# removing objects from the R environment
rm(list=ls())

# shutting down the H2o machine with shutdown()
h2o.shutdown()
Y

## Additional plots using ggplot2 ; other plots are created by Shiny App
#Donut Chart
# Creating a data frame to create the chart; the count of each risk category is taken for count
dat = data.frame(count=c(6207,
                         6552,
                         1014,
                         1428,
                         5432,
                         11233,
                         8027,19489), category=c("1", "2", "3", "4", "5", "6", "7", "8"))
#Displaying the data frame
dat
# Creating the Variables of the data frame
dat$fraction = dat$count / sum(dat$count)
dat = dat[order(dat$fraction), ]
dat$ymax = cumsum(dat$fraction)
dat$ymin = c(0, head(dat$ymax, n=-1))

attach(dat)
#Displaying the variables
fraction
dat
ymax
ymin
# Creating the Donut Chart
p1 = ggplot(dat, aes(fill=category, ymax=ymax, ymin=ymin, xmax=9, xmin=4)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0,9)) +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  annotate("text", x = 0, y = 0, label = "Response") +
  labs(title="Donut Chart") + geom_label(aes(label=paste(round((fraction*100),2)), x=3.5,y=(ymin+ymax)/2),
                                         inherit.aes = TRUE, show.legend = FALSE)
#Displaying the graph
p1 

#Circular Barplot -the count of each risk category is taken for values
data=data.frame(group=c("1 ","2 ","3 ","4 ","5 ","6 ","7 ","8 ") , value=c(6207,
                                                                           6552,
                                                                           1014,
                                                                           1428,
                                                                           5432,
                                                                           11233,
                                                                           8027,
                                                                           19489) )
ggplot(data, aes(x = group, y = value ,fill = group)) + 
  geom_bar(width = 0.85, stat="identity") +    
  
  # To use a polar plot and not a basic barplot
  coord_polar(theta = "y") +    
  
  #Remove useless labels of axis
  xlab("") + ylab("") +
  
  #Increase ylim to avoid having a complete circle
  ylim(c(0,75)) + 
  
  #Add group labels close to the bars :
  geom_text(data = data, hjust = 1, size = 3, aes(x = group, y = 0, label = group)) +
  
  #Remove useless legend, y axis ticks and y axis text
  theme(legend.position = "none" , axis.text.y = element_blank() , axis.ticks = element_blank())


# Usual bar plot :
ggplot(data, aes(x = group, y = value ,fill = group )) + 
  geom_bar(width = 0.85, stat="identity")

# Circular barplot one
g= ggplot(data, aes(x = group, y = value ,fill = group)) + 
  geom_bar(width = 0.85, stat="identity") +    
  
  # To use a polar plot and not a basic barplot
  coord_polar(theta = "y") +    
  
  #Remove useless labels of axis
  xlab("") + ylab("") +
  
  #Add group labels close to the bars :
  geom_text(data = data, hjust = 1, size = 3, aes(x = group, y = 0, label = group)) + xlab("")
# displaying the graph
g

### End of Plots

## End of script #####
