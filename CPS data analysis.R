setwd("~/sub_directory1")
getwd()


library(tidyverse)
library(scales)
install.packages("viridis")
library(viridis)
install.packages("hrbrthemes")
library(hrbrthemes)
library(reshape2)
library(ggplot2)
install.packages("readr")
library(readr)
library(dplyr)
install.packages("skimr")
library(skimr)
install.packages("devtools")
library(devtools)
devtools::install_github("ropensci/visdat")
library(visdat)

install.packages("e1071")
install.packages("caTools")
install.packages("class")



#loading the data,because we have multiple files we will list all files in a sub-directory and import from there as a single file
fileList <- list.files(
  pattern="*.csv$",
  recursive=TRUE,
  full.name=TRUE,
)


#library used for this is tidyverse
df <-do.call(rbind, lapply(fileList[1:42], function(f){
  dat.fl <- read.csv(f) %>% 
    mutate(filename = f)
}))

#this gives a preview of the data and its data characteristics using the dplyr library
glimpse(df)
dim(df)
View(df)
summary(df[5,])
#library skimr
skim(df)
#library devtools(this show the visualization of the distribution of data, type of data and characteristics of the data type)
vis_miss(df)
vis_dat(df)

#Data cleaning


#Before attempting to clean the data we should get insights on the size of the data and determine if we wont run into RAM shortage during the course of our analysis
object.size(df)
str(df)



#create a function to Extract text from right hand side to enable us extract the date and year from the file name
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#See unique file names
head(df)


#Get the year out Extract the Month and Year variables from the file name
df_new <-df %>% 
  mutate(Year = substr(substrRight(filename, 8), 1, 4)) %>%
  mutate(filename = tolower(filename)) %>% 
  mutate(filename =  gsub("principal_offence_category_", "", filename)) %>% 
  separate(filename, c("Month", "Year2"), sep = "_") %>% 
  mutate(Year = gsub(".csv", "", Year)) %>% 
  mutate(Month = substring(Month, 8)) %>% 
  select(-Year2) %>% 
  rename(location = X)

#preview columns
head(df_new)


#Clean column names by creating a function for cleaning strings
clean_names <- function(.data, unique = FALSE) {
  n <- if (is.data.frame(.data)) colnames(.data) else .data
  
  n <- gsub("%+", "_pct_", n)
  n <- gsub("\\$+", "_dollars_", n)
  n <- gsub("\\++", "_plus_", n)
  n <- gsub("-+", "_minus_", n)
  n <- gsub("\\*+", "_star_", n)
  n <- gsub("#+", "_cnt_", n)
  n <- gsub("&+", "_and_", n)
  n <- gsub("@+", "_at_", n)
  
  n <- gsub("[^a-zA-Z0-9_]+", "_", n)
  n <- gsub("([A-Z][a-z])", "_\\1", n)
  n <- tolower(trimws(n))
  
  n <- gsub("(^_+|_+$)", "", n)
  
  n <- gsub("_+", "_", n)
  
  if (unique) n <- make.unique(n, sep = "_")
  
  if (is.data.frame(.data)) {
    colnames(.data) <- n
    .data
  } else {
    n
  }
}


#Clean column names
df_clean <- df_new %>% 
  clean_names()

#head
head(df_clean)

glimpse(df_clean)


#check the dimension of our dataset to see if our changes where reflected
dim(df_clean)


#more cleaning needs to be done on There are a few issues with the data including missing values, incorrect data types as some numerical columns are represented as characters.

#first off Select all percentage columns Remove all the % signs from the percentage columns and then convert to numerical column
perc <- df_clean %>%
  select(year, month, location, contains("percentage")) %>% #select Columns having percentage
  pivot_longer(cols = starts_with("percentage"), #Pivot 
               names_to = "offence_type_percent",
               values_to = "percent_values") %>% 
  mutate(percent_values = as.double(gsub("%", "", percent_values))) #remove the '%' and convert to numerical type


#Select all number columns and then remove the ',' in values and then convert to numerical
#Select all number columns and clean
num <-df_clean %>%
  select(year, month, location, contains("number")) %>% #select Columns having numbers
  mutate(across(starts_with("number"), as.character)) %>% #convert all number columns to character first
  pivot_longer(cols = starts_with("number"), #pivot
               names_to = "offence_type_number",
               values_to = "number_values") %>% 
  mutate(number_values = as.numeric(gsub(",", "", number_values))) %>% #remove the ',' and then convert to number
  select(-year, -month, -location) #drop columns that are not needed

#After cleaning the percent and number columns, combine then into a single dataframe
#Bind
df_complete <- cbind(perc, num)

#preview
head(df_complete)

glimpse(df_complete)


dim(df_complete)


#Exploratory data analysis
#After cleaning the data, we will explore using correlation heatmap to explore the relationship within the dataset variables with conviction values.
#Select only number columns
data <-df_clean %>% 
  select(location, contains("number")) %>% 
  mutate_at(vars(everything()),~ str_replace(., ",", "")) %>% 
  mutate(across(starts_with("number"), as.numeric)) %>% 
  select(contains("convictions"))

cor(data) %>% melt() %>% 
  ggplot(., aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "grey", high = "darkred") +
  geom_text(aes(Var1, Var2, label = round(value,2)), size = 2)+
  labs(title = "Correlation Matrix", x = "Numeric column", y = "Numeric Column",
       fill = "Coefficient Range") +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 90),
        plot.title = element_text(face = "bold", hjust = 0.5))

#There exist multicolinearity within values in the dataframe (conviction variables). We will chose a few columns and explore if there exist enough information to predict relationships.
#based on the corelations observed we explore more with visualiztion
#relationship between sexual offenders and violent crimes based on convictions




################
# exploratory data analysis include code for box plot
################

library(readxl)
data2 <- read_excel('maindf.xlsx')



# data exploration plots
## drop the national column 
maindata <- data2[ !(data2$location == "National"), ]
summary(maindata)


#######
# Convert the entire data frame to numeric characters 
#######


removed_comma <- function(i){
  gsub(",","",i)
}

maindata<-as.data.frame(lapply(maindata,removed_comma))



i <- c(2, 50)

maindata[,i] <- apply(maindata[,i],2,
                      function(x)as.numeric(x))

glimpse(maindata)



#########
# Plot box for independent variables
#########


# Sexual offence

class(maindata$number_of_sexual_offences_convictions)


maindata$number_of_sexual_offences_convictions <- as.numeric(as.character(maindata$number_of_sexual_offences_convictions))  # Convert one variable to numeric


ggplot(maindata, aes(x= location, y= number_of_sexual_offences_convictions)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(vjust=0.5, angle=90))+
  labs(title = "Sexual Offence Convictions", x = "Location", y = "Sexual Offence Convictions",
  ) 
ggsave("sexualoffence.png", dpi = 1000, width = 10, height = 8)


# Scatter plot (edit)

ggplot(maindata, mapping = aes(x= number_of_offences_against_the_person_convictions , y= number_of_sexual_offences_convictions, colour = year )) + 
  geom_point(alpha = .6)+
  theme_bw()+
labs(title = "Physical Harm against Person", x = "Homicide Convictions", y = "Sexual Offence Convictions",
) 
ggsave("sexualoffence.png", dpi = 1000, width = 10, height = 8)




# Burglary

class(maindata$number_of_burglary_convictions)


maindata$number_of_burglary_convictions <- as.numeric(as.character(maindata$number_of_burglary_convictions))  # Convert one variable to numeric


ggplot(maindata, aes(x= location, y= number_of_burglary_convictions)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(vjust=0.5, angle=90))+
  labs(title = "Burglary Convictions", x = "Location", y = "Burglary Convictions",
  ) 
ggsave("burglary.png", dpi = 1000, width = 10, height = 8)



# Robbery

class(maindata$number_of_robbery_convictions)


maindata$number_of_robbery_convictions <- as.numeric(as.character(maindata$number_of_robbery_convictions))  # Convert one variable to numeric


ggplot(maindata, aes(x= location, y= number_of_robbery_convictions)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(vjust=0.5, angle=90))+
  labs(title = "Robbery Convictions", x = "Location", y = "Robbery Convictions",
  ) 
ggsave("robbery.png", dpi = 1000, width = 10, height = 8)


# theft and handling

class(maindata$number_of_theft_and_handling_convictions)


maindata$number_of_theft_and_handling_convictions <- as.numeric(as.character(maindata$number_of_theft_and_handling_convictions))  # Convert one variable to numeric


ggplot(maindata, aes(x= location, y= number_of_theft_and_handling_convictions)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(vjust=0.5, angle=90))+
  labs(title = "Theft and Handling Convictions", x = "Location", y = "Theft and handling Convictions",
  ) 
ggsave("theft and handling.png", dpi = 1000, width = 10, height = 8)



# homicide
class(maindata$number_of_homicide_convictions)

maindata$number_of_homicide_convictions <- as.numeric(as.character(maindata$number_of_homicide_convictions))  # Convert one variable to numeric

ggplot(data = final_data, aes(x = location, y = number_of_homicide_convictions))+
  geom_boxplot(fill = "slateblue", alpha = 0.2)+
  xlab("location")

#offences against person

class(maindata$number_of_offences_against_the_person_convictions)



ggplot(maindata, aes(x= location, y= number_of_offences_against_the_person_convictions)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(vjust=0.5, angle=90))+
  labs(title = "Offence against persons", x = "Location", y = "Offence against Person Conviction",
  ) 
ggsave("offenceperson.png", dpi = 1000, width = 10, height = 8)



##########
# End ofexploratory data analysis, drill down to sub group analysis
#########



#In this case, we will select
#- Number of homicide convictions
#- Number of Burglary convictions
#- Number of theft and handling convictions
#- Number of robbery convictions

#All these involve some form of theft, or motivated with an end goal of theft. 


#Select  homicide , burglary, robbery, theft columns
data %>% 
  select(number_of_burglary_convictions, 
         number_of_robbery_convictions, 
         number_of_theft_and_handling_convictions, 
         number_of_homicide_convictions) %>% 
  cor() %>% 
  melt() %>% 
  ggplot(., aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "grey", high = "darkred") +
  geom_text(aes(Var1, Var2, label = round(value,2)), size = 2)+
  labs(title = "Correlation Matrix", x = "Numeric column", y = "Numeric Column",
       fill = "Coefficient Range") +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 90),
        plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("Correlation Sub group.png", dpi = 1000, width = 10, height = 8)



#Finally, we will select these coluns as our choice variables

#Select columns and clean
final_data <-df_clean %>% 
  select(location, year, month, number_of_burglary_convictions, 
         number_of_robbery_convictions, 
         number_of_theft_and_handling_convictions, 
         number_of_homicide_convictions) %>% 
  mutate_at(vars(everything()),~ str_replace(., ",", "")) %>% 
  mutate(across(starts_with("number"), as.numeric))
#write data to csv
final_data %>% 
  write.csv("final_data.csv", row.names = F)

dim(final_data)

summary(final_data)


glimpse(final_data)


#read final cleaned data
df <- read.csv("final_data.csv")

#Gather data
df_long <- df %>% pivot_longer(cols = starts_with("number"),
                               names_to = "offence_type",
                               values_to = "values")

#head
ggplot(df_long, aes(x=values)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1,
               outlier.size=1) +
  facet_wrap(~offence_type, scales = "free") +
  ggtitle("Boxplot for Outlier Detection") +
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"))

ggsave("outliers.png", dpi = 1000, width = 10, height = 8)

#There are clearly outliers within the variables. We will clip the values by removing all values that lie beyond the 90th percentile.

#Remove outliers
df_outlier <- df_long %>% 
  group_by(offence_type) %>% 
  mutate(q99 = quantile(values, 0.90), row = row_number()) %>% 
  arrange(location, year, month, offence_type) %>% 
  filter(values <= q99) %>% 
  select(-q99) 


#Make a faceted box plot
ggplot(df_outlier, aes(x=values)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1,
               outlier.size=1)  +
  facet_wrap(~offence_type, scales = "free")
ggsave("boxplotwithout.png", dpi = 1000, width = 10, height = 8)



#Make histogram
ggplot(df_outlier, aes(x=values)) + 
  geom_histogram(binwidth = 20)  +
  facet_wrap(~offence_type, scales = "free") + ggtitle("Histogram for Outlier Detection") +
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"))
ggsave("histogram.png", dpi = 1000, width = 10, height = 8)


############
# hypothesis
##########


#Distribution of offence types across years
df_outlier %>% 
  group_by(year, offence_type) %>% 
  summarise(total_convictions = sum(values))%>%   # this is telling the computer to sum the total convictions by year
  ggplot(., aes(year,total_convictions,  fill = offence_type)) +
  geom_bar(stat = "identity") + 
  ggtitle("Offence Type Summary across years") +
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
        legend.position = "bottom")

ggsave("subgroup by year.png", dpi = 1000, width = 10, height = 8)



#Offence types across months and years
df_outlier %>% 
  mutate(month = factor(month, levels =tolower(month.name))) %>% 
  group_by(year, month) %>% 
  summarise(total_convictions = sum(values))%>% 
  drop_na() %>% 
  ggplot(., aes(month,total_convictions, fill = as.character(year))) +
  geom_bar(stat = "identity") +
  theme(legend.position = "bottom") +
  labs(fill = "Year") + 
  ggtitle("Offence Type Summary across Months") +
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
        legend.position = "bottom")


ggsave("subgroup by month.png", dpi = 1000, width = 10, height = 8)



#Offence trends across the years
df_outlier %>% 
  mutate(month = factor(month, levels =tolower(month.name))) %>% 
  group_by(year,month, offence_type) %>% 
  summarise(n = sum(values)) %>% 
  ungroup() %>% 
  mutate(rown = row_number()) %>% 
  mutate(date = as.Date(paste(year,as.integer(month), 01), "%Y %m %d")) %>% 
  ggplot(., aes(date, n)) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~offence_type, scales = "free") + 
  ggtitle("Offence Type Trend (2014 - 2018)") +
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
        legend.position = "bottom")

ggsave("offence trend.png", dpi = 1000, width = 10, height = 8)


#Offences summary across different location
df_outlier %>% 
  group_by(location, offence_type) %>% 
  summarise(total_convictions = sum(values))%>% 
  drop_na() %>% 
  ggplot(., aes(reorder(location, total_convictions), total_convictions, fill = offence_type)) +
  geom_bar(stat = "identity", position = "fill") +
  coord_flip() +
  #scale_x_continuous(breaks = round(seq(0, 24, by = 2),0), expand = c(0,0)) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0))+
  theme(legend.position = "none") + 
  ggtitle("Total Convictions across location") +
  xlab("Location") +
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
        legend.position = "bottom")

ggsave("offence summary by loc.png", dpi = 1000, width = 10, height = 8)



#average per Locations with highest average numbers in convictions variables
df_outlier %>% 
  group_by(location, offence_type) %>% 
  summarise(total_convictions = mean(values))%>% 
  drop_na() %>% 
  ggplot(., aes(reorder(location, total_convictions), total_convictions, fill = offence_type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(legend.position = "none") + 
  ggtitle("Average Convictions across location") +
  xlab("Location") +
  theme(plot.title = element_text(hjust = 0.5, size = 17, face = "bold"),
        legend.position = "bottom")
#scale_x_continuous(breaks = round(seq(0, 24, by = 2),0), expand = c(0,0)) +
#sscale_y_continuous(labels = scales::percent, expand = c(0, 0))

ggsave("offence summary by loc.png", dpi = 1000, width = 10, height = 8)



#################
#] hypothesis modeling
################


#We will model a relationship where out hypothesis will be that: Homicide convictions can be predicted by robbery, theft abd burglary convictions

df %>% names() #view the variables present in the dataset


library(caTools)
set.seed(42) # set a vixed point for randomisation

#df
ml_df <-df %>% select(4:7) #assign our machine learning data set

summary(ml_df)

glimpse(ml_df)

view(ml_df)

# Train/Test split in 70:30 ratio
sample_split <- sample.split(Y = ml_df$number_of_homicide_convictions, SplitRatio = 0.7)
train_set <- subset(x = ml_df, sample_split == TRUE)
test_set <- subset(x = ml_df, sample_split == FALSE)

# Fit the model and obtain summary
model <- lm( number_of_homicide_convictions~ ., data = train_set)

summary(model)

model

# Get residuals
lm_residuals <- as.data.frame(residuals(model))


view(lm_residuals)

summary(lm_residuals)
# plot a histogram of the residuals Visualize residuals
ggplot(lm_residuals, aes(residuals(model))) +
  geom_histogram(fill = "#0099f9", color = "black") +
  theme_classic() +
  labs(title = "Residuals plot")

ggsave("hist lm_residual.png", dpi = 1000, width = 10, height = 8)


# prediction of Homicide conviction

pred <- predict(model, test_set)
actual  <- test_set$number_of_homicide_convictions

view(actual)

summary(actual)

summary(pred)



# Evaluate model
mse <- mean((pred - actual)^2)
rmse <- sqrt(mse)

mse()
rmse()


##########
# clustering model
#########


#Using the elbow curve, we will find out the optimal number of centroids. The largest decline in the curve is seen between 1 and 2. Which implies most of the points  can be put in two groups.

tot.withinss <- vector(mode="character", length=10)
for (i in 1:10){
  cluster <- kmeans(ml_df[,1:4], center=i, nstart=20)
  tot.withinss[i] <- cluster$tot.withinss
}

plot(1:10, tot.withinss, type="b", pch=19)


# We will create a cluster of these variables usng 2 centroids to initiate this

con_cluster <- kmeans(ml_df[1:4], centers = 2, nstart = 50)

con_cluster

con_cluster$centers


con_cluster$withinss

# view the cluster by location

table(con_cluster$cluster, df$location)

# visualization of the cluster

#kmeans$

library(cluster)
clusplot(ml_df, con_cluster$cluster, color=T, shade=T, labels=0, lines=0)





################
# classification model
###############



library(e1071)
library(caTools)
library(class)


#############
#############

ml_df


set.seed(49)


library(caTools)



spl = sample.split(ml_df$number_of_homicide_convictions, SplitRatio = 0.7)
train = subset(ml_df, spl==TRUE)
test = subset(ml_df, spl==FALSE)


classifier_knn <- knn(train = train,
                      test = test,
                      cl = train$number_of_homicide_convictions,
                      k = 1)

classifier_knn



# Confusiin Matrix
cm <- table(test$number_of_homicide_convictions, classifier_knn)

cm


# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test$number_of_homicide_convictions)
print(paste('Accuracy =', 1-misClassError))

# K = 3
classifier_knn <- knn(train = train,
                      test = test,
                      cl = train$number_of_homicide_convictions,
                      k = 3)

classifier_knn

misClassError <- mean(classifier_knn != test$number_of_homicide_convictions)
print(paste('Accuracy =', 1-misClassError))

# K = 5
classifier_knn <- knn(train = train,
                      test = test,
                      cl = train$number_of_homicide_convictions,
                      k = 5)
misClassError <- mean(classifier_knn != test$number_of_homicide_convictions)
print(paste('Accuracy =', 1-misClassError))

# K = 7

classifier_knn <- knn(train = train,
                      test = test,
                      cl = train$number_of_homicide_convictions,
                      k = 7)
classifier_knn

misClassError <- mean(classifier_knn != test$number_of_homicide_convictions)
print(paste('Accuracy =', 1-misClassError))

# K = 15
classifier_knn <- knn(train = train,
                      test = test,
                      cl = train$number_of_homicide_convictions,
                      k = 15)
misClassError <- mean(classifier_knn != test$number_of_homicide_convictions)
print(paste('Accuracy =', 1-misClassError))

# K = 19
classifier_knn <- knn(train = train,
                      test = test,
                      cl = train$number_of_homicide_convictions,
                      k = 19)
misClassError <- mean(classifier_knn != test$number_of_homicide_convictions)
print(paste('Accuracy =', 1-misClassError))


# k = 50
classifier_knn <- knn(train = train,
                      test = test,
                      cl = train$number_of_homicide_convictions,
                      k = 14)
misClassError <- mean(classifier_knn != test$number_of_homicide_convictions)
print(paste('Accuracy =', 1-misClassError))

#the accuracy is not impressive




###############
#hyperfitting the model to improve the accuracy using glm
##############

#Add the cluster results to our ml_df dataset as the classification labels


#We can build a classification model that predicts which zone of incidence (High or low) homicide convictions will be.

#the clustering results shows to regions of concentrations in the cluster, where 0 = low and 1 shows regions of high occurence. We can build a classification model off of the back of the result generation


ml_df$Rate <- con_cluster$cluster

# Adding our cluster to the machinelearning data as a new column
ml_df$Rate

#relabel homicide convictions to represent the clusters

ml_df <- ml_df %>% 
  mutate(Rate = if_else(Rate == 1, 0, 1)) %>% #0 refers to regions with low features concentrations and 1 represent high incident regions
  select(-number_of_homicide_convictions)


ml_df

summary(ml_df)
#Classification model
set.seed(100)


# split the data


spl = sample.split(ml_df$Rate, SplitRatio = 0.7)
train = subset(ml_df, spl==TRUE)
test = subset(ml_df, spl==FALSE)

model_glm = glm(Rate ~ . , family="binomial", data = train)
summary(model_glm)


#Predictions on the test set
predictTest = predict(model_glm, newdata = test, type = "response")

# Confusion matrix on test set
table(test$Rate, predictTest >= 0.5)

#Accuracy score
542/nrow(test) 


#################















































