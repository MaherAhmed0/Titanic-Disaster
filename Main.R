##################################
###------- Import Data --------###
##################################
Train_Data <- read.csv("train.csv", stringsAsFactors = F)
Test_Data  <- read.csv("test.csv", stringsAsFactors = F)

Test_Data$Survived <- NA

##################################
###------ Exploring Data ------###
##################################
cat("Dataset shape:(", dim(Train_Data),")")
cat("Dataset shape:(", dim(Test_Data),")")

print("Data stucture:")
str(Train_Data)
str(Test_Data)

names(Train_Data)
names(Test_Data)

summary(Train_Data)
summary(Test_Data)

hist(Train_Data$Survived)

print("Missing or null values column wise")
sapply(Data,function(x) sum(is.na(x)))
sapply(Test_Data,function(x) sum(is.na(x)))

NA_Cols <- which(colSums(is.na(Train_Data)) > 0)
sort(colSums(sapply(Train_Data[NA_Cols], is.na)), decreasing = TRUE)
cat('There are', length(NA_Cols), 'columns with missing values')

NA_Cols_t <- which(colSums(is.na(Test_Data)) > 0)
sort(colSums(sapply(Test_Data[NA_Cols_t], is.na)), decreasing = TRUE)
cat('There are', length(NA_Cols_t), 'columns with missing values')

##################################
###---- Analyzing features ----###
##################################
pclass_target <-Train_Data %>% 
  group_by(Pclass) %>%
  summarize(mean_survived = mean(Survived)) %>%
  arrange(desc(mean_survived))

Sex_target <-Train_Data %>% 
  group_by(Sex) %>%
  summarize(mean_survived = mean(Survived)) %>%
  arrange(desc(mean_survived))

SibSp_target <-Train_Data %>% 
  group_by(SibSp) %>%
  summarize(mean_survived = mean(Survived)) %>%
  arrange(desc(mean_survived))

Parch_target <-Train_Data %>% 
  group_by(Parch) %>%
  summarize(mean_survived = mean(Survived)) %>%
  arrange(desc(mean_survived))

take.off_target <- Train_Data %>% 
  group_by(take.off) %>%
  summarize(mean_survived = mean(Survived)) %>%
  arrange(desc(mean_survived))

ggplot(Data, aes(x=Age, fill=factor(Survived))) + 
  geom_histogram(alpha=0.5, bins=20) + 
  facet_grid(Pclass~.) + 
  labs(x="Age", y="Count") + 
  theme_bw()

Numeric_Data <- which(sapply(Train_Data, is.numeric)) #index vector numeric variables
cat('There are', length(Numeric_Data), 'numeric variables')

All_Num_Data <- Train_Data[, Numeric_Data]
Corr_Num_Data <- cor(All_Num_Data, use="pairwise.complete.obs")

Data_Sorted <- as.matrix(sort(Corr_Num_Data[,'Survived'], decreasing = TRUE))

# Select only high correlations
High_Corr <- names(which(apply(Data_Sorted, 1, function(x) abs(x) > 0.1)))
Corr_Table <- Corr_Num_Data[High_Corr, High_Corr]

C <- cor(Corr_Table)

corrplot(C, method="circle")

corrplot(C, method="pie")

corrplot(C, method="number")
##################################
###------ Cleaning Data -------###
##################################
Data <- Train_Data
##########################  Name  ##########################
# Grab title from passengers names
Data$Title <- gsub('(.*, )|(\\..*)', '', Data$Name)
Test_Data$Title <- gsub('(.*, )|(\\..*)', '', Test_Data$Name)

table(Data$Sex, Data$Title)

Rare_Title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 
                'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

Data$Title[Data$Title == 'Mlle'] <- 'Miss' 
Data$Title[Data$Title == 'Ms'] <- 'Miss'
Data$Title[Data$Title == 'Mme'] <- 'Mrs' 
Data$Title[Data$Title %in% Rare_Title]  <- 'Rare Title'

Test_Data$Title[Test_Data$Title == 'Mlle'] <- 'Miss' 
Test_Data$Title[Test_Data$Title == 'Ms'] <- 'Miss'
Test_Data$Title[Test_Data$Title == 'Mme'] <- 'Mrs' 
Test_Data$Title[Test_Data$Title %in% Rare_Title]  <- 'Rare Title'

# Show title counts by sex
table(Data$Sex, Data$Title)

Data$Title <- as.integer(factor(Data$Title))
Test_Data$Title <- as.integer(factor(Test_Data$Title))

##########################  Sex  ##########################
Data$Sex <- ifelse(Data$Sex == 'female', 1, 0)
Test_Data$Sex <- ifelse(Test_Data$Sex == 'female', 1, 0)

##########################  Age  ##########################
Data$Age <- as.integer(Data$Age)
Test_Data$Age <- as.integer(Test_Data$Age)

set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_model <- mice(Data, method='rf')
mice_model_t <- mice(Test_Data, method='rf')

Data <- complete(mice_model)
Test_Data <- complete(mice_model_t)

Data$Age_Band <- cut(Data$Age, breaks = 5)
Test_Data$Age_Band <- cut(Test_Data$Age, breaks = 5)

Data$Age <- ifelse(Data$Age <= 16, 0,
                      ifelse(Data$Age > 16 & Data$Age <= 32, 1,
                             ifelse(Data$Age > 32 & Data$Age <= 48, 2,
                                    ifelse(Data$Age > 48 & Data$Age <= 64, 3, 4))))
Test_Data$Age <- ifelse(Test_Data$Age <= 16, 0,
                   ifelse(Test_Data$Age > 16 & Test_Data$Age <= 32, 1,
                          ifelse(Test_Data$Age > 32 & Test_Data$Age <= 48, 2,
                                 ifelse(Test_Data$Age > 48 & Test_Data$Age <= 64, 3, 4))))

##########################  SibSp & Parch  ##########################
Data$Family_Size <- Data$SibSp + Data$Parch + 1
Test_Data$Family_Size <- Test_Data$SibSp + Test_Data$Parch + 1

Data$Family_Size[Data$Family_Size == 1] <- 0
Test_Data$Family_Size[Test_Data$Family_Size == 1] <- 0

Data$Is_Alone <- 1
Test_Data$Is_Alone <- 1

Data$Is_Alone[Data$Family_Size >= 1] <- 0
Test_Data$Is_Alone[Test_Data$Family_Size >= 1] <- 0

##########################  take.off  ##########################
mode_value <- names(which.max(table(Data$take.off)))
mode_value_t <- names(which.max(table(Test_Data$take.off)))

Data$take.off <- ifelse(Data$take.off %in% c("", NA), mode_value, Data$take.off)
Test_Data$take.off <- ifelse(Test_Data$take.off %in% c("", NA), mode_value, Test_Data$take.off)

Data$take.off <- ifelse(Data$take.off == 'S', 0,
                        ifelse(Data$take.off == 'C', 1,
                               ifelse(Data$take.off == 'Q', 2, NA)))

Test_Data$take.off <- ifelse(Test_Data$take.off == 'S', 0,
                        ifelse(Test_Data$take.off == 'C', 1,
                               ifelse(Test_Data$take.off == 'Q', 2, NA)))

##########################  Age_Class  ##########################
Data$Age_Class <- Data$Age * Data$Pclass
Test_Data$Age_Class <- Test_Data$Age * Test_Data$Pclass

##########################  Fare  ##########################
Data$FareBand <- as.integer(cut(Data$Fare, breaks = quantile(Data$Fare, probs = seq(0, 1, 0.25)),
                                include.lowest = TRUE))

Test_Data$FareBand <- as.integer(cut(Test_Data$Fare,
                                breaks = quantile(Test_Data$Fare, probs = seq(0, 1, 0.25)),
                                include.lowest = TRUE))

Data$Fare <- ifelse(Data$Fare <= 7.91, 0, 
                       ifelse(Data$Fare <= 14.454, 1, 
                              ifelse(Data$Fare <= 31, 2, 3)))

Test_Data$Fare <- ifelse(Test_Data$Fare <= 7.91, 0, 
                    ifelse(Test_Data$Fare <= 14.454, 1, 
                           ifelse(Test_Data$Fare <= 31, 2, 3)))

###################################################################################
rm(Train_Data)

Data <- Data[, !(colnames(Data) %in% c("PassengerId","Name","Ticket","Family_Size", "FareBand",
                                       "Cabin","SibSp","Parch","Age_Band"))]
Test_Data <- Test_Data[, !(colnames(Test_Data) %in% c("Survived","Name","Ticket","Family_Size",
                                                      "FareBand","Cabin","SibSp","Parch","Age_Band"))]

##################################
### -------- Modeling -------- ###
##################################
set.seed(754)

rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + 
                           Fare + take.off + Title + Age_Class +
                           Is_Alone, data = Data)

tree_model <- rpart(factor(Survived) ~ Pclass + Sex + Age + 
                      Fare + take.off + Title + Age_Class +
                      Is_Alone, data = Data, method = "class")


prediction_rf <- predict(rf_model, Test_Data)

prediction_dt <- predict(tree_model, Test_Data, type = "class")

# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

plot(tree_model)
text(tree_model)

# Save the solution to a data frame with two columns: Passenger Id and Survived (prediction)
solution_rf <- data.frame(PassengerID = Test_Data$PassengerId, Survived = prediction_rf)
solution_dt <- data.frame(PassengerID = Test_Data$PassengerId, Survived = prediction_dt)

# Write the solution to file
write.csv(solution_rf, file = 'RF_model.csv', row.names = F)
write.csv(solution_dt, file = 'DT_model.csv', row.names = F)

Train <- Data[1:891,]
Test <- Data[892:1309,]

