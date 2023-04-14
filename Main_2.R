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

##################################
###------ Cleaning Data -------###
##################################
Data <- Train_Data[, !(colnames(Train_Data) %in% c("PassengerId","Ticket"))]
Test_Data <- Test_Data[, !(colnames(Test_Data) %in% c("Ticket"))]

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

Data <- Data[, !(colnames(Data) %in% c("Name"))]
Test_Data <- Test_Data[, !(colnames(Test_Data) %in% c("Name", "Survived"))]

##########################  Sex  ##########################
Data$Sex <- as.integer(factor(Data$Sex))
Test_Data$Sex <- as.integer(factor(Test_Data$Sex))

##########################  Age  ##########################
Data$Age <- as.integer(Data$Age)
Test_Data$Age <- as.integer(Test_Data$Age)

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_model <- mice(Data, method='rf')
mice_model_t <- mice(Test_Data, method='rf')

Data <- complete(mice_model)
Test_Data <- complete(mice_model_t)

Bands <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, Inf)

# Bin the Age column
Data$Age_Band <- cut(Data$Age, Bands)
Test_Data$Age_Band <- cut(Test_Data$Age, Bands)


Data <- Data %>% 
  group_by(Pclass, Sex) %>% 
  mutate(Age_mean = mean(Age_Band, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Age_Band = ifelse(is.na(Age_Band), Age_mean, Age_Band)) %>% 
  select(-Age_mean)

Test_Data <- Test_Data %>% 
  group_by(Pclass, Sex) %>% 
  mutate(Age_mean = mean(Age_Band)) %>% 
  ungroup() %>% 
  mutate(Age_Band = ifelse(is.na(Age_Band), Age_mean, Age_Band)) %>% 
  select(-Age_mean)

med <- median(Data$Age_Band, na.rm = TRUE)
med <- median(Test_Data$Age_Band, na.rm = TRUE)

# fill null values with the median
Data$Age_Band <- ifelse(is.na(Data$Age_Band), med, Data$Age_Band)
Test_Data$Age_Band <- ifelse(is.na(Test_Data$Age_Band), med, Test_Data$Age_Band)

Data$Age_Band <- ifelse(is.na(Data$Age_Band), 1, Data$Age_Band)
Test_Data$Age_Band <- ifelse(is.na(Test_Data$Age_Band), 1, Test_Data$Age_Band)

# Encode the labels
Data$Age_Band <- as.numeric(factor(Data$Age_Band, levels = unique(Data$Age_Band)))
Test_Data$Age_Band <- as.numeric(factor(Test_Data$Age_Band, levels = unique(Test_Data$Age_Band)))

##########################  SibSp & Parch  ##########################
Data$Family_Size <- Data$SibSp + Data$Parch + 1
Test_Data$Family_Size <- Test_Data$SibSp + Test_Data$Parch + 1

Data$Family_Size[Data$Family_Size == 1] <- 0
Data$Family_Size[(Data$Family_Size > 1) & (Data$Family_Size <= 4)] <- 1
Data$Family_Size[(Data$Family_Size > 4) & (Data$Family_Size <= 6)] <- 2
Data$Family_Size[Data$Family_Size > 6] <- 3

Test_Data$Family_Size[Test_Data$Family_Size == 1] <- 0
Test_Data$Family_Size[(Test_Data$Family_Size > 1) & (Test_Data$Family_Size <= 4)] <- 1
Test_Data$Family_Size[(Test_Data$Family_Size > 4) & (Test_Data$Family_Size <= 6)] <- 2
Test_Data$Family_Size[Test_Data$Family_Size > 6] <- 3

Data$Is_Alone <- ifelse(Data$Family_Size >= 1, 0, 1)
Test_Data$Is_Alone <- ifelse(Test_Data$Family_Size >= 1, 0, 1)

Data <- Data[, !(colnames(Data) %in% c("SibSp","Parch"))]
Test_Data <- Test_Data[, !(colnames(Test_Data) %in% c("SibSp","Parch"))]

##########################  Cabin  ##########################
Data$HasCabin <- as.logical(!is.na(Data$Cabin) & Data$Cabin != "")
Data$HasCabin <- as.numeric(as.logical(Data$HasCabin))

Test_Data$HasCabin <- as.logical(!is.na(Test_Data$Cabin) & Test_Data$Cabin != "")
Test_Data$HasCabin <- as.numeric(as.logical(Test_Data$HasCabin))

Data <- Data[, !(colnames(Data) %in% c("Cabin"))]
Test_Data <- Test_Data[, !(colnames(Test_Data) %in% c("Cabin"))]

##########################  take.off  ##########################
mode_value <- names(which.max(table(Data$take.off)))
Data$take.off <- ifelse(is.na(Data$take.off), mode_value, Data$take.off)

Data$take.off <- as.integer(factor(Data$take.off))

mode_value <- names(which.max(table(Test_Data$take.off)))
Test_Data$take.off <- ifelse(is.na(Test_Data$take.off), mode_value, Test_Data$take.off)

Test_Data$take.off <- as.integer(factor(Test_Data$take.off))

##########################  Age_Class  ##########################
Data$Age_Class <- Data$Age_Band * Data$Pclass
Test_Data$Age_Class <- Test_Data$Age_Band * Test_Data$Pclass

##################################
### -------- Modeling -------- ###
##################################
set.seed(754)

# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + 
                           Fare + take.off + Title + Age_Band +
                           Is_Alone + Age_Class + HasCabin + Family_Size, data = Data)


model <- glm(factor(Survived) ~ Pclass + Sex + Age + 
               Fare + take.off + Title + Age_Band +
               Is_Alone + Age_Class + HasCabin + Family_Size, data = Data, family = binomial())

summary(model)
predictions <- predict(model, Test_Data, type = "response")
predictions <- ifelse(predictions >= 0.5, 1, 0)

# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

prediction <- predict(rf_model, Test_Data)


# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = Test_Data$PassengerId, Survived = prediction)

solution_lr <- data.frame(PassengerID = Test_Data$PassengerId, Survived = predictions)


# Write the solution to file
write.csv(solution, file = 'RF_model.csv', row.names = F)

write.csv(solution_lr, file = 'LR_model_p.csv', row.names = F)

Train <- Data[1:891,]
Test <- Data[892:1309,]
