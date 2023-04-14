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
sapply(Train_Data,function(x) sum(is.na(x)))
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
Data <- Train_Data[, !(colnames(Train_Data) %in% c("PassengerId","Ticket", "Cabin"))]
Test_Data <- Test_Data[, !(colnames(Test_Data) %in% c("Ticket", "Cabin"))]

# Data <- rbind(Train_Data, Test_Data)

# rm(Train_Data)
# rm(Test_Data)

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

Data$Sex <- as.integer(factor(Data$Sex))
Test_Data$Sex <- as.integer(factor(Test_Data$Sex))

Data$Age <- as.integer(Data$Age)
Test_Data$Age <- as.integer(Test_Data$Age)

Data$Family_Size <- Data$SibSp + Data$Parch + 1
Test_Data$Family_Size <- Test_Data$SibSp + Test_Data$Parch + 1

Data <- Data[, !(colnames(Data) %in% c("SibSp","Parch"))]
Test_Data <- Test_Data[, !(colnames(Test_Data) %in% c("SibSp","Parch"))]

mode_value <- names(which.max(table(Data$take.off)))
Data$take.off <- ifelse(is.na(Data$take.off), mode_value, Data$take.off)

Data$take.off <- as.integer(factor(Data$take.off))



mode_value <- names(which.max(table(Test_Data$take.off)))
Test_Data$take.off <- ifelse(is.na(Test_Data$take.off), mode_value, Test_Data$take.off)

Test_Data$take.off <- as.integer(factor(Test_Data$take.off))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_model <- mice(Data, method='rf')

Data <- complete(mice_model)

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_model_t <- mice(Test_Data, method='rf')

Test_Data <- complete(mice_model_t)


set.seed(754)


# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + 
                           Fare + take.off + Title + 
                           Family_Size, data = Data)

split <- sample.split(Data, SplitRatio = 0.7)
train_cl <- subset(Data, split == "TRUE")
test_cl <- subset(Data, split == "FALSE")

knn_model <- knn(train = Data, test = Test_Data, cl = Data$Survived, k = 4)

# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

prediction <- predict(rf_model, Test_Data)


# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = Test_Data$PassengerId, Survived = prediction)
solution_knn <- data.frame(PassengerID = Test_Data$PassengerId, Survived = knn_model)

# Write the solution to file
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)
write.csv(solution_knn, file = 'knn_model_Solution.csv', row.names = F)



Train <- Data[1:891,]
Test <- Data[892:1309,]

