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

print("Dataset stucture:")
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

ggplot(Train_Data, aes(x=Age, fill=factor(Survived))) + 
  geom_histogram(alpha=0.5, bins=20) + 
  facet_grid(Pclass~.) + 
  labs(x="Age", y="Count") + 
  theme_bw()

##################################
###------ Cleaning Data -------###
##################################
Train_Data <- Train_Data[, !(colnames(Train_Data) %in% c("PassengerId","Ticket", "Cabin"))]
Test_Data <- Test_Data[, !(colnames(Test_Data) %in% c("PassengerId","Ticket", "Cabin"))]

Data <- rbind(Train_Data, Test_Data)

# rm(Train_Data)
# rm(Test_Data)

# Grab title from passengers names
Data$Title <- gsub('(.*, )|(\\..*)', '', Data$Name)

table(Data$Sex, Data$Title)

Rare_Title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 
                'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

Data$Title[Data$Title == 'Mlle'] <- 'Miss' 
Data$Title[Data$Title == 'Ms'] <- 'Miss'
Data$Title[Data$Title == 'Mme'] <- 'Mrs' 
Data$Title[Data$Title %in% Rare_Title]  <- 'Rare Title'

# Show title counts by sex
table(Data$Sex, Data$Title)

Data$Title <- as.integer(factor(Data$Title))

Data <- Data[, !(colnames(Data) %in% c("Name"))]

Data$Sex <- as.integer(factor(Data$Sex))

Data$Age <- as.integer(Data$Age)

Data$Family_Size <- Data$SibSp + Data$Parch + 1

Data <- Data[, !(colnames(Data) %in% c("SibSp","Parch"))]

mode_value <- names(which.max(table(Data$take.off)))
Data$take.off <- ifelse(is.na(Data$take.off), mode_value, Data$take.off)

Data$take.off <- as.integer(factor(Data$take.off))


Train <- Data[1:891,]
Test <- Data[892:1309,]

