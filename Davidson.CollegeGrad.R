## HarvardX: Ph125.9x Data Science Capstone College Graduation Rates Prediction Project
## Chris Davidson

#####################################################
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")

#####################################################
## Import IPEDS CSV Files from Github & Clean Data ##
sectors <- read.csv("https://raw.githubusercontent.com/drcdavidson/gradrates/main/IPEDS_Data/sectors.csv")
sectorlabels <- read.csv("https://raw.githubusercontent.com/drcdavidson/gradrates/main/IPEDS_Data/sectorlabels.csv")
fte <- read.csv("https://raw.githubusercontent.com/drcdavidson/gradrates/main/IPEDS_Data/fte.csv")
pell <- read.csv("https://raw.githubusercontent.com/drchrisd/gradrates/master/pell.csv")
f.s.ratio <- read.csv("https://raw.githubusercontent.com/drcdavidson/gradrates/main/IPEDS_Data/pell.csv")
admit <- read.csv("https://raw.githubusercontent.com/drcdavidson/gradrates/main/IPEDS_Data/percent.admit.csv")
retention <- read.csv("https://raw.githubusercontent.com/drcdavidson/gradrates/main/IPEDS_Data/retention.csv")
gradrates <- read.csv("https://raw.githubusercontent.com/drcdavidson/gradrates/main/IPEDS_Data/grad.csv") 
variables <- read.csv("https://raw.githubusercontent.com/drcdavidson/gradrates/main/IPEDS_Data/Variables.csv")

###################################################
## Clean Data and Combine Files into the Dataset ##

# Clean Graduation Rates
names(gradrates) <- c("UnitID", "InstName", 2018:2009) #rename columns 
grad <- gradrates %>% select(UnitID, "2018") %>% mutate(Year = 2018)   #create subset of rates
names(grad) <- c("UnitID", "GradRate", "Year")   #rename columns 

#Bind rows into a tidy format 
grad <- bind_rows(grad, data.frame("UnitID" = gradrates$UnitID, "Year" = 2017, "GradRate" = gradrates$'2017')) %>%
  bind_rows(grad, data.frame("UnitID" = gradrates$UnitID, "Year" = 2016, "GradRate" = gradrates$'2016')) %>%
  bind_rows(grad, data.frame("UnitID" = gradrates$UnitID, "Year" = 2015, "GradRate" = gradrates$'2015')) %>%
  bind_rows(grad, data.frame("UnitID" = gradrates$UnitID, "Year" = 2014, "GradRate" = gradrates$'2014')) %>%
  bind_rows(grad, data.frame("UnitID" = gradrates$UnitID, "Year" = 2013, "GradRate" = gradrates$'2013')) %>%
  bind_rows(grad, data.frame("UnitID" = gradrates$UnitID, "Year" = 2012, "GradRate" = gradrates$'2012')) %>%
  bind_rows(grad, data.frame("UnitID" = gradrates$UnitID, "Year" = 2011, "GradRate" = gradrates$'2011')) %>%
  bind_rows(grad, data.frame("UnitID" = gradrates$UnitID, "Year" = 2010, "GradRate" = gradrates$'2010')) %>%
  bind_rows(grad, data.frame("UnitID" = gradrates$UnitID, "Year" = 2009, "GradRate" = gradrates$'2009'))

# Remove objects not needed
rm(gradrates)

# Clean Student:Faculty Ratio
names(f.s.ratio) <- c("UnitID", "InstName", 2018:2009) #rename columns 
ratio <- f.s.ratio %>% select(UnitID, "2018") %>% mutate(Year = 2018)   #create subset of ratios
names(ratio) <- c("UnitID", "SF.Ratio", "Year")   #rename columns 

#Bind rows into a tidy format 
ratio <- bind_rows(ratio, data.frame("UnitID" = f.s.ratio$UnitID, "Year" = 2017, "SF.Ratio" = f.s.ratio$'2017')) %>%
  bind_rows(ratio, data.frame("UnitID" = f.s.ratio$UnitID, "Year" = 2016, "SF.Ratio" = f.s.ratio$'2016')) %>%
  bind_rows(ratio, data.frame("UnitID" = f.s.ratio$UnitID, "Year" = 2015, "SF.Ratio" = f.s.ratio$'2015')) %>%
  bind_rows(ratio, data.frame("UnitID" = f.s.ratio$UnitID, "Year" = 2014, "SF.Ratio" = f.s.ratio$'2014')) %>%
  bind_rows(ratio, data.frame("UnitID" = f.s.ratio$UnitID, "Year" = 2013, "SF.Ratio" = f.s.ratio$'2013')) %>%
  bind_rows(ratio, data.frame("UnitID" = f.s.ratio$UnitID, "Year" = 2012, "SF.Ratio" = f.s.ratio$'2012')) %>%
  bind_rows(ratio, data.frame("UnitID" = f.s.ratio$UnitID, "Year" = 2011, "SF.Ratio" = f.s.ratio$'2011')) %>%
  bind_rows(ratio, data.frame("UnitID" = f.s.ratio$UnitID, "Year" = 2010, "SF.Ratio" = f.s.ratio$'2010')) %>%
  bind_rows(ratio, data.frame("UnitID" = f.s.ratio$UnitID, "Year" = 2009, "SF.Ratio" = f.s.ratio$'2009'))

# Remove objects not needed
rm(f.s.ratio)

# Clean FTE
names(fte) <- c("UnitID", "InstName", 2018:2009) #rename columns 
FTE <- fte %>% select(UnitID, "2018") %>% mutate(Year = 2018)   #create subset of ratios
names(FTE) <- c("UnitID", "FTE", "Year")   #rename columns 

#Bind rows into a tidy format 
FTE <- bind_rows(FTE, data.frame("UnitID" = fte$UnitID, "Year" = 2017, "FTE" = fte$'2017')) %>%
  bind_rows(FTE, data.frame("UnitID" = fte$UnitID, "Year" = 2016, "FTE" = fte$'2016')) %>%
  bind_rows(FTE, data.frame("UnitID" = fte$UnitID, "Year" = 2015, "FTE" = fte$'2015')) %>%
  bind_rows(FTE, data.frame("UnitID" = fte$UnitID, "Year" = 2014, "FTE" = fte$'2014')) %>%
  bind_rows(FTE, data.frame("UnitID" = fte$UnitID, "Year" = 2013, "FTE" = fte$'2013')) %>%
  bind_rows(FTE, data.frame("UnitID" = fte$UnitID, "Year" = 2012, "FTE" = fte$'2012')) %>%
  bind_rows(FTE, data.frame("UnitID" = fte$UnitID, "Year" = 2011, "FTE" = fte$'2011')) %>%
  bind_rows(FTE, data.frame("UnitID" = fte$UnitID, "Year" = 2010, "FTE" = fte$'2010')) %>%
  bind_rows(FTE, data.frame("UnitID" = fte$UnitID, "Year" = 2009, "FTE" = fte$'2009'))

# Remove objects not needed
rm(fte)

# Clean Pell
names(pell) <- c("UnitID", "InstName", 2018:2009) #rename columns 
PELL <- pell %>% select(UnitID, "2018") %>% mutate(Year = 2018)   #create subset of ratios
names(PELL) <- c("UnitID", "Pell", "Year")   #rename columns 

#Bind rows into a tidy format 
PELL <- bind_rows(PELL, data.frame("UnitID" = pell$UnitID, "Year" = 2017, "Pell" = pell$'2017')) %>%
  bind_rows(PELL, data.frame("UnitID" = pell$UnitID, "Year" = 2016, "Pell" = pell$'2016')) %>%
  bind_rows(PELL, data.frame("UnitID" = pell$UnitID, "Year" = 2015, "Pell" = pell$'2015')) %>%
  bind_rows(PELL, data.frame("UnitID" = pell$UnitID, "Year" = 2014, "Pell" = pell$'2014')) %>%
  bind_rows(PELL, data.frame("UnitID" = pell$UnitID, "Year" = 2013, "Pell" = pell$'2013')) %>%
  bind_rows(PELL, data.frame("UnitID" = pell$UnitID, "Year" = 2012, "Pell" = pell$'2012')) %>%
  bind_rows(PELL, data.frame("UnitID" = pell$UnitID, "Year" = 2011, "Pell" = pell$'2011')) %>%
  bind_rows(PELL, data.frame("UnitID" = pell$UnitID, "Year" = 2010, "Pell" = pell$'2010')) %>%
  bind_rows(PELL, data.frame("UnitID" = pell$UnitID, "Year" = 2009, "Pell" = pell$'2009'))

# Remove objects not needed
rm(pell)

# Clean Percent Admitted
names(admit) <- c("UnitID", "InstName", 2018:2009) #rename columns 
p.admit <- admit %>% select(UnitID, "2018") %>% mutate(Year = 2018)   #create subset of ratios
names(p.admit) <- c("UnitID", "PercAdmit", "Year")   #rename columns 

#Bind rows into a tidy format 
p.admit <- bind_rows(p.admit, data.frame("UnitID" = admit$UnitID, "Year" = 2017, "PercAdmit" = admit$'2017')) %>%
  bind_rows(p.admit, data.frame("UnitID" = admit$UnitID, "Year" = 2016, "PercAdmit" = admit$'2016')) %>%
  bind_rows(p.admit, data.frame("UnitID" = admit$UnitID, "Year" = 2015, "PercAdmit" = admit$'2015')) %>%
  bind_rows(p.admit, data.frame("UnitID" = admit$UnitID, "Year" = 2014, "PercAdmit" = admit$'2014')) %>%
  bind_rows(p.admit, data.frame("UnitID" = admit$UnitID, "Year" = 2013, "PercAdmit" = admit$'2013')) %>%
  bind_rows(p.admit, data.frame("UnitID" = admit$UnitID, "Year" = 2012, "PercAdmit" = admit$'2012')) %>%
  bind_rows(p.admit, data.frame("UnitID" = admit$UnitID, "Year" = 2011, "PercAdmit" = admit$'2011')) %>%
  bind_rows(p.admit, data.frame("UnitID" = admit$UnitID, "Year" = 2010, "PercAdmit" = admit$'2010')) %>%
  bind_rows(p.admit, data.frame("UnitID" = admit$UnitID, "Year" = 2009, "PercAdmit" = admit$'2009'))

# Remove objects not needed
rm(admit)

# Clean Retention Rate
names(retention) <- c("UnitID", "InstName", 2018:2009) #rename columns 
Retention <- retention %>% select(UnitID, "2018") %>% mutate(Year = 2018)   #create subset of ratios
names(Retention) <- c("UnitID", "Retention", "Year")   #rename columns 

#Bind rows into a tidy format 
Retention <- bind_rows(Retention, data.frame("UnitID" = retention$UnitID, "Year" = 2017, "Retention" = retention$'2017')) %>%
  bind_rows(Retention, data.frame("UnitID" = retention$UnitID, "Year" = 2016, "Retention" = retention$'2016')) %>%
  bind_rows(Retention, data.frame("UnitID" = retention$UnitID, "Year" = 2015, "Retention" = retention$'2015')) %>%
  bind_rows(Retention, data.frame("UnitID" = retention$UnitID, "Year" = 2014, "Retention" = retention$'2014')) %>%
  bind_rows(Retention, data.frame("UnitID" = retention$UnitID, "Year" = 2013, "Retention" = retention$'2013')) %>%
  bind_rows(Retention, data.frame("UnitID" = retention$UnitID, "Year" = 2012, "Retention" = retention$'2012')) %>%
  bind_rows(Retention, data.frame("UnitID" = retention$UnitID, "Year" = 2011, "Retention" = retention$'2011')) %>%
  bind_rows(Retention, data.frame("UnitID" = retention$UnitID, "Year" = 2010, "Retention" = retention$'2010')) %>%
  bind_rows(Retention, data.frame("UnitID" = retention$UnitID, "Year" = 2009, "Retention" = retention$'2009'))

# Remove objects not needed
rm(retention)

#Create College dataset for modeling using left joins
College <- grad %>% 
  left_join(sectors, by = 'UnitID') %>%   
  left_join(sectorlabels, by = 'Sector.of.institution') %>% 
  select(-Sector.of.institution)
College <- College %>% cbind("FTE" = FTE$FTE, 
                             "PercAdmit" = p.admit$PercAdmit, 
                             "PELL" = PELL$Pell, 
                             "SF.Ratio" = ratio$SF.Ratio, 
                             "Retention" = Retention$Retention)

#Remove objects
rm(FTE, grad, p.admit, PELL, ratio, Retention, sectorlabels, sectors)

#Rename column
names(College)[5] <- "Sector" 

#Reorder columns
College <- College[,c(1,4,5,3,2,6:10)]

#Remove cases with at least one null value
College <- College[complete.cases(College),]

#Test set will be 20% College dataset
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = College$GradRate, times = 1, p = 0.2, list = FALSE)
train <- College[-test_index,]
test <- College[test_index,]

#Remove unneeded objects
rm(test_index)

#Create data.frame with variables and definitions
kable(variables, booktabs=T, caption = "College Graduation Dataset Variables and Definitions") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F) %>%
  column_spec(2, width = "30em")

## First 6 Rows of College Dataset ##
head(College) %>% 
  knitr::kable("latex", booktabs=TRUE, caption = "First Six Rows of the College Graduation Dataset") %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position"))

#####################################################
## Descriptives of College Dataset ##
## Descriptives of College Dataset ##
summary(College) %>% 
  knitr::kable("latex", booktabs=TRUE, caption = "Descriptives of the College Graduation Dataset") %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position"))

########################################################
##### Exploratory Analysis of the Training DataSet #####
#Descriptives of Training Dataset 
kable(summary(train), booktabs=T, caption = "Descriptives of Training Dataset") %>%
  kable_styling(full_width = F, latex_options = c("striped", "scale_down", "hold_position")) 

#Distribution of Graduation Rates 
train %>% ggplot(aes(GradRate)) +
  geom_histogram(fill = "dark blue", color = "white", bins = 10) +
  ggtitle("Distribution of Graduation Rates")

#Distribution of Graduation Rates by Institution Sector
train %>% ggplot(aes(GradRate, fill = Sector)) +
  geom_histogram(bins = 10, alpha = .5, aes(y = ..count..),
                 position = 'identity', color = 'white') +
  scale_fill_manual(values = c("darkred", "darkblue", "orchid3")) +
  ggtitle("Disribution of Graduation Rates by Sector")

#Graduation Rates by Year
train %>% group_by(Sector, Year) %>%
  summarise(GradRate1 = mean(GradRate, na.rm = TRUE)) %>%
  ggplot(aes(Year, GradRate1, color = Sector)) +
  geom_point() +
  geom_line() +
  ylab("Graduation Rate") +
  scale_color_manual(values = c("darkred", "darkblue", "orchid3")) +
  ggtitle("Average Graduation Rate by Year")

#Distribution of FTEs
train %>% ggplot(aes(FTE)) +
  geom_histogram(fill = "dark blue", color = "white", bins = 15) +
  scale_x_continuous(breaks = c(seq(0, 60000, 15000))) +
  ggtitle("Distribution of FTEs")

#Distribution of Percent Admitted
train %>% ggplot(aes(PercAdmit)) +
  geom_histogram(fill = "darkblue", color = "white", bins = 10) +
  ggtitle("Distribution of Percent of Students Admitted")

#Distribution of Percent of Pell Recipients
train %>% ggplot(aes(PELL)) +
  geom_histogram(fill = "darkblue", color = "white", bins = 10) +
  ggtitle("Distribution of Percent of PELL Recipients")

#Distribution of Student to Faculty Ratio
train %>% ggplot(aes(SF.Ratio)) +
  geom_histogram(fill = "darkblue", color = "white", bins = 15) +
  scale_x_continuous(breaks = c(seq(0, 40, 10))) +
  ggtitle("Distribution of Student to Faculty Ratio")

#Distribution of Retention Rate
train %>% ggplot(aes(Retention)) +
  geom_histogram(fill = "darkblue", color = "white", bins = 10) +
  ggtitle("Distribution of Retention Rate")

########################################
## Modeling Tuning based on Train Set ##
#Set Seed 
set.seed(1, sample.kind="Rounding")


#Create Function to Calculate RMSE
RMSE <- function(predicted_ratings, true_ratings,...){
  sqrt(mean((predicted_ratings - true_ratings)^2,...))
}

#############################################
#MODEL 1: BEST GUESS MODEL 
mu <- mean(train$GradRate)

# Test Model 1
RMSE1 <- RMSE(test$GradRate, mu)

#Create Data Frame for RMSE Results of Models 
Models <- data_frame(Method="Model I: Best Guess - Baseline", RMSE = RMSE1)
Models %>% knitr::kable(booktabs=T, caption = "Results of RMSEs") %>%
  kable_styling(full_width = F, latex_options = "hold_position")

#######################################################################
#MODEL 2: MULTIPLE LINEAR REGRESSION 
#Calculate Linear Regression Model
LinearFit <- lm(GradRate ~ Sector + Year + FTE + PercAdmit + PELL + 
                  SF.Ratio + Retention, data = train)

#Review Linear Model
tidy(LinearFit) %>%  knitr::kable(booktabs=T, caption = "Multiple Linear Regression") %>%
  kable_styling(latex_options = "hold_position", full_width = F) 

#Use Model to make Predictions 
LinearPredict <- predict(LinearFit, test)

#Evaluate the accuracy by RMSE 
RMSE2 <- RMSE(LinearPredict, test$GradRate)

#Add RMSE Restult to Data Frame
Models <- bind_rows(Models,
                    data_frame(Method="Model II: Multiple Linear Regression", RMSE = RMSE2))

Models %>% knitr::kable(booktabs=T, caption = "Results of RMSEs") %>%
  kable_styling(full_width = F, latex_options = "hold_position")

######################################################################
#MODEL 3: Regression Tree
#Apply rpart to obtain regression tree 
RTreeFit <- rpart(GradRate ~ Sector + Year + FTE + PercAdmit + PELL + 
                    SF.Ratio + Retention, data = train)

#View regression tree
fancyRpartPlot(RTreeFit, palettes = "Blues")

#Use model to make prediction
RTreePredict <- predict(RTreeFit, test)

##Evaluate the accuracy by RMSE 
RMSE3 <- RMSE(RTreePredict, test$GradRate)

#Add RMSE Result to Data Frame
Models <- bind_rows(Models,
                    data_frame(Method="Model III: Regression Tree", RMSE = RMSE3))
Models %>% knitr::kable(booktabs=T, caption = "Results of RMSEs") %>%
  kable_styling(full_width = F, latex_options = "hold_position")

######################################################################
#MODEL 4: Pruned Regression Tree
#Review cross validation results to find optimal CP (xerror column)
#9 Splits, CP = .01
tidy(printcp(RTreeFit))

#Prune the regression tree
RTreePruned <- prune(RTreeFit, cp=.01)

#View pruned regression tree
fancyRpartPlot(RTreePruned, palettes = "Blues")

#Use pruned tree model to make prediction
PrunedRT_Predict <- predict(RTreePruned, test)

#Evalute the accuracy by RMSE
RMSE4 <- RMSE(PrunedRT_Predict, test$GradRate)

#Add RMSE Result to Data Frame
Models <- bind_rows(Models,
                    data_frame(Method="Model IV: Pruned Regresstion Tree", RMSE = RMSE4))

#########################################
# MODEL 5: RANDOM FORESTS
#Create a random forest 
RForest <- randomForest(GradRate ~ Sector + Year + FTE + 
                          PercAdmit + PELL + SF.Ratio + Retention, data = train)

#Plot to see errors by number of trees with line at minimum error
plot(RForest) +
  abline(v = which.min(RForest$mse), col="blue")

#Number of trees to reach minimum error estimate 
which.min(RForest$mse) %>%  knitr::kable(booktabs=T, caption = "Minimum Error Estimate") %>%
  kable_styling(full_width = F, latex_options = "hold_position")

#Use the importance() function to determine the importance of each variable 
RFImportance <- varImp(RForest, type=2, scale=TRUE)
names(RFImportance) <- c("IncNodePurity")    #rename column
RFImportance %>% knitr::kable(booktabs=T, caption = "Variable Importance") %>%
  kable_styling(full_width = F, latex_options = "hold_position")

#Use the model to make prediction 
RForestPredict <- predict(RForest, test)

#Evalute the accuracy by RMSE
RMSE5 <- RMSE(RForestPredict, test$GradRate)

#Add RMSE Result to Data Frame
Models <- bind_rows(Models,
                    data_frame(Method="Model V: Random Forest", RMSE = RMSE5))

#Print RMSE Table
Models %>% knitr::kable(booktabs=T, caption = "Results of RMSEs") %>%
  kable_styling(full_width = F, latex_options = "hold_position")