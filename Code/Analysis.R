#### packages needed ####
library(readr)
library(readxl)
library(reshape2)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(cluster)
library(factoextra)
library(ggsignif)
library(rstatix)
library(caret)
library(FactoMineR)
library(corrr)
library(ggcorrplot)
library(GGally)
library(lfe)
library(sjPlot)
# install.packages("rnaturalearth")
# devtools::install_github("ropensci/rnaturalearthdata")
# devtools::install_github("ropensci/rnaturalearthhires")
library(rnaturalearth)
library(caret)
library(randomForest)
library(pROC)
library(plotROC)

library(qacEDA)
library(qacDR)

#### data subsets ####
df <- read_csv("df.csv")

# 3 subsets
subjective<- df[,c(1:3,5:12)]
social<- df[,c(1:3,13:18,32,34,35,37)]
economic<- df[,c(1:3,4,19:31,33,36)]

num_data_subjective <- subjective[, sapply(subjective, is.numeric)] 
num_data_social <- social[, sapply(social, is.numeric)] 
num_data_economic <- economic[, sapply(economic, is.numeric)] 
scale_num_subjective <- scale(num_data_subjective)
scale_num_social <- scale(num_data_social)
scale_num_economic <- scale(num_data_economic)

corr_matrix1 <- cor(scale_num_subjective)
ggcorrplot(corr_matrix1)

corr_matrix2 <- cor(scale_num_social)
ggcorrplot(corr_matrix2)

corr_matrix3 <- cor(scale_num_economic)
ggcorrplot(corr_matrix3)


# the new subsets after a selection of the parameters
PCA_economic<-economic[,c(1:4,8,12,14,15,16,18)]
num_PCA_economic <- PCA_economic[, sapply(PCA_economic, is.numeric)] 
ggpairs(num_PCA_economic)

PCA_social<-social[,c(1:4,8,11:13)]
num_PCA_social <- PCA_social[, sapply(PCA_social, is.numeric)] 
ggpairs(num_PCA_social)

ggpairs(num_data_subjective) #PCA_subjective is simply subjective


# z scores
# subjective
final_subjective<-scale(subjective[,3:11], center = TRUE, scale = TRUE)
final_subjective<-as.data.frame(final_subjective)
final_subjective$country<-subjective$country_name
final_subjective$year<-subjective$year
final_subjective <- final_subjective %>% relocate(country, .before = `Life Ladder`) %>%
  relocate(year, .before = `Life Ladder`) 
# economic
final_economic<-scale(PCA_economic[,3:10], center = TRUE, scale = TRUE)
final_economic<-as.data.frame(final_economic)
final_economic$country<-PCA_economic$country_name
final_economic$year<-PCA_economic$year
final_economic <- final_economic %>% relocate(country, .before = `Life Ladder`) %>%
  relocate(year, .before = `Life Ladder`) 
# social
final_social<-scale(PCA_social[,3:8], center = TRUE, scale = TRUE)
final_social<-as.data.frame(final_social)
final_social$country<-PCA_social$country_name
final_social$year<-PCA_social$year
final_social <- final_social %>% relocate(country, .before = `Life Ladder`) %>%
  relocate(year, .before = `Life Ladder`) 

# correlation matrix
corr_matrix1 <- cor(final_subjective[,3:11])
ggcorrplot(corr_matrix1)

corr_matrix2 <- cor(final_economic[,3:10])
ggcorrplot(corr_matrix2)

corr_matrix3 <- cor(final_social[,3:8])
ggcorrplot(corr_matrix3)

colnames(final_subjective)[colnames(final_subjective) == "Life Ladder"] <- "Happiness"
colnames(final_economic)[colnames(final_economic) == "Life Ladder"] <- "Happiness"
colnames(final_social)[colnames(final_social) == "Life Ladder"] <- "Happiness"


#### fixed effects ####
fixed_lm1<-felm(formula= Happiness~
                  `Log GDP per capita`+
                  `Agricultural land (sq. km)`+
                  `GDP growth (annual %)`+
                  `Gross savings (% of GNI)`+
                  `Inflation, consumer prices (annual %)`+
                  `Mortality rate, infant (per 1,000 live births)`+
                  `Labor force, total` | country + year,
                data=final_economic)
summary(fixed_lm1)

tab_model(fixed_lm1, show.aic = TRUE, dv.labels = "Economic on Happiness")
# Residuals vs. Fitted Values Plot
plot(fitted(fixed_lm1), resid(fixed_lm1))
abline(h = 0, col = "red")

#Normal Q-Q Plot
qqnorm(resid(fixed_lm1))
qqline(resid(fixed_lm1))

#Scale-Location Plot
plot(fitted(fixed_lm1), sqrt(abs(resid(fixed_lm1))))
abline(h = 0, col = "red")

#Residuals vs. Leverage Plot
#plot(fixed_lm1, which = 5)


fixed_lm2<-felm(formula=`Happiness`~`Access to clean fuels and technologies for cooking (% of population)`+
                  `Access to electricity (% of population)` +
                  `Forest area (% of land area)`+
                  `Arable land (% of land area)`+
                  `Individuals using the Internet (% of population)`| country + year,
                data=final_social)
summary(fixed_lm2)
tab_model(fixed_lm2, show.aic = TRUE, dv.labels = "Social on Happiness")

# Residuals vs. Fitted Values Plot
plot(fitted(fixed_lm2), resid(fixed_lm2))
abline(h = 0, col = "red")

#Normal Q-Q Plot
qqnorm(resid(fixed_lm2))
qqline(resid(fixed_lm2))

#Scale-Location Plot
plot(fitted(fixed_lm2), sqrt(abs(resid(fixed_lm2))))
abline(h = 0, col = "red")

#Residuals vs. Leverage Plot
#plot(fixed_lm2, which = 5)



fixed_lm3<-felm(formula=`Happiness`~`Social support`+
                  `Healthy life expectancy at birth` +
                  `Freedom to make life choices`+
                  `Generosity`+
                  `Perceptions of corruption`+
                  `Positive affect` +
                  `Negative affect`+
                  `Confidence in national government` | country + year,
                data=final_subjective)
summary(fixed_lm3)
tab_model(fixed_lm3, show.aic = TRUE, dv.labels = "Subjective on Happiness")
# Residuals vs. Fitted Values Plot
plot(fitted(fixed_lm3), resid(fixed_lm3))
abline(h = 0, col = "red")

#Normal Q-Q Plot
qqnorm(resid(fixed_lm3))
qqline(resid(fixed_lm3))

#Scale-Location Plot
plot(fitted(fixed_lm3), sqrt(abs(resid(fixed_lm3))))
abline(h = 0, col = "red")

#Residuals vs. Leverage Plot
#plot(fixed_lm3, which = 5)
df2<- scale(df[,3:37], center = TRUE, scale = TRUE)
df2<-as.data.frame(df2)
colnames(df2)[colnames(df2) == "Life Ladder"] <- "Happiness"
df2$country<-df$country_name
df2$year<-df$year
fixed_lm4<-felm(formula=`Happiness`~`Social support`+
                  `Healthy life expectancy at birth` +
                  `Freedom to make life choices`+
                  `Generosity`+
                  `Perceptions of corruption`+
                  `Positive affect` +
                  `Negative affect`+
                  `Confidence in national government` +
                  `Log GDP per capita`+
                  `Agricultural land (sq. km)`+
                  `GDP growth (annual %)`+
                  `Gross savings (% of GNI)`+
                  `Inflation, consumer prices (annual %)`+
                  `Mortality rate, infant (per 1,000 live births)`+
                  `Labor force, total`+
                  `Access to clean fuels and technologies for cooking (% of population)`+
                  `Access to electricity (% of population)` +
                  `Forest area (% of land area)`+
                  `Arable land (% of land area)`+
                  `Individuals using the Internet (% of population)`| country + year,
                data=df2)
summary(fixed_lm4)
tab_model(fixed_lm4, show.aic = TRUE,dv.labels = "All variables on Happiness")


#### kmeans 2018 subjective####
kmeans2018<- final_subjective %>% filter(year==2018) 
kmeans2018a<- final_subjective %>% filter(year==2018) 
kmeans2018<- kmeans2018 %>% select(-year)
rownames(kmeans2018)<-kmeans2018$country
kmeans2018<- kmeans2018 %>% select(-country)

# methods to see how many clusters
set.seed(123)
fviz_nbclust(kmeans2018, kmeans, method = "wss") 
fviz_nbclust(kmeans2018, kmeans, method = "silhouette") 
# 3 clusters
set.seed(123)
subjectiveK <- kmeans(kmeans2018, centers = 3, nstart = 25)
plot_subjectiveK<-fviz_cluster(subjectiveK,data=kmeans2018, main="Cluster Plot for Subjective Variables 2018") 
kmeans2018$cluster <- subjectiveK$cluster 
kmeans2018 <- kmeans2018 %>% relocate(cluster, .before = Happiness)
#final dataset with clusters is called kmeans_subjective
profile_plot(kmeans2018)


# world map
# Load country data
countries <- ne_countries(scale = "medium", returnclass = "sf")
countries$geounit[countries$geounit == "Republic of Serbia"] <- 'Serbia'
countries$geounit[countries$geounit == "United States of America"] <- 'United States'

#check if the country names are different from my country in df
kmeans2018$country<-kmeans2018a$country
missing_countries <- setdiff(kmeans2018$country, countries$geounit)


# Merge data with country data
country_data <- merge(countries, kmeans2018, by.x = "geounit", by.y = "country", all.x = TRUE)

# Set the color palette for clusters
cluster_palette <- c("#FF0000", "#00FF00", "#0000FF")

# Create the world map
ggplot() +
  geom_sf(data = country_data, aes(fill = factor(cluster))) +
  scale_fill_manual(values = c(cluster_palette,"grey"), na.value = "gray") +
  theme_minimal()+
  labs(title = "Clusters of Countries on Citizen Opinions 2018",
       fill = "Cluster")



#### kmeans 2019 subjective####
kmeans2019<- final_subjective %>% filter(year==2019) 
kmeans2019a<- final_subjective %>% filter(year==2019) 
kmeans2019<- kmeans2019 %>% select(-year)
rownames(kmeans2019)<-kmeans2019$country
kmeans2019<- kmeans2019 %>% select(-country)

# methods to see how many clusters
set.seed(123)
fviz_nbclust(kmeans2019, kmeans, method = "wss") 
fviz_nbclust(kmeans2019, kmeans, method = "silhouette") 
# 3 clusters
set.seed(123)
subjectiveK <- kmeans(kmeans2019, centers = 3, nstart = 25)
plot_subjectiveK<-fviz_cluster(subjectiveK,data=kmeans2019, main="Cluster Plot for Subjective Variables 2019") 
kmeans2019$cluster <- subjectiveK$cluster 
kmeans2019 <- kmeans2019 %>% relocate(cluster, .before = Happiness)
#final dataset with clusters is called kmeans_subjective
profile_plot(kmeans2019)


# world map
# Load country data
countries <- ne_countries(scale = "medium", returnclass = "sf")
countries$geounit[countries$geounit == "Republic of Serbia"] <- 'Serbia'
countries$geounit[countries$geounit == "United States of America"] <- 'United States'

#check if the country names are different from my country in df
kmeans2019$country<-kmeans2019a$country
missing_countries <- setdiff(kmeans2019$country, countries$geounit)


# Merge data with country data
country_data <- merge(countries, kmeans2019, by.x = "geounit", by.y = "country", all.x = TRUE)

# Set the color palette for clusters
cluster_palette <- c("#FF0000", "#00FF00", "#0000FF")

# Create the world map
ggplot() +
  geom_sf(data = country_data, aes(fill = factor(cluster))) +
  scale_fill_manual(values = c(cluster_palette,"grey"), na.value = "gray") +
  theme_minimal()+
  labs(title = "Clusters of Countries on Citizen Opinions 2019",
       fill = "Cluster")


#### kmeans 2020 subjective####
kmeans2020<- final_subjective %>% filter(year==2020) 
kmeans2020a<- final_subjective %>% filter(year==2020) 
kmeans2020<- kmeans2020 %>% select(-year)
rownames(kmeans2020)<-kmeans2020$country
kmeans2020<- kmeans2020 %>% select(-country)

# methods to see how many clusters
set.seed(123)
fviz_nbclust(kmeans2020, kmeans, method = "wss") 
fviz_nbclust(kmeans2020, kmeans, method = "silhouette") 
# 3 clusters
set.seed(123)
subjectiveK <- kmeans(kmeans2020, centers = 3, nstart = 25)
plot_subjectiveK<-fviz_cluster(subjectiveK,data=kmeans2020, main="Cluster Plot for Subjective Variables 2020") 
kmeans2020$cluster <- subjectiveK$cluster 
kmeans2020 <- kmeans2020 %>% relocate(cluster, .before = Happiness)
#final dataset with clusters is called kmeans_subjective
profile_plot(kmeans2020)


# world map
# Load country data
countries <- ne_countries(scale = "medium", returnclass = "sf")
countries$geounit[countries$geounit == "Republic of Serbia"] <- 'Serbia'
countries$geounit[countries$geounit == "United States of America"] <- 'United States'

#check if the country names are different from my country in df
kmeans2020$country<-kmeans2020a$country
missing_countries <- setdiff(kmeans2020$country, countries$geounit)


# Merge data with country data
country_data <- merge(countries, kmeans2020, by.x = "geounit", by.y = "country", all.x = TRUE)

# Set the color palette for clusters
cluster_palette <- c("#FF0000", "#00FF00", "#0000FF")

# Create the world map
ggplot() +
  geom_sf(data = country_data, aes(fill = factor(cluster))) +
  scale_fill_manual(values = c(cluster_palette,"grey"), na.value = "gray") +
  theme_minimal()+
  labs(title = "Clusters of Countries on Citizen Opinions 2020",
       fill = "Cluster")



#### kmeans 2018 economic####
kmeans2018<- final_economic %>% filter(year==2018) 
kmeans2018a<- final_economic %>% filter(year==2018) 
kmeans2018<- kmeans2018 %>% select(-year)
rownames(kmeans2018)<-kmeans2018$country
kmeans2018<- kmeans2018 %>% select(-country)

# methods to see how many clusters
set.seed(123)
fviz_nbclust(kmeans2018, kmeans, method = "wss") 
fviz_nbclust(kmeans2018, kmeans, method = "silhouette") 
# 2 clusters
set.seed(123)
subjectiveK <- kmeans(kmeans2018, centers = 2, nstart = 25)
plot_subjectiveK<-fviz_cluster(subjectiveK,data=kmeans2018, main="Cluster Plot for Economic Variables 2018") 
kmeans2018$cluster <- subjectiveK$cluster 
kmeans2018 <- kmeans2018 %>% relocate(cluster, .before = Happiness)
#final dataset with clusters is called final_economic
profile_plot(kmeans2018)


# world map
# Load country data
countries <- ne_countries(scale = "medium", returnclass = "sf")
countries$geounit[countries$geounit == "Republic of Serbia"] <- 'Serbia'
countries$geounit[countries$geounit == "United States of America"] <- 'United States'

#check if the country names are different from my country in df
kmeans2018$country<-kmeans2018a$country
missing_countries <- setdiff(kmeans2018$country, countries$geounit)


# Merge data with country data
country_data <- merge(countries, kmeans2018, by.x = "geounit", by.y = "country", all.x = TRUE)

# Set the color palette for clusters
cluster_palette <- c("#FF0000", "#00FF00", "#0000FF")

# Create the world map
ggplot() +
  geom_sf(data = country_data, aes(fill = factor(cluster))) +
  scale_fill_manual(values = c(cluster_palette,"grey"), na.value = "gray") +
  theme_minimal()+
  labs(title = "Clusters of Countries on Economic 2018",
       fill = "Cluster")



#### kmeans 2019 economic####
kmeans2019<- final_economic %>% filter(year==2019) 
kmeans2019a<- final_economic %>% filter(year==2019) 
kmeans2019<- kmeans2019 %>% select(-year)
rownames(kmeans2019)<-kmeans2019$country
kmeans2019<- kmeans2019 %>% select(-country)

# methods to see how many clusters
set.seed(123)
fviz_nbclust(kmeans2019, kmeans, method = "wss") 
fviz_nbclust(kmeans2019, kmeans, method = "silhouette") 
# 2 clusters
set.seed(123)
subjectiveK <- kmeans(kmeans2019, centers = 3, nstart = 25)
plot_subjectiveK<-fviz_cluster(subjectiveK,data=kmeans2019, main="Cluster Plot for Economic Variables 2019") 
kmeans2019$cluster <- subjectiveK$cluster 
kmeans2019 <- kmeans2019 %>% relocate(cluster, .before = Happiness)
#final dataset with clusters is called final_economic
profile_plot(kmeans2019)


# world map
# Load country data
countries <- ne_countries(scale = "medium", returnclass = "sf")
countries$geounit[countries$geounit == "Republic of Serbia"] <- 'Serbia'
countries$geounit[countries$geounit == "United States of America"] <- 'United States'

#check if the country names are different from my country in df
kmeans2019$country<-kmeans2019a$country
missing_countries <- setdiff(kmeans2019$country, countries$geounit)


# Merge data with country data
country_data <- merge(countries, kmeans2019, by.x = "geounit", by.y = "country", all.x = TRUE)

# Set the color palette for clusters
cluster_palette <- c("#FF0000", "#00FF00", "#0000FF")

# Create the world map
ggplot() +
  geom_sf(data = country_data, aes(fill = factor(cluster))) +
  scale_fill_manual(values = c(cluster_palette,"grey"), na.value = "gray") +
  theme_minimal()+
  labs(title = "Clusters of Countries on Economics 2019",
       fill = "Cluster")


#### kmeans 2020 economic####
kmeans2020<- final_economic %>% filter(year==2020) 
kmeans2020a<- final_economic %>% filter(year==2020) 
kmeans2020<- kmeans2020 %>% select(-year)
rownames(kmeans2020)<-kmeans2020$country
kmeans2020<- kmeans2020 %>% select(-country)

# methods to see how many clusters
set.seed(123)
fviz_nbclust(kmeans2020, kmeans, method = "wss") 
fviz_nbclust(kmeans2020, kmeans, method = "silhouette") 
# 2 clusters
set.seed(123)
subjectiveK <- kmeans(kmeans2020, centers = 3, nstart = 25)
plot_subjectiveK<-fviz_cluster(subjectiveK,data=kmeans2020, main="Cluster Plot for Economic Variables 2020") 
kmeans2020$cluster <- subjectiveK$cluster 
kmeans2020 <- kmeans2020 %>% relocate(cluster, .before = Happiness)
#final dataset with clusters is called kmeans_economic
profile_plot(kmeans2020)


# world map
# Load country data
countries <- ne_countries(scale = "medium", returnclass = "sf")
countries$geounit[countries$geounit == "Republic of Serbia"] <- 'Serbia'
countries$geounit[countries$geounit == "United States of America"] <- 'United States'

#check if the country names are different from my country in df
kmeans2020$country<-kmeans2020a$country
missing_countries <- setdiff(kmeans2020$country, countries$geounit)


# Merge data with country data
country_data <- merge(countries, kmeans2020, by.x = "geounit", by.y = "country", all.x = TRUE)

# Set the color palette for clusters
cluster_palette <- c("#FF0000", "#00FF00", "#0000FF")

# Create the world map
ggplot() +
  geom_sf(data = country_data, aes(fill = factor(cluster))) +
  scale_fill_manual(values = c(cluster_palette,"grey"), na.value = "gray") +
  theme_minimal()+
  labs(title = "Clusters of Countries on Economic 2020",
       fill = "Cluster")




#### LPI and gap analysis ####
# LPI
Legatum_Prosperity_Index <- read_excel("Legatum Prosperity Index.xlsx",
                                       col_types = c("text", "numeric", "numeric",
                                                           "numeric", "numeric", "numeric",
                                                           "numeric", "numeric", "numeric",
                                                           "numeric", "numeric", "numeric",
                                                           "numeric", "numeric", "numeric"))
# machine learning, pre post covid
# maybe do this by continent

# cross sectional/longitudinal





























#KNN

#random forest

#train and test
set.seed(1234)
train <- final_economic %>% filter(year<2019) %>%
  select(-c(country,year))
test  <- final_economic %>% filter(year>=2019)%>%
  select(-c(country,year))
tab(train, Happiness)
tab(test, Happiiness)

# cross validation for the machine learning, choose the trees,
# run the model
# try to predict
# 

# cross sectional analysis 
# in sample error and out of train error
# MAE MAPE 


train.control <- trainControl(method="none", 
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary)
set.seed(1234)
model.rf <- train(Happiness ~ ., 
                  data = train,
                  method = "rf",
                  metric = "ROC",
                  tuneGrid = data.frame(mtry=4),
                  trControl=train.control,
                  ntree=100,
                  importance=TRUE)

train$prob <- predict(model.rf, train, type="prob")[[2]]
#2020 might be horrible
# lagged y variables as,
# changes in the years rather than just year
library(pROC)
library(plotROC)
ggplot(train, aes(d=y, m=prob)) +
  geom_roc(labelround=2, n.cuts=15, labelsize=3) + 
  style_roc(major.breaks=seq(0, 1, .1),
            minor.breaks=seq(0, 1, .05),
            theme=theme_grey) +
  labs(title="ROC Plot")
auc(train$y, train$prob)






# Specify the formula for the model
formula <- Happiness ~
  `Log GDP per capita`+
  `Agricultural land (sq. km)`+
  `GDP growth (annual %)`+
  `Gross savings (% of GNI)`+
  `Inflation, consumer prices (annual %)`+
  `Mortality rate, infant (per 1,000 live births)`+
  `Labor force, total`

# Fit the Random Forest model
model <- randomForest(formula=Happiness~train$`Log GDP per capita`+
                        train$`Agricultural land (sq. km)`+
                        train$`GDP growth (annual %)`+
                        train$`Gross savings (% of GNI)`+
                        train$`Inflation, consumer prices (annual %)`+
                        train$`Labor force, total`+
                        train$`Mortality rate, infant (per 1,000 live births)`, data = train)

# Make predictions on the test data
predictions <- predict(model, newdata = test, na.action = "na.pass")

# Evaluate the model
mse <- mean((predictions - test_data$happiness_score)^2)


#networks










