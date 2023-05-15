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
# if(!require(remotes)){
#   install.packages("remotes")
# }
# remotes::install_github("rkabacoff/qacEDA")
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


# combine prosperity with df2


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




#### LPI####

Legatum_Prosperity_Index <- read_excel("Legatum Prosperity Index.xlsx",
                                       col_types = c("text", "numeric", "numeric",
                                                           "numeric", "numeric", "numeric",
                                                           "numeric", "numeric", "numeric",
                                                           "numeric", "numeric", "numeric",
                                                           "numeric", "numeric", "numeric"))


# check countries that have different names
setdiff(unique(Legatum_Prosperity_Index$country),unique(df$country_name))
setdiff(unique(df$country_name),unique(Legatum_Prosperity_Index$country))
colnames(df)[colnames(df) == "country_name"] <- "country"
colnames(df)[colnames(df) == "Life Ladder"] <- "happiness"
Legatum_Prosperity_Index$country[Legatum_Prosperity_Index$country == "Democratic Republic Congo"] <- 'Dem Republic of the Congo'
Legatum_Prosperity_Index$country[Legatum_Prosperity_Index$country == "Czech Republic"] <- 'Czechia'
Legatum_Prosperity_Index$country[Legatum_Prosperity_Index$country == "Côte d'Ivoire"] <- 'Ivory Coast'
Legatum_Prosperity_Index$country[Legatum_Prosperity_Index$country == "Congo"] <- 'Republic of the Congo'
Legatum_Prosperity_Index$country[Legatum_Prosperity_Index$country == "Macedonia"] <- 'North Macedonia'
Legatum_Prosperity_Index$country[Legatum_Prosperity_Index$country == "Swaziland"] <- 'Eswatini'
# merge with df
mergedf<-merge(df, Legatum_Prosperity_Index, by =c("country","year"),all.x=TRUE)
prosperity<-mergedf[,c(1:3, 38:50)]
#prosperity only has 2007-2023, so the merged dataset will have NAs for 2006
prosperity<-na.omit(prosperity) #1331 to 1274 observations


#scale
scale_prosperity<-scale(prosperity[,3:16], center = TRUE, scale = TRUE)
final_prosperity<-as.data.frame(scale_prosperity)
final_prosperity$country<-prosperity$country
final_prosperity$year<-prosperity$year
final_prosperity <- final_prosperity %>% relocate(country, .before = happiness) %>%
  relocate(year, .before = happiness) 

#correlation matrix
corr_matrix4 <- cor(final_prosperity[,3:16])
ggcorrplot(corr_matrix4)
ggpairs(final_prosperity[,3:16])
#PCA
data.pca4 <-princomp(corr_matrix4)
summary(data.pca4)
data.pca4$loadings[,1:5]
fviz_eig(data.pca4, addlabels = TRUE)
fviz_pca_var(data.pca4, col.var = "black")
fviz_cos2(data.pca4, choice="var",axes =1:3)
fviz_pca_var(data.pca4, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

# fixed effects

fixed_lm5<-felm(formula= happiness~
                  `Safety and Security`+
                  `Personal Freedom`+
                  Governance+
                  `Social Capital`+
                  `Investment Environment`+
                  `Enterprise Conditions`+
                  `Infrastructure and Market Access`+
                  `Economic Quality`+
                  `Living Conditions`+
                  Health+
                  Education+
                  `Natural Environment`| country + year,
                data=final_prosperity)
summary(fixed_lm5)

tab_model(fixed_lm5, show.aic = TRUE, dv.labels = "Prosperity on Happiness")

# fixed effects all
# merge df2 and final prosperity
df_all<-merge(final_prosperity, df2, by =c("country","year"),all.x=TRUE)
fixed_lm6<-felm(formula= happiness~
                  `Safety and Security`+
                  `Personal Freedom`+
                  Governance+
                  `Social Capital`+
                  `Investment Environment`+
                  `Enterprise Conditions`+
                  `Infrastructure and Market Access`+
                  `Economic Quality`+
                  `Living Conditions`+
                  Health+
                  Education+
                  `Natural Environment` +
                  `Social support`+
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
                data=df_all)
summary(fixed_lm6)
tab_model(fixed_lm6, show.aic = TRUE, dv.labels = "Prosperity, Economic, Citizen's Opinion, Subjective on Happiness")






#### cross sectional economic, subjective, prosperity 2015- 2020####
final_economic2015<-final_economic %>% filter(year==2015)
economic2015_lm<- lm(Happiness ~
                       `Log GDP per capita`+
                       `Agricultural land (sq. km)`+
                       `GDP growth (annual %)`+
                       `Gross savings (% of GNI)`+
                       `Inflation, consumer prices (annual %)`+
                       `Mortality rate, infant (per 1,000 live births)`+
                       `Labor force, total`, data = final_economic2015)
summary(economic2015_lm)
tab_model(economic2015_lm, show.aic = TRUE, dv.labels = "Economic on Happiness 2015")

final_economic2016<-final_economic %>% filter(year==2016)
economic2016_lm<- lm(Happiness ~
                       `Log GDP per capita`+
                       `Agricultural land (sq. km)`+
                       `GDP growth (annual %)`+
                       `Gross savings (% of GNI)`+
                       `Inflation, consumer prices (annual %)`+
                       `Mortality rate, infant (per 1,000 live births)`+
                       `Labor force, total`, data = final_economic2016)
summary(economic2016_lm)
tab_model(economic2016_lm, show.aic = TRUE, dv.labels = "Economic on Happiness 2016")

final_economic2017<-final_economic %>% filter(year==2017)
economic2017_lm<- lm(Happiness ~
                       `Log GDP per capita`+
                       `Agricultural land (sq. km)`+
                       `GDP growth (annual %)`+
                       `Gross savings (% of GNI)`+
                       `Inflation, consumer prices (annual %)`+
                       `Mortality rate, infant (per 1,000 live births)`+
                       `Labor force, total`, data = final_economic2017)
summary(economic2017_lm)
tab_model(economic2017_lm, show.aic = TRUE, dv.labels = "Economic on Happiness 2017")

final_economic2018<-final_economic %>% filter(year==2018)
economic2018_lm<- lm(Happiness ~
                       `Log GDP per capita`+
                       `Agricultural land (sq. km)`+
                       `GDP growth (annual %)`+
                       `Gross savings (% of GNI)`+
                       `Inflation, consumer prices (annual %)`+
                       `Mortality rate, infant (per 1,000 live births)`+
                       `Labor force, total`, data = final_economic2018)
summary(economic2018_lm)
tab_model(economic2018_lm, show.aic = TRUE, dv.labels = "Economic on Happiness 2018")

final_economic2019<-final_economic %>% filter(year==2019)
economic2019_lm<- lm(Happiness ~
                       `Log GDP per capita`+
                       `Agricultural land (sq. km)`+
                       `GDP growth (annual %)`+
                       `Gross savings (% of GNI)`+
                       `Inflation, consumer prices (annual %)`+
                       `Mortality rate, infant (per 1,000 live births)`+
                       `Labor force, total`, data = final_economic2019)
summary(economic2019_lm)
tab_model(economic2019_lm, show.aic = TRUE, dv.labels = "Economic on Happiness 2019")

final_economic2020<-final_economic %>% filter(year==2020)
economic2020_lm<- lm(Happiness ~
                       `Log GDP per capita`+
                       `Agricultural land (sq. km)`+
                       `GDP growth (annual %)`+
                       `Gross savings (% of GNI)`+
                       `Inflation, consumer prices (annual %)`+
                       `Mortality rate, infant (per 1,000 live births)`+
                       `Labor force, total`, data = final_economic2020)
summary(economic2020_lm)
tab_model(economic2020_lm, show.aic = TRUE, dv.labels = "Economic on Happiness 2020")


# subjective

final_subjective2015<-final_subjective %>% filter(year==2015)
subjective2015_lm<- lm(`Happiness`~`Social support`+
                         `Healthy life expectancy at birth` +
                         `Freedom to make life choices`+
                         `Generosity`+
                         `Perceptions of corruption`+
                         `Positive affect` +
                         `Negative affect`+
                         `Confidence in national government`, data = final_subjective2015)
summary(subjective2015_lm)
tab_model(subjective2015_lm, show.aic = TRUE, dv.labels = "Subjective on Happiness 2015")

final_subjective2016<-final_subjective %>% filter(year==2016)
subjective2016_lm<- lm(`Happiness`~`Social support`+
                       `Healthy life expectancy at birth` +
                       `Freedom to make life choices`+
                       `Generosity`+
                       `Perceptions of corruption`+
                       `Positive affect` +
                       `Negative affect`+
                       `Confidence in national government`, data = final_subjective2016)
summary(subjective2016_lm)
tab_model(subjective2016_lm, show.aic = TRUE, dv.labels = "Subjective on Happiness 2016")

final_subjective2017<-final_subjective %>% filter(year==2017)
subjective2017_lm<- lm(`Happiness`~`Social support`+
                       `Healthy life expectancy at birth` +
                       `Freedom to make life choices`+
                       `Generosity`+
                       `Perceptions of corruption`+
                       `Positive affect` +
                       `Negative affect`+
                       `Confidence in national government`, data = final_subjective2017)
summary(subjective2017_lm)
tab_model(subjective2017_lm, show.aic = TRUE, dv.labels = "Subjective on Happiness 2017")

final_subjective2018<-final_subjective %>% filter(year==2018)
subjective2018_lm<- lm(`Happiness`~`Social support`+
                       `Healthy life expectancy at birth` +
                       `Freedom to make life choices`+
                       `Generosity`+
                       `Perceptions of corruption`+
                       `Positive affect` +
                       `Negative affect`+
                       `Confidence in national government`, data = final_subjective2018)
summary(subjective2018_lm)
tab_model(subjective2018_lm, show.aic = TRUE, dv.labels = "Subjective on Happiness 2018")

final_subjective2019<-final_subjective %>% filter(year==2019)
subjective2019_lm<- lm(`Happiness`~`Social support`+
                       `Healthy life expectancy at birth` +
                       `Freedom to make life choices`+
                       `Generosity`+
                       `Perceptions of corruption`+
                       `Positive affect` +
                       `Negative affect`+
                       `Confidence in national government`, data = final_subjective2019)
summary(subjective2019_lm)
tab_model(subjective2019_lm, show.aic = TRUE, dv.labels = "Subjective on Happiness 2019")

final_subjective2020<-final_subjective %>% filter(year==2020)
subjective2020_lm<- lm(`Happiness`~`Social support`+
                       `Healthy life expectancy at birth` +
                       `Freedom to make life choices`+
                       `Generosity`+
                       `Perceptions of corruption`+
                       `Positive affect` +
                       `Negative affect`+
                       `Confidence in national government`, data = final_subjective2020)
summary(subjective2020_lm)
tab_model(subjective2020_lm, show.aic = TRUE, dv.labels = "Subjective on Happiness 2020")


# prosperity

final_prosperity2015<-final_prosperity %>% filter(year==2015)
prosperity2015_lm<- lm(happiness~
                         `Safety and Security`+
                         `Personal Freedom`+
                         Governance+
                         `Social Capital`+
                         `Investment Environment`+
                         `Enterprise Conditions`+
                         `Infrastructure and Market Access`+
                         `Economic Quality`+
                         `Living Conditions`+
                         Health+
                         Education+
                         `Natural Environment`, data = final_prosperity2015)
summary(prosperity2015_lm)
tab_model(prosperity2015_lm, show.aic = TRUE, dv.labels = "Prosperity on Happiness 2015")

final_prosperity2016<-final_prosperity %>% filter(year==2016)
prosperity2016_lm<- lm(happiness~
                         `Safety and Security`+
                         `Personal Freedom`+
                         Governance+
                         `Social Capital`+
                         `Investment Environment`+
                         `Enterprise Conditions`+
                         `Infrastructure and Market Access`+
                         `Economic Quality`+
                         `Living Conditions`+
                         Health+
                         Education+
                         `Natural Environment`, data = final_prosperity2016)
summary(prosperity2016_lm)
tab_model(prosperity2016_lm, show.aic = TRUE, dv.labels = "Prosperity on Happiness 2016" )

final_prosperity2017<-final_prosperity %>% filter(year==2017)
prosperity2017_lm<- lm(happiness~
                         `Safety and Security`+
                         `Personal Freedom`+
                         Governance+
                         `Social Capital`+
                         `Investment Environment`+
                         `Enterprise Conditions`+
                         `Infrastructure and Market Access`+
                         `Economic Quality`+
                         `Living Conditions`+
                         Health+
                         Education+
                         `Natural Environment`, data = final_prosperity2017)
summary(prosperity2017_lm)
tab_model(prosperity2017_lm, show.aic = TRUE, dv.labels = "Prosperity on Happiness 2017")

final_prosperity2018<-final_prosperity %>% filter(year==2018)
prosperity2018_lm<- lm(happiness~
                         `Safety and Security`+
                         `Personal Freedom`+
                         Governance+
                         `Social Capital`+
                         `Investment Environment`+
                         `Enterprise Conditions`+
                         `Infrastructure and Market Access`+
                         `Economic Quality`+
                         `Living Conditions`+
                         Health+
                         Education+
                         `Natural Environment`, data = final_prosperity2018)
summary(prosperity2018_lm)
tab_model(prosperity2018_lm, show.aic = TRUE, dv.labels = "Prosperity on Happiness 2018")

final_prosperity2019<-final_prosperity %>% filter(year==2019)
prosperity2019_lm<- lm(happiness~
                         `Safety and Security`+
                         `Personal Freedom`+
                         Governance+
                         `Social Capital`+
                         `Investment Environment`+
                         `Enterprise Conditions`+
                         `Infrastructure and Market Access`+
                         `Economic Quality`+
                         `Living Conditions`+
                         Health+
                         Education+
                         `Natural Environment`, data = final_prosperity2019)
summary(prosperity2019_lm)
tab_model(prosperity2019_lm, show.aic = TRUE, dv.labels = "Prosperity on Happiness 2019")

final_prosperity2020<-final_prosperity %>% filter(year==2020)
prosperity2020_lm<- lm(happiness~
                         `Safety and Security`+
                         `Personal Freedom`+
                         Governance+
                         `Social Capital`+
                         `Investment Environment`+
                         `Enterprise Conditions`+
                         `Infrastructure and Market Access`+
                         `Economic Quality`+
                         `Living Conditions`+
                         Health+
                         Education+
                         `Natural Environment`, data = final_prosperity2020)
summary(prosperity2020_lm)
tab_model(prosperity2020_lm, show.aic = TRUE, dv.labels = "Prosperity on Happiness 2020")








# cross sectional all
df_all2015<-df_all %>% filter(year==2015)
all2015_lm<- lm(formula= happiness~
                  `Safety and Security`+
                  `Personal Freedom`+
                  Governance+
                  `Social Capital`+
                  `Investment Environment`+
                  `Enterprise Conditions`+
                  `Infrastructure and Market Access`+
                  `Economic Quality`+
                  `Living Conditions`+
                  Health+
                  Education+
                  `Natural Environment` +
                  `Social support`+
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
                  `Individuals using the Internet (% of population)`, data = df_all2015)
summary(all2015_lm)
tab_model(all2015_lm, show.aic = TRUE, dv.labels = "All Indicators on Happiness 2015")

df_all2016<-df_all %>% filter(year==2016)
all2016_lm<- lm(formula= happiness~
                       `Safety and Security`+
                       `Personal Freedom`+
                       Governance+
                       `Social Capital`+
                       `Investment Environment`+
                       `Enterprise Conditions`+
                       `Infrastructure and Market Access`+
                       `Economic Quality`+
                       `Living Conditions`+
                       Health+
                       Education+
                       `Natural Environment` +
                       `Social support`+
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
                       `Individuals using the Internet (% of population)`, data = df_all2016)
summary(all2016_lm)
tab_model(all2016_lm, show.aic = TRUE, dv.labels = "All Indicators on Happiness 2016")

df_all2017<-df_all %>% filter(year==2017)
all2017_lm<- lm(formula= happiness~
                       `Safety and Security`+
                       `Personal Freedom`+
                       Governance+
                       `Social Capital`+
                       `Investment Environment`+
                       `Enterprise Conditions`+
                       `Infrastructure and Market Access`+
                       `Economic Quality`+
                       `Living Conditions`+
                       Health+
                       Education+
                       `Natural Environment` +
                       `Social support`+
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
                       `Individuals using the Internet (% of population)`, data = df_all2017)
summary(all2017_lm)
tab_model(all2017_lm, show.aic = TRUE, dv.labels = "All Indicators on Happiness 2017")


df_all2018<-df_all %>% filter(year==2018)
all2018_lm<- lm(formula= happiness~
                       `Safety and Security`+
                       `Personal Freedom`+
                       Governance+
                       `Social Capital`+
                       `Investment Environment`+
                       `Enterprise Conditions`+
                       `Infrastructure and Market Access`+
                       `Economic Quality`+
                       `Living Conditions`+
                       Health+
                       Education+
                       `Natural Environment` +
                       `Social support`+
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
                       `Individuals using the Internet (% of population)`, data = df_all2018)
summary(all2018_lm)
tab_model(all2018_lm, show.aic = TRUE, dv.labels = "All Indicators on Happiness 2018")


df_all2019<-df_all %>% filter(year==2019)
all2019_lm<- lm(formula= happiness~
                       `Safety and Security`+
                       `Personal Freedom`+
                       Governance+
                       `Social Capital`+
                       `Investment Environment`+
                       `Enterprise Conditions`+
                       `Infrastructure and Market Access`+
                       `Economic Quality`+
                       `Living Conditions`+
                       Health+
                       Education+
                       `Natural Environment` +
                       `Social support`+
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
                       `Individuals using the Internet (% of population)`, data = df_all2019)
summary(all2019_lm)
tab_model(all2019_lm, show.aic = TRUE, dv.labels = "All Indicators on Happiness 2019")


df_all2020<-df_all %>% filter(year==2020)
all2020_lm<- lm(formula= happiness~
                       `Safety and Security`+
                       `Personal Freedom`+
                       Governance+
                       `Social Capital`+
                       `Investment Environment`+
                       `Enterprise Conditions`+
                       `Infrastructure and Market Access`+
                       `Economic Quality`+
                       `Living Conditions`+
                       Health+
                       Education+
                       `Natural Environment` +
                       `Social support`+
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
                       `Individuals using the Internet (% of population)`, data = df_all2020)
summary(all2020_lm)
tab_model(all2020_lm, show.aic = TRUE, dv.labels = "All Indicators on Happiness 2020")








#### Gap Analysis ####
# machine learning, pre post covid
# maybe do this by continent

# cross validation, choose trees
# run the model
#predict
# in sample error, out of train error, MAE MAPE
# 2020 were horrible
# maybe do this by lagged? so changes in the years rather than just years

# all indicators, about 48 variables, less than observations
train_all<-df_all %>% filter(year<2019) %>%
  select(-country & -year & -prosperity) 
test_all<-df_all %>% filter(year>=2019)%>%
  select(-country & -year & -prosperity) 

dim(train_all)
dim(test_all)

# Step 1: Prepare the input variables and target variable
set.seed(1234)
train_X <- train_all[, c(2:48)]  # Include the relevant independent variables
train_y <- train_all$happiness 

# Step 2: Train the random forest model
set.seed(1234)
rf_model <- randomForest(train_X, train_y, ntree = 100, importance = TRUE) # how many trees do we want?
rf_model

# Get variable importance from the model fit
ImpData <- as.data.frame(importance(rf_model))
ImpData$Var.Names <- row.names(ImpData)

ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )


#simple importance chart
importance(rf_model)
varImpPlot(rf_model)





# Step 3: Predict using the trained model
test_X <- test_all[, c(2:48)]  # Include the same independent variables as in training
predictions <- predict(rf_model, test_X)

# Step 4: Evaluate the model's performance
actuals <- test_all$happiness  # Replace "target_variable" with the actual name of the target variable
accuracy <- sum(predictions == actuals) / length(actuals)
print(paste("Accuracy:", accuracy)) # 0 accuracy since none of the predictions matched the actual

#method 2, evaluate
accuracy_all<- mean(predictions==test_all$happiness,na.rm=TRUE) #accuracy still 0


# method 3: Mean Squared Error (MSE) or Root Mean Squared Error (RMSE)
#predictions <- predict(rf_model, test_economic)
mse <- mean((predictions - test_all$happiness)^2)
rmse <- sqrt(mse)

# method 4: R-squared (Coefficient of Determination)
r_squared <- cor(predictions, test_all$happiness)^2

# method 5: Cross-Validation
library(caret)
# Define cross-validation parameters (e.g., k-fold cross-validation)
control <- trainControl(method = "cv", number = 5)  
# Train the model using cross-validation
model_cv <- train(happiness ~ ., data = train_all, method = "rf", trControl = control)
# Get the cross-validated performance metrics
cv_results <- model_cv$results
cv_accuracy <- max(cv_results$Accuracy)  # Example: Accuracy metric
cv_rmse <- min(cv_results$RMSE)  # Example: RMSE metric
cv_r_squared <- max(cv_results$Rsquared)  # Example: R-squared metric





























#### ignore ####



# step 1: split data pre 2019 and 2019-2020
train_economic<-final_economic %>% filter(year<2019) %>%
  select(-country & -year) 
test_economic<-final_economic %>% filter(year>=2019)%>%
  select(-country & -year) 

dim(train_economic)
dim(test_economic)
# cross validation
# Set the number of folds for cross-validation
num_folds <- 5

# Create the training control object for cross-validation
ctrl <- trainControl(
  method = "cv",
  number = num_folds,
  verboseIter = FALSE
)

# Perform cross-validation using random forest
set.seed(123)  # Set a seed for reproducibility
model <- train(
  Happiness ~ .,  # Replace with your response variable and predictor variables
  data = train_economic,  # Replace with your dataset
  method = "rf",  # Random forest method
  trControl = ctrl  # Use the training control object
)
model
















# random forest, not good
randomforest_economic<-randomForest(
  formula= Happiness ~
    train_economic$`Log GDP per capita`+
    train_economic$`Agricultural land (sq. km)`+
    train_economic$`GDP growth (annual %)`+
    train_economic$`Gross savings (% of GNI)`+
    train_economic$`Inflation, consumer prices (annual %)`+
    train_economic$`Mortality rate, infant (per 1,000 live births)`+
    train_economic$`Labor force, total`,
  data= train_economic,
  importance=TRUE,
  ntree = 500
)

predict_economic<- predict(randomforest_economic,newdata = train_economic, na.action="na.pass")

accuracy_economic<- mean(predictions==test_economic$Happiness,na.rm=TRUE) # accuracy 0



















# different method of random forest
# Step 1: Prepare the input variables and target variable
set.seed(1234)
train_X <- train_economic[, c(2:8)]  # Include the relevant independent variables
train_y <- train_economic$Happiness  # Replace "target_variable" with the actual name of the target variable

# Step 2: Train the random forest model
rf_model <- randomForest(train_X, train_y, ntree = 100)

# Step 3: Predict using the trained model
test_X <- test_economic[, c(2:8)]  # Include the same independent variables as in training
predictions <- predict(rf_model, test_X)

# Step 4: Evaluate the model's performance
actuals <- test_economic$Happiness  # Replace "target_variable" with the actual name of the target variable
accuracy <- sum(predictions == actuals) / length(actuals)
print(paste("Accuracy:", accuracy)) # 0 accuracy since none of the predictions matched the actual

#method 2, evaluate
accuracy_economic<- mean(predictions==test_economic$Happiness,na.rm=TRUE) #accuracy still 0


# method 3: Mean Squared Error (MSE) or Root Mean Squared Error (RMSE)
predictions <- predict(rf_model, test_economic)
mse <- mean((predictions - test_economic$Happiness)^2)
rmse <- sqrt(mse)

# method 4: R-squared (Coefficient of Determination)
r_squared <- cor(predictions, test_economic$Happiness)^2

# method 5: Cross-Validation
library(caret)
# Define cross-validation parameters (e.g., k-fold cross-validation)
control <- trainControl(method = "cv", number = 5)  
# Train the model using cross-validation
model_cv <- train(Happiness ~ ., data = train_economic, method = "rf", trControl = control)
# Get the cross-validated performance metrics
cv_results <- model_cv$results
cv_accuracy <- max(cv_results$Accuracy)  # Example: Accuracy metric
cv_rmse <- min(cv_results$RMSE)  # Example: RMSE metric
cv_r_squared <- max(cv_results$Rsquared)  # Example: R-squared metric


test_economic$pred<-predictions
test_economic$country<-test_economic2$country
test_economic$year<-test_economic2$year
test_economic$diff <- test_economic$Happiness - test_economic$pred

# Plotting the scatter plot of diff over time
plot(factor(test_economic$year), test_economic$diff, pch = 16, xlab = "Year", ylab = "Difference (Actual - Pred)")
test_economic_2019<-test_economic %>% select(c("country","year","Happiness","pred")) %>%
  filter(year==2019)

test_economic_2019 <- test_economic_2019 %>%
  pivot_longer(cols = c(Happiness, pred), names_to = "Type", values_to = "Happiness") %>%
  mutate(category = ifelse(Type == "Happiness", "Actual", "Pred"))

# Create the scatter plot
ggplot(test_economic_2019, aes(x = country, y = Happiness, color = category)) +
  geom_point() +
  labs(x = "Country", y = "Value", color = "Category") +
  ggtitle("Scatter Plot of Value by Category")

ggplot(test_economic_2019, aes(x = country, y = Happiness, color = category)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Country", y = "Value", color = "Category") +
  ggtitle("Scatter Plot of Value by Category")

ggplot(test_economic_2019, aes(x = country, y = Happiness, color = category, shape = category)) +
  geom_point() +
  labs(x = "Country", y = "Value", color = "Category", shape = "Category") +
  ggtitle("Scatter Plot of Value by Country (Patterned by Category)") +
  coord_flip()
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










