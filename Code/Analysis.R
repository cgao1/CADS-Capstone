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
library(nFactors)
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
# if(!require(remotes)){
#   install.packages("remotes")
# }
# remotes::install_github("rkabacoff/qacDR")
library(qacDR)
library(psych)
library(cluster)
library(NbClust)
library(flexclust)
library(fMultivar)
library(rattle)
library(mvoutlier)
library(CGPfunctions)


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
# does not include prosperity

# Next combine prosperity with df2


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

#### prosperity 2020 ####
kmeans2020<- final_prosperity %>% filter(year==2020) 
kmeans2020a<- final_prosperity %>% filter(year==2020) 
kmeans2020<- kmeans2020 %>% select(-year)
rownames(kmeans2020)<-kmeans2020$country
kmeans2020<- kmeans2020 %>% select(-country)

# methods to see how many clusters
set.seed(123)
fviz_nbclust(kmeans2020, kmeans, method = "wss") 
fviz_nbclust(kmeans2020, kmeans, method = "silhouette") 
# 2 clusters
set.seed(123)
subjectiveK <- kmeans(kmeans2020, centers = 2, nstart = 25)
plot_subjectiveK<-fviz_cluster(subjectiveK,data=kmeans2020, main="Cluster Plot for prosperity Variables 2020") 
kmeans2020$cluster <- subjectiveK$cluster 
kmeans2020 <- kmeans2020 %>% relocate(cluster, .before = happiness)
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
  labs(title = "Clusters of Countries on Prosperity 2020",
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
prosperity<-na.omit(prosperity) #1331 to 1275 observations


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
df_all<- df_all %>% select(-Happiness)
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






#### cross sectional economic, 2015- 2020 ####
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
tab_model(economic2015_lm,economic2016_lm,economic2017_lm,economic2018_lm,economic2019_lm,economic2020_lm, 
          show.aic = TRUE, title="Economic on Happiness",
          dv.labels = c("2015","2016","2017","2018","2019","2020"))


#### subjective cross sectional ####

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



tab_model(subjective2015_lm,subjective2016_lm,subjective2017_lm,subjective2018_lm,subjective2019_lm,subjective2020_lm, 
          show.aic = TRUE, title="Citizen Opinion on Happiness",
          dv.labels = c("2015","2016","2017","2018","2019","2020"))



#### prosperity cross sectional ####

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


tab_model(prosperity2015_lm,prosperity2016_lm,prosperity2017_lm,prosperity2018_lm,prosperity2019_lm,prosperity2020_lm, 
          show.aic = TRUE, title="Prosperity on Happiness",
          dv.labels = c("2015","2016","2017","2018","2019","2020"))





#### cross sectional all ####
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


tab_model(all2015_lm,all2016_lm,all2017_lm,all2018_lm,all2019_lm,all2020_lm, 
          show.aic = TRUE, title="All on Happiness",
          dv.labels = c("2015","2016","2017","2018","2019","2020"))






#### factor analysis ####
fa_all<-df_all[,c(3,5:50)] # all the variables together
fa_pca<-df_all[,c(1:3,5:16,17:25,35,39,41,42,46,43,26,30,47,48,50)] #selected variables from PCA
numfa_pca<- fa_pca %>% filter(year==2020) %>%
  select(-country &-year)
numfa_pca2<- fa_pca %>% filter(year==2020)
numfa_pca_cor<-cor(numfa_pca)
# psyc package
# Perform parallel analysis to find nfactor
parallel_results <- fa.parallel(numfa_pca_cor, fm = "pa", fa = "both")
abline(a=1, b=0)
# recommends 3 factors


#qacDR factor analysis
fit.fa <- FA(numfa_pca, nfactors=3, rotate="varimax")
#fa(numfa_pca, nfactors=3, rotate="varimax", fm="pa")
# plot factor analysis
plot(fit.fa, sort=TRUE)
plot(fit.fa, sort=TRUE, type="table")

fviz_nbclust(numfa_pca, pam, method = "wss")
set.seed(1234)
rownames(numfa_pca)<-numfa_pca2$country
mydata <- score(numfa_pca, fit.fa)
head(mydata)
fit.pam <- pam(mydata[,34:36], k=3, stand=FALSE)
clusplot(fit.pam, main="Bivariate Cluster Plot")
mydata <- cbind(mydata[,34:36], cluster = fit.pam$cluster)
fviz_cluster(fit.pam, data=mydata)
mydata$country<-numfa_pca2$country

# world map
# Load country data
countries <- ne_countries(scale = "medium", returnclass = "sf")
countries$geounit[countries$geounit == "Republic of Serbia"] <- 'Serbia'
countries$geounit[countries$geounit == "United States of America"] <- 'United States'
country_data <- merge(countries, mydata, by.x = "geounit", by.y = "country", all.x = TRUE)

# Set the color palette for clusters
cluster_palette <- c("#FF0000", "#00FF00", "#0000FF")

# Create the world map
ggplot() +
  geom_sf(data = country_data, aes(fill = factor(cluster))) +
  scale_fill_manual(values = c(cluster_palette,"grey"), na.value = "gray") +
  theme_minimal()+
  labs(title = "Clusters of Countries on Factor Analysis 2020",
       fill = "Cluster")


#### cluster analysis PAM

#check outliers
x <- log(numfa_pca)
outliers <- aq.plot(numfa_pca, delta=qchisq(0.975, df = ncol(numfa_pca)),
                    quan = 1/2, alpha = 0.05)
str(outliers)

#nbclust
num_clust2020<-fa_pca %>% filter(year==2020) %>%
  select(-country & -year)
nc<- NbClust(num_clust2020, min.nc = 2, max.nc = 15, method="kmeans")
nc<- NbClust(numfa_pca, min.nc = 2, max.nc = 15, method="kmeans")


#pam
all_pam<-pam(num_clust2020, k=5, stand=TRUE)
all_pam$medoids
clusplot(all_pam, main = "Cluster Plot for 2020")

all_pam<-pam(numfa_pca, k=2, stand=TRUE)
all_pam$medoids
clusplot(all_pam, main = "Cluster Plot for All years")

factor_year<- fa_pca %>% 
  filter(country %in% c("United States","India","Canada","Japan","South Korea","Germany",
                        "Spain","France","Italy","Burundi","Ghana","Sweden","Finland","Norway")) %>%
  mutate(year = factor(year),
         happiness = round(happiness, digits = 2))
factor_year_all<- fa_pca %>% 
  mutate(year = factor(year),
         happiness = round(happiness, digits = 2))
# country by variable
newggslopegraph(factor_year_all, year, happiness, country) +
  labs(title="Happiness by Country", 
       subtitle = "Happiness in Z scores",
       caption="source: World Happiness Report")

# country by variable
newggslopegraph(factor_year, year, happiness, country) +
  labs(title="Happiness by Country", 
       subtitle = "Happiness in Z scores",
       caption="source: World Happiness Report")













#### Gap Analysis ####
# machine learning, pre post covid
# could maybe do this by continent in future

# cross validation, choose trees
# run the model
# predict
# in sample error, out of train error, MAE MAPE
# 2020 were horrible models
# maybe do this by lagged, such that we look at changes in the years rather than just years
# this section does not have a great predictive model, future research can expand and improve this section

# all indicators, about 48 variables, less than observations
train_all<-fa_pca %>% filter(year<2019) %>%
  select(-country & -year) 
test_all<-fa_pca %>% filter(year>=2019)%>%
  select(-country & -year) 

dim(train_all)
dim(test_all)

# Step 1: Prepare the input variables and target variable
set.seed(1234)
train_X <- train_all[, c(2:33)]  # Include the relevant independent variables
train_y <- train_all$happiness 

# Step 2: Train the random forest model
set.seed(1234)
rf_model <- randomForest(train_X, train_y, ntree = 100, importance = TRUE) # how many trees do we want?
rf_model

# Get variable importance from the model fit
ImpData <- as.data.frame(randomForest::importance(rf_model))
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
randomForest::importance(rf_model)
varImpPlot(rf_model)





# Step 3: Predict using the trained model
test_X <- test_all[, c(2:33)]  # Include the same independent variables as in training
predictions <- predict(rf_model, test_X)

# Step 4: Evaluate the model's performance
actuals <- test_all$happiness  
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
model_cv <- train(happiness ~ ., 
                  data = train_all, 
                  method = "rf", 
                  trControl = control)
# Get the cross-validated performance metrics
cv_results <- model_cv$results
cv_accuracy <- max(cv_results$Accuracy)  # Example: Accuracy metric
cv_rmse <- min(cv_results$RMSE)  # Example: RMSE metric
cv_r_squared <- max(cv_results$Rsquared)  # Example: R-squared metric






















#### ignore ####

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
# cross validation for the machine learning, choose the trees,
# run the model
# try to predict
# cross sectional analysis 
# in sample error and out of train error
# MAE MAPE 


train.control <- trainControl(method="none", 
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary)

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




#networks



