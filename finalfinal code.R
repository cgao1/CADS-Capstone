
barplot(table(factor(df3$year)),main="Frequency of Available Data by Years" ,xlab="Years",
        ylab="Frequency", col="cornflowerblue")
barplot((colMeans(is.na(df3)))*100, main = "Percent of Missing Data in Merged Dataset", xlab= "Variables", ylab = "Percentage")

# upload dataset
df<-read_csv("df.csv")
morethan10missing<- function(x){
  nlist<-c()
  for (i in 1:length(colnames(x))){
    if((colMeans(is.na(x[i])))*100>=10){
      nlist<-append(nlist,i)
    }
    else{
      next
    }
  }
  return(nlist)
}
missing<-morethan10missing(df)
# 38 variables are left with no more than 10% missing in df3
df3<-subset(df,select=-missing)
df3<-subset(df3, df3$year != 2005)
df3<- na.omit(df3) # 1331 observations left from 1740

subjective<- df3[,c(1:4,6:13)]
social<- df3[,c(1:4,14:19,33,35,36,38)]
economic<- df3[,c(1:4,5,20:32,34,37)]



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





PCA_economic<-economic[,c(1:5,9,12,13,15,16,17,19)]
num_PCA_economic <- PCA_economic[, sapply(PCA_economic, is.numeric)] 
ggpairs(num_PCA_economic)

PCA_social<-social[,c(1:4,5,9,12,13,14)]
num_PCA_social <- PCA_social[, sapply(PCA_social, is.numeric)] 
ggpairs(num_PCA_social)

# z scores
final_subjective<- scale(num_data_subjective, center = TRUE, scale = TRUE)
final_economic<- scale(num_PCA_economic, center = TRUE, scale = TRUE)
final_social<- scale(num_PCA_social, center = TRUE, scale = TRUE)

colnames(final_subjective)[colnames(final_subjective) == "Life Ladder"] <- "Happiness"
colnames(final_economic)[colnames(final_economic) == "Life Ladder"] <- "Happiness"
colnames(final_social)[colnames(final_social) == "Life Ladder"] <- "Happiness"

# final_subjective<-as.data.frame()
# subjective regression
lm1<-lm(Happiness~.,data = as.data.frame(final_subjective))
summary(lm1)
stepAIC(lm1)
tab_model(lm1, show.aic = TRUE,show.obs = FALSE, dv.labels = "Subjective on Happiness")
layout(matrix(c(1,2,3,4),2,2))
plot(lm1)


lm2<-lm(Happiness~.,data = as.data.frame(final_economic))
summary(lm2)
stepAIC(lm2)
layout(matrix(c(1,2,3,4),2,2))
plot(lm2_2)
lm2_2<-lm(Happiness~`year`+
            `Agricultural land (sq. km)`+
            poly(`GDP per capita (constant 2015 US$)`, degree=2) +
            `GDP growth (annual %)`+
            `Gross savings (% of GNI)`+
            `Inflation, consumer prices (annual %)`+
            `Mortality rate, infant (per 1,000 live births)`+
            `Labor force, total`,data = as.data.frame(final_economic))
summary(lm2_2)

tab_model(lm2_2, show.aic = TRUE,show.obs = FALSE, dv.labels = "Economic on Happiness")




lm3<-lm(Happiness~.,data = as.data.frame(final_social))
summary(lm3)
stepAIC(lm3)
layout(matrix(c(1,2,3,4),2,2))
plot(lm3)


tab_model(lm3, show.aic = TRUE,show.obs = FALSE, dv.labels = "Social on Happiness")





train_economic<-subset(PCA_economic, year %in% c(2006:2015)) # 64%
test_economic<-subset(PCA_economic, year %in% c(2016:2020))
num_train_economic <- train_economic[, sapply(train_economic, is.numeric)] 
num_test_economic <- test_economic[, sapply(test_economic, is.numeric)] 
scale_train_economic<-scale(num_train_economic)
scale_test_economic<-scale(num_test_economic)


model.lm1 <- train(`Life Ladder` ~`year`+
                     `Agricultural land (sq. km)`+
                     poly(`GDP per capita (constant 2015 US$)`, degree=2) +
                     `GDP growth (annual %)`+
                     `Gross savings (% of GNI)`+
                     `Inflation, consumer prices (annual %)`+
                     `Mortality rate, infant (per 1,000 live births)`+
                     `Labor force, total`,
                  data=num_train_economic,
                  method="lm",
                  trControl = trainControl(method="none"),
                  metric="RMSE")
summary(model.lm1)

library(visreg)
visreg(model.lm1, gg=TRUE)

model.lm2 <- train(`Life Ladder` ~`year`+
                     `Agricultural land (sq. km)`+
                     poly(`GDP per capita (constant 2015 US$)`, degree=2) +
                     `Mortality rate, infant (per 1,000 live births)`,
                   data=num_train_economic,
                   method="lm",
                   trControl = trainControl(method="none"),
                   metric="RMSE")
summary(model.lm2)


test_economic$pred_happiness <- predict(model.lm1, newdata=num_test_economic)
postResample(test_economic$pred_happiness, num_test_economic$`Life Ladder`)
# we want minimize MSE, RSS, RSE
# RMSE 0.627
# MAE 0.5055
# R squared 0.68





#social
train_social<-subset(PCA_social, year %in% c(2006:2015)) # 64%
test_social<-subset(PCA_social, year %in% c(2016:2020))
num_train_social <- train_social[, sapply(train_social, is.numeric)] 
num_test_social <- test_social[, sapply(test_social, is.numeric)] 
scale_train_social<-scale(num_train_social)
scale_test_social<-scale(num_test_social)

model.lm3 <- train(`Life Ladder` ~ .,
                   data=num_train_social,
                   method="lm",
                   trControl = trainControl(method="none"),
                   metric="RMSE")
summary(model.lm3)

model.lm4 <- train(`Life Ladder` ~ `Access to electricity (% of population)`+
                    `Arable land (% of land area)`+
                    `Individuals using the Internet (% of population)`,
                   data=num_train_social,
                   method="lm",
                   trControl = trainControl(method="none"),
                   metric="RMSE")
summary(model.lm4)

test_social$pred_happiness <- predict(model.lm3, newdata=num_test_social)
postResample(test_social$pred_happiness, num_test_social$`Life Ladder`)
# we want minimize MSE, RSS, RSE
# RMSE 0.78
# MAE 0.6267
# R squared 0.59





#subjective
train_subjective<-subset(num_data_subjective, year %in% c(2006:2015)) # 64%
test_subjective<-subset(num_data_subjective, year %in% c(2016:2020))
num_train_subjective <- train_subjective[, sapply(train_subjective, is.numeric)] 
num_test_subjective <- test_subjective[, sapply(test_subjective, is.numeric)] 
scale_train_subjective<-scale(num_train_subjective)
scale_test_subjective<-scale(num_test_subjective)

model.lm5 <- train(`Life Ladder` ~ .,
                   data=num_train_subjective,
                   method="lm",
                   trControl = trainControl(method="none"),
                   metric="RMSE")
summary(model.lm5)

model.lm6 <- train(`Life Ladder` ~ year+
                   `Social support`+
                     `Healthy life expectancy at birth`+
                     Generosity+
                     `Perceptions of corruption`+
                     `Positive affect`+
                     `Confidence in national government`+
                     `Positive affect`*`Social support`,
                   data=num_train_subjective,
                   method="lm",
                   trControl = trainControl(method="none"),
                   metric="RMSE")
summary(model.lm6)

test_subjective$pred_happiness <- predict(model.lm5, newdata=num_test_subjective)
postResample(test_subjective$pred_happiness, num_test_subjective$`Life Ladder`)
# we want minimize MSE, RSS, RSE
# RMSE 0.56
# MAE 0.42
# R squared 0.74



ggplot(df3, aes(as.factor(year), `Life Ladder`, fill = year)) + 
  labs(x="Year", Y = "Happiness", title = "Happiness over the Years")+
  geom_point(alpha = 0.20) +
  geom_boxplot(alpha = 0.25) 




#kmeans cluster
kmeans_subjective<-subjective
kmeans_subjective$country_name<-make.names(kmeans_subjective$country_name,unique = TRUE)
numK_subjective <- kmeans_subjective[, sapply(kmeans_subjective, is.numeric)] 
rownames(numK_subjective)<-kmeans_subjective$country_name
scaled_kmeans_subjective <- scale(numK_subjective)


# 3 methods to see how many clusters
set.seed(123)
fviz_nbclust(scaled_kmeans_subjective, kmeans, method = "wss") # 4-5 is a good cluster
fviz_nbclust(scaled_kmeans_subjective, kmeans, method = "silhouette") # 2
set.seed(123)
gap_stat <- clusGap(scaled_kmeans_subjective, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat) # this says 1...


# kmeans test
set.seed(123)
subjectiveK <- kmeans(scaled_kmeans_subjective, centers = 2, nstart = 25)
p_subjective1<-fviz_cluster(subjectiveK,data=scaled_kmeans_subjective, main="Cluster Plot for Subjective Variables") 
subjectiveK2 <- kmeans(scaled_kmeans_subjective, centers = 3, nstart = 25)
p_subjective2<-fviz_cluster(subjectiveK2,data=scaled_kmeans_subjective, main="Cluster Plot for Subjective Variables") 
subjectiveK3 <- kmeans(scaled_kmeans_subjective, centers = 4, nstart = 25)
p_subjective3<-fviz_cluster(subjectiveK3,data=scaled_kmeans_subjective, main="Cluster Plot for Subjective Variables") 
subjectiveK4 <- kmeans(scaled_kmeans_subjective, centers = 5, nstart = 25)
p_subjective4<-fviz_cluster(subjectiveK4,data=scaled_kmeans_subjective, main="Cluster Plot for Subjective Variables") 
library(gridExtra)
grid.arrange(p_subjective1, p_subjective2, p_subjective3, p_subjective4, nrow = 2)

kmeans_subjective$cluster <- subjectiveK3$cluster 
kmeans_subjective <- kmeans_subjective %>% relocate(cluster, .before = year)
#final dataset with clusters is called kmeans_subjective


profile_plot(kmeans_subjective[,c(4:12)], type="bar")





# social  
kmeans_social<-social
kmeans_social$country_name<-make.names(kmeans_social$country_name,unique = TRUE)
numK_social<- kmeans_social[, sapply(kmeans_social, is.numeric)] 
rownames(numK_social)<-kmeans_social$country_name
scaled_kmeans_social <- scale(numK_social)

set.seed(123)
fviz_nbclust(scaled_kmeans_social, kmeans, method = "wss") # 2 is a good cluster
set.seed(123)
fviz_nbclust(scaled_kmeans_social, kmeans, method = "silhouette") # 2 is a good cluster
# kmeans test
set.seed(123)
social_economicK <- kmeans(scaled_kmeans_social, centers = 5, nstart = 25)
fviz_cluster(social_economicK,data=scaled_kmeans_social, main="Cluster Plot for Social Economic Variables") 
# contains every year, very clustered
kmeans_social_economic$cluster <- social_economicK$cluster 
kmeans_social_economic <- kmeans_social_economic %>% relocate(cluster, .before = year)
#final dataset with clusters is called kmeans_social_economic












###### fixed effects


subjective<- df3[,c(1:4,6:13)]
social<- df3[,c(1:4,14:19,33,35,36,38)]
economic<- df3[,c(1:4,5,20:32,34,37)]



# num_data_subjective <- subjective[, sapply(subjective, is.numeric)] 
# num_data_social <- social[, sapply(social, is.numeric)] 
# num_data_economic <- economic[, sapply(economic, is.numeric)] 
scale_subjective <- data.frame(subjective[, 1:3], scale(subjective[, 4:12]))
scale_social <- data.frame(social[, 1:3], scale(social[, 4:14]))
scale_economic <- data.frame(economic[, 1:3], scale(economic[, 4:20]))
names(scale_subjective) <- gsub("\\.", " ", names(scale_subjective))
names(scale_social) <- gsub("\\.", " ", names(scale_social))
names(scale_economic) <- gsub("\\.", " ", names(scale_economic))
corr_matrix1 <- cor(scale_subjective[,c(3:12)])
ggcorrplot(corr_matrix1)

corr_matrix2 <- cor(scale_social[,c(3:14)])
ggcorrplot(corr_matrix2)

corr_matrix3 <- cor(scale_economic[,c(3:20)])
ggcorrplot(corr_matrix3)





PCA_economic2<-scale_economic[,c(1:5,9,12,13,15,16,17,19)]
num_PCA_economic2 <- PCA_economic[, sapply(PCA_economic, is.numeric)] 
ggpairs(num_PCA_economic)

PCA_social2<-scale_social[,c(1:4,5,9,12,13,14)]
num_PCA_social <- PCA_social[, sapply(PCA_social, is.numeric)] 
ggpairs(num_PCA_social)

# z scores
final_subjective<- scale(num_data_subjective, center = TRUE, scale = TRUE)
final_economic<- scale(PCA_economic2, center = TRUE, scale = TRUE)
final_social<- scale(num_PCA_social, center = TRUE, scale = TRUE)

colnames(scale_subjective)[colnames(scale_subjective) == "Life.Ladder"] <- "Happiness"
colnames(scale_subjective)[colnames(scale_subjective) == "name"] <- "Region"
colnames(scale_subjective)[colnames(scale_subjective) == "Social.support"] <- "Social support"
colnames(scale_subjective)[colnames(scale_subjective) == "Healthy.life.expectancy.at.birth"] <- "Healthy life expectancy at birth"
colnames(scale_subjective)[colnames(scale_subjective) == "Freedom.to.make.life.choices"] <- "Freedom to make life choices"
colnames(scale_subjective)[colnames(scale_subjective) == "Perceptions.of.corruption"] <- "Perceptions of corruption"
colnames(scale_subjective)[colnames(scale_subjective) == "Positive.affect"] <- "Positive affect"
colnames(scale_subjective)[colnames(scale_subjective) == "Negative.affect"] <- "Negative affect"
colnames(scale_subjective)[colnames(scale_subjective) == "Confidence.in.national.government"] <- "Confidence in national government"




colnames(PCA_economic2)[colnames(PCA_economic2) == "Life.Ladder"] <- "Happiness"
colnames(PCA_economic2)[colnames(PCA_economic2) == "Log.GDP.per.capita"] <- "Log GDP per capita"
colnames(PCA_economic2)[colnames(PCA_economic2) == "Agricultural.land..sq..km."] <- "Agricultural land (sq km)"
colnames(PCA_economic2)[colnames(PCA_economic2) == "GDP.per.capita..constant.2015.US.."] <- "GDP per capita (constant 2015US)"                                    
colnames(PCA_economic2)[colnames(PCA_economic2) == "GDP.growth..annual..."] <- "GDP growth (annual %)"
colnames(PCA_economic2)[colnames(PCA_economic2) == "Gross.savings....of.GNI."] <- "Gross savings (% of GNI)"
colnames(PCA_economic2)[colnames(PCA_economic2) == "Inflation..consumer.prices..annual..."] <- "Inflation consumer prices (annual %)"
colnames(PCA_economic2)[colnames(PCA_economic2) == "Labor.force..total"] <- "Labor force, total"
colnames(PCA_economic2)[colnames(PCA_economic2) == "Mortality.rate..infant..per.1.000.live.births."] <- "Mortality rate, infant (per 1,000 live births)"
colnames(PCA_economic2)[colnames(PCA_economic2) == "name"] <- "Region"

colnames(PCA_social2)[colnames(PCA_social2) == "Life.Ladder"] <- "Happiness"
colnames(PCA_social2)[colnames(PCA_social2) == "name"] <- "Region"
colnames(PCA_social2)[colnames(PCA_social2) == "Access.to.clean.fuels.and.technologies.for.cooking....of.population."] <- "Access to clean fuels and technologies for cooking (% of population)"
colnames(PCA_social2)[colnames(PCA_social2) == "Access.to.electricity....of.population."] <- "Access to electricity (% of population)"
colnames(PCA_social2)[colnames(PCA_social2) == "Forest.area....of.land.area."] <- "Forest area (% of land area)"
colnames(PCA_social2)[colnames(PCA_social2) == "Arable.land....of.land.area."] <- "Arable land (% of land area)"
colnames(PCA_social2)[colnames(PCA_social2) == "Individuals.using.the.Internet....of.population."] <- "Individuals using the Internet (% of population)"



# log gdp, changes gdp to changes in percentages
# fixed effects
fixed_lm1<-felm(formula= Happiness~
                  `Log GDP per capita`+
                  `Agricultural land (sq km)`+
                  `GDP growth (annual %)`+
                  `Gross savings (% of GNI)`+
                  `Inflation consumer prices (annual %)`+
                  `Mortality rate, infant (per 1,000 live births)`+
                  `Labor force, total` | country_name + year,
                data=PCA_economic2)
summary(fixed_lm1)


##########
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
plot(fixed_lm1, which = 5)
#########
tab_model(fixed_lm1, show.aic = TRUE, dv.labels = "Economic on Happiness")

fixed_lm2<-felm(formula=`Happiness`~`Access to clean fuels and technologies for cooking (% of population)`+
                  `Access to electricity (% of population)` +
                  `Forest area (% of land area)`+
                  `Arable land (% of land area)`+
                  `Individuals using the Internet (% of population)`| country_name + year,
                data=PCA_social2)
summary(fixed_lm2)
tab_model(fixed_lm2, show.aic = TRUE, dv.labels = "Social on Happiness")
##########
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
plot(fixed_lm2, which = 5)
#########

fixed_lm3<-felm(formula=`Happiness`~`Social support`+
                  `Healthy life expectancy at birth` +
                  `Freedom to make life choices`+
                  `Generosity`+
                  `Perceptions of corruption`+
                  `Positive affect` +
                  `Negative affect`+
                  `Confidence in national government` | country_name + year,
                data=scale_subjective)
summary(fixed_lm3)
tab_model(fixed_lm3, show.aic = TRUE, dv.labels = "Subjective on Happiness")
##########
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
plot(fixed_lm3, which = 5)
#########

# Load necessary packages
library(lmtest)
library(fixest)
library(ggplot2)

# Run fixed effects regression using scaled variables
model <- felm(Y ~ scale(X1) + scale(X2) + factor(country) + factor(time) | country + time, data = mydata)

# Check assumptions of fixed effects regression
# 1. Homoscedasticity
# Plot standardized residuals against fitted values
ggplot(fixed_lm3, aes(.fitted, resid / sqrt(.sigma))) +
  geom_point() +
  labs(x = "Fitted values", y = "Standardized residuals") +
  ggtitle("Homoscedasticity: Residuals vs Fitted")

# 2. Normality of residuals
# Plot quantiles of standardized residuals against theoretical quantiles
qqnorm(fixed_lm3$.resid / sqrt(fixed_lm3$.sigma), main = "Normality: Normal Q-Q Plot")
qqline(fixed_lm3$.resid / sqrt(fixed_lm3$.sigma))

# 3. Linearity of relationship between predictor and response variables
# Plot standardized residuals against each predictor variable
ggplot(model, aes(scale(X1), resid / sqrt(.sigma))) +
  geom_point() +
  labs(x = "X1", y = "Standardized residuals") +
  ggtitle("Linearity: Residuals vs X1")

ggplot(model, aes(scale(X2), resid / sqrt(.sigma))) +
  geom_point() +
  labs(x = "X2", y = "Standardized residuals") +
  ggtitle("Linearity: Residuals vs X2")

# 4. Independence of observations
# Plot residuals against observation order
plot(model$.resid / sqrt(model$.sigma), type = "l", main = "Independence: Residuals vs Order", xlab = "Observation order")






stepAIC(fixed_lm3) # can't do to fixed effect, multicolinearity

library(lmtest)
resettest(fixed_lm3)
#p<0.05 is an indication that non-linear variables were 
#add squared and cubic terms for all of the predictors
resettest(fixed_lm3, type = "regressor")



fixed_lm1_1<-felm(formula= Happiness~
                  `Gross savings (% of GNI)`+
                    `Log GDP per capita`
                  | country_name + year,
                data=PCA_economic2)
summary(fixed_lm1_1)


fixed_lm1<-felm(formula= Happiness~
                  `Log GDP per capita`+
                  `Agricultural land (sq km)`+
                  `GDP growth (annual %)`+
                  `Gross savings (% of GNI)`+
                  `Inflation consumer prices (annual %)`+
                  `Mortality rate, infant (per 1,000 live births)`+
                  `Labor force, total` | country_name + year,
                data=PCA_economic2)
summary(fixed_lm1)


fixed_lm2_2<-felm(formula=`Happiness`~
                    `Individuals using the Internet (% of population)`+
                    `Arable land (% of land area)`
                  | country_name + year,
                data=PCA_social2)
summary(fixed_lm2_2)

fixed_lm2<-felm(formula=`Happiness`~`Access to clean fuels and technologies for cooking (% of population)`+
                  `Access to electricity (% of population)` +
                  `Forest area (% of land area)`+
                  `Arable land (% of land area)`+
                  `Individuals using the Internet (% of population)`| country_name + year,
                data=PCA_social2)
summary(fixed_lm2)


fixed_lm3<-felm(formula=`Happiness`~
                  `Confidence in national government`+
                  `Negative affect`
                | country_name + year,
                data=scale_subjective)
summary(fixed_lm3)


fixed_lm3<-felm(formula=`Happiness`~`Social support`+
                  `Healthy life expectancy at birth` +
                  `Freedom to make life choices`+
                  `Generosity`+
                  `Perceptions of corruption`+
                  `Positive affect` +
                  `Negative affect`+
                  `Confidence in national government` | country_name + year,
                data=scale_subjective)
summary(fixed_lm3)














df5 <- data.frame(df3[, 1:3], scale(df3[, 4:38]))
corr_matrix4 <- cor(df5[,4:38])
ggcorrplot(corr_matrix4)

data.pca4 <- princomp(corr_matrix4)
summary(data.pca4)
data.pca4$loadings[, 1:5]
fviz_eig(data.pca4, addlabels = TRUE)
fviz_pca_var(data.pca4, col.var = "black")
fviz_cos2(data.pca4, choice = "var", axes = 1:3)
fviz_pca_var(data.pca4, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)



fixed_lm4<-felm(formula=`Life.ladder`~`Social support`+
                  `Healthy life expectancy at birth` +
                  `Freedom to make life choices`+
                  `Generosity`+
                  `Perceptions of corruption`+
                  `Positive affect` +
                  `Negative affect`+
                  `Confidence in national government` +
                  `Log GDP per capita`+
                  `Agricultural land (sq km)`+
                  `GDP growth (annual %)`+
                  `Gross savings (% of GNI)`+
                  `Inflation consumer prices (annual %)`+
                  `Mortality rate, infant (per 1,000 live births)`+
                  `Labor force, total`+
                  `Access to clean fuels and technologies for cooking (% of population)`+
                  `Access to electricity (% of population)` +
                  `Forest area (% of land area)`+
                  `Arable land (% of land area)`+
                  `Individuals using the Internet (% of population)`| country_name + year,
                data=df5)

fixed_lm4<-felm(formula=`Life.Ladder`~ df5$Log.GDP.per.capita+
                  df5$Social.support+
                  df5$Healthy.life.expectancy.at.birth+
                  df5$Freedom.to.make.life.choices+
                  df5$Generosity+
                  df5$Perceptions.of.corruption+
                  df5$Positive.affect+
                  df5$Negative.affect+
                  df5$Confidence.in.national.government+
                  df5$Access.to.clean.fuels.and.technologies.for.cooking....of.population.+
                  df5$Access.to.electricity....of.population.+
                  df5$Agricultural.land..sq..km.+
                  df5$Log.GDP.per.capita+
                  df5$GDP.growth..annual...+
                  df5$Gross.savings....of.GNI.+
                  df5$Inflation..consumer.prices..annual...+
                  df5$Labor.force..total+
                  df5$Mortality.rate..infant..per.1.000.live.births.+
                  df5$Forest.area....of.land.area.+
                  df5$Arable.land....of.land.area.+
                  df5$Individuals.using.the.Internet....of.population.| country_name + year,
                data=df5)
summary(fixed_lm4)
tab_model(fixed_lm4, show.aic = TRUE, dv.labels = "All on Happiness")




kmeanspresent<- scale_subjective %>% filter(year==2020) 
kmeanspresent<- kmeanspresent %>% select(-year)

kmeans_subjective$country_name<-make.names(kmeans_subjective$country_name,unique = TRUE)
num_kmeanspresent <- kmeanspresent[, sapply(kmeanspresent, is.numeric)] 
rownames(num_kmeanspresent)<-kmeanspresent$country_name
scaled_kmeans_subjective <- scale(num_data_subjective)


# 3 methods to see how many clusters
set.seed(123)
fviz_nbclust(num_kmeanspresent, kmeans, method = "wss") # 4-5 is a good cluster
fviz_nbclust(num_kmeanspresent, kmeans, method = "silhouette") # 2
set.seed(123)
gap_stat <- clusGap(scaled_kmeans_subjective, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat) # this says 10...

# kmeans test
set.seed(123)
subjectiveK <- kmeans(num_kmeanspresent, centers = 2, nstart = 25)
p_subjective1<-fviz_cluster(subjectiveK,data=num_kmeanspresent, main="Cluster Plot for Subjective Variables") 
subjectiveK2 <- kmeans(num_kmeanspresent, centers = 3, nstart = 25)
p_subjective2<-fviz_cluster(subjectiveK2,data=num_kmeanspresent, main="Cluster Plot for Subjective Variables") 
subjectiveK3 <- kmeans(num_kmeanspresent, centers = 4, nstart = 25)
p_subjective3<-fviz_cluster(subjectiveK3,data=num_kmeanspresent, main="Cluster Plot for Subjective Variables") 
subjectiveK4 <- kmeans(num_kmeanspresent, centers = 5, nstart = 25)
p_subjective4<-fviz_cluster(subjectiveK4,data=num_kmeanspresent, main="Cluster Plot for Subjective Variables") 
library(gridExtra)
grid.arrange(p_subjective1, p_subjective2, p_subjective3, p_subjective4, nrow = 2)

kmeanspresent$cluster <- subjectiveK2$cluster 
kmeanspresent <- kmeanspresent %>% relocate(cluster, .before = year)
#final dataset with clusters is called kmeans_subjective
profile_plot(kmeanspresent)



# Load required libraries
library(maps)
library(ggplot2)
library(rnaturalearth)
# Sample data

# Load world map data
world_map <- map_data("world")
world_map <- world_map %>% rename(country_name = region)
# Merge data with world map data
map_data <- merge(world_map, kmeanspresent, by = "country_name", all.x = TRUE)
map_data<-na.omit(map_data)

map_data <- merge(kmeanspresent, world_map, by.x = "country_name", by.y = "region", all.x = TRUE)
map_data <- map_data[!duplicated(map_data$country_name), ]

ggplot(map_data, aes(x = long, y = lat, group = group, fill = as.factor(cluster))) +
  geom_polygon() +
  scale_fill_manual(values = c("gray",  "red",  "blue",  "green")) +
  theme_void() +
  labs(title = "World Map by Cluster")


# Load country data
countries <- ne_countries(scale = "medium", returnclass = "sf")

missing_countries <- setdiff(kmeanspresent$country_name, countries$geounit)

# Merge data with country data
country_data <- merge(countries, kmeanspresent, by.x = "admin", by.y = "country_name", all.x = TRUE)

# Set the color palette for clusters
cluster_palette <- c("#FF0000", "#00FF00", "#0000FF")

# Create the world map
ggplot() +
  geom_sf(data = country_data, aes(fill = factor(cluster))) +
  scale_fill_manual(values = c(cluster_palette,"grey"), na.value = "gray") +
  theme_minimal()
