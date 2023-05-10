####packages needed####
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

####begining stages of merging datasets####
# read the datasets from WHR and worldbank
WHR2022Table2 <- read_excel("WHR2022Table2.xls")
worldbank <- read_csv("worldbank.csv")

# rename the years variables (column names) in world bank data set
wb1 <-rename(worldbank, "2000" = "2000 [YR2000]",
           "2001" = "2001 [YR2001]",
           "2002" = "2002 [YR2002]",
           "2003" = "2003 [YR2003]",
           "2004" = "2004 [YR2004]",
           "2005" = "2005 [YR2005]",
           "2006" = "2006 [YR2006]",
           "2007" = "2007 [YR2007]",
           "2008" = "2008 [YR2008]",
           "2009" = "2009 [YR2009]",
           "2010" = "2010 [YR2010]",
           "2011" = "2011 [YR2011]",
           "2012" = "2012 [YR2012]",
           "2013" = "2013 [YR2013]",
           "2014" = "2014 [YR2014]",
           "2015" = "2015 [YR2015]",
           "2016" = "2016 [YR2016]",
           "2017" = "2017 [YR2017]",
           "2018" = "2018 [YR2018]",
           "2019" = "2019 [YR2019]",
           "2020" = "2020 [YR2020]",
           "2021" = "2021 [YR2021]")

#change worldbank from wide to long
wb_long <- gather(wb1, "year", value, "2000":"2021", factor_key=TRUE)

#change WHR from wide to long
whr_long <- gather(WHR2022Table2, "c(Country name, year)", value, "Life Ladder":"Confidence in national government", factor_key=TRUE)

#rename some variables to match
finalWHR<-rename(whr_long, "variables" = "c(Country name, year)", "country_name" = "Country name")
finalwb<-rename(wb_long, "variables" = "Series Name", "country_name" = "Country Name")

# at this point, we have finalWHR, containing variables from WHR
# finalwb, containing variables from world bank
#let's check our ID variable country name, since the two datasets may have different names for certain countries
WHRcountries<-unique(finalWHR$country_name)
WBcountries<-unique(finalwb$country_name)

# See which countries are different, the code below prints out countries that the WB has but WHR does not
wb_not_in_whr<-WBcountries[!(WBcountries %in% WHRcountries)]
# countries that the WHR has but WB does not
whr_not_in_wb<-WHRcountries[!(WHRcountries %in% WBcountries)]

# edit the country names, rename
finalwb$country_name[finalwb$country_name == "Congo, Rep."] <- 'Republic of the Congo'
finalWHR$country_name[finalWHR$country_name == "Congo (Brazzaville)"] <- 'Republic of the Congo'
finalwb$country_name[finalwb$country_name == "Congo, Dem. Rep."] <- 'Dem Republic of the Congo'
finalWHR$country_name[finalWHR$country_name == "Congo (Kinshasa)"] <- 'Dem Republic of the Congo'
finalwb$country_name[finalwb$country_name == "Egypt, Arab Rep."] <- 'Egypt'
finalwb$country_name[finalwb$country_name == "Gambia, The"] <- 'Gambia'
finalwb$country_name[finalwb$country_name == "Hong Kong SAR, China"] <- 'Hong Kong SAR of China'
finalWHR$country_name[finalWHR$country_name == "Hong Kong S.A.R. of China"] <- 'Hong Kong SAR of China'
finalwb$country_name[finalwb$country_name == "Iran, Islamic Rep."] <- 'Iran'
finalwb$country_name[finalwb$country_name == "Cote d'Ivoire"] <- 'Ivory Coast'
finalwb$country_name[finalwb$country_name == "Kyrgyz Republic"] <- 'Kyrgyzstan'
finalwb$country_name[finalwb$country_name == "Lao PDR"] <- 'Laos'
finalwb$country_name[finalwb$country_name == "Russian Federation"] <- 'Russia'
finalwb$country_name[finalwb$country_name == "Slovak Republic"] <- 'Slovakia'
finalwb$country_name[finalwb$country_name == "Korea, Rep."] <- 'South Korea'
finalwb$country_name[finalwb$country_name == "Syrian Arab Republic"] <- 'Syria'
finalwb$country_name[finalwb$country_name == "Turkiye"] <- 'Turkey'
finalwb$country_name[finalwb$country_name == "Venezuela, RB"] <- 'Venezuela'
finalwb$country_name[finalwb$country_name == "Yemen, Rep."] <- 'Yemen'

# check which names are left
WHRcountries<-unique(finalWHR$country_name)
WBcountries<-unique(finalwb$country_name)

# See which countries are different, the code below prints out countries that the WB has but WHR does not
wb_not_in_whr<-WBcountries[!(WBcountries %in% WHRcountries)]
# countries that the WHR has but WB does not
whr_not_in_wb<-WHRcountries[!(WHRcountries %in% WBcountries)]

# drop the rows with countries that are not matching
finalwb2 <- finalwb[ ! finalwb$country_name %in% wb_not_in_whr, ]
finalWHR2 <- finalWHR[ ! finalWHR$country_name %in% whr_not_in_wb, ]


# Time to merge the two datasets
finalwb1<-finalwb2[-c(2,4)]
total <- rbind(finalWHR2,finalwb1)
total<- total[order(total$country_name, total$year, total$variables),]

# change final dataset from long to wide
#wide_total<-dcast(total, country_name + year ~ total$variables, value.var="value")
wide_total2<- spread(total, variables, value)

#the final dataset we have is called wide_total2 and we save it as FINAL.csv
#let's add a variable that categorizes the countries into their continents

#read files
FINAL <- read_csv("FINAL.csv")
countries_region <- read_excel("countries_region.xlsx")

#change countries region file to long
long <- countries_region %>% 
  pivot_longer(
    cols = `Eastern Africa`:`OCEANIA`
  )
#rename value to country_name
long <- long %>% rename(country_name = value)

sum(is.na(long$country_name)) # 295 NAs
long<-na.omit(long)


# See which countries are different
finalcn<-unique(FINAL$country_name)
crcn<-(long$country_name)
#the code below prints out countries that the are in countries region has but FINAL does not
cr_not_final<-crcn[!(crcn %in% finalcn)]
# countries that the FINAL has but countries region does not
final_not_cr<-finalcn[!(finalcn %in% crcn)] # 7 countries need to be edited









####clean environment and import new countries region and add region####
FINAL <- read_csv("FINAL.csv")
countries_region <- read_csv("countries_region.csv")

long <- countries_region %>% 
  pivot_longer(
    cols = `Eastern Africa`:`Oceania`
  ) %>% rename(country_name = value)

finaldf <- FINAL %>% left_join(long) %>% relocate(name, .before = year)
sum(is.na(finaldf)) #15660 NAs throughout the dataset

#omitted only the NAs from the WHR
finalnaomit<-na.omit(finaldf) # from 3564 observations to 1741

# Time to do some analysis
table(factor(finalnaomit$year)) #count(finalnaomit,factor(finalnaomit$year))
#ranges from 90-125 observations per year
#but 2005 only 1 observation, 2006 72
#years range from 2005 to 2021
barplot(table(factor(finalnaomit$year)),main="Frequency of Available Data by Years" ,xlab="Years",
        ylab="Frequency", col="cornflowerblue")



#both of these do the same
df<-replace(finalnaomit,finalnaomit=="..",NA)
#finalnaomit[finalnaomit == ".."] <- NA






#### clean data, missing data ####
df<-read_csv("df.csv")
barplot((colMeans(is.na(df)))*100, main = "Percent of Missing Data in Merged Dataset", xlab= "Variables", ylab = "Percentage")
# there are a lot of variables with missing data for more than 5% of the column
df4<-na.omit(df) # this ends up taking every data out

morethan5missing<- function(x){
  nlist<-c()
  for (i in 1:length(colnames(x))){
    if((colMeans(is.na(x[i])))*100>=5){
      nlist<-append(nlist,i)
    }
    else{
      next
    }
  }
  return(nlist)
}
missing<-morethan5missing(df)
# there are about 62 variables that are missing at least 95% the data
df2<-subset(df,select=-missing)
#we are left with 23 variables for variables no more than 5% missing


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
barplot((colMeans(is.na(df3)))*100, main = "Percent of Missing Data in Merged Dataset", xlab= "Variables", ylab = "Percentage")

# Checking the number of country datas per year, we find that 2005 has very few information
# consider dropping 2005
barplot(table(factor(df$year)),main="Frequency of Available Data by Years" ,xlab="Years",
        ylab="Frequency", col="cornflowerblue")
df4<-subset(df, df$year != 2005) # drop 2005, 1 observation dropped

# Let's check df to see which countries have the most missing data
barplot(sort(table(factor(df4$country_name))),main="Frequency of Available Data by Countries" ,xlab="Country",
        ylab="Frequency", col="cornflowerblue")
# There appears to be some countries with low numbers of frequencies
max(table(factor(df4$country_name))) #16
min(table(factor(df4$country_name))) #1
sort(table(factor(df4$country_name)))
# let's take out countries that appear less than or equal to 5
countrycount<-table(df4$country_name)
df5<-names(countrycount[countrycount <= 5]) # 18 countries with <= 5
df6<-subset(df4, !country_name %in% df5)
sort(table(factor(df6$country_name)))
barplot((colMeans(is.na(df6)))*100) 
# there still remains a lot of variables that have missing data


#### using df3 with no more than 10% missing load df from line 178, 201-215, kmeans####
#back to df3 with no more than 10% missing data and let's take out 2005
df3<-subset(df3, df3$year != 2005)
# we need to make this a numerical dataset, change the countries to rows names
#split up the dataset to years
data_by_year <- split(df3, df3$year)

###working with 2006 first###
year_2006<-data_by_year$`2006`
#kmeans for 2006 using the Euclidean distance
kmeansdf<-na.omit(year_2006)
num_data <- kmeansdf[, sapply(kmeansdf, is.numeric)] 
num_data$year<-NULL # since this is only 2006, not important and would become NA would scaled
#country names become the row names
rownames(num_data)<-kmeansdf$country_name
scaled_kmeans <- scale(num_data)
k2 <- kmeans(scaled_kmeans, centers = 3, nstart = 25)
cluster2006<-fviz_cluster(k2,data=scaled_kmeans)
plot(cluster2006)
# to compare to variables we want
scaled_kmeans %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         Country = row.names(scaled_kmeans)) %>%
  ggplot(aes(`Life Ladder`, `Social support`, color = factor(cluster), label = Country)) +
  geom_text()

#check the number of optimal clusters
set.seed(123)
fviz_nbclust(scaled_kmeans, kmeans, method = "wss") # 3 is a good cluster
#method 2
fviz_nbclust(scaled_kmeans, kmeans, method = "silhouette") # another method says 4
#method 3
set.seed(123)
gap_stat <- clusGap(scaled_kmeans, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat) # this says 7...



### 2020 ###
year_2020<-data_by_year$`2020`
#kmeans for 2006 using the Euclidean distance
kmeansdf<-na.omit(year_2020)
num_data <- kmeansdf[, sapply(kmeansdf, is.numeric)] 
num_data$year<-NULL # since this is only 2006, not important and would become NA would scaled
#country names become the row names
rownames(num_data)<-kmeansdf$country_name
scaled_kmeans <- scale(num_data)
k2 <- kmeans(scaled_kmeans, centers = 3, nstart = 25)
cluster2020<-fviz_cluster(k2,data=scaled_kmeans)
plot(cluster2020)


### all years ###

#can df3, with all the years be used in kmeans?
df4<-df3
df4$country_name<-make.names(df4$country_name,unique = TRUE)
kmeansdf4<-na.omit(df4)
num_data4 <- kmeansdf4[, sapply(kmeansdf4, is.numeric)] 
rownames(num_data4)<-kmeansdf4$country_name
scaled_kmeans4 <- scale(num_data4)
# how many clusters?
set.seed(123)
fviz_nbclust(scaled_kmeans4, kmeans, method = "wss") # 5-7 is a good cluster
set.seed(123)
fviz_nbclust(scaled_kmeans4, kmeans, method = "silhouette") # another method says 4

# kmeans test
k4 <- kmeans(scaled_kmeans4, centers = 5, nstart = 25)
fviz_cluster(k4,data=scaled_kmeans4) # contains every year, very clustered
kmeansdf4$cluster <- k4$cluster 
kmeansdf4 <- kmeansdf4 %>% relocate(cluster, .before = year)


#### separating the dataset into subjective, social, economic and do kmeans ####
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


# Separate the variables in the dataset into  the following
# subjective (surveys)
subjective<- df3[,c(1:4,6:13)]
kmeans_subjective<-subjective
kmeans_subjective$country_name<-make.names(kmeans_subjective$country_name,unique = TRUE)
num_data_subjective <- kmeans_subjective[, sapply(kmeans_subjective, is.numeric)] 
rownames(num_data_subjective)<-kmeans_subjective$country_name
scaled_kmeans_subjective <- scale(num_data_subjective)


# 3 methods to see how many clusters
set.seed(123)
fviz_nbclust(scaled_kmeans_subjective, kmeans, method = "wss") # 4-5 is a good cluster
fviz_nbclust(scaled_kmeans_subjective, kmeans, method = "silhouette") # 2
set.seed(123)
gap_stat <- clusGap(scaled_kmeans_subjective, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat) # this says 10...

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

kmeans_subjective$cluster <- subjectiveK$cluster 
kmeans_subjective <- kmeans_subjective %>% relocate(cluster, .before = year)
#final dataset with clusters is called kmeans_subjective






# social economic 
social_economic<- df3[,c(1:3,14:19,33,35:38)]

kmeans_social_economic<-social_economic
kmeans_social_economic$country_name<-make.names(kmeans_social_economic$country_name,unique = TRUE)
num_data_social_economic <- kmeans_social_economic[, sapply(kmeans_social_economic, is.numeric)] 
rownames(num_data_social_economic)<-kmeans_social_economic$country_name
scaled_kmeans_social_economic <- scale(num_data_social_economic)

set.seed(123)
fviz_nbclust(scaled_kmeans_social_economic, kmeans, method = "wss") # 2 is a good cluster
set.seed(123)
fviz_nbclust(scaled_kmeans_social_economic, kmeans, method = "silhouette") # 2 is a good cluster
# kmeans test
set.seed(123)
social_economicK <- kmeans(scaled_kmeans_social_economic, centers = 5, nstart = 25)
fviz_cluster(social_economicK,data=scaled_kmeans_social_economic, main="Cluster Plot for Social Economic Variables") 
# contains every year, very clustered
kmeans_social_economic$cluster <- social_economicK$cluster 
kmeans_social_economic <- kmeans_social_economic %>% relocate(cluster, .before = year)
#final dataset with clusters is called kmeans_social_economic




# GDP
economic<- df3[,c(1:3,5,20:32,34)]

kmeans_economic<-economic
kmeans_economic$country_name<-make.names(kmeans_economic$country_name,unique = TRUE)
num_data_economic <- kmeans_economic[, sapply(kmeans_economic, is.numeric)] 
rownames(num_data_economic)<-kmeans_economic$country_name
scaled_kmeans_economic <- scale(num_data_economic)

set.seed(123)
fviz_nbclust(scaled_kmeans_economic, kmeans, method = "wss") # 4-5 is a good cluster
set.seed(123)
fviz_nbclust(scaled_kmeans_economic, kmeans, method = "silhouette") # 2

# kmeans test
set.seed(123)
economicK <- kmeans(scaled_kmeans_economic, centers = 2, nstart = 25)
p_economic<-fviz_cluster(economicK,data=scaled_kmeans_economic, main="Cluster Plot for Economic Variables") 
economicK2 <- kmeans(scaled_kmeans_economic, centers = 3, nstart = 25)
p_economic2<-fviz_cluster(economicK2,data=scaled_kmeans_economic, main="Cluster Plot for Subjective Variables") 
economicK3 <- kmeans(scaled_kmeans_economic, centers = 4, nstart = 25)
p_economic3<-fviz_cluster(economicK3,data=scaled_kmeans_economic, main="Cluster Plot for Subjective Variables") 
economicK4 <- kmeans(scaled_kmeans_economic, centers = 5, nstart = 25)
p_economic4<-fviz_cluster(economicK4,data=scaled_kmeans_economic, main="Cluster Plot for Subjective Variables") 
library(gridExtra)
grid.arrange(p_economic, p_economic2, p_economic3, p_economic4, nrow = 2)


kmeans_economic$cluster <- economicK$cluster 
kmeans_economic <- kmeans_economic %>% relocate(cluster, .before = year)
#final dataset with clusters is called kmeans_social_economic
# US and India are separated from other clusters


#### test if the countries are the same cluster throughout the year####
# do this by looking at 
kmeans_economic
kmeans_social_economic
kmeans_subjective

# make a for loop? to test for the same country, if the cluster is not the same, put the country name and year in a list
economicsdf<-kmeans_economic
economicsdf$country_name<- na.omit(economic)$country_name

# Find duplicates in country variable
dup_country <- duplicated(economicsdf$country_name)
# Get unique list of countries
unique_countries <- unique(economicsdf$country_name)
# Loop through each unique country and check for different clusters
for (i in unique_countries) {
  clusters <- unique(economicsdf$cluster[economicsdf$country_name == i])
  if (length(clusters) > 1) {
    print(paste0(i, " has different clusters: ", paste(clusters, collapse = ", ")))
  }
}


# social
socialdf<-kmeans_social_economic
socialdf$country_name<- na.omit(social_economic)$country_name

# Find duplicates in country variable
dup_country <- duplicated(socialdf$country_name)
# Get unique list of countries
unique_countries <- unique(socialdf$country_name)
# Loop through each unique country and check for different clusters
for (i in unique_countries) {
  clusters <- unique(socialdf$cluster[socialdf$country_name == i])
  if (length(clusters) > 1) {
    print(paste0(i, " has different clusters: ", paste(clusters, collapse = ", ")))
  }
}


# subjective
subjectivedf<-kmeans_subjective
subjectivedf$country_name<- na.omit(subjective)$country_name

# Find duplicates in country variable
dup_country <- duplicated(subjectivedf$country_name)
# Get unique list of countries
unique_countries <- unique(subjectivedf$country_name)
# Loop through each unique country and check for different clusters
for (i in unique_countries) {
  clusters <- unique(subjectivedf$cluster[subjectivedf$country_name == i])
  if (length(clusters) > 1) {
    print(paste0(i, " has different clusters: ", paste(clusters, collapse = ", ")))
  }
}




# life ladder from economic and social economic
#mortality in economic
# matrix of distance, find the average of the distance per country?


# life ladder as demographic
#PCA, per cluster type

table(kmeansdf4$cluster, kmeansdf4$name)
summary(k4$centers)



str(kmeans_output)
print(kmeans_output$centers)
print(table(kmeans_output$cluster))
kmeansdf$cluster <- kmeans_output$cluster 
kmeansdf <- kmeansdf %>% relocate(cluster, .before = year)
#add cluster labels to dataset

fviz_cluster(kmeans_output,data=kmeansdf)



#### work on this avg distance, euclidean by default ####
# I want the average of each country by year's distance within their own country
# so for each US, 

#subjective
library(usedist)
distance_formula<- function(x){

  x_2<-x
  unqiuecountry<-unique(x[[1]])
  x_list<-split(x_2, x_2[[1]])
  final<-data.frame(country_name=character(), avg_distance=numeric())
  
  for(i in unqiuecountry){
    x1<-x_list[[i]]
    x2<-x1[, sapply(x1, is.numeric)] 
    scaled_x2 <- scale(x2)
    x3 <- get_dist(scaled_x2)
    avg_dist<-mean(x3)
    final<-rbind(final,c(i, avg_dist))
  }
  return(final)
}
final_economic<-distance_formula(economic)
final_subjective<-distance_formula(subjective)
final_social<-distance_formula(social_economic)







# testing
burun<-subjective_list[["Albania"]]
num_data_subjective2 <- burun[, sapply(burun, is.numeric)] 
scaled_kmeans_subjective2 <- scale(num_data_subjective2)
distance_subjective <- get_dist(scaled_kmeans_subjective2)
print(distance_subjective)
avg_distance<-mean(distance_subjective)

fviz_dist(distance_subjective)

str(distance)
fviz_dist(distance_subjective, gradient = list(low="blue", mid="white", high="red"))







#### PCA ####

num_data_subjective <- subjective[, sapply(subjective, is.numeric)] 
num_data_social_economic <- social_economic[, sapply(social_economic, is.numeric)] 
num_data_economic <- economic[, sapply(economic, is.numeric)] 
scale_num_subjective <- scale(num_data_subjective)
scale_num_social_economic <- scale(num_data_social_economic)
scale_num_economic <- scale(num_data_economic)

corr_matrix1 <- cor(scale_num_subjective)
ggcorrplot(corr_matrix1)

corr_matrix2 <- cor(scale_num_social_economic)
ggcorrplot(corr_matrix2)
# there are a few that are very positively correlated

corr_matrix3 <- cor(scale_num_economic)
ggcorrplot(corr_matrix3)


data.pca1 <- princomp(corr_matrix1)
summary(data.pca1)

data.pca2 <- princomp(corr_matrix2)
summary(data.pca2)

data.pca3 <- princomp(corr_matrix3)
summary(data.pca3)


data.pca1$loadings[, 1:5]
data.pca2$loadings[, 1:5]
data.pca3$loadings[, 1:5]

fviz_eig(data.pca1, addlabels = TRUE)
fviz_eig(data.pca2, addlabels = TRUE)
fviz_eig(data.pca3, addlabels = TRUE)

# Graph of the variables
fviz_pca_var(data.pca1, col.var = "black")
# Graph of the variables
fviz_pca_var(data.pca2, col.var = "black")
# Graph of the variables
fviz_pca_var(data.pca3, col.var = "black")


fviz_cos2(data.pca1, choice = "var", axes = 1:3)
fviz_cos2(data.pca2, choice = "var", axes = 1:4)
fviz_cos2(data.pca3, choice = "var", axes = 1:3)

fviz_pca_var(data.pca1, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

fviz_pca_var(data.pca2, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

fviz_pca_var(data.pca3, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)


#### regression ####
lm2<-lm(subjective$`Life Ladder`~subjective$Generosity)
summary(lm2)
lm1<-lm(economic$`Life Ladder`~economic$`GDP (current US$)`)
summary(lm1)
lm1<-lm(social_economic$`Life Ladder`~economic$`Agricultural land (% of land area)`)
summary(lm1)
lm1<-lm(social_economic$`Life Ladder`~economic$`Agricultural land (% of land area)`+economic$`Agricultural land (sq. km)`)
summary(lm1)


SE<-social_economic[,c(4,5,12:13,15)]
ggpairs(SE_ggpair)
E<-economic[,c(4,11,14,13,15, 16, 8,9,19,17)]
ggpairs(SE_ggpair)

lm1<-lm(social_economic$`Life Ladder`~social_economic$`Access to clean fuels and technologies for cooking (% of population)`)
summary(lm1)
lm1<-lm(social_economic$`Life Ladder`~social_economic$`Individuals using the Internet (% of population)`)
summary(lm1)
lm1<-lm(social_economic$`Life Ladder`~social_economic$`Arable land (% of land area)`)
summary(lm1)
lm1<-lm(social_economic$`Life Ladder`~social_economic$`Forest area (% of land area)`)
summary(lm1)
lm1<-lm(social_economic$`Life Ladder`~social_economic$`Forest area (% of land area)`+social_economic$`Arable land (% of land area)`+social_economic$`Individuals using the Internet (% of population)`+social_economic$`Access to clean fuels and technologies for cooking (% of population)`)
summary(lm1)

#subjective
lm1<-lm(num_data_subjective$`Life Ladder`~., data=num_data_subjective)
summary(lm1)
lm1<-lm(num_data_subjective$`Life Ladder`~num_data_subjective$`Social support`, data=num_data_subjective)
summary(lm1)
lm1<-lm(num_data_subjective$`Life Ladder`~num_data_subjective$`Healthy life expectancy at birth`, data=num_data_subjective)
summary(lm1)
lm1<-lm(num_data_subjective$`Life Ladder`~num_data_subjective$`Freedom to make life choices`, data=num_data_subjective)
summary(lm1)
lm1<-lm(num_data_subjective$`Life Ladder`~num_data_subjective$Generosity, data=num_data_subjective)
summary(lm1)
lm1<-lm(num_data_subjective$`Life Ladder`~num_data_subjective$`Perceptions of corruption`, data=num_data_subjective)
summary(lm1)
lm1<-lm(num_data_subjective$`Life Ladder`~num_data_subjective$`Positive affect`, data=num_data_subjective)
summary(lm1)
lm1<-lm(num_data_subjective$`Life Ladder`~num_data_subjective$`Negative affect`, data=num_data_subjective)
summary(lm1)
lm1<-lm(num_data_subjective$`Life Ladder`~num_data_subjective$`Confidence in national government`, data=num_data_subjective)
summary(lm1)

lm1<-lm(num_data_subjective$`Life Ladder`~num_data_subjective$`Positive affect`+num_data_subjective$`Negative affect`, data=num_data_subjective)
summary(lm1)
lm1<-lm(num_data_subjective$`Life Ladder`~num_data_subjective$`Positive affect`+num_data_subjective$`Negative affect`+ num_data_subjective$`Social support`, data=num_data_subjective)
summary(lm1)
lm1<-lm(num_data_subjective$`Life Ladder`~num_data_subjective$`Negative affect`+ num_data_subjective$`Social support`, data=num_data_subjective)
summary(lm1)
lm1<-lm(num_data_subjective$`Life Ladder`~num_data_subjective$`Negative affect`+ num_data_subjective$`Confidence in national government`, data=num_data_subjective)
summary(lm1)

subjective_NA<-num_data_subjective[,-9]
lm1<-lm(subjective_NA$`Life Ladder`~., data=subjective_NA)
summary(lm1)

# SE
lm1<-lm(SE$`Life Ladder`~ SE$`Individuals using the Internet (% of population)`+SE$`Forest area (% of land area)`, data=SE)
summary(lm1)

#E
lm1<-lm(E$`Life Ladder`~ E$`Mortality rate, infant (per 1,000 live births)` +E$`GDP (current US$)`, data=E)
summary(lm1)
lm1<-lm(E$`Life Ladder`~ E$`Agricultural land (% of land area)` +E$`Mortality rate, infant (per 1,000 live births)`, data=E)
summary(lm1)

#### regressions pt2 ####
subjective<- df3[,c(1:4,6:13)]
social_economic<- df3[,c(1:4,14:19,33,35:38)]
economic<- df3[,c(1:4,5,20:32,34)]


#make sure the dataset is numeric
num_data_subjective <- subjective[, sapply(subjective, is.numeric)] 
num_data_social_economic <- social_economic[, sapply(social_economic, is.numeric)] 
num_data_economic <- economic[, sapply(economic, is.numeric)] 
ggpairs(num_data_subjective)
ggpairs(num_data_social_economic)
ggpairs(num_data_economic)

reg<-lm(num_data_subjective$`Life Ladder`~., data = num_data_subjective)
summary(reg)

reg2<-lm(num_data_social_economic$`Life Ladder`~., data = num_data_social_economic)
summary(reg2)

reg3<-lm(num_data_economic$`Life Ladder`~., data = num_data_economic)
summary(reg3)


social_ec<-lm(num_data_social_economic$`Life Ladder`~num_data_social_economic$`Access to clean fuels and technologies for cooking (% of population)`, data = num_data_social_economic)
summary(social_ec)
social_ec<-lm(num_data_social_economic$`Life Ladder`~num_data_social_economic$`Access to clean fuels and technologies for cooking, urban (% of urban population)`, data = num_data_social_economic)
summary(social_ec)
social_ec<-lm(num_data_social_economic$`Life Ladder`~num_data_social_economic$`Access to electricity, rural (% of rural population)`, data = num_data_social_economic)
summary(social_ec)
social_ec<-lm(num_data_social_economic$`Life Ladder`~num_data_social_economic$`Access to clean fuels and technologies for cooking (% of population)`, data = num_data_social_economic)
summary(social_ec)
social_ec<-lm(num_data_social_economic$`Life Ladder`~num_data_social_economic$`Access to clean fuels and technologies for cooking (% of population)`, data = num_data_social_economic)
summary(social_ec)
social_ec<-lm(num_data_social_economic$`Life Ladder`~num_data_social_economic$`Access to clean fuels and technologies for cooking (% of population)`, data = num_data_social_economic)
summary(social_ec)
social_ec<-lm(num_data_social_economic$`Life Ladder`~num_data_social_economic$`Access to clean fuels and technologies for cooking (% of population)`, data = num_data_social_economic)
summary(social_ec)
social_ec<-lm(num_data_social_economic$`Life Ladder`~num_data_social_economic$`Access to clean fuels and technologies for cooking (% of population)`, data = num_data_social_economic)
summary(social_ec)
social_ec<-lm(num_data_social_economic$`Life Ladder`~num_data_social_economic$`Access to clean fuels and technologies for cooking (% of population)`, data = num_data_social_economic)
summary(social_ec)
social_ec<-lm(num_data_social_economic$`Life Ladder`~num_data_social_economic$`Access to clean fuels and technologies for cooking (% of population)`, data = num_data_social_economic)
summary(social_ec)
social_ec<-lm(num_data_social_economic$`Life Ladder`~num_data_social_economic$`Access to clean fuels and technologies for cooking (% of population)`, data = num_data_social_economic)
summary(social_ec)
social_ec<-lm(num_data_social_economic$`Life Ladder`~num_data_social_economic$`Access to clean fuels and technologies for cooking (% of population)`, data = num_data_social_economic)
summary(social_ec)
social_ec<-lm(num_data_social_economic$`Life Ladder`~num_data_social_economic$`Access to clean fuels and technologies for cooking (% of population)`, data = num_data_social_economic)
summary(social_ec)





test<-lm(num_data_economic$`Life Ladder`~num_data_economic$`Log GDP per capita`+ num_data_economic$`GDP (constant 2015 US$)`, data = num_data_economic)
summary(test)

test<-lm(num_data_economic$`Life Ladder`~num_data_economic$`Log GDP per capita`+ num_data_economic$`Agricultural land (% of land area)`, data = num_data_economic)
summary(test)




subjective_test<-lm(num_data_subjective$`Life Ladder`~num_data_subjective$`Social support`, data = num_data_subjective)
summary(subjective_test)

subjective_test<-lm(num_data_subjective$`Life Ladder`~num_data_subjective$`Healthy life expectancy at birth`, data = num_data_subjective)
summary(subjective_test)

subjective_test<-lm(num_data_subjective$`Life Ladder`~num_data_subjective$`Freedom to make life choices`, data = num_data_subjective)
summary(subjective_test)

subjective_test<-lm(num_data_subjective$`Life Ladder`~num_data_subjective$Generosity, data = num_data_subjective)
summary(subjective_test)
subjective_test<-lm(num_data_subjective$`Life Ladder`~num_data_subjective$`Perceptions of corruption`, data = num_data_subjective)
summary(subjective_test)

subjective_test<-lm(num_data_subjective$`Life Ladder`~num_data_subjective$`Positive affect`, data = num_data_subjective)
summary(subjective_test)

subjective_test<-lm(num_data_subjective$`Life Ladder`~num_data_subjective$`Negative affect`, data = num_data_subjective)
summary(subjective_test)

subjective_test<-lm(num_data_subjective$`Life Ladder`~num_data_subjective$`Confidence in national government`, data = num_data_subjective)
summary(subjective_test)


SE1<-lm(SE$`Life Ladder`~., data = SE)

E1<-lm(E$`Life Ladder`~., data = E)

stepAIC(reg1, direction = "both", trace = 1) # subjective with all
stepAIC(reg2, direction = "both", trace = 1) # SE with all
stepAIC(reg3, direction = "both", trace = 1) # Economic with all
stepAIC(SE1, direction = "both", trace = 1) # SE 4 variables
stepAIC(E1, direction = "both", trace = 1) # E 4 variables


num_data_social_economic2<-num_data_social_economic
for (i in 1:ncol(num_data_social_economic2)) {
  names(num_data_social_economic2)[i] <- gsub(" ", "_", names(num_data_social_economic2)[i])
}
# Check updated variable names
names(num_data_social_economic2)


#### cross validation ####
#LOOCV
# R program to implement
# Leave one out cross validation
# defining training control
# as Leave One Out Cross Validation
train_control <- trainControl(method = "LOOCV")

# training the model by assigning sales column
# as target variable and rest other column
# as independent variable
model <- train(`Life Ladder` ~., data = SE,
               method = "lm",
               trControl = train_control)

# printing model performance metrics
# along with other details
print(model)



# K-fold cross-validation
# setting seed to generate a reproducible random sampling
set.seed(125)
# defining training control as cross-validation and
# value of K equal to 10
train_control <- trainControl(method = "cv",
                              number = 10)

# training the model by assigning sales column as target variable and rest other column
# as independent variable
model <- train(`Life Ladder` ~., data = SE,
               method = "lm",
               trControl = train_control)

# printing model performance metrics
# along with other details
print(model)


#### regression part 3, standardize, normalize ####
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
social_economic<- df3[,c(1:4,14:19,33,35:38)]
economic<- df3[,c(1:4,5,20:32,34)]

#make sure the dataset is numeric
num_data_subjective <- subjective[, sapply(subjective, is.numeric)] 
num_data_social_economic <- social_economic[, sapply(social_economic, is.numeric)] 
num_data_economic <- economic[, sapply(economic, is.numeric)] 
#PCA data
SE<-social_economic[,c(4,5,12:13,15)]
E<-economic[,c(4,11,14,13,15, 16, 8,9,19,17)]
S<-num_data_subjective[,c(2:10)]
attr(S, "na.action")<-NULL
attr(SE, "na.action")<-NULL
attr(E, "na.action")<-NULL

#normality test
set.seed(123)
#perform kolmogorov-smirnov test
ks.test(S, 'pnorm')
# this tests says subjective is not normally distributed (p is less than .05), but there is an error?

#perform shapiro-wilk test
shapiro.test(S)

# Create a QQ plot, does not worl
qqnorm(S)
qqline(S)

#also all not normal
shapiro.test(S$`Life Ladder`)
shapiro.test(S$`Social support`)
shapiro.test(S$`Healthy life expectancy at birth`)
shapiro.test(S$`Freedom to make life choices`)
shapiro.test(S$Generosity)
shapiro.test(S$`Perceptions of corruption`)
shapiro.test(S$`Positive affect`)
shapiro.test(S$`Negative affect`)
shapiro.test(S$`Confidence in national government`)
# transform variables to log scale and create new dataset, generated NAs because of negative or zero
# values, maybe add the min+1 to make it positive?
S_log <- data.frame(lapply(S, function(x) log(x)))
S_log_scaled <- scale(S_log) #standardize

b<-boxcox(lm(S$`Life Ladder`~1))
lambda <- b$x[which.max(b$y)]
new_x_exact <- (S$`Life Ladder` ^ lambda - 1) / lambda
shapiro_test(new_x_exact)

# rename variables with "log" prefix
names(S_log) <- paste0("log_", names(S))
shapiro.test(S_log[,1])


ggplot(S, aes(x = 1:nrow(S_log), y = S$`Life Ladder`)) +  # Apply nrow function
  geom_point()








lm1<-lm(`Life Ladder`~., data=S)
resettest(lm1)
plot(S_log$`log_Social support`, S_log$`Life Ladder`)
plot(lm1)

lm1_1<-lm(`Life Ladder`~`Negative affect`+`Social support`,data=S)
layout(matrix(c(1,2,3,4),2,2))
plot(lm1)
summary(lm1)

stepAIC(lm1)
tab_model(lm1,lm1_1, show.aic = TRUE,show.obs = FALSE, dv.labels = "Subjective on Happiness")





#E
set.seed(123)
#perform kolmogorov-smirnov test
ks.test(E, 'pnorm')
# this tests says subjective is not normally distributed, but there is an warning?

#perform shapiro-wilk test
shapiro.test(E)

for (col in colnames(E)) {
  shapiro.test(E[,col])
}

# Create a QQ plot
qqnorm(E)
qqline(E)



lm5<-lm(S$`Life Ladder`~S$`Negative affect`,data=S)
summary(lm5)


#all not normal?
shapiro.test(E$`Life Ladder`)
shapiro.test(E$`GDP (current US$)`)
shapiro.test(E$`GDP per capita (current US$)`)
shapiro.test(E$`GDP growth (annual %)`)
shapiro.test(E$`Gross savings (% of GNI)`)
shapiro.test(E$`Inflation, consumer prices (annual %)`)
shapiro.test(E$`Agricultural land (% of land area)`)
shapiro.test(E$`Agricultural land (sq. km)`)
shapiro.test(E$`Mortality rate, infant (per 1,000 live births)`)
shapiro.test(E$`Labor force, total`)
E_log <- data.frame(lapply(E, function(x) log(x)))
E_log_scaled <- scale(SE_log) #standardize
shapiro_test(E_log$GDP..current.US..)

lm2<-lm(`Life Ladder`~ `GDP (current US$)`+
          poly(`GDP per capita (current US$)`, degree=2) +
          `GDP growth (annual %)`+
          `Gross savings (% of GNI)`+
          `Inflation, consumer prices (annual %)`+
          `Agricultural land (% of land area)`+
          `Agricultural land (sq. km)`+
          `Mortality rate, infant (per 1,000 live births)`+
          `Labor force, total`, data=E)
summary(lm2)

lm2<-lm(`Life Ladder`~., data=E)
tab_model(lm2, lm2.1,lm2.2, show.aic=TRUE,dv.labels = "Economic on Happiness", show.obs = FALSE)




lm3<-lm(E$`Life Ladder`~ E$`GDP (current US$)`+
          poly(E$`GDP per capita (current US$)`, degree=2) +
          E$`Inflation, consumer prices (annual %)`+
          E$`Agricultural land (sq. km)`+
          E$`Mortality rate, infant (per 1,000 live births)`, data=E)
summary(lm3)

lm2.1<-lm(`Life Ladder`~  poly(`GDP per capita (current US$)`, degree=2) +  `Gross savings (% of GNI)`
        , data=E)
summary(lm2.2)
lm2.2<-lm(`Life Ladder`~  poly(`GDP per capita (current US$)`, degree=2) +  `Agricultural land (% of land area)`
          , data=E)

tab_model(lm1, show.aic = TRUE, show.ci = FALSE, show.obs = FALSE,  auto.label = FALSE, dv.labels = "subjective")

plot(lm2M)


library(MASS)
stepAIC(lm2, direction = "backward")
ggpairs()
# poly, anova
# reset
resettest(lm2, power=2, type="regressor", data = E)

lm2<-lm(E$`Life Ladder`~ E$`Agricultural land (% of land area)`+E$`Mortality rate, infant (per 1,000 live births)`+E$`GDP per capita (current US$)`, data=E)

summary(lm2)



#SE
set.seed(123)
#perform kolmogorov-smirnov test
ks.test(SE, 'pnorm')
# this tests says subjective is not normally distributed, but there is an warning?

#perform shapiro-wilk test
shapiro.test(SE)
for (col in colnames(SE)) {
  shapiro.test(SE[,col])
}

# they are all p < 2.2e-16, not normal
shapiro.test(SE$`Life Ladder`)
shapiro.test(SE$`Access to clean fuels and technologies for cooking (% of population)`)
shapiro.test(SE$`Forest area (% of land area)`)
shapiro.test(SE$`Arable land (% of land area)`)
shapiro.test(SE$`Individuals using the Internet (% of population)`)
SE_log <- data.frame(lapply(SE, function(x) log(x)))
SE_log_scaled <- scale(SE_log) #standardize
# Create a QQ plot
qqnorm(SE)
qqline(SE)


lm4<-lm(`Life Ladder`~., data=SE)
stepAIC(lm4)

str(SE$`Access to clean fuels and technologies for cooking (% of population)`)
plot(lm3)

lm4.1<-lm(`Life Ladder`~`Individuals using the Internet (% of population)`+`Forest area (% of land area)`, data=SE)
summary(lm4.1)

lm3<-lm(SE$`Life Ladder`~
          SE$`Forest area (% of land area)`+ 
          SE$`Individuals using the Internet (% of population)`+
          SE$`Access to clean fuels and technologies for cooking (% of population)`, data=SE)
summary(lm3)



tab_model(lm4,lm4.1, dv.labels = "Social Economic on Happiness", show.obs = FALSE, show.aic = TRUE)

#moderators, confounder
# normalizing test, scale?
#logistic regression
# standardize
# cross validation


#### some statistical tests ####

summary(df2$`Life Ladder`)
sd(df2$`Life Ladder`)

sapply(df2[4:13],summary)
sapply(df2[4:13],sd)

#simple tests with life ladder/happiness~social support
ggplot(data=df2)+
  geom_point(aes(x=df2$`Social support`,y=df2$`Life Ladder`))+
  geom_smooth(aes(x=df2$`Social support`,y=df2$`Life Ladder`),method = "lm")

cor.test(df2$`Life Ladder`,df2$`Social support`)

lm1 <- lm(df2$`Life Ladder`~df2$`Social support`,data=df2)
summary(lm1)

colnames(df3)
# new test with gdp
ggplot(data=df2)+
  geom_point(aes(x=`Individuals using the Internet (% of population)`,y=`Life Ladder`))+
  geom_smooth(aes(x=`Individuals using the Internet (% of population)`,y=`Life Ladder`),
              method = "lm")

cor.test(df2$`Individuals using the Internet (% of population)`,df2$`Life Ladder`)

lm2 <- lm(df2$`Life Ladder`~df2$`Individuals using the Internet (% of population)`,data=df2)
summary(lm2)


# new test with confidence in gov
ggplot(data=df2)+
  geom_point(aes(x=`Confidence in national government`,y=`Life Ladder`))+
  geom_smooth(aes(x=`Confidence in national government`,y=`Life Ladder`),
              method = "lm")

cor.test(df2$`Confidence in national government`,df2$`Life Ladder`)

lm3 <- lm(df2$`Life Ladder`~df2$`Confidence in national government`,data=df2)
summary(lm3)

#pairwise statistical tests

#clusters
#radar graph


# cluster on df3
# per country, what percent ends in the same cluster,
# per cluster, what are their demographics
# climate change impact data?, which countries are affected the most



#factor analysis on the variables as well
#then see whats going to be analysis, we were doing exploratory







#### scrap ####
finalcountry<-unique(FINAL$country_name)
#UN_countries<-finalcountry[ ! finalcountry %in% countries_region, ]
country_in_region<- function(x,y){
  names<-data.frame()
  finalx<-x[[1]]
  i<-length(colnames(y))
  for (ii in 1:length(finalx)){
    for(cn in 1:i){
      for(rn in 1:28)
      if(TRUE %in% (finalx[ii] == y[rn,cn])){
        names<-rbind(names,colnames(y[cn]))
        
      }
      else{
        next
      }
    }
  }
  finaldf<-cbind(x,names)
}


debugonce(country_in_region)
df<-country_in_region(FINAL,countries_region)


long2<-na.omit(long)

country_region<-function(x,y){
  names<-data.frame()
  finalx<-x[[1]]
  for(i in 1:length(finalx)){
    for(cn in 1:length(y[[1]])){
      if(TRUE %in% (finalx[i]==y[cn,2])){
        names<-rbind(names,y[cn,1])
      }
      else{
        next
      }
        
    }
  }
  finaldf<-cbind(x,names)
  return(finaldf)
}


debugonce(country_region)
df<-country_region(FINAL,long)


Western<-c("Iran","Afghanistan")
Arab_World

# misc 
total <- merge(data_long,df2,by=c("Country Name","year"))
save(total,file="total.Rdata")
load(file = "total.Rdata")

# save as csv files
write.table(df, file = "df.csv",
            sep = ",", row.names = F)




