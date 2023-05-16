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
write.table(wide_total2, file = "FINAL.csv",
            sep = ",", row.names = F)
#the final dataset we have is called wide_total2 and we save it as FINAL.csv

#read files
FINAL <- read_csv("FINAL.csv")
finalnaomit<-na.omit(FINAL) # from 3564 observations to 1741

# Time to do some analysis
table(factor(finalnaomit$year)) #count(finalnaomit,factor(finalnaomit$year))
# 2005 only 1 observation, 2006 72
#years range from 2005 to 2021
barplot(table(factor(finalnaomit$year)),main="Frequency of Available Data by Years" ,xlab="Years",
        ylab="Frequency", col="cornflowerblue")
df<-replace(finalnaomit,finalnaomit=="..",NA)

barplot((colMeans(is.na(df)))*100, main = "Percent of Missing Data in Merged Dataset", xlab= "Variables", ylab = "Percentage")

# using ggplot to make the barplot
# Calculate the percentage of missing data per variable
missing_percent <- colMeans(is.na(df)) * 100
# Create a data frame with variable names and missing percentages
missing_df <- data.frame(variable = names(missing_percent), missing_percent)
# Create the bar plot
ggplot(missing_df, aes(x = variable, y = missing_percent)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Variable", y = "Percentage of Missing Data",
       title = "Percentage of Missing Data per Variable") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  coord_flip()


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
# 37 variables are left with no more than 10% missing in df3
df3<-subset(df,select=-missing)
barplot((colMeans(is.na(df3)))*100, main = "Percent of Missing Data in Merged Dataset", xlab= "Variables", ylab = "Percentage")
# Checking the number of country datas per year, we find that 2005 has very few information
# consider dropping 2005
barplot(table(factor(df$year)),main="Frequency of Available Data by Years" ,xlab="Years",
        ylab="Frequency", col="cornflowerblue")
df3<-subset(df3, df3$year != 2005)

df3<- na.omit(df3) # 1331 observations left from 1740

write.table(df3, file = "df.csv",
            sep = ",", row.names = F)







