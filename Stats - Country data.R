

#############################################################################################
#                                      STATISTICS PROJECT
#############################################################################################


# points to cover:

# 1. DO EDA - Data Cleaning
# 2. Do Visulization
# 3. Discriptive Stata
# 4. Inferential Stats - Define Hypo


## Data From kaggle

# Load the data:

countries = read.csv('country_profile_variables.csv')

str(countries)

View(countries)

# Select Few Important Variables only

countries = countries[,c(1,2,3,4,7,9,10,11,12,13,14,15,20,21,16,40,42)]

attach(countries)

names(countries) = c('Country', 'Region', 'Area', 'Population', 'GDP', 'GDP_per_capita', 
                  'Agri_GAV', 'Industry_GAV', 'Service_GAV', 
                  'Employment_Agri', 'Employment_Industry', 'Employment_Service', 
                  'Export', 'Import', 'Unemployment', 'Mobile_User', 'Internet_User')




## EXPLORATERY DATA ANALYSIS ##

# Check Missing Values

colSums(is.na(countries))    # Seems no missing values

# Check the Structure of the data

str(countries)

# Summary of the data

summary(countries)

# Convert factores into numbers

attach(countries)

countries$Agri_GAV = as.numeric(countries$Agri_GAV)
countries$Area = as.numeric(countries$Area)
countries$Employment_Agri = as.numeric(countries$Employment_Agri)
countries$Employment_Industry = as.numeric(countries$Employment_Industry)
countries$Employment_Service = as.numeric(countries$Employment_Service)

countries$Export = as.numeric(countries$Export)
countries$Import = as.numeric(countries$Import)

countries$Unemployment = as.numeric(countries$Unemployment)
countries$Mobile_User = as.numeric(countries$Mobile_User)

attach(countries)



# Data Cleaning requires 
# 1. Remove Remove -99 and ~0 entries from vars like Area, GDP etc..

countries = countries[which(Area >= 45 & GDP >=1 & GDP_per_capita >= 1 &
                              Agri_GAV >= 1 & Industry_GAV >= 0.1 & Service_GAV >= 0.1 &
                              Import >= 0.1 & Export >= 0.1 & Unemployment >= 0.1 & Mobile_User >= 1, Internet_User >= 1),]

attach(countries)

summary(countries)

# RPivot Table Amalysis

library(rpivotTable)

rpivotTable(countries)

# POINTS: 
# 1. Avg. area size, NorthAmerica covers the highest area and NorthenAfrica covers least area.
# 2. SouthernAsia has highest total poppulation & Micronesia regian has least total population
# 3. NorthenAmerican and EasternAsia has highest avg. GDP size and Micronesia and Polynesia has least agv GDP size
# 4. EasternAfrica has highest total employment in agri and Industry.
# 5. But, WesternAsia and NorthenEurope;s most employment is in Service sector. 
# 6. EasetrnAfrica, EsternEurope, and NortheEurope are amongs top list in Unemployment.
# 7. EasetrnAfrica and WesternAsia has the highest total numner of Mobile users
# 8. Where, South-easternAsia has most Internet users.


## Data Visulization

# BoxPlot

par(mfrow = c(1,1))

boxplot(Area, data = countries, main = 'Area', horizontal = TRUE, col  = 'Seagreen')

boxplot(Population, data = countries, main = 'Population', horizontal = TRUE, col  = 'orange')

boxplot(GDP, data = countries, main = 'GDP', horizontal = TRUE, col  = 'darkviolet')

boxplot(GDP_per_capita, data = countries, main = 'GDP Per Capita', horizontal = TRUE, col  = 'salmon')

boxplot(Agri_GAV, data = countries, main = 'Agri GAV', horizontal = TRUE, col  = 'Yellowgreen')

boxplot(Industry_GAV, data = countries, main = 'Industry GAV', horizontal = TRUE, col  = 'hotpink')

boxplot(Service_GAV, data = countries, main = 'Service GAV', horizontal = TRUE, col  = 'Seagreen')

boxplot(Employment_Agri, data = countries, main = 'Emp Agri', horizontal = TRUE, col  = 'deepskyblue3')

boxplot(Employment_Industry, data = countries, main = 'Emp Industry', horizontal = TRUE, col  = 'gold')

boxplot(Employment_Service, data = countries, main = 'Emp Service', horizontal = TRUE, col  = 'red')

boxplot(Import, data = countries, main = 'Import', horizontal = TRUE, col  = 'darkviolet')

boxplot(Export, data = countries, main = 'Export', horizontal = TRUE, col  = 'orange')

boxplot(Unemployment, data = countries, main = 'Enemployment', horizontal = TRUE, col  = 'yellowgreen')

boxplot(Mobile_User, data = countries, main = 'Mobile Users', horizontal = TRUE, col  = 'salmon')

boxplot(Internet_User, data = countries, main = 'Internet Users', horizontal = TRUE, col  = 'Seagreen')



# Histogram plot 


hist(Area, data = countries, main = 'Area', col  = 'Seagreen')

hist(Population, data = countries, main = 'Population', col  = 'orange')

hist(GDP, data = countries, main = 'GDP', col  = 'darkviolet')

hist(GDP_per_capita, data = countries, main = 'GDP Per Capita', col  = 'salmon')

hist(Agri_GAV, data = countries, main = 'Agri GAV', col  = 'Yellowgreen')

hist(Industry_GAV, data = countries, main = 'Industry GAV', col  = 'hotpink')

hist(Service_GAV, data = countries, main = 'Service GAV', col  = 'Seagreen')

hist(Employment_Agri, data = countries, main = 'Emp Agri', col  = 'deepskyblue3')

hist(Employment_Industry, data = countries, main = 'Emp Industry', col  = 'Yellowgreen')

hist(Employment_Service, data = countries, main = 'Emp Service', col  = 'salmon')

hist(Import, data = countries, main = 'Import', col  = 'orange')

hist(Export, data = countries, main = 'Export', col  = 'darkvoilet')

hist(Unemployment, data = countries, main = 'Enemployment', col  = 'red')

hist(Mobile_User, data = countries, main = 'Mobile Users', col  = 'gold')

hist(Internet_User, data = countries, main = 'Internet Users', col  = 'Seagreen')




# Correlation 

library(corrplot)
library(RColorBrewer)

corrplot(cor(countries[, c(-1,-2)]), type = 'upper', order = 'hclust', col = brewer.pal(n = 7, name = 'YlGnBu'))

pairs(countries[, c(-1,-2)], col = 'salmon')


# Form the Hypothesis

# Research Objective: Based on the given data, which has the highest impact on GDP

# Hypothesis test 1:

# Ho: All vars are equally important for GDP
# Ha: All vars are not equally important for GDP  


anova = aov(GDP ~ Area + Population + GDP_per_capita +
              Agri_GAV + Service_GAV + Industry_GAV +
              Employment_Agri + Employment_Industry + Employment_Service +
              Export + Import + Unemployment + 
              Mobile_User + Internet_User, data = countries)



lm = lm(GDP ~ Area + Population + GDP_per_capita +
          Agri_GAV + Service_GAV + Industry_GAV +
          Employment_Agri + Employment_Industry + Employment_Service +
          Export + Import + Unemployment + 
          Mobile_User + Internet_User, data = countries)


