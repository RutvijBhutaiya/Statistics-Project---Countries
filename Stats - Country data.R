

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














