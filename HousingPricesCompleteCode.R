
library("tidyverse")

library("rpart")

library("rpart.plot")

library("caret")

library("forecast")

rm(list=ls())

set.seed(30)

houses <- read.csv("C:/Users/warnd/OneDrive/Documents/oba_455_555_ddpm_r/rproject/HousingPrices/data.csv")

# Data Cleaning, these lines exclude some of the extreme outliers in the dataset
# based on our output variable Price. We ran the models with and without these rows,
# and found results with much better error measures. For the regression tree model
# specifically we also removed 1 row of data with City = Medina, and 1 row of data
# with City = Yarrow Point because the model could not run when the Train and Validation
# data did not have at least one of each city. 

houses = subset(houses, price < 1000001)
houses = subset(houses, price >99999)
houses = subset(houses, city = "Medina")
houses = subset(houses, city = "Yarrow Point")

# Factor city variable, comparing to Auburn

houses = houses %>%
  mutate(city = factor(houses$city, levels = c("Auburn", "Seattle", "Beaux Arts Village",
                                               "Bellevue", "Black Diamond", 
                                               "Bothell", "Burien", "Carnation",
                                               "Clyde Hall", "Covington", "Des Moines",
                                               "Duvall", "Enumclaw", "Fall City",
                                               "Federal Way", "Inglewood-Finn Hill",
                                               "Issaquah", "Kenmore", "Kent",
                                               "Kirkland", "Lake Forest Park", 
                                               "Maple Valley", "Medina", "Mercer Island",
                                               "Milton","Newcastle","cityNormandy Park",
                                               "North Bend","Pacific","Preston",
                                               "Ravensdale","Redmond","Renton",
                                               "Sammamish","SeaTac",
                                               "Shoreline","Skykomish","Snoqualmie",
                                               "Snoqualmie Pass","cityTukwila","Vashon",
                                               "Woodinville","Yarrow Point")))

# Mutating data to allow for the creation of age_when_listed variable creation

houses = houses %>%
  mutate(date = as.Date(houses$date)) %>%
  mutate(date = format(date, format="%Y"))

houses = houses %>%
  mutate(date = as.numeric(houses$date)) %>%
  mutate(yr_built = as.numeric(houses$yr_built)) %>%
  mutate(age_when_listed = date - yr_built)

houses = houses %>%
  mutate(age_when_listed = (houses$date - houses$yr_built))

# If else statement to fix the renovated variable. It originally has the
# year it was renovated or a 0 if it hasn't been renovated before. Returns
# a 1 if it has been renovated, 0 if it has not.

houses= houses %>%
  mutate(renovated = ifelse(yr_renovated > 0, 1,0))

houses = houses %>%
  rename(price_actual = price)

houses = houses %>%
  mutate(id = 1:nrow(houses))

train = houses %>%
  sample_frac(0.7)

validation = houses %>%
  slice(setdiff(houses$id, train$id))

houses.rt = rpart(price_actual ~ bedrooms + bathrooms + sqft_above + sqft_lot
                  + condition + floors + sqft_basement + view + city + waterfront +
                    age_when_listed + renovated , data = train, method = "anova",
                  cp = 0.001, minsplit = 10, xval =10)

plotcp(houses.rt)


cp.table = as_tibble(houses.rt$cptable)

# Optimal CP value of 27 because on the plot of houses.rt the error begins to
# stabilize at this point

optimal.cp = cp.table %>%
  filter(nsplit == 27)

pruned.ct = prune(houses.rt, optimal.cp$CP)

prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10)

















# Regression Tree Output nsplit of 7 for visual clarity


prediction = predict(pruned.ct, validation, type = "vector")

validation = validation %>%
  mutate(price_prediction = prediction)

accuracy(validation$price_prediction, validation$price_actual)

# Accuracy measure results for simple Regression Tree model
#    ME               RMSE          MAE            MPE           MAPE
# 4093.218     126977.6     93925.04     -6.81149     22.54763

# K-Fold Cross Validation for Regression Tree

rm(list=ls())

set.seed(30)

houses <- read.csv("C:/Users/warnd/OneDrive/Documents/oba_455_555_ddpm_r/rproject/HousingPrices/data.csv")

houses = subset(houses, price < 1000001)
houses = subset(houses, price >99999)
houses = subset(houses, city = "Medina")
houses = subset(houses, city = "Yarrow Point")

houses = houses %>%
  mutate(city = factor(houses$city, levels = c("Auburn", "Seattle", "Beaux Arts Village",
                                               "Bellevue", "Black Diamond", 
                                               "Bothell", "Burien", "Carnation",
                                               "Clyde Hall", "Covington", "Des Moines",
                                               "Duvall", "Enumclaw", "Fall City",
                                               "Federal Way", "Inglewood-Finn Hill",
                                               "Issaquah", "Kenmore", "Kent",
                                               "Kirkland", "Lake Forest Park", 
                                               "Maple Valley", "Medina", "Mercer Island",
                                               "Milton","Newcastle","cityNormandy Park",
                                               "North Bend","Pacific","Preston",
                                               "Ravensdale","Redmond","Renton",
                                               "Sammamish","SeaTac",
                                               "Shoreline","Skykomish","Snoqualmie",
                                               "Snoqualmie Pass","Tukwila","Vashon",
                                               "Woodinville","Yarrow Point")))

houses = houses %>%
  mutate(date = as.Date(houses$date)) %>%
  mutate(date = format(date, format="%Y"))

houses = houses %>%
  mutate(date = as.numeric(houses$date)) %>%
  mutate(yr_built = as.numeric(houses$yr_built)) %>%
  mutate(age_when_listed = date - yr_built)

houses = houses %>%
  mutate(age_when_listed = (houses$date - houses$yr_built))

houses= houses %>%
  mutate(renovated = ifelse(yr_renovated > 0, 1,0))

houses = houses %>%
  rename(price_actual = price)

houses = houses %>%
  mutate(id = 1:nrow(houses))

houses = houses %>%
  mutate(fold = sample(1:10, 4202, replace = TRUE))

library(tibble)
measures <- tibble()

K = 10

for(i in 1:K)
{
  train = houses %>%
    filter(fold != i)
  
  validation = houses %>%
    filter(fold == i)
  
  houses.rt = rpart(price_actual ~ bedrooms + bathrooms + sqft_above + sqft_lot
                    + condition + floors + sqft_basement + view + city + waterfront +
                      age_when_listed + renovated , data = train, method = "anova",
                    cp = 0.001, minsplit = 10, xval =10)
  
  cp.table = as_tibble(houses.rt$cptable)
  
  optimal.cp = cp.table %>%
    filter(nsplit == 27)
  
  pruned.ct = prune(houses.rt, optimal.cp$CP)
  
  prediction = predict(pruned.ct, validation, type = "vector")
  
  validation = validation %>%
    mutate(price_prediction = prediction)
  
  validation = validation %>%
    mutate(price_prediction = predict(houses.rt, validation))
  
  accuracy = accuracy(validation$price_prediction, validation$price_actual)
  
  measures = measures %>%
    bind_rows(as_tibble(list(run=i, RMSE = accuracy[2], MAPE= accuracy[5])))
  print(paste("iteration", i, "completed"), sep =" ")
}

measures %>%
  summarise(mean_RMSE = mean(RMSE), sd_RMSE = sd(RMSE), mean_MAPE = mean(MAPE), sd_MAPE = sd(MAPE))

# Mean/SD of K-Fold Accuracy Measures of Regression Tree
#  mean_RMSE  sd_RMSE mean_MAPE sd_MAPE
#    123480    5876      21.4   0.689









rm(list=ls())
#KNN Regression

library("tidyverse")
library("FNN")
library("forecast")
library("caret")
library("stats")
library("caret")
library("dplyr")
library("ggplot2")


houses=read_csv("C:/Users/warnd/OneDrive/Documents/oba_455_555_ddpm_r/rproject/HousingPrices/data.csv")
# drop 0 values and columns we won't be using 
houses = subset(houses, price < 1000001)
houses = subset(houses, price >99999)



houses =houses %>%
  select(-street, -statezip,-country)

# Mutating data to allow for the creation of age_when_listed variable creation

houses = houses %>%
  mutate(date = as.Date(houses$date)) %>%
  mutate(date = format(date, format="%Y"))

houses = houses %>%
  mutate(date = as.numeric(houses$date)) %>%
  mutate(yr_built = as.numeric(houses$yr_built)) %>%
  mutate(age_when_listed = date - yr_built)

houses = houses %>%
  mutate(age_when_listed = (houses$date - houses$yr_built))

# If else statement to fix the renovated variable. It originally has the
# year it was renovated or a 0 if it hasn't been renovated before. Returns
# a 1 if it has been renovated, 0 if it has not.

houses= houses %>%
  mutate(renovated = ifelse(yr_renovated > 0, 1,0))

houses = houses %>%
  mutate(cityShoreline = ifelse(city == "Shoreline", 1,0)) %>%
  mutate(cityKent = ifelse(city == "Kent", 1,0)) %>%
  mutate(cityBellevue = ifelse(city == "Bellevue", 1,0)) %>%
  mutate(cityRedmond = ifelse(city == "Redmond", 1,0)) %>%
  mutate(citySeattle = ifelse(city == "Seattle", 1,0)) %>%
  mutate(cityMapleValley = ifelse(city == "Maple Valley", 1,0)) %>%
  mutate(cityLakeForestPark = ifelse(city == "Lake Forest Park", 1,0)) %>%
  mutate(cityNorthBend  = ifelse(city == "North Bend ", 1,0)) %>%
  mutate(cityAuburn = ifelse(city == "Auburn", 1,0)) %>%
  mutate(cityDesMoines = ifelse(city == "Des Moines", 1,0)) %>%
  mutate(cityBothell = ifelse(city == "Bothell", 1,0)) %>%
  mutate(cityFederalWay = ifelse(city == "Federal Way", 1,0)) %>%
  mutate(cityKirkland = ifelse(city == "Kirkland", 1,0)) %>%
  mutate(cityIssaquah = ifelse(city == "Issaquah", 1,0)) %>%
  mutate(cityWoodinville = ifelse(city == "Woodinville", 1,0)) %>%
  mutate(cityFallCity = ifelse(city == "Fall City", 1,0)) %>%
  mutate(cityRenton = ifelse(city == "Renton", 1,0)) %>%
  mutate(cityCarnation = ifelse(city == "Carnation", 1,0)) %>%
  mutate(citySnoqualmie = ifelse(city == "Snoqualmie", 1,0)) %>%
  mutate(cityDuvall = ifelse(city == "Duvall", 1,0)) %>%
  mutate(cityBurien = ifelse(city == "Burien", 1,0)) %>%
  mutate(cityInglewoodFinnHill = ifelse(city == "Inglewood-Finn Hill", 1,0)) %>%
  mutate(cityKenmore = ifelse(city == "Kenmore", 1,0)) %>%
  mutate(cityNewcastle = ifelse(city == "Newcastle", 1,0)) %>%
  mutate(cityBlackDiamond = ifelse(city == "Black Diamond", 1,0)) %>%
  mutate(cityRavensdale = ifelse(city == "Ravensdale", 1,0)) %>%
  mutate(cityMercerIsland = ifelse(city == "Mercer Island", 1,0)) %>%
  mutate(citySkykomish = ifelse(city == "Skykomish", 1,0)) %>%
  mutate(cityVashon = ifelse(city == "Vashon", 1,0)) %>%
  mutate(citySeaTac = ifelse(city == "SeaTac", 1,0)) %>%
  mutate(cityEnumclaw = ifelse(city == "Enumclaw", 1,0)) %>%
  mutate(citySnoqualmiePass = ifelse(city == "Snoqualmie Pass", 1,0)) %>%
  mutate(cityPacific = ifelse(city == "Pacific", 1,0)) %>%
  mutate(cityBeauxArtsVillage = ifelse(city == "Beaux Arts Village", 1,0)) %>%
  mutate(cityPreston = ifelse(city == "Preston", 1,0)) %>%
  mutate(cityMilton = ifelse(city == "Milton", 1,0)) %>%
  mutate(cityYarrowPoint = ifelse(city == "Yarrow Point", 1,0)) %>%
  mutate(cityMedina = ifelse(city == "Medina", 1,0)) %>%
  mutate(cityNormandyPark = ifelse(city == "NormandyPark", 1,0)) %>%
  mutate(cityAlgona = ifelse(city == "Algona", 1,0)) %>%
  mutate(cityClydeHill = ifelse(city == "ClydeHill", 1,0)) %>%
  mutate(cityTukwila = ifelse(city == "TukWila", 1,0)) %>%
  mutate(cityCovington = ifelse(city == "Covington", 1,0)) %>%
  mutate(citySammamish = ifelse(city == "Sammamish", 1,0))



#normalize data for significant variables.
b1= mean(houses$bedrooms)
b2=sd(houses$bedrooms)

c1= mean(houses$bathrooms)
c2=sd(houses$bathrooms)

e1= mean(houses$sqft_lot)
e2=sd(houses$sqft_lot)

f1= mean(houses$floors)
f2=sd(houses$floors)

g1= mean(houses$view)
g2=sd(houses$view)

h1=mean(houses$waterfront)
h2=sd(houses$waterfront)

i1= mean(houses$condition)
i2=sd(houses$condition)

j1= mean(houses$sqft_above)
j2=sd(houses$sqft_above)

k1= mean(houses$sqft_basement)
k2=sd(houses$sqft_basement)

l1= mean(houses$age_when_listed)
l2=sd(houses$age_when_listed)

m1=mean(houses$yr_renovated)
m2=sd(houses$yr_renovated)

Shoreline1=mean(houses$cityShoreline)
Shoreline2=sd(houses$cityShoreline)

Kent1=mean(houses$cityKent)
Kent2=sd(houses$cityKent)

Bellevue1=mean(houses$cityBellevue)
Bellevue2=sd(houses$cityBellevue)

Redmond1=mean(houses$cityRedmond)
Redmond2=sd(houses$cityRedmond)

Seattle1=mean(houses$citySeattle)
Seattle2=sd(houses$citySeattle)

MapleValley1=mean(houses$cityMapleValley)
MapleValley2=sd(houses$cityMapleValley)

LakeForestPark1=mean(houses$cityLakeForestPark)
LakeForestPark2=sd(houses$cityLakeForestPark)

Sammamish1=mean(houses$citySammamish)
Sammamish2=sd(houses$citySammamish)

Auburn1=mean(houses$cityAuburn)
Auburn2=sd(houses$cityAuburn)

unique(houses$city)

DesMoines1=mean(houses$cityDesMoines)
DesMoines2=sd(houses$cityDesMoines)

Bothell1=mean(houses$cityBothell)
Bothell2=sd(houses$cityBothell)

FederalWay1=mean(houses$cityFederalWay)
FederalWay2=sd(houses$cityFederalWay)

Kirkland1=mean(houses$cityKirkland)
Kirkland2=sd(houses$cityKirkland)

Issaquah1=mean(houses$cityIssaquah)
Issaquah2=sd(houses$cityIssaquah)

Woodinville1=mean(houses$cityWoodinville)
Woodinville2=sd(houses$cityWoodinville)

FallCity1=mean(houses$cityFallCity)
FallCity2=sd(houses$cityFallCity)

Renton1=mean(houses$cityRenton)
Renton2=sd(houses$cityRenton)

Carnation1=mean(houses$cityCarnation)
Carnation2=sd(houses$cityCarnation)

Snoqualmie1=mean(houses$citySnoqualmie)
Snoqualmie2=sd(houses$citySnoqualmie)

Duvall1=mean(houses$cityDuvall)
Duvall2=sd(houses$cityDuvall)

Burien1=mean(houses$cityBurien)
Burien2=sd(houses$cityBurien)

Covington1=mean(houses$cityCovington)
Covington2=sd(houses$cityCovington)

InglewoodFinnHill1=mean(houses$cityInglewoodFinnHill)
InglewoodFinnHill2=sd(houses$cityInglewoodFinnHill)

Kenmore1=mean(houses$cityKenmore)
Kenmore2=sd(houses$cityKenmore)

Newcastle1=mean(houses$cityNewcastle)
Newcastle2=sd(houses$cityNewcastle)

BlackDiamond1=mean(houses$cityBlackDiamond)
BlackDiamond2=sd(houses$cityBlackDiamond)

Ravensdale1=mean(houses$cityRavensdale)
Ravensdale2=sd(houses$cityRavensdale)

Algona1=mean(houses$cityAlgona)
Algona2=sd(houses$cityAlgona)

MercerIsland1=mean(houses$cityMercerIsland)
MercerIsland2=sd(houses$cityMercerIsland)

Skykomish1=mean(houses$citySkykomish)
Skykomish2=sd(houses$citySkykomish)

Vashon1=mean(houses$cityVashon)
Vashon2=sd(houses$cityVashon)

SeaTac1=mean(houses$citySeaTac)
SeaTac2=sd(houses$citySeaTac)

Enumclaw1=mean(houses$cityEnumclaw)
Enumclaw2=sd(houses$cityEnumclaw)

SnoqualmiePass1=mean(houses$citySnoqualmiePass)
SnoqualmiePass2=sd(houses$citySnoqualmiePass)

Pacific1=mean(houses$cityPacific)
Pacific2=sd(houses$cityPacific)

BeauxArtsVillage1=mean(houses$cityBeauxArtsVillage)
BeauxArtsVillage2=sd(houses$cityBeauxArtsVillage)

Preston1=mean(houses$cityPreston)
Preston2=sd(houses$cityPreston)

Milton1=mean(houses$cityMilton)
Milton2=sd(houses$cityMilton)

YarrowPoint1=mean(houses$cityYarrowPoint)
YarrowPoint2=sd(houses$cityYarrowPoint)

Medina1=mean(houses$cityMedina)
Medina2=sd(houses$cityMedina)



houses = houses %>%
  select(bedrooms, bathrooms, 
         sqft_lot, floors, view,condition, sqft_above,
         sqft_basement, age_when_listed, waterfront, yr_renovated, price,
         cityShoreline,cityKent,cityBellevue,cityRedmond,citySeattle,cityMapleValley,cityLakeForestPark,
         citySammamish,cityAuburn, cityDesMoines, cityBothell, cityFederalWay, cityKirkland, cityIssaquah,cityWoodinville,
         cityFallCity, cityRenton, cityCarnation,citySnoqualmie,cityDuvall, cityBurien, cityCovington,
         cityInglewoodFinnHill, cityKenmore,cityNewcastle, cityBlackDiamond, cityRavensdale,cityAlgona,cityMercerIsland,
         citySkykomish, cityVashon, citySeaTac,cityEnumclaw, citySnoqualmiePass, cityPacific,cityBeauxArtsVillage,
         cityMilton, cityYarrowPoint, cityMedina, cityPreston)


houses = houses %>%
  mutate(bedrooms_norm = (bedrooms-b1)/b2,bathrooms_norm = (bathrooms-c1)/c2,
         sqft_lot_norm = (sqft_lot-e1)/e2,floors_norm = (floors-f1)/f2,
         view_norm = (view-g1)/g2, condition_norm = (condition-i1)/i2,
         sqft_above_norm = (sqft_above-j1)/j2, sqft_basement_norm = (sqft_basement-k1)/k2,
         age_when_listed_norm = (age_when_listed-l1)/l2, waterfront_norm=(waterfront-h1)/h2,
         yr_renovated_norm=(yr_renovated-m1)/m2, cityShoreline_norm =(cityShoreline-Shoreline1)/Shoreline2
         ,cityKent_norm= (cityKent-Kent1)/Kent2
         ,cityBellevue_norm =(cityBellevue-Bellevue1)/Bellevue2
         ,cityRedmond_norm =(cityRedmond-Redmond1)/Redmond2
         ,citySeattle_norm =(citySeattle-Seattle1)/Seattle2
         ,cityMapleValley_norm=(cityMapleValley-MapleValley1)/MapleValley2
         ,cityLakeForestPark_norm = (cityLakeForestPark-LakeForestPark1)/LakeForestPark2
         ,citySammamish_norm = (citySammamish- Sammamish1)/ Sammamish2
         ,cityAuburn_norm= (cityAuburn- Auburn1)/ Auburn2
         , cityDesMoines_norm=( cityDesMoines-  DesMoines1)/  DesMoines2
         , cityBothell_norm= (cityBothell- Bothell1)/ Bothell2
         , cityFederalWay_norm= (cityFederalWay- FederalWay1)/ FederalWay2
         , cityKirkland_norm= (cityKirkland- Kirkland1)/ Kirkland2
         , cityIssaquah_norm= (cityIssaquah- Issaquah1)/ Issaquah2
         ,cityWoodinville_norm= (cityWoodinville- Woodinville1)/ Woodinville2
         , cityFallCity_norm= (cityFallCity- FallCity1)/ FallCity2
         , cityRenton_norm=(cityRenton- Renton1)/ Renton2
         , cityCarnation_norm=(cityCarnation- Carnation1)/ Carnation2
         ,citySnoqualmie_norm=(citySnoqualmie- Snoqualmie1)/ Snoqualmie2
         ,cityDuvall_norm=(cityDuvall- Duvall1)/ Duvall2,
         cityBurien_norm=(cityBurien- Burien1)/ Burien2,
         cityCovington_norm=(cityCovington- Covington1)/ Covington2,
         cityInglewoodFinnHill_norm=(cityInglewoodFinnHill- InglewoodFinnHill1)/ InglewoodFinnHill2
         , cityKenmore_norm=(cityKenmore- Kenmore1)/ Kenmore2
         ,cityNewcastle_norm=(cityNewcastle- Newcastle1)/ Newcastle2
         , cityBlackDiamond_norm=(cityBlackDiamond- BlackDiamond1)/ BlackDiamond2
         , cityRavensdale_norm=(cityRavensdale- Ravensdale1)/ Ravensdale2
         ,cityAlgona_norm=(cityAlgona- Algona1)/ Algona2
         ,cityMercerIsland_norm=(cityMercerIsland- MercerIsland1)/ MercerIsland2,
         citySkykomish_norm=(citySkykomish- Skykomish1)/ Skykomish2
         , cityVashon_norm=(cityVashon- Vashon1)/ Vashon2
         , citySeaTac_norm=(citySeaTac- SeaTac1)/ SeaTac2
         ,cityEnumclaw_norm=(cityEnumclaw- Enumclaw1)/ Enumclaw2
         , citySnoqualmiePass_norm=(citySnoqualmiePass- SnoqualmiePass1)/ SnoqualmiePass2
         , cityPacific_norm=(cityPacific- Pacific1)/ Pacific2
         ,cityBeauxArtsVillage_norm=(cityBeauxArtsVillage- BeauxArtsVillage1)/ BeauxArtsVillage2,
         cityMilton_norm=(cityMilton- Milton1)/ Milton2,
         cityYarrowPoint_norm=(cityYarrowPoint- YarrowPoint1)/ YarrowPoint2,
         cityMedina_norm=( cityMedina-  Medina1)/  Medina2,
         cityPreston_norm=(cityPreston- Preston1)/ Preston2)

houses_input_norm = houses %>%
  select(bedrooms_norm, bathrooms_norm, 
         sqft_lot_norm, floors_norm, view_norm,condition_norm, sqft_above_norm,
         sqft_basement_norm, age_when_listed_norm, waterfront_norm, yr_renovated_norm, price, cityShoreline_norm
         ,cityKent_norm ,cityBellevue_norm  ,cityRedmond_norm ,citySeattle_norm  ,cityMapleValley_norm
         ,cityLakeForestPark_norm  ,citySammamish_norm  ,cityAuburn_norm , cityDesMoines_norm
         , cityBothell_norm, cityFederalWay_norm, cityKirkland_norm, cityIssaquah_norm,cityWoodinville_norm,
         cityFallCity_norm, cityRenton_norm, cityCarnation_norm,citySnoqualmie_norm
         ,cityDuvall_norm,cityBurien_norm,cityCovington_norm,cityInglewoodFinnHill_norm, cityKenmore_norm
         ,cityNewcastle_norm, cityBlackDiamond_norm, cityRavensdale_norm,cityAlgona_norm,cityMercerIsland_norm,
         citySkykomish_norm, cityVashon_norm, citySeaTac_norm
         ,cityEnumclaw_norm, citySnoqualmiePass_norm, cityPacific_norm,cityBeauxArtsVillage_norm,cityMilton_norm,
         cityYarrowPoint_norm, cityMedina_norm, cityPreston_norm)


#partition data 70/30

inTrain = createDataPartition(houses_input_norm$price, p=.7, list=FALSE)
training= houses_input_norm[inTrain, ]
validation= houses_input_norm[-inTrain, ]

tuneGrid= expand.grid(k = seq (1,45, by= 2))

# Use Caret’s train function to cross validate using k-fold
knn_reg_fit = train(price ~ .,data = training, method= 'knn',
                    trControl =trainControl(method= 'repeatedcv', number =10),
                    tuneGrid= tuneGrid)

knn_reg_fit

#find k value
results <- knn_reg_fit$results

results |> ggplot(aes(x= k, y = RMSE)) +geom_point() +geom_line()






# Linear Regression

library("tidyverse")

library("forecast")

rm(list=ls())

set.seed(30)

houses <- read.csv("C:/Users/warnd/OneDrive/Documents/oba_455_555_ddpm_r/rproject/HousingPrices/data.csv")

houses = subset(houses, price < 1000001)
houses = subset(houses, price >99999)

houses = houses %>%
  mutate(city = factor(houses$city, levels = c("Auburn", "Seattle", "Beaux Arts Village",
                                               "Bellevue", "Black Diamond", 
                                               "Bothell", "Burien", "Carnation",
                                               "Clyde Hall", "Covington", "Des Moines",
                                               "Duvall", "Enumclaw", "Fall City",
                                               "Federal Way", "Inglewood-Finn Hill",
                                               "Issaquah", "Kenmore", "Kent",
                                               "Kirkland", "Lake Forest Park", 
                                               "Maple Valley", "Medina", "Mercer Island",
                                               "Milton","Newcastle","cityNormandy Park",
                                               "North Bend","Pacific","Preston",
                                               "Ravensdale","Redmond","Renton",
                                               "Sammamish","SeaTac",
                                               "Shoreline","Skykomish","Snoqualmie",
                                               "Snoqualmie Pass","cityTukwila","Vashon",
                                               "Woodinville","Yarrow Point")))

houses = houses %>%
  rename(price_actual = price) %>%
  mutate(renovated = if_else(yr_renovated > 0, 1, 0))

houses = houses %>%
  mutate(date = as.Date(houses$date)) %>%
  mutate(date = format(date, format="%Y"))

houses = houses %>%
  mutate(date = as.numeric(houses$date)) %>%
  mutate(yr_built = as.numeric(houses$yr_built)) %>%
  mutate(age_when_listed = date - yr_built)

houses = houses %>%
  mutate(id = 1:nrow(houses))

train = houses %>%
  sample_frac(0.7)

validation = houses %>%
  slice(setdiff(houses$id, train$id))

train.mlr = lm(price_actual ~ bedrooms + bathrooms + sqft_basement + sqft_above + sqft_lot +
                 view + condition +  floors + waterfront + + age_when_listed + city, train)

summary(train.mlr)



# K-Fold Cross Validation for Linear Regression

rm(list=ls())

set.seed(30)

houses = read.csv("C:/Users/warnd/OneDrive/Documents/oba_455_555_ddpm_r/rproject/HousingPrices/data.csv")

houses = subset(houses, price < 1000001)
houses = subset(houses, price >99999)

houses = houses %>%
  rename(price_actual = price) %>%
  mutate(renovated = if_else(yr_renovated > 0, 1, 0))

houses = houses %>%
  mutate(date = as.Date(houses$date)) %>%
  mutate(date = format(date, format="%Y"))

houses = houses %>%
  mutate(date = as.numeric(houses$date)) %>%
  mutate(yr_built = as.numeric(houses$yr_built)) %>%
  mutate(age_when_listed = date - yr_built)

#Some cities emitted for K-Fold cross validation because of low representation in the dataset
# (Cities with 1 row of data)
houses = houses %>%
  mutate(city = factor(houses$city, levels = c("Auburn", "Seattle",
                                               "Bellevue", "Black Diamond", 
                                               "Bothell", "Burien", "Carnation",
                                               "Clyde Hall", "Covington", "Des Moines",
                                               "Duvall", "Enumclaw", "Fall City",
                                               "Federal Way",
                                               "Issaquah", "Kenmore", "Kent",
                                               "Kirkland", "Lake Forest Park", 
                                               "Maple Valley", "Mercer Island",
                                               "Milton","Newcastle","cityNormandy Park",
                                               "North Bend","Pacific","Preston",
                                               "Ravensdale","Redmond","Renton",
                                               "Sammamish","SeaTac",
                                               "Shoreline","Skykomish","Snoqualmie",
                                               "Tukwila","Vashon",
                                               "Woodinville")))

houses = houses %>%
  mutate(id = 1:nrow(houses))

houses = houses %>%
  mutate(fold = sample(1:10, 4202, replace = TRUE))

measures <- tibble()

K = 10

for(i in 1:K)
{
  train = houses %>%
    filter(fold != i)
  
  validation = houses %>%
    filter(fold == i)
  
  train.mlr = lm(price_actual ~ bedrooms + bathrooms + sqft_basement + sqft_lot 
                 + view + condition + sqft_above + floors + waterfront+ city + age_when_listed, train)
  
  validation = validation %>%
    mutate(price_prediction = predict(train.mlr, validation))
  
  accuracy = accuracy(validation$price_prediction, validation$price_actual)
  
  measures = measures %>%
    bind_rows(as_tibble(list(run=i, RMSE = accuracy[2], MAPE= accuracy[5])))
  print(paste("iteration", i, "completed"), sep =" ")
}

measures %>%
  summarise(mean_RMSE = mean(RMSE), sd_RMSE = sd(RMSE), mean_MAPE = mean(MAPE), sd_MAPE = sd(MAPE))

# Mean/SD of K-Fold Accuracy Measures of Linear Regression
#    mean_RMSE     sd_RMSE          mean_MAPE       sd_MAPE
#         110677           6257                   19.7                1.04  





