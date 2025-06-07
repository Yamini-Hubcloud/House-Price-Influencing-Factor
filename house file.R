install.packages("readxl") 
library(readxl) 
AH <- read_excel("C:/Users/Admin/Desktop/MSc BA/7100 SB/austinHousing.xlsx") 
AH <- AH 
View(AH) 
# Summary statistics 
summary(AH) 
# Check structure of data 
str(AH) 
# Check for missing values 
colSums(is.na(AH)) 
missing_values <- colSums(is.na(AH)) 
print(missing_values) 
# IMPUTE OR REMOVE MISSING VALUES 
install.packages("dplyr") 
library("dplyr") 
AH <- AH %>% 
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) 
AH$sale_price[is.na(AH$sale_price)] <-mean(AH$sale_price, na.rm = TRUE) 
AH<- AH %>% 
  mutate(has_view= ifelse(has_view == "n", "no", has_view)) 
AH<- AH %>% 
  mutate(has_view= ifelse(has_view == "y", "yes", has_view)) 
#  outliers 
boxplot(AH$sale_price, plot = FALSE)$out 
boxplot(AH$total_schools, plot = FALSE)$out 
15 
boxplot(AH$total_features, plot = FALSE)$out 
boxplot(AH$stories, plot = FALSE)$out 
boxplot(AH$bedrooms, plot = FALSE)$out 
boxplot(AH$year_built, plot = FALSE)$out 
16 
boxplot(AH$latest_year_sale, plot = FALSE)$out 
boxplot(AH$house type, plot = FALSE)$out 
boxplot(AH$city, plot = FALSE)$out 
boxplot(AH$property_tax_rate, plot = FALSE)$out 
# Define a threshold to determine extreme values 
Q1 <- quantile(AH$sale_price, 0.25)   
Q3 <- quantile(AH$sale_price, 0.75)   
IQR <- Q3 - Q1                   
# Define extreme threshold  
upper_limit <- Q3 + 1.5 * IQR 
lower_limit <- Q1 - 1.5 * IQR 
summary(AH$sale_price) 
install.packages("ggplot2") 
library(ggplot2) 
17 
#1.HISTOGRAM: PROPERTY TAX VS.SALE PRICE 
hist(AH$property_tax_rate,AH$sale_price, breaks = 10, col = "lightpink",freq = TRUE, 
     main = "CORRELATION BETWEEN PROPERT TAX RATE AND SALE PRICE", 
     xlab = "propertytax", ylab = "saleprice") 
cor.test(x = AH$property_tax_rate, AH$sale_price) 
# 2. VIOLIN PLOT : SALE PRICE BY CITY 
ggplot(AH, aes(x = city, y =sale_price)) + 
  geom_violin(trim = FALSE, fill = "lightblue", color = "darkblue") + 
  geom_boxplot(width = 0.1, outlier.shape = NA) +  
  labs(title = "Price Distribution by Region", x = "city", y = "Price") + 
  theme_minimal() 
CON_TB<-table(AH$sale_price,AH$city) 
t.test(CON_TB) 
# 3. BAR PLOT: CHANGE IN PRICE OVER TIME 
boxplot(AH$price_changes)$out 
ggplot(AH, aes(x = latest_sale_year, y = sale_price, fill = "red")) + 
  geom_bar(stat = "identity") + 
  labs(title = "Price Change by Year", x = "Year", y = "Price") 
cor.test(x = AH$latest_sale_year, AH$sale_price) 
# PRICE PER SQUARE FOOT 
AH$Price_PerSqft <- AH$sale_price / AH$lot_area 
# TOTAL SCHOOLS NEARBY 
AH$Total_Schools <- AH$primary_schools + AH$elementary_schools +   AH$middle_schools+ 
  AH$high_schools 
# TOTAL FEATURES  
AH$Total_features <- rowSums(AH == "yes", na.rm = TRUE) 
# 4. CREATE A BAR GRAPH FOR THE FEATURE COUNTS BY SALE PRICE 
feature_count <- table(AH$Total_features,AH$house_type) 
barplot( 
  feature_count, 
  beside = TRUE,   
  col = c("skyblue", "lightgreen", "lightcoral","red","blue","yellow"),  
  main = "Feature Counts by House Type", 
  xlab = "House Type", 
  ylab = "Feature Count", 
  legend = rownames(feature_count),   
  args.legend = list(title = "Features", x = "topright") 
) 
CONT<-table(AH$Total_features,AH$house_type) 
t.test(CONT) 
18 
# 5.SCATTER PLOT OF PRICE PER SQFT VS TOTAL SCHOOLS NEARBY 
library(ggplot2) 
boxplot(AH$Total_Schools)$out 
boxplot(AH$Price_PerSqft)$out 
ggplot(AH, aes(x = Total_Schools, y = Price_PerSqft)) + 
  geom_point(color = "green", alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE, color = "lightpink") + 
  labs(title = "Price per Square Foot vs. Total Schools Nearby", 
       x = "Total Schools Nearby", 
       y = "Price per Square Foot") 
cor.test(x = AH$Total_Schools,AH$Price_PerSqft) 
#6. SCATTER PLOT: STORIES VS SELLING PRICE 
ggplot(AH, aes(x = stories, y = sale_price)) + 
  geom_point() + 
  labs(title = "scatter plot ", x = "stories count", y = "Sale Price") 
CNTG<-table(AH$stories,AH$sale_price) 
t.test(CNTG) 
#7.VIOLIN PLOT: BEDROOM VS A SALE PRICE 
ggplot(AH, aes(x = bedrooms, y =sale_price)) + 
  geom_violin(trim = FALSE, fill = "lightblue", color = "darkblue") + 
  geom_boxplot(width = 0.1, outlier.shape = NA) +  
  labs(title = "Price Distribution", x = "beedrooms", y = "sale Price") + 
  theme_minimal() 
cor.test(x = AH$bedrooms,AH$sale_price) 
# 8.BAR GRAPH: YEAR BUILT VS SALE PRICE 
ggplot(AH, aes(x = year_built, y = sale_price,fill = "cyan")) + 
  geom_bar(stat = "identity") + 
  labs(title = "Bar Plot", x = "year built", y = "Price") 
table(AH$year_built,AH$sale_price) 
t.test(CNTGs) 
# 9  CREATE A BAR GRAPH FOR THE FEATURE COUNTS SALE PRICE 
Feature_count <- table(AH$sale_price,AH$Total_features 
                       barplot( 
                         feature_count, 
                         beside = TRUE,   
                         main = "Feature Counts", 
                         xlab = "Feature Count", 
                         ylab = "price", 
                       ) 
                       cor.test(x = AH$Total_features, AH$sale_price) 
                       install.packages("caret") 
                       library(caret) 
                       19 
                       # SPLIT DATA INTO TRAINING AND TEST SETS 
                       set.seed(40459080) 
                       index <- createDataPartition(AH$sale_price,times = 1, p = 0.8, list = FALSE) 
                       train <- AH[index, ] 
                       test<- AH[-index, ] 
                       # 1 Train a linear regression model 
                       formula <- sale_price ~ stories+bedrooms 
                       model1<- lm(formula = formula,data = train) 
                       summary(model) 
                       predictions <-predict(model1,test) 
                       postResample(predictions,test$sale_price) 
                       vif(model) 
                       install.packages("lmtest") 
                       library(lmtest) 
                       dwtest(model1)       
                       sum(cooks.distance(model1)>1)    
                       max(cooks.distance(model1))  
                       # 2 Train a linear regression model 
                       formula1 <- sale_price ~ stories+bedrooms+property_tax_rate 
                       model2<- lm(formula = formula1,data = train) 
                       summary(model2) 
                       predictions1 <-predict(model2,test) 
                       postResample(predictions1,test$sale_price) 
                       vif(model2) 
                       install.packages("lmtest") 
                       library(lmtest) 
                       dwtest(model2)       
                       sum(cooks.distance(model2)>1)    
                       max(cooks.distance(model2))  
                       #  3 Train a linear regression model 
                       formula2 <- sale_price ~ stories+bedrooms+property_tax_rate+year_built 
                       20 
                       model3<- lm(formula = formula2,data = train) 
                       summary(model3) 
                       predictions2 <-predict(model3,test) 
                       postResample(predictions2,test$sale_price) 
                       vif(model3) 
                       install.packages("lmtest") 
                       library(lmtest) 
                       dwtest(model3)       
                       sum(cooks.distance(model3)>1)    
                       max(cooks.distance(model3)) 
                       # 4  Train a linear regression model 
                       formula3 <- sale_price ~ stories+bedrooms+property_tax_rate+year_built 
                       model4<- lm(formula = formula3,data = train) 
                       summary(model4) 
                       predictions3 <-predict(model4,test) 
                       postResample(predictions3,test$sale_price) 
                       vif(model3) 
                       install.packages("lmtest") 
                       library(lmtest) 
                       dwtest(model4)       
                       sum(cooks.distance(model4)>1)    
                       max(cooks.distance(model4)) 
                       # 5 Train a linear regression model 
                       formula4 <- sale_price ~ 
                         stories+bedrooms+property_tax_rate+bathrooms+waterfront_features+parking_spaces 
                       model5<- lm(formula = formula4,data = train) 
                       summary(model5) 
                       predictions4 <-predict(model5,test) 
                       postResample(predictions4,test$sale_price) 
                       vif(model5) 
                       install.packages("lmtest") 
                       21 
                       library(lmtest) 
                       dwtest(model5)       
                       sum(cooks.distance(model5)>1)    
                       max(cooks.distance(model5)) 
                       # 6 Train a linear regression model 
                       formula6<- sale_price ~ 
                         stories+bedrooms+property_tax_rate+bathrooms+waterfront_features+parking_spaces+ 
                         security_features+window_features+has_garage+has_association+has_view+lot_area+living_area+a
                       ppliances+year_built 
                       model7<- lm(formula = formula6,data = train) 
                       summary(model7) 
                       predictions6 <-predict(model7,test) 
                       postResample(predictions6,test$sale_price) 
                       vif(model7) 
                       install.packages("lmtest") 
                       library(lmtest) 
                       dwtest(model7)       
                       sum(cooks.distance(model7)>1)    
                       max(cooks.distance(model7)) 
                       THE BEST MODEL FIT FROM ALL THE ACCURACY 
                       # 7 Train a linear regression model 
                       formula<- sale_price ~ 
                         stories+bedrooms+property_tax_rate+bathrooms+waterfront_features+parking_spaces+ 
                         security_features+school_dist+has_garage+has_association+has_view+lot_area+living_area+Total_
                       Schools+year_built 
                       model<- lm(formula = formula,data = train) 
                       summary(model) 
                       predictions <-predict(model,test) 
                       22 
                       postResample(predictions,test$sale_price) 
                       vif(model) 
                       install.packages("lmtest") 
                       library(lmtest) 
                       dwtest(model)       
                       summary(cooks.distance(model))    
                       max(cooks.distance(model)) 
                       plot(model) 
                       