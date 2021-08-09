# balanced accuracy: 0.7005477
#install.packages("caTools")
#install.packages("textrecipes")
#install.packages("snow")
require(caTools)
#library(snow)
library(dplyr)
library(summarytools)
library(lubridate)
library(textrecipes)
# Load for model
library(tidymodels)
# Load the library tidyverse
library(tidyverse)
# Load for variable importance
library(vip)

#install.packages("caTools")
library(yardstick)
set.seed(2021)

# Load the data from the file companies.csv
company_df = read_csv("C:/Users/saury/Desktop/BA Analytics Cup/companies.csv")
payments_df = read_csv("C:/Users/saury/Desktop/BA Analytics Cup/payments.csv")
physicians_df = read.csv("C:/Users/saury/Desktop/BA Analytics Cup/physicians.csv")

#Remove columns from df
payments_df <- subset( payments_df, select = -c(City_of_Travel,State_of_Travel,Country_of_Travel,Third_Party_Covered,Contextual_Information
                                                        ,Product_Code_2,Product_Code_3,Product_Type_2,Product_Type_3,Product_Name_2,Product_Name_3,
                                                        Product_Category_1,Product_Category_2,Product_Category_3) )
physicians_df <- subset( physicians_df, select = -c(First_Name, Province,Name_Suffix,License_State_3,License_State_4,License_State_5,Country,Middle_Name,
                                                    Last_Name,City,Zipcode))

#Added Ownership column in Physician df
Phywith_owner<- payments_df %>%
  select(Physician_ID,Ownership_Indicator) %>%
  filter(Ownership_Indicator=="Yes") %>% 
  group_by(Physician_ID) %>%
  distinct(Physician_ID)

Phywith_owner$Ownership_Indicator="Yes"

physicians_df<-merge(physicians_df, Phywith_owner, by.x = "id",  by.y = "Physician_ID", all.x=TRUE)
physicians_df <- physicians_df %>%
  mutate(Ownership_Indicator = if_else(is.na(Ownership_Indicator), "No", Ownership_Indicator))


#Features:
# Total Amount
physicians_df_totalDollar <- payments_df %>% group_by(Physician_ID) %>% summarise_at(vars(Total_Amount_of_Payment_USDollars), sum)
physicians_df <- merge(physicians_df, physicians_df_totalDollar, by.x = "id",  by.y = "Physician_ID")

#Charity
payments_df<-payments_df %>%
  mutate(
    Charity=factor(Charity, labels = c(0,1))
  )

charity_0<-payments_df %>% 
  select(Physician_ID,Charity) %>%
  filter(Charity=="0") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(No_Charity=count)

physicians_df <- merge(physicians_df, charity_0, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
remove(charity_0)
remove(charity_1)

charity_1<-payments_df %>% 
  select(Physician_ID,Charity) %>%
  filter(Charity=="1") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(Yes_Charity=count)

physicians_df <- merge(physicians_df, charity_1, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

head(physicians_df)

#Date 
payments_df$temp <- rep(1,nrow(payments_df))
head(payments_df$temp)
Year <-format(as.Date(payments_df$Date, format="%m/%d/%Y"),"%Y")
nr_py <- aggregate(temp ~ Physician_ID + Year, data=payments_df, FUN=sum)

rest<-nr_py %>% 
  select(Physician_ID,Year,temp) %>%
  filter(Year=="2013") %>%
  group_by(Physician_ID) %>%
  select(Physician_ID,temp) %>%
  rename(Year_2013=temp)

physicians_df <- merge(physicians_df, rest, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

rest<-nr_py %>% 
  select(Physician_ID,Year,temp) %>%
  filter(Year=="2014") %>%
  group_by(Physician_ID) %>%
  select(Physician_ID,temp) %>%
  rename(Year_2014=temp)

physicians_df <- merge(physicians_df, rest, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

rest<-nr_py %>% 
  select(Physician_ID,Year,temp) %>%
  filter(Year=="2015") %>%
  group_by(Physician_ID) %>%
  select(Physician_ID,temp) %>%
  rename(Year_2015=temp)

physicians_df <- merge(physicians_df, rest, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

rest<-nr_py %>% 
  select(Physician_ID,Year,temp) %>%
  filter(Year=="2016") %>%
  group_by(Physician_ID) %>%
  select(Physician_ID,temp) %>%
  rename(Year_2016=temp)

physicians_df <- merge(physicians_df, rest, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

rest<-nr_py %>% 
  select(Physician_ID,Year,temp) %>%
  filter(Year=="2017") %>%
  group_by(Physician_ID) %>%
  select(Physician_ID,temp) %>%
  rename(Year_2017=temp)

physicians_df <- merge(physicians_df, rest, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

rest<-nr_py %>% 
  select(Physician_ID,Year,temp) %>%
  filter(Year=="2018") %>%
  group_by(Physician_ID) %>%
  select(Physician_ID,temp) %>%
  rename(Year_2018=temp)

physicians_df <- merge(physicians_df, rest, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

rest<-nr_py %>% 
  select(Physician_ID,Year,temp) %>%
  filter(Year=="2019") %>%
  group_by(Physician_ID) %>%
  select(Physician_ID,temp) %>%
  rename(Year_2019=temp)

physicians_df <- merge(physicians_df, rest, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

head(physicians_df)

#Primary Speciality
physicians_df$Primary_Specialty[is.na(physicians_df$Primary_Specialty)] <- "Not Given"
physicians_df$Primary_Specialty <- do.call(rbind, strsplit(as.character(physicians_df$Primary_Specialty), "\\|"))[,1]
physicians_df$Primary_Specialty <- as.factor(physicians_df$Primary_Specialty)

#State
physicians_df$State[is.na(physicians_df$State)] <- "Not Given"
physicians_df$State <- as.factor(physicians_df$State)


#No of Payments
physicians_df_totalNrPayments <- payments_df %>% group_by(Physician_ID) %>% summarise_at(vars(Number_of_Payments), sum)
physicians_df <- merge(physicians_df, physicians_df_totalNrPayments, by.x = "id",  by.y = "Physician_ID")

##product code
#data.frame(table((payments_df %>%filter(Ownership_Indicator == "Yes"))$Product_Code_1))
#data.frame(table(payments_df$Product_Code_1))

product_code_6446<-payments_df %>%
  select(Physician_ID,Product_Code_1) %>%
  filter(Product_Code_1=="64406-008-01" | Product_Code_1 == "64406-006-02") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(product_code_6446=count)
physicians_df <- merge(physicians_df, product_code_6446, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$product_code_6446[is.na(physicians_df$product_code_6446)] <- 0

product_code_lwyes <-payments_df %>%
  select(Physician_ID,Product_Code_1) %>%
  filter(Product_Code_1=="00178-0900-0" | Product_Code_1 == "0074-0033-01" |
         Product_Code_1=="45043-001-02" | Product_Code_1 == "5167252817" |
         Product_Code_1=="59627-333-04" | Product_Code_1 == "52579-1695-1" |
         Product_Code_1=="64406-802-01" | Product_Code_1 == "64406-011-01" | Product_Code_1 == "70301-1001-1") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(product_code_lwyes=count)
physicians_df <- merge(physicians_df, product_code_lwyes, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$product_code_lwyes[is.na(physicians_df$product_code_lwyes)] <- 0

#Third_Party_Recipient
Third_Party_Payment_No<-payments_df %>% 
  select(Physician_ID,Third_Party_Recipient) %>%
  filter(Third_Party_Recipient=="No Third Party Payment") %>%
  group_by(Physician_ID) %>%
  summarise(No_Third_PPayment = n()) 
physicians_df <- merge(physicians_df, Third_Party_Payment_No, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

Third_Party_Payment_Individual<-payments_df %>% 
  select(Physician_ID,Third_Party_Recipient) %>%
  filter(Third_Party_Recipient=="Individual") %>%
  group_by(Physician_ID) %>%
  summarise(Individual_Payment = n()) 
physicians_df <- merge(physicians_df, Third_Party_Payment_Individual, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

Third_Party_Payment_Entity<-payments_df %>% 
  select(Physician_ID,Third_Party_Recipient) %>%
  filter(Third_Party_Recipient=="Entity") %>%
  group_by(Physician_ID) %>%
  summarise(Entity_Payment = n()) 
physicians_df <- merge(physicians_df, Third_Party_Payment_Entity, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

#Total no of Transactions
nr_py <- aggregate(temp ~ Physician_ID, data=payments_df, FUN=sum) %>% rename(Total_No_Transactions=temp)
physicians_df <- merge(physicians_df, nr_py, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

#Remove unnecessary variables from the memory
remove(charity_0)
remove(charity_1)
remove(nr_py)
remove(rest)
remove(Third_Party_Payment_No)
remove(Third_Party_Payment_Individual)
remove(Third_Party_Payment_Entity)
remove(physicians_df_totalNrPayments)
remove(physicians_df_totalNrPayments)
remove(Phywith_owner)

#Related Product Indicator
#Recode to Yes/No/Combined
payments_df$Related_Product_Indicator[payments_df$Related_Product_Indicator == "None"] <- "No"
payments_df$Related_Product_Indicator[payments_df$Related_Product_Indicator == "Non-Covered"] <- "Yes"
payments_df$Related_Product_Indicator[payments_df$Related_Product_Indicator == "Covered"] <- "Yes"
payments_df$Related_Product_Indicator[payments_df$Related_Product_Indicator == "Combination"] <- "Yes"

#factorize to 1 = Combination(changed), 2 = No, 3 = Yes
payments_df$Related_Product_Indicator <- factor(payments_df$Related_Product_Indicator)

payments_df$Related_Product_Indicator <- as.numeric(payments_df$Related_Product_Indicator)

#payments_df$Related_Product_Indicator <-recode(payments_df$Related_Product_Indicator, 'No'=0, 'Yes'=1, 'Combined'=2(changed))
payments_df$Related_Product_Indicator <- as.numeric(payments_df$Related_Product_Indicator)

#add No column to physician dataframe
payments_relatedPI_No<-payments_df %>%
  select(Physician_ID,Related_Product_Indicator) %>%
  filter(Related_Product_Indicator=="1") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(RelatedPI_No=count)

physicians_df <- merge(physicians_df, payments_relatedPI_No, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$RelatedPI_No <- physicians_df$RelatedPI_No %>% replace(is.na(.), 0)

#add Yes column to physician dataframe
payments_relatedPI_Yes<-payments_df %>%
  select(Physician_ID,Related_Product_Indicator) %>%
  filter(Related_Product_Indicator=="2") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(RelatedPI_Yes=count)

physicians_df <- merge(physicians_df, payments_relatedPI_Yes, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$RelatedPI_Yes <- physicians_df$RelatedPI_Yes %>% replace(is.na(.), 0)

#cash
payments_form_cash<-payments_df %>%
  select(Physician_ID,Form_of_Payment_or_Transfer_of_Value) %>%
  filter(Form_of_Payment_or_Transfer_of_Value=="Cash or cash equivalent") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(payments_form_cash=count)
physicians_df <- merge(physicians_df, payments_form_cash, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$payments_form_cash[is.na(physicians_df$payments_form_cash)] <- 0

#In-kind items
payments_form_inkindItems<-payments_df %>%
  select(Physician_ID,Form_of_Payment_or_Transfer_of_Value) %>%
  filter(Form_of_Payment_or_Transfer_of_Value=="In-kind items and services") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(payments_form_inkindItems=count)
physicians_df <- merge(physicians_df, payments_form_inkindItems, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$payments_form_inkindItems[is.na(physicians_df$payments_form_inkindItems)] <- 0

#other payment type
payments_form_other<-payments_df %>%
  select(Physician_ID,Form_of_Payment_or_Transfer_of_Value) %>%
  filter(Form_of_Payment_or_Transfer_of_Value!="In-kind items and services" & Form_of_Payment_or_Transfer_of_Value!= "Cash or cash equivalent") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(payments_form_other=count)
physicians_df <- merge(physicians_df, payments_form_other, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$payments_form_other[is.na(physicians_df$payments_form_other)] <- 0

#Feature Company 114
company_id_114<-payments_df %>%
  select(Physician_ID,Company_ID) %>%
  filter(Company_ID=="114") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(company_id_114=count)
physicians_df <- merge(physicians_df, company_id_114, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$company_id_114[is.na(physicians_df$company_id_114)] <- 0

# Feature Company 772
# company_id_772<-payments_df %>%
#   select(Physician_ID,Company_ID) %>%
#   filter(Company_ID=="772") %>%
#   group_by(Physician_ID) %>%
#   summarise(count = n()) %>%
#   rename(company_id_772=count)
# physicians_df <- merge(physicians_df, company_id_772, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
# physicians_df$company_id_772[is.na(physicians_df$company_id_772)] <- 0

#other id
company_id_other<-payments_df %>%
  select(Physician_ID,Company_ID) %>%
  filter(Company_ID!="114") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(company_id_other=count)
physicians_df <- merge(physicians_df, company_id_other, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$company_id_other[is.na(physicians_df$company_id_other2)] <- 0

#Merge Company_df with Payments_df
payments_df <- merge(payments_df, company_df, by.x = "Company_ID",  by.y = "Company_ID", all.x = TRUE)

#company state Massachussets
company_state_MA<-payments_df %>%
  select(Physician_ID,State) %>%
  filter(State=="MA") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(company_state_MA=count)
physicians_df <- merge(physicians_df, company_state_MA, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$company_state_MA[is.na(physicians_df$company_state_MA)] <- 0

physicians_df$company_state_MA[physicians_df$company_state_MA > 0] <- 1
#company state Ohio
# company_state_OH<-payments_df %>%
#   select(Physician_ID,State) %>%
#   filter(State=="OH") %>%
#   group_by(Physician_ID) %>%
#   summarise(count = n()) %>%
#   rename(company_state_OH=count)
# physicians_df <- merge(physicians_df, company_state_OH, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
# physicians_df$company_state_OH[is.na(physicians_df$company_state_OH)] <- 0

#company state other
company_state_Other<-payments_df %>%
  select(Physician_ID,State) %>%
  filter(State != "MA") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(company_state_Other=count)
physicians_df <- merge(physicians_df, company_state_Other, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$company_state_Other[is.na(physicians_df$company_state_Other)] <- 0

#PCA Analysis
physicians_df$Year_2013[is.na(physicians_df$Year_2013)] <- 0
which(is.na(physicians_df$Year_2013))

physicians_df.pca <- prcomp(physicians_df[,c("Year_2013","Year_2014","Year_2015" ,"Year_2016" ,"Year_2017" ,"Year_2018" ,"Year_2019")],center = TRUE, scale. = TRUE)
# physicians_df.pca$rotation[1,1]

# summary(physicians_df.pca)
# biplot(physicians_df.pca, scale = 0)

physicians_df.pca$x
pca1 <- physicians_df.pca$x[,1]
trial.df <- as.data.frame(pca1)

pcatest <- as.data.frame(physicians_df.pca$x)

year_df <- as.data.frame(physicians_df.pca$x)
physicians_df$PC1_Year <- year_df$PC1
physicians_df$PC2_Year <- year_df$PC2


# Payment nature ownership interest
ownership_interest<-payments_df %>%
  select(Physician_ID,Nature_of_Payment_or_Transfer_of_Value) %>%
  filter(Nature_of_Payment_or_Transfer_of_Value=="Current or prospective ownership or investment interest") %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(ownership_interest=count)
physicians_df <- merge(physicians_df, ownership_interest, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$ownership_interest[is.na(physicians_df$ownership_interest)] <- 0

# Use for numeric values:
#delete to count
#physicians_df$ownership_interest[physicians_df$ownership_interest > 0] <- 1

#  NOownership_interest<-payments_df %>%
#   select(Physician_ID,Nature_of_Payment_or_Transfer_of_Value) %>%
#   filter(Nature_of_Payment_or_Transfer_of_Value!="Current or prospective ownership or investment interest") %>%
#   group_by(Physician_ID) %>%
#   summarise(count = n()) %>%
#   rename(NOownership_interest=count)
# physicians_df <- merge(physicians_df, NOownership_interest, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
# physicians_df$NOownership_interest[is.na(physicians_df$NOownership_interest)] <- 0

#faculty talk
faculty_talk<-payments_df %>%
  select(Physician_ID,Nature_of_Payment_or_Transfer_of_Value) %>%
  filter(Nature_of_Payment_or_Transfer_of_Value == "Compensation for serving as faculty or as a speaker for a non-accredited and noncertified continuing education program" | 
           Nature_of_Payment_or_Transfer_of_Value == "Compensation for serving as faculty or as a speaker for an accredited or certified continuing education program" |
           Nature_of_Payment_or_Transfer_of_Value == "Grant" ) %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(faculty_talk=count)
physicians_df <- merge(physicians_df, faculty_talk, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$faculty_talk[is.na(physicians_df$faculty_talk)] <- 0

# other nature of payments
Other_Nature_of_Payment <-payments_df %>%
  select(Physician_ID,Nature_of_Payment_or_Transfer_of_Value) %>%
  filter(Nature_of_Payment_or_Transfer_of_Value != "Compensation for serving as faculty or as a speaker for a non-accredited and noncertified continuing education program" &&
           Nature_of_Payment_or_Transfer_of_Value != "Compensation for serving as faculty or as a speaker for an accredited or certified continuing education program" &&
           Nature_of_Payment_or_Transfer_of_Value != "Grant" &&
         Nature_of_Payment_or_Transfer_of_Value !="Current or prospective ownership or investment interest" ) %>%
  group_by(Physician_ID) %>%
  summarise(count = n()) %>%
  rename(Other_Nature_of_Payment=count)
physicians_df <- merge(physicians_df, Other_Nature_of_Payment, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)
physicians_df$faculty_talk[is.na(physicians_df$Other_Nature_of_Payment)] <- 0


#Total Amount >100
amt<-payments_df %>%
  mutate(
    amount_range = case_when(
      Total_Amount_of_Payment_USDollars<=100 ~ "A",
      TRUE ~ "B"
    )
  )%>%
  select(Physician_ID,amount_range) %>%
  filter(amount_range=="A") %>%
  group_by(Physician_ID) %>%
  summarise(amt_below_100 = n()) 

physicians_df <- merge(physicians_df, amt, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

amt_above<-payments_df %>%
  mutate(
    amount_range = case_when(
      Total_Amount_of_Payment_USDollars<=100 ~ "A",
      TRUE ~ "B"
    )
  )%>%
  select(Physician_ID,amount_range) %>%
  filter(amount_range=="B") %>%
  group_by(Physician_ID) %>%
  summarise(amt_above_100 = n()) 

physicians_df <- merge(physicians_df, amt_above, by.x = "id",  by.y = "Physician_ID", all.x = TRUE)

#Remove unnecessary variables from the memory
remove(charity_0)
remove(charity_1)
remove(nr_py)
remove(rest)
remove(Third_Party_Payment_No)
remove(Third_Party_Payment_Individual)
remove(Third_Party_Payment_Entity)
remove(physicians_df_totalNrPayments)
remove(Phywith_owner)
remove(payments_relatedPI_Yes)
remove(payments_relatedPI_No)
remove(payments_relatedPI_Combination)
remove(physicians_df_totalDollar)
remove(payments_form_cash)
remove(payments_form_inkindItems)
remove(payments_form_other)
remove(company_id_other)
remove(company_id_772)
remove(company_id_114)

physicians_df$Primary_Specialty <- as.factor(physicians_df$Primary_Specialty)
physicians_df$License_State_1 <- as.factor(physicians_df$License_State_1)
physicians_df$License_State_2 <- as.factor(physicians_df$License_State_2)

colnames(physicians_df)
physicians_df <- physicians_df %>% mutate_if(is.integer, ~replace(., is.na(.), 0))
#physicians_df <- addNA(physicians_df$Primary_Specialty)

#Separate test and train set
physicians_train <- physicians_df %>%
  select(everything())%>%
  filter(set=="train")

physicians_test <- physicians_df %>%
  select(everything())%>%
  filter(set=="test")

physicians_train$Ownership_Indicator <- as.factor(physicians_train$Ownership_Indicator)
physicians_test$Ownership_Indicator <- as.factor(physicians_test$Ownership_Indicator)

physicians_test <- subset( physicians_test, select = -c(set))
physicians_train <- subset( physicians_train, select = -c(set))
physicians_train <- subset( physicians_train, select = -c(X,License_State_2,Primary_Specialty.I,Primary_Specialty.II,
                                                          Primary_Specialty.II.1))
physicians_test <- subset( physicians_test, select = -c(X,License_State_2,Primary_Specialty.I,Primary_Specialty.II,
                                                          Primary_Specialty.II.1))

splits <-physicians_train %>% initial_split(prop = 0.80)
phy_train <- training(splits)
phy_tesy <- testing(splits)

splits

library(ROSE)
over<-ovun.sample(Ownership_Indicator ~ . ,data=phy_train, method="over", N=5000)$data
table(over$Ownership_Indicator)


#Version 2 (Tidyverse Models, Recipes, etc.)

#Prepare recipe for preprocessing 
rec <- recipe(
  Ownership_Indicator ~  Total_Amount_of_Payment_USDollars  + Total_No_Transactions
  + Number_of_Payments + No_Third_PPayment + Individual_Payment  
  + payments_form_cash + payments_form_inkindItems + payments_form_other
  + company_id_114 + company_id_other + product_code_6446
  + company_state_Other  + PC1_Year 
  #good effect
  + ownership_interest  + Other_Nature_of_Payment
  # think twice
  + License_State_1 + No_Charity  + State + product_code_lwyes + RelatedPI_No + RelatedPI_Yes 
  + amt_above_100 + amt_below_100
  , data = physicians_train) %>% 
  
  #update_role(id, new_role = "ID") %>% 
# step_mutate_at(where(is.Date), fn=decimal_date) %>%    
  step_mutate_at (where(is.integer),
                 fn= ~replace_na(.,0)) %>%
  # impute all other numeric columns with their mean
  step_meanimpute(all_numeric()) %>% 
  
  # convert all strings to factors
  step_string2factor(all_nominal(), -all_outcomes()) %>% 
  # remove constant columns
  step_zv(all_predictors())

rec
#Preprocessed data
physicians_train_preprocessed <- rec %>% prep(over) %>% juice()

rec %>% prep() %>% bake(phy_tesy)

folds <- vfold_cv(physicians_train_preprocessed)

#Random forest Model Start
rf_model <-
  rand_forest( mtry = tune(),
               trees = 1000,
               min_n = tune()) %>%
  set_args(mtry = tune()) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

rf_model

rf_workflow <-
  workflow() %>%
  add_recipe(rec) %>%
  add_model(rf_model)

rf_workflow

#tuning layer (we specify the range of mtry values we want to try)
rf_grid <-  grid_regular(
  mtry(range = c(5,8)),
  min_n(range = c(5,10)),
  levels = 5
)

# extract results
rf_tune_results <- rf_workflow %>%
  tune_grid(resamples = folds, #CV object
            grid = rf_grid, # grid of values to try
            metrics = metric_set(yardstick::bal_accuracy) # metrics we care about
  )


rf_tune_results %>%
  collect_metrics()

allMetrics <- rf_tune_results %>%
  collect_metrics()

param_final <- rf_tune_results %>%
  select_best(metric = "bal_accuracy")
param_final

rf_workflow <- rf_workflow %>%
  finalize_workflow(param_final)

rf_fit <- rf_workflow %>%
  # fit on the training set and evaluate on test set
  last_fit(splits)

rf_fit

rf_test_performance <- rf_fit %>% collect_metrics()
rf_test_performance

rf_test_predictions <- rf_fit %>% collect_predictions()
rf_test_predictions

rf_test_predictions %>%
  conf_mat(truth = Ownership_Indicator, estimate = .pred_class)

rf_test_predictions <- rf_fit %>% pull(.predictions)
rf_test_predictions

rf_final_model <- fit(rf_workflow, physicians_train_preprocessed)
rf_final_model

#predict our new test data
rf_predicted <- predict(rf_final_model, new_data = physicians_test)

#variable importance
rf_ranger_obj <- pull_workflow_fit(rf_final_model)$fit
rf_ranger_obj
rf_ranger_obj$variable.importance

rf_test_results <- physicians_test %>% select(id)

rf_test_results$Ownership_pred <- rf_predicted
rf_test_results$Ownership_pred <- ifelse(rf_test_results$Ownership_pred=="Yes", 1, 0)

names(rf_test_results)[1] <- "id"
names(rf_test_results)[2] <- "prediction"
write.csv(rf_test_results,"rf_test_results_v8.csv", row.names= FALSE)

#Random forest Model End
