library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(jalcal)
library(tsibble)
library(ggpubr)
library(feasts)
library(scales)
library(lubridate)
library(gridExtra)

sales_1395<-read_xlsx("Sales_Data/Sales_1395.xls")
sales_1396<-read_xlsx("Sales_Data/Sales_1396.xls")
sales_1397<-read_xlsx("Sales_Data/Sales_1397.xls")
sales_1398<-read_xlsx("Sales_Data/Sales_1398.xls")
sales_1399<-read_xlsx("Sales_Data/Sales_1399.xls")
sales_1400<-read_xlsx("Sales_Data/Sales_1400.xls")

returns_1395<-read_xlsx("Sales_Return/Returns_1395.xls")
returns_1396<-read_xlsx("Sales_Return/Returns_1396.xls")
returns_1397<-read_xlsx("Sales_Return/Returns_1397.xls")
returns_1398<-read_xlsx("Sales_Return/Returns_1398.xls")
returns_1399<-read_xlsx("Sales_Return/Returns_1399.xls")
returns_1400<-read_xlsx("Sales_Return/Returns_1400.xls")
IRNCPI<-read_csv("IRNCPI.csv")


#### Creating a function that selects, renames, and drops na's from the sales datasets.
sales_modify<- function (dataset) {
  dataset<-dataset %>%
    rename(Date = "تاريخ", 
           Product_Name= "نام كالا", 
           Unit_Price= "قيمت واحد",
           Total_Sales = "مبلغ كل",
           Discount = "تخفيف",
           Customer_Name= "نام مشترى",
           Commissions = "سهم واسطه") %>%
    transmute(Date = as.Date(Date, "%Y/%m/%d"), Product_Name, Unit_Price, 
              Total_Sales, Discount, Customer_Name, Commissions) %>%
    drop_na()
  return(dataset)
}

#### Using above function on the six datasets.
sales_1395 <- sales_modify(sales_1395)
sales_1396 <- sales_modify(sales_1396)
sales_1397 <- sales_modify(sales_1397)
sales_1398 <- sales_modify(sales_1398)
sales_1399 <- sales_modify(sales_1399)
sales_1400 <- sales_modify(sales_1400)
#### Creating a function that selects, renames, and drops na's from the returns datasets.
returns_modify<- function (dataset) {
  dataset<-dataset %>%
    rename(Date = "تاريخ",
           Total_Value_Returned= "مبلغ كل",
           Commissions_Returned = "سهم واسطه") %>%
    transmute(Date = as.Date(Date, "%Y/%m/%d"), Total_Value_Returned, 
              Commissions_Returned) %>%
    drop_na()
  return(dataset)
}


returns_1395 <- returns_modify(returns_1395)
returns_1396 <- returns_modify(returns_1396)
returns_1397 <- returns_modify(returns_1397)
returns_1398 <- returns_modify(returns_1398)
returns_1399 <- returns_modify(returns_1399)
returns_1400 <- returns_modify(returns_1400)


#### A function that converts Jalali dates to Gregorian dates for each dataset.
dates_modify<- function (dataset) {
  # Seperating Year, Month, Day from Date column.
  Year<-format(dataset$Date, format = "%Y")
  Month <-format(dataset$Date, format = "%m")
  Day <-format(dataset$Date, format = "%d")
  # Initializing an empty date tibble.
  date_tibble <- tibble(
    Date = as.Date(character()))
  # Converting Jalali dates to Gregorian dates.
  i <- 1
  while (i<=nrow(dataset)){
    date_tibble<- add_row(date_tibble, Date = jal2greg(Year[i], Month[i], Day[i]))
    i=i+1
  }
  # Adding Gregorian dates to the dataset and removing Jalili Date column.
  dataset<-dplyr::select(dataset,-Date)
  dataset <- tibble(date_tibble, dataset) 
  #dataset <- select(dataset,-Date)
  return(dataset)
}

sales_2016 <- dates_modify(sales_1395)
sales_2017 <- dates_modify(sales_1396)
sales_2018 <- dates_modify(sales_1397)
sales_2019 <- dates_modify(sales_1398)
sales_2020 <- dates_modify(sales_1399)
sales_2021 <- dates_modify(sales_1400)

returns_2016 <- dates_modify(returns_1395)
returns_2017 <- dates_modify(returns_1396)
returns_2018 <- dates_modify(returns_1397)
returns_2019 <- dates_modify(returns_1398)
returns_2020 <- dates_modify(returns_1399)
returns_2021 <- dates_modify(returns_1400)


sales_2021 %>%
  filter(str_detect(Product_Name, "قفل")) %>%
  length()


# There were more than 200 unique product names in the dataset, but there are anly 4 product
#categories: Binder, MetalRing, File box, and clip board. Since they have different colors 
# and each data input did not exactly match one another, it was listed as 200 unique. Here, 
# I modified to have only 4 distinct product.
ProductNameModify<-function (dataset) {
  i<-1
  while (i<=nrow(dataset)){
    if (str_detect(dataset[i,2], "مقوا|چسب|جلد|كارت ليبل|كلاسور") && !str_detect(dataset[i,2], "تخته شاسى|قفل|جامجله اى")) {
      dataset[i,2]= "Binder"}
    else if (str_detect(dataset[i,2], "فيلتر|قفل|چفت")) {
      dataset[i,2]= "MetalRing"}
    else if (str_detect(dataset[i,2], "جامجله اى|پاكت|پوشه|فولدر|كيف|كلر بوك|ديوايدر|ML 310|فايل باكس")) {
      dataset[i,2]= "FileBox"}
    else if (str_detect(dataset[i,2], "Lexi |پك ملزومات")){
      dataset[i,2]= "other"}
    else if (str_detect(dataset[i,2], "زير دستى|تخته شاسى")) {
      dataset[i,2]= "ClipBoard"}
    i=i+1
  }
  return(dataset)
}

sales_2016<-ProductNameModify(sales_2016)
sales_2016<-sales_2016 %>%
  filter(Product_Name %in% c("Binder","MetalRing","FileBox","ClipBoard"))
sales_2017<-ProductNameModify(sales_2017)
sales_2017<-sales_2017 %>%
  filter(Product_Name %in% c("Binder","MetalRing","FileBox","ClipBoard"))
sales_2018<-ProductNameModify(sales_2018)
sales_2019<-ProductNameModify(sales_2019)
sales_2020<-ProductNameModify(sales_2020)
sales_2021<-ProductNameModify(sales_2021)


# joining sales datasets with the corresponding returns datasets to find the real sales
# after deducting the returns.
sales_returns_2016<-full_join(sales_2016, returns_2016, by="Date")
sales_returns_2017<-full_join(sales_2017, returns_2017, by="Date")
sales_returns_2018<-full_join(sales_2018, returns_2018, by="Date")
sales_returns_2019<-full_join(sales_2019, returns_2019, by="Date")
sales_returns_2020<-full_join(sales_2020, returns_2020, by="Date")
sales_returns_2021<-full_join(sales_2021, returns_2021, by="Date")


# Modifying the datasets so that each colomn represents sales for each day for each product. 
# Additionally, the total daily sales and total daily commissions are true values computed after 
# taking returns into account. We also added distinct customer column which is an integer representing
# the number of unique costumers in each day for each Product. total sales, unit price, total discount,
# and total commissions are converted from rials to USD using proper exchange rate for each year.

ToProperFormat<-function (dataset) {
  dataset<-dataset %>%
    group_by(Date,Product_Name) %>%
    mutate(Total_Value_Returned = replace(Total_Value_Returned, is.na(Total_Value_Returned), 0),
           Commissions_Returned = replace(Commissions_Returned, is.na(Commissions_Returned), 0)) %>%
    summarise(unit_price= median(Unit_Price),
              total_sales= sum(Total_Sales),
              tot_discount= sum(Discount), 
              tot_commissions= sum(Commissions),
              distinct_customers= length(unique(Customer_Name)),
              tot_sales_returned= sum(Total_Value_Returned))
  return(dataset)
}


true_sales_2016<-ToProperFormat(sales_returns_2016)
true_sales_2017<-ToProperFormat(sales_returns_2017)
true_sales_2018<-ToProperFormat(sales_returns_2018)
true_sales_2019<-ToProperFormat(sales_returns_2019)
true_sales_2020<-ToProperFormat(sales_returns_2020)
true_sales_2021<-ToProperFormat(sales_returns_2021)

# merging all the rows of all 6 datasets to produce one dataset.
merged_data<-rbind(true_sales_2016,true_sales_2017,true_sales_2018,
                   true_sales_2019,true_sales_2020,true_sales_2021)

# Converting the merged dataset to a tsibble.
merged_data_tsbl <- as_tsibble(merged_data, key=Product_Name, index=Date)

monthly_sales_returns<-merged_data_tsbl %>% 
  filter(year(Date)<2022) %>%
  index_by(year_month = yearmonth(Date)) %>%
  group_by(Product_Name) %>%
  summarise(total_sales= sum(total_sales),
            total_returned= sum(tot_sales_returned),
            total_customers= sum(distinct_customers)) %>%
  drop_na()


# Graph of all products
ggplot(monthly_sales_returns, aes(year_month, total_sales)) +
  facet_wrap(~Product_Name, scales = "free", ncol = 2) + geom_line() 


################# INFLATION ADJUSTMENT
# CPI data:
IRNCPI_ts<- IRNCPI %>%
  as_tsibble(index = as.numeric(DATE),
             key = CPI) %>%
  index_by(year_month= yearmonth(DATE)) %>%
  summarise(year_month, CPI)

monthly_sales_returns<-left_join(monthly_sales_returns, IRNCPI_ts, by = "year_month")

i<-1
while (i<=nrow(monthly_sales_returns)){
  if (is.na(monthly_sales_returns[i,6])){
    monthly_sales_returns[i,6]= monthly_sales_returns[i-1,6]
  }
  i=i+1
}

# real values of the nominal vriables.
real_monthly_sales_returns<-monthly_sales_returns %>%
  mutate(real_sales= total_sales/CPI) %>%
  mutate(real_returned= total_returned/CPI)

# adjusted total sales of metal ring plot:
p1<-real_monthly_sales_returns%>%
  filter(Product_Name=="MetalRing") %>%
  autoplot(total_sales)
p2<- real_monthly_sales_returns%>%
  filter(Product_Name=="MetalRing") %>%
  autoplot(real_sales)
grid.arrange(p1, p2, ncol=1)


real_monthly_sales_returns %>%
  filter(Product_Name== "MetalRing") %>%
  autoplot(real_returned)

################# BOX-COX TRANSFORMATION applied only when the data is not normally distributed.
real_monthly_sales_returns %>%
  filter(Product_Name == "MetalRing") %>%
  features(real_sales, features = guerrero) 


metalRing_real <- real_monthly_sales_returns %>%
  filter(Product_Name == "MetalRing") %>%
  mutate(real_sales_trans = box_cox(real_sales, 0.730))

# Box Cox trnasformed plot:
p1<-metalRing_real%>%
  autoplot(real_sales)
p2<- metalRing_real%>%
  autoplot(real_sales_trans)
grid.arrange(p1, p2, ncol=1)

##################### Time series decomposition
dcmp<-metalRing_real %>%
  model(stl = STL(real_sales_trans))
components(dcmp)

metalRing_real %>%
  autoplot(real_sales_trans, color='gray') +
  autolayer(components(dcmp), trend, color='blue') 

components(dcmp) %>% autoplot()


################################# Overall comparison plots
# Before transformations:
dsgraph <- monthly_sales_returns %>%
  filter(Product_Name== "MetalRing")
g1<-ggplot(dsgraph,aes(year_month,total_sales)) + geom_line()
g2<-ggplot(dsgraph,aes(year_month,total_returned)) +geom_line()
g3 <- ggplot(dsgraph,aes(year_month,total_discount)) +geom_line()
g4 <- ggplot(dsgraph,aes(year_month,total_customers)) +geom_line()
figure1 <- ggarrange(g1, g2,g3,g4,
                     ncol = 2, nrow = 2)

# After transformations:
g1<-ggplot(metalRing_real,aes(year_month,real_sales_trans)) + geom_line()
g2<-ggplot(metalRing_real,aes(year_month,real_returned_trans)) +geom_line()
g3 <- ggplot(metalRing_real,aes(year_month,real_discount_trans)) +geom_line()
g4 <- ggplot(metalRing_real,aes(year_month,total_customers)) +geom_line()
figure2 <- ggarrange(g1, g2,g3,g4,
                     ncol = 2, nrow = 2)
grid.arrange(figure1, figure2, ncol=2)
##########################################################################################
#GRAPHING 
# graph of seasonality for metalring
metalRing_real %>% 
  gg_subseries(real_sales_trans)

# graph of autocorrelation for metalring
metalRing_real %>%
  ACF(real_sales_trans, lag_max = 24) %>%
  autoplot() + labs(title="real_sales_trans ACF")

metalRing_real<-metalRing_real %>%
  mutate(lag_real_returned=lag(real_returned)) %>%
  mutate(lag_total_customers=lag(total_customers))

######################################################################################
# Forecasting:
library(forecast)
library(vars)
library(tseries)
library(urca)
library(mFilter)
library(GGally)
library(fable)

data<-metalRing_real %>%
  dplyr::select(real_sales_trans, real_returned,total_customers) %>%
  mutate(myindex = row_number(year_month),
         t_lin = myindex,
         t_19 = cumsum(as.numeric(myindex >= 43)))
 
# train test split 80-20 -->14 months of test
train <- data %>%
  filter(year_month < yearmonth("2020 Nov"))

test <- data %>%
  filter(year_month >= yearmonth("2020 Nov"))

autoplot(train) +
  autolayer(test, color= "red")


# model training
fit_data <- train %>%
  model(
    # Seasonal Naive
    Seasonal_naive=SNAIVE(real_sales_trans),
    # model 2
    stl_arima = decomposition_model(
      # does decomposition and models seasonal part
      STL(real_sales_trans, robust = TRUE),
      # models non-seasonal part using ARIMA
      ARIMA(season_adjust)),
    # model 3
    # models the y directly
    arima = ARIMA(real_sales_trans),
    # Model
    stl_lm1 = decomposition_model(
      # does decomposition and models seasonal part
      STL(real_sales_trans, robust = TRUE),
      # models non-seasonal part using multiple regression
      TSLM(season_adjust ~ trend() + real_returned + total_customers)),
    # model 4
    stl_lm2 = decomposition_model(
      # does decomposition and models seasonal part
      STL(real_sales_trans, robust = TRUE),
      # models non-seasonal part using multiple regression
      TSLM(season_adjust ~ trend() + real_returned + lag(real_returned,1) + lag(total_customers,1)+total_customers)),
    # linear:
    linear = TSLM(real_sales_trans ~ t_lin),
    # model 6
    piecewise_linear = TSLM(real_sales_trans ~ t_lin + t_19)
  )


fc_data <- fit_data %>%
  forecast(test)

fc_data %>%
  autoplot(level = NULL) +
  autolayer(data, color = "black")

accuracy(fc_data, test) # winner= stl_lm MAPE:29.4

#Fitting:
fit_data <- test %>%
  model(
    stl_lm2 = decomposition_model(
      # does decomposition and models seasonal part
      STL(real_sales_trans, robust = TRUE),
      # models non-seasonal part using multiple regression
      TSLM(season_adjust ~ trend() + real_returned + lag(real_returned,1)+ lag(total_customers,1)+total_customers))
  )

fitted(fit_data) %>%
  autoplot(.fitted, color = "red") +
  autolayer(data, real_sales_trans)



# residual analysis
fit_data %>%
  gg_tsresiduals(lag = 24) # not good

# Autocorrelation
augment(fit_data) %>%
  features(.resid, ljung_box, lag=8, dof=4) # p>0.05 =0.0950

metalRing_real %>%
  select( real_sales_trans, real_returned,total_customers) %>%
  as_tibble() %>%
  select(-year_month) %>%
  GGally::ggpairs()





