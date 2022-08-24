# Predictive Sales Analytics of My Family’s Business

Link to the paper [here](https://htmlpreview.github.io/?https://raw.githubusercontent.com/keyvanip/ECO-312-Seminar-in-Forecasting/main/ProjectMarkdown.html)

## I. Abstract
In this research project I use data analytics tools to find sales insights and predict the sales performance of my family’s business. 
The company gathers the data in a daily time-series fashion where each row corresponds to an order being placed. 
The data include 12 datasets: 6 datasets for sales data and another 6 datasets for returns data. The data values are in Farsi language and dates are in Jalali format. 
The dependent variable to be studied is Total Sales and the independent variables that might help with obtaining better forecasts are: <br />
• **The Buying Power**: The buying power of the consumer is largely dependent on their income. The amount of the product which the consumer is willing and able to purchase also depends on the type of goods. 
Since school and office supplies are considered normal goods, a consumer will buy more if his income increases. <br />
• **The Number of Customers (Lagged)**: The number of customers positively affect the sales of a company since more consumers suggest more demand for the company’s products. <br />
• **Discounts**: a price discount provides a monetary gain that incentivizes consumers to purchase the product. 
Consumers also perceive a higher level of savings for a product when a higher price discount is provided. Therefore, a higher price discount leads to higher sales. <br />
• **Sale Return (Lagged)**: as total sales increase, sales return also increases because as sales increase, there is a higher possibility of returns.
The methods employed for forecasting the time series data are exponential smoothing (SNAIVE), ARIMA, and multiple regression with seasonal decomposition, and piecewise_linear model. 
The exponential smooth- ing is used to indicate the baseline model performance, and other models are used to predict future sales.

## II. Results
The goal of this paper was to predict my family’s business product sales to improve the company’s sales performance. 
By preparing and analyzing the company’s raw data, producing visualizations, and building various forecasting models to predict the products’ future sales, we were able to gain valuable insights. 
Additionally, we were able to build a model with 87 percent prediction accuracy that is 24 percent points more accurate than the baseline model performance. 
