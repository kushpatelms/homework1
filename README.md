# Data Science Homework - Homework assignment for Data Scientist candidate
### Objective: Develop a predictive model based on the provided Order and Online customer behavior data (data.zip). The analysis can be done in R or Python and should be presented in an R-Studio Notebook or Jupyter Notebook. The assignment should produce a multi-class classification supervised learning model. It is up to you to design the analysis and provide a rationale of chosen approach. Feel free to use any open source tools or functions (out of the box or your own) that will facilitate the analysis. In your workflow, please touch on each of the following areas:							

1) Exploration and understanding of the data sets									
2) Feature engineering									
3) Feature selection									
4) Model design and sampling									
5) Model generation									
6) Model evaluation
7) Summary of results: 2-3 paragraphs textual summary
									
It is not necessary to produce a highly predictive model, but, rather, to illustrate your understanding and practical knowledge of the model building process. 									


### Data Sets

Table order.csv  263278 obs. of  6 variables:

|Columns|Data|Column Description|
|----|----|----|
|custno   | int  18944 18944 18944 36096 1 6401 25601 57601 2 2 ...		|Customer number		|
|ordno    | int  64694 28906 114405 62681 1 8187 41198 112311 70848 2 ...	|Order number			|
|orderdate| POSIXct, format: "2016-11-27 20:57:20" "2017-04-23 21:31:03"  	|Order date			|
|prodcat2 | int  NA NA NA NA NA NA NA NA NA NA ...				|Product category -detail	|
|prodcat1 | int  1 1 1 1 1 1 1 1 1 1 ...					|Product category		|
|revenue  | num  76.4 130.7 139.2 72.5 100.2 ...				|Revenue			|

	
Table: online.csv	954774 obs. of  7 variables:

|Columns|Data|Column Description|
|----|----|----|
|session| int  419542 3030130 2638740 880408 2612179 880953 418956 281663 26191 1363670 ...	|online session key|
|visitor| int  140970 14501 419353 90673 191542 419268 14938 419163 419163 14464 ...	|Online visitor key|
|dt| POSIXct, format: "2016-09-16 05:03:23"  ...	|Online activity date|
|custno| int  3840 70400 21248 39168 47616 47616 47872 49920 49920 54784 ...	|Customer number|
|category| int  1 1 1 1 1 1 1 1 1 1 ...	|Online browsing category|
|event1  | int  NA NA NA NA NA NA NA NA NA NA ...	|Online event 1|
|event2  | int  1 1 1 1 1 1 1 1 1 1 ...	|Online event 2|


## Summary of my analysis

To summarise, we had the customer order data as well as customer online activity data. We explored the data and found out that there is a seasonality in the revenue, number of orders, as well as the online activity. It peaks during Nov and Dec and drops during the months of Feb and Oct. We also saw that this trend is consitent across browsing categories as well as differnet online events. We also found out that most of the online activity is done on browsing category 3.

We also tried associating customer order data with online behaviour. While doing so, we made an assumption  
*1. Only the last 30 days of online activity drives buying behaviour for a customer.  
Any activity before that does not affect buying behaviour significantly. Based on this assumption, we have merged the online data with the order data. We were also able to derive som metrics like, count of each online event before the purchase, Number of days since last order, order month etc. We have used this metric to predict product category-1.

The predictive problem we are trying to solve is: Based on historical purchases and online beviour, what is the likelihood that a customer would purchase a product from a product-category1. To fit and measure the performance of the model, we first split the data into training and test sample. 70% of the data is used for training and 30% for measure its performance on unseen data. We have tried multinomial logistic regression, random forest, and random forest with hyper parameter tuning. Both the models performed decent on the test data. The models are good at identifying whether a customer is not going to buy from a particular product-category. The AUC value shows that the model does a decent job of separating classes in logistic regression. To improve the performance of the random forest model, we can perform hyperparameter tuning using a grid-search approach. This process takes a lot of time and computation capacity. I have shared the code for doing that.

With a better predicitve power, we can predict with high accuracy the product category where a customer is mostly likely to shop from. We can use this information to run targeted campaign or provide incentive to buy back from us.

