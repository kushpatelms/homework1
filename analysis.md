---
title: "R Notebook"
output: 
  html_document:
    keep_md: true
---

#Data Science Homework - Homework assignment for Data Scientist candidate

#####Loading the needed libraries and setting the working directory


```r
library(tidyverse)
```

```
## -- Attaching packages -------------------------------------------------------------------------------------- tidyverse 1.2.1 --
```

```
## v ggplot2 2.2.1     v purrr   0.2.5
## v tibble  1.4.2     v dplyr   0.7.5
## v tidyr   0.8.1     v stringr 1.3.1
## v readr   1.1.1     v forcats 0.3.0
```

```
## -- Conflicts ----------------------------------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(caret)
```

```
## Loading required package: lattice
```

```
## 
## Attaching package: 'caret'
```

```
## The following object is masked from 'package:purrr':
## 
##     lift
```

```r
library(HandTill2001)
```

```
## Warning: package 'HandTill2001' was built under R version 3.5.3
```

```r
rm(list=ls())
getwd()
```

```
## [1] "C:/Users/KPatel/OneDrive - CBRE, Inc/Documents/career/Shutterfly"
```

```r
setwd('C:/Users/KPatel/OneDrive - CBRE, Inc/Documents/career/Shutterfly')
```

#####Importing the data from the zip file

```r
order <- read.table(unz("data.zip", "order.csv"), header=T, quote="\"", sep=",")
online<- read.table(unz("data.zip", "online.csv"), header=T, quote="\"", sep=",")
```


##1. Exploration and understanding of the data sets

###Order dataset
Lets look at the structure of the data

```r
str(order)
```

```
## 'data.frame':	263278 obs. of  6 variables:
##  $ custno   : int  18944 18944 18944 36096 1 6401 25601 57601 2 2 ...
##  $ ordno    : int  64694 114405 28906 62681 1 8187 41198 112311 2 70848 ...
##  $ orderdate: Factor w/ 149482 levels "2016-01-01 05:05:14",..: 44835 68024 67301 7798 75039 63162 88311 58274 12156 4041 ...
##  $ prodcat2 : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ prodcat1 : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ revenue  : num  53.3 0.1 141.66 36.82 8.35 ...
```
we can see that the custno, ordno, prodcat2, prodcat1 are integer but should be factors. Also, orderdate is a factor which should be a Datetime column. Changing the data type of these columns.

```r
order[c('custno','ordno','prodcat1','prodcat2')] <- lapply(order[c('custno','ordno','prodcat1','prodcat2')], factor)  ## as.factor() could also be used

order$orderdate <- strptime(x = as.character(order$orderdate),
                                format = "%Y-%m-%d %H:%M:%S")
order$orderdate <- as.POSIXct(order$orderdate, tz = "", format="%Y-%m-%d %H:%M:%S")

str(order)
```

```
## 'data.frame':	263278 obs. of  6 variables:
##  $ custno   : Factor w/ 70264 levels "1","2","3","4",..: 18898 18898 18898 35970 1 6382 25539 57238 2 2 ...
##  $ ordno    : Factor w/ 149717 levels "1","2","3","4",..: 64382 113630 28757 62375 1 8123 41017 111562 2 70445 ...
##  $ orderdate: POSIXct, format: "2016-11-27 20:57:20" "2017-04-29 20:18:04" ...
##  $ prodcat2 : Factor w/ 251 levels "2","3","4","5",..: NA NA NA NA NA NA NA NA NA NA ...
##  $ prodcat1 : Factor w/ 6 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ revenue  : num  53.3 0.1 141.66 36.82 8.35 ...
```

Now lets look at the summary of the data.

```r
summary(order)
```

```
##      custno           ordno          orderdate                  
##  56     :   626   6076   :    26   Min.   :2016-01-01 05:05:14  
##  2758   :   451   4803   :    21   1st Qu.:2016-09-28 11:22:56  
##  2511   :   368   5207   :    20   Median :2017-06-25 08:56:10  
##  1055   :   350   23233  :    19   Mean   :2017-06-29 11:35:41  
##  709    :   345   28041  :    19   3rd Qu.:2018-03-25 09:39:11  
##  3844   :   345   23907  :    17   Max.   :2019-01-02 23:54:58  
##  (Other):260793   (Other):263156                                
##     prodcat2      prodcat1     revenue      
##  3      : 40908   1:48672   Min.   :  0.00  
##  4      : 15797   2:88684   1st Qu.: 37.43  
##  13     : 13567   3:44019   Median : 74.93  
##  16     : 13470   4:46681   Mean   : 74.94  
##  11     : 13400   5:11180   3rd Qu.:112.28  
##  (Other):164313   7:24042   Max.   :150.00  
##  NA's   :  1823
```
We can see that we have the order data for years 2016, 2017, and 2018. We can also see that there are some missing values in prodcat2.

Lets explore at prodcat2

```r
length(unique(order$prodcat2))
```

```
## [1] 252
```
We have 252 levels in the prodcat2 categorical variable. Lets try to understand the mapping between prodcat1 and prodcat2

```r
tab<-table(order$prodcat2,order$prodcat1)
head(tab,50)
```

```
##     
##          1     2     3     4     5     7
##   2      0  5148     0     0     0     0
##   3      0 40908     0     0     0     0
##   4      0 15797     0     0     0     0
##   5      0 12946     0     0     0     0
##   6      0     0  5393     0     0     0
##   7      0  2294     0     0     0     0
##   8      0     0   133     0     0     0
##   9      0     0 12671     0     0     0
##   10     0   570     0     0     0     0
##   11 13400     0     0     0     0     0
##   12  1891     0     0     0     0     0
##   13     0     0     0 13567     0     0
##   14  4251     0     0     0     0     0
##   15     0     0     0   730     0     0
##   16     0     0     0 13470     0     0
##   17     0     0     0     0   127     0
##   18     0     0     0  2758     0     0
##   19   926     0     0     0     0     0
##   20     0     0     0     0  1081     0
##   21   906     0     0     0     0     0
##   23     0    30     0     0     0     0
##   24   461     0     0     0     0     0
##   25     0     0     0     0     0  4773
##   26     0     0   266     0     0     0
##   27     0     0  3535     0     0     0
##   28     0     0     0     0     0  4785
##   30     0     0  4217     0     0     0
##   32     0     0   216     0     0     0
##   33   303     0     0     0     0     0
##   34    55     0     0     0     0     0
##   35     0   735     0     0     0     0
##   38     0     0     0     0  6552     0
##   39     0     0     0     0  2321     0
##   40  1344     0     0     0     0     0
##   41  1156     0     0     0     0     0
##   42     0  2282     0     0     0     0
##   43     0     0  2400     0     0     0
##   44     0     0  1647     0     0     0
##   45     0     0  2357     0     0     0
##   46     0   217     0     0     0     0
##   47     0     0     0     0     0   208
##   48     0     0   472     0     0     0
##   49     0     0   279     0     0     0
##   50     0     0     0     0     0  2493
##   51     0     0   798     0     0     0
##   52     0     0   182     0     0     0
##   53     0     0    33     0     0     0
##   54     0     0     0  1090     0     0
##   55     0   138     0     0     0     0
##   56     0     0    99     0     0     0
```
There are also NA's in the prodcat2.

```r
summary(order$prodcat2)
```

```
##       3       4      13      16      11       5      89       9      78 
##   40908   15797   13567   13470   13400   12946   12772   12671    8341 
##      38       6       2      28      25      14      30     110      27 
##    6552    5393    5148    4785    4773    4251    4217    3954    3535 
##      96      18      58     119      50      43      45      39       7 
##    3520    2758    2596    2572    2493    2400    2357    2321    2294 
##      42      59      12     107      44      77     145      75      40 
##    2282    2068    1891    1673    1647    1604    1457    1448    1344 
##     147      41      54      20      85      99      19     213      21 
##    1340    1156    1090    1081     960     960     926     922     906 
##      51      70      93      35      15      69     131      74     155 
##     798     766     745     735     730     726     711     656     584 
##      97      10     116      80      48     115      24     180     149 
##     577     570     522     511     472     470     461     451     444 
##     169      67     113     168     195     132      91      33      94 
##     425     385     384     374     356     341     324     303     291 
##     134      49      57      66     171      26     165     138      92 
##     284     279     277     273     271     266     264     263     259 
##     144      62      73     174      46     114      32      47      90 
##     241     231     223     220     217     217     216     208     202 
##     140      79      72     214      52     146     183     112 (Other) 
##     202     201     193     187     182     180     179     173    6889 
##    NA's 
##    1823
```
There are 1823 NA's in the prodcat2 field. We could impute these values using statistical methods. However, given the time-constraint, we won't dive deep into prodcat2.

Lets first understand the level of the data. Intuitively, it looks like that the data is at product level. Lets check that by aggregating data.

```r
level_ord<- order %>% group_by(custno,ordno,orderdate,prodcat1,prodcat2) %>% summarise(cnt=n()) %>% arrange(desc(cnt))
head(level_ord)
```

```
## # A tibble: 6 x 6
## # Groups:   custno, ordno, orderdate, prodcat1 [5]
##   custno ordno orderdate           prodcat1 prodcat2   cnt
##   <fct>  <fct> <dttm>              <fct>    <fct>    <int>
## 1 9      23204 2016-01-03 18:27:48 2        3            2
## 2 9      23204 2016-01-03 18:27:48 2        110          2
## 3 42     46577 2017-03-07 16:29:29 1        14           2
## 4 42     46577 2017-03-07 16:29:29 3        6            2
## 5 42     59808 2017-08-16 11:38:10 2        3            2
## 6 79     40494 2017-08-12 20:28:36 1        145          2
```

We can see that for custno 9 and ordno 23204, we are getting the cnt of more than 1 for each combination of prodcat1 and prodcat2. Lets look at the order data for this case

```r
cust_9<-order %>% filter(custno==9,ordno==23204)
cust_9
```

```
##   custno ordno           orderdate prodcat2 prodcat1 revenue
## 1      9 23204 2016-01-03 18:27:48        3        2    1.27
## 2      9 23204 2016-01-03 18:27:48        3        2   33.11
## 3      9 23204 2016-01-03 18:27:48      110        2   90.07
## 4      9 23204 2016-01-03 18:27:48      110        2  131.97
```
We clearly see from the above example that the data is at product level. Lets get it at order and prodcat1 level.

```r
order_agg<- order %>% group_by(custno,ordno,orderdate,prodcat1) %>% summarise(revenue = sum(revenue), num_prod = n())
head(order_agg,10)
```

```
## # A tibble: 10 x 6
## # Groups:   custno, ordno, orderdate [9]
##    custno ordno  orderdate           prodcat1 revenue num_prod
##    <fct>  <fct>  <dttm>              <fct>      <dbl>    <int>
##  1 1      1      2017-06-12 08:27:59 1           8.35        1
##  2 2      2      2016-03-29 13:04:27 1          13.6         1
##  3 2      15645  2016-08-29 10:50:33 1         150.          1
##  4 2      70848  2016-01-26 14:14:21 1          23.8         1
##  5 2      78052  2017-05-02 12:24:02 1          31.8         1
##  6 2      78052  2017-05-02 12:24:02 7         112.          1
##  7 2      112461 2016-01-26 14:23:21 1          94.5         1
##  8 3      3      2017-01-31 19:16:49 1          95.8         1
##  9 3      12236  2017-03-30 16:16:14 1         142.          1
## 10 3      98773  2017-03-23 10:29:42 1          98.9         1
```



Lets look at revenue.

```r
summary(order$revenue)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00   37.43   74.93   74.94  112.28  150.00
```

```r
hist(order$revenue,breaks = 30)
```

![](analysis_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
We can see from the histogram that revenue is uniformly distributed. Lets check revenue and ordersize across different prodcat1 and customers

```r
as.data.frame(order %>% group_by(prodcat1) %>% summarise(avg_rev_per_product=mean(revenue),total_rev=sum(revenue),num_orders=length(unique(ordno)),cnt_cust=length(unique(custno))))
```

```
##   prodcat1 avg_rev_per_product total_rev num_orders cnt_cust
## 1        1            74.88081 3644598.6      29875    18241
## 2        2            75.20853 6669793.0      53510    30260
## 3        3            74.74744 3290307.4      36538    17614
## 4        4            74.65373 3484911.0      28188    17775
## 5        5            75.67661  846064.5       9724     4684
## 6        7            74.62753 1794195.1      21669    10670
```
From the above data we can see that the prodcat1=1 has the highest sales volume both in terms of revenue, number of orders as well as number of customers, whereas prodcat1=5 has the lowest.


Lets look at trend in revenue with time

```r
library(lubridate)


order %>% mutate(Year_Month=format(as.Date(orderdate), "%Y-%m")) %>% group_by(Year_Month) %>% summarise(revenue=sum(revenue)) %>% ggplot(aes(x=Year_Month,y=revenue)) + geom_bar(stat='identity')+
xlab('orderdate')+ylab('Revenue')+ theme(axis.text.x = element_text(angle = 90, hjust = 1))#+ggtitle("Cumulative")
```

![](analysis_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
We can see a seasonlaity here. The Revenue peaks during the months of Dec and Jan. It then drops during the month of Feb and starts rising again and reaches another peak around May-June and then plunges in Oct. Then it rises again and peaks in Dec-Jan. The highest revenue was earned in Jan-2017 and lowest in Oct-2018.Lets break it down across product category and see if we get any seasonality across prodcat1. 


```r
order %>% mutate(Year_Month=format(as.Date(orderdate), "%Y-%m")) %>% group_by(Year_Month,prodcat1) %>% summarise(revenue=sum(revenue)) %>% ggplot(aes(x=Year_Month,y=revenue,color=prodcat1,group=prodcat1)) +                    geom_line()+ facet_wrap(~prodcat1,nrow=3, scales = 'free') +
xlab('orderdate')+ylab('Revenue')+ theme(axis.text.x = element_text(angle = 90, hjust = 1))#+ggtitle("Cumulative")
```

![](analysis_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
We can see a seasonality across all the prodcat1 similar to the overall revenue category

We expect a similar trend for num_products ordered. Lets calculate the correlation between revenue and number of products ordered.


```r
cor(order_agg$revenue,order_agg$num_prod)
```

```
## [1] 0.7414783
```
The two fields are highly correlated which is evident from the scatter-plot given below

```r
order_agg %>% ggplot(aes(x=revenue,y=num_prod)) + geom_point()
```

![](analysis_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

Lets look at the scatterplot across each product category and calculate the correlation at that level.

```r
order_agg %>% ggplot(aes(x=revenue,y=num_prod)) + geom_point()+ facet_wrap(~prodcat1,nrow=3, scales = 'free')
```

![](analysis_files/figure-html/unnamed-chunk-18-1.png)<!-- -->


```r
as.data.frame(order_agg %>% group_by(prodcat1) %>% summarise(cor_rev_prod=cor(revenue,num_prod)))
```

```
##   prodcat1 cor_rev_prod
## 1        1    0.7101406
## 2        2    0.7956114
## 3        3    0.6424980
## 4        4    0.6902827
## 5        5    0.5314255
## 6        7    0.5569722
```
The correlation across each prodcat1 is high.


###Online dataset
Lets look at the structure of the data

```r
str(online)
```

```
## 'data.frame':	954774 obs. of  7 variables:
##  $ session : int  419542 3030130 2638740 880408 2612179 880953 418956 281663 26191 1363670 ...
##  $ visitor : int  140970 14501 419353 90673 191542 419268 14938 419163 419163 14464 ...
##  $ dt      : Factor w/ 942579 levels "2016-01-01 00:00:08",..: 302552 854890 652906 833133 156326 222492 891922 466197 367097 644408 ...
##  $ custno  : int  3840 70400 21248 39168 47616 47616 47872 49920 49920 54784 ...
##  $ category: int  1 1 1 1 1 1 1 1 1 1 ...
##  $ event1  : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ event2  : int  1 1 1 1 1 1 1 1 1 1 ...
```

The categorical columns custno, category, event1, event2, session, and visitor are in integer and the dt column should be of datetime datetype.

```r
online[c('session','visitor','custno','category','event1','event2')] <- lapply(online[c('session','visitor','custno','category','event1','event2')], factor)  ## as.factor() could also be used

online$dt <- strptime(x = as.character(online$dt),
                                format = "%Y-%m-%d %H:%M:%S")
online$dt <- as.POSIXct(online$dt, tz = "", format="%Y-%m-%d %H:%M:%S")

str(online)
```

```
## 'data.frame':	954774 obs. of  7 variables:
##  $ session : Factor w/ 850235 levels "2","3","6","9",..: 93667 680561 591950 197824 586838 197906 93569 62818 6501 307342 ...
##  $ visitor : Factor w/ 259950 levels "1","3","5","6",..: 73857 9086 217101 48286 99883 217040 9388 216958 216958 9058 ...
##  $ dt      : POSIXct, format: "2016-09-16 05:03:23" "2017-11-13 04:58:12" ...
##  $ custno  : Factor w/ 57584 levels "6","7","8","9",..: 3173 56713 17326 31495 38364 38364 38560 40169 40169 43840 ...
##  $ category: Factor w/ 3 levels "1","2","3": 1 1 1 1 1 1 1 1 1 1 ...
##  $ event1  : Factor w/ 10 levels "1","2","4","5",..: NA NA NA NA NA NA NA NA NA NA ...
##  $ event2  : Factor w/ 10 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
```

Lets look at the summary of the data

```r
summary(online)
```

```
##     session          visitor             dt                     
##  72733  :     5   328620 :   440   Min.   :2016-01-01 00:00:08  
##  157423 :     5   178635 :   310   1st Qu.:2016-07-25 15:01:06  
##  161973 :     5   227902 :   281   Median :2016-12-27 07:21:38  
##  692928 :     5   53505  :   276   Mean   :2017-01-10 19:15:08  
##  811699 :     5   279881 :   276   3rd Qu.:2017-07-10 18:51:39  
##  1178030:     5   328827 :   254   Max.   :2017-12-31 23:58:05  
##  (Other):954744   (Other):952937                                
##      custno       category       event1           event2      
##  713    :   855   1: 52964   1      : 51567   7      :367857  
##  10785  :   505   2:194890   4      : 23858   3      :129795  
##  34515  :   479   3:706920   2      : 23312   8      :122402  
##  336    :   473              11     : 20586   4      : 94230  
##  28702  :   470              6      : 16537   1      : 86496  
##  57148  :   425              (Other): 29068   9      : 52145  
##  (Other):951567              NA's   :789846   (Other):101849
```
We can see that the duration of the online data is from Jan-2016 to Dec-2017. We have 3 levels in online browsing category and multiple levels in event1 and event2. Lets first compare the number of customers in online vs order.

```r
length(unique(online$custno))
```

```
## [1] 57584
```

```r
length(unique(order$custno))
```

```
## [1] 70264
```
There are 57584 customers in online and 70264 customers in the order dataset. Getting a list of customers common to both the datasets

```r
common_cust<-unique(order[which(order$custno %in% unique(online$custno)),]$custno)
length(common_cust)
```

```
## [1] 56764
```
Lets calculate online activity across each of online browsing categories.

```r
as.data.frame(online %>% group_by(category) %>% summarise(activity=n(),unique_sessions=length(unique(session)),num_cust=length(unique(custno))))
```

```
##   category activity unique_sessions num_cust
## 1        1    52964           47228    10001
## 2        2   194890          175809    30466
## 3        3   706920          627275    51222
```
Here activity means any sort of activity, be it creation of session, change in event1 or event2 etc. Unique sessions is the count of unique sessions created on each category and new_cust is the number of unique customers that used that category. Here, online browsing category could mean which device or what channel is used by the customer to browse the website.

Moving onto event1, we can see that majority of the values in that field are null

```r
prop.table(table(online$event1,useNA = "ifany"))*100
```

```
## 
##          1          2          4          5          6          7 
##  5.4009640  2.4416249  2.4988112  0.1542773  1.7320329  1.1956756 
##          8          9         10         11       <NA> 
##  1.1485441  0.4178999  0.1280931  2.1561123 82.7259645
```
83% of the values are null in event1. We will assume here that all the null values in event1 belong to one class which is class 0.

```r
levels(online$event1)
```

```
##  [1] "1"  "2"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11"
```

```r
online$event1<-factor(ifelse(is.na(online$event1), 0, paste(online$event1)), levels = c(levels(online$event1), 0))
levels(online$event1)
```

```
##  [1] "1"  "2"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11" "0"
```

```r
prop.table(table(online$event1))*100
```

```
## 
##          1          2          4          5          6          7 
##  5.4009640  2.4416249  2.4988112  0.1542773  1.7320329  1.1956756 
##          8          9         10         11          0 
##  1.1485441  0.4178999  0.1280931  2.1561123 82.7259645
```


Lets look at event2,

```r
prop.table(table(online$event2,useNA = "ifany"))*100
```

```
## 
##          1          2          3          4          5          6 
##  9.0593167  1.6149371 13.5943166  9.8693513  5.0569035  3.1329927 
##          7          8          9         10 
## 38.5281753 12.8199972  5.4615019  0.8625078
```
We can see that event2 is a highly imbalanced class with 39% of the data is in class 7 and 14% in class3. Lets look at online activity across each of the class in event2.

```r
as.data.frame(online %>% group_by(event2) %>% summarise(activity=n(),unique_sessions=length(unique(session)),num_cust=length(unique(custno))))
```

```
##    event2 activity unique_sessions num_cust
## 1       1    86496           86496    28945
## 2       2    15419           15175     9607
## 3       3   129795          129788    34495
## 4       4    94230           94123    32906
## 5       5    48282           48023    21858
## 6       6    29913           29873    14487
## 7       7   367857          355901    50696
## 8       8   122402          122388    36507
## 9       9    52145           52144    24552
## 10     10     8235            8057     3985
```
AS expected, most of the activity is across class7 followed by class3 and class8. However, its still not enough to tell us what the value of each those class means.

Lets look at the time series trends of online activity

```r
online %>% mutate(Year_Month=format(as.Date(dt), "%Y-%m")) %>% group_by(Year_Month) %>% summarise(activity=n(),unique_sessions=length(unique(session)),num_cust=length(unique(custno)))%>% ggplot() + geom_line(aes(x = Year_Month, y = activity, group=1, colour = "activity")) + 
geom_line(aes(x = Year_Month, y = unique_sessions, group=2, colour = "unique_sessions")) + 
geom_line(aes(x = Year_Month, y = num_cust, group=2, colour = "num_cust"))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](analysis_files/figure-html/unnamed-chunk-30-1.png)<!-- -->

We will further break it down by browsing category

```r
online %>% mutate(Year_Month=format(as.Date(dt), "%Y-%m")) %>% group_by(Year_Month,category) %>% summarise(activity=n(),unique_sessions=length(unique(session)),num_cust=length(unique(custno)))%>% ggplot() + geom_line(aes(x = Year_Month, y = activity, group=1, colour = "activity")) + 
geom_line(aes(x = Year_Month, y = unique_sessions, group=2, colour = "unique_sessions")) + 
geom_line(aes(x = Year_Month, y = num_cust, group=2, colour = "num_cust"))+
facet_wrap(~category, nrow=2,scales = 'free') +
theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](analysis_files/figure-html/unnamed-chunk-31-1.png)<!-- -->


Lets break down online activity by event2.

```r
online %>% mutate(Year_Month=format(as.Date(dt), "%Y-%m")) %>% group_by(Year_Month,event2) %>% summarise(activity=n(),unique_sessions=length(unique(session)),num_cust=length(unique(custno)))%>% ggplot() + geom_line(aes(x = Year_Month, y = activity, group=1, colour = "activity")) + 
geom_line(aes(x = Year_Month, y = unique_sessions, group=2, colour = "unique_sessions")) + 
geom_line(aes(x = Year_Month, y = num_cust, group=2, colour = "num_cust"))+
facet_wrap(~event2, nrow=5,scales = 'free') +
theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](analysis_files/figure-html/unnamed-chunk-32-1.png)<!-- -->


From all the online activity plot, we can see a seasonality trend. The online activity starts peaking from Nov till Jan then falls to the lowest in Feb. It then remains pretty steady till Oct. This trend is similar to the one we obtained for revenue and number of orders.

##2.Feature engineering
Lets try to understand the level of the data in online dataset.

```r
level_online<-online %>% group_by(session,custno,dt,event1,event2)%>% summarise(cnt=n()) %>% arrange(desc(cnt)) 
head(level_online)
```

```
## # A tibble: 6 x 6
## # Groups:   session, custno, dt, event1 [6]
##   session custno dt                  event1 event2   cnt
##   <fct>   <fct>  <dttm>              <fct>  <fct>  <int>
## 1 2       60013  2016-11-15 13:02:21 0      9          1
## 2 3       61668  2016-05-15 06:53:55 0      8          1
## 3 6       71122  2016-08-17 16:25:30 0      9          1
## 4 9       637    2017-10-04 13:07:03 0      7          1
## 5 9       637    2017-10-04 13:36:14 0      3          1
## 6 10      16552  2017-06-24 17:30:21 0      3          1
```
We can see that each record is uniquely identified at session, customer, datetime, event1, and event2. Lets change the level of the data by spreading the online data on event2

```r
online$val<-1
online_spread_e2<-spread(online,event2,val,fill=0,sep = '_')
head(online_spread_e2)
```

```
##   session visitor                  dt custno category event1 event2_1
## 1  419542  140970 2016-09-16 05:03:23   3840        1      0        1
## 2 3030130   14501 2017-11-13 04:58:12  70400        1      0        1
## 3 2638740  419353 2017-05-24 16:10:38  21248        1      0        1
## 4  880408   90673 2017-10-28 13:42:38  39168        1      0        1
## 5 2612179  191542 2016-05-17 06:30:32  47616        1      0        1
## 6  880953  419268 2016-07-15 12:36:42  47616        1      0        1
##   event2_2 event2_3 event2_4 event2_5 event2_6 event2_7 event2_8 event2_9
## 1        0        0        0        0        0        0        0        0
## 2        0        0        0        0        0        0        0        0
## 3        0        0        0        0        0        0        0        0
## 4        0        0        0        0        0        0        0        0
## 5        0        0        0        0        0        0        0        0
## 6        0        0        0        0        0        0        0        0
##   event2_10
## 1         0
## 2         0
## 3         0
## 4         0
## 5         0
## 6         0
```

```r
online$val<-NULL
```
We will further spread the online data on key event1.

```r
online_spread_e2$val<-1
online_spread_e2e1<-spread(online_spread_e2,event1,val,fill=0,sep = '_')

head(online_spread_e2e1)
```

```
##   session visitor                  dt custno category event2_1 event2_2
## 1  419542  140970 2016-09-16 05:03:23   3840        1        1        0
## 2 3030130   14501 2017-11-13 04:58:12  70400        1        1        0
## 3 2638740  419353 2017-05-24 16:10:38  21248        1        1        0
## 4  880408   90673 2017-10-28 13:42:38  39168        1        1        0
## 5 2612179  191542 2016-05-17 06:30:32  47616        1        1        0
## 6  880953  419268 2016-07-15 12:36:42  47616        1        1        0
##   event2_3 event2_4 event2_5 event2_6 event2_7 event2_8 event2_9 event2_10
## 1        0        0        0        0        0        0        0         0
## 2        0        0        0        0        0        0        0         0
## 3        0        0        0        0        0        0        0         0
## 4        0        0        0        0        0        0        0         0
## 5        0        0        0        0        0        0        0         0
## 6        0        0        0        0        0        0        0         0
##   event1_1 event1_2 event1_4 event1_5 event1_6 event1_7 event1_8 event1_9
## 1        0        0        0        0        0        0        0        0
## 2        0        0        0        0        0        0        0        0
## 3        0        0        0        0        0        0        0        0
## 4        0        0        0        0        0        0        0        0
## 5        0        0        0        0        0        0        0        0
## 6        0        0        0        0        0        0        0        0
##   event1_10 event1_11 event1_0
## 1         0         0        1
## 2         0         0        1
## 3         0         0        1
## 4         0         0        1
## 5         0         0        1
## 6         0         0        1
```

There is also a possibility that a customer can use different browsing categories (for example, custno 6). To account for that we have to spread the data by category as well.

```r
online_spread_e2e1$val<-1
online_spread_e2e1cat<-spread(online_spread_e2e1,category,val,fill=0,sep = '_')

head(online_spread_e2e1cat)
```

```
##   session visitor                  dt custno event2_1 event2_2 event2_3
## 1  419542  140970 2016-09-16 05:03:23   3840        1        0        0
## 2 3030130   14501 2017-11-13 04:58:12  70400        1        0        0
## 3 2638740  419353 2017-05-24 16:10:38  21248        1        0        0
## 4  880408   90673 2017-10-28 13:42:38  39168        1        0        0
## 5 2612179  191542 2016-05-17 06:30:32  47616        1        0        0
## 6  880953  419268 2016-07-15 12:36:42  47616        1        0        0
##   event2_4 event2_5 event2_6 event2_7 event2_8 event2_9 event2_10 event1_1
## 1        0        0        0        0        0        0         0        0
## 2        0        0        0        0        0        0         0        0
## 3        0        0        0        0        0        0         0        0
## 4        0        0        0        0        0        0         0        0
## 5        0        0        0        0        0        0         0        0
## 6        0        0        0        0        0        0         0        0
##   event1_2 event1_4 event1_5 event1_6 event1_7 event1_8 event1_9 event1_10
## 1        0        0        0        0        0        0        0         0
## 2        0        0        0        0        0        0        0         0
## 3        0        0        0        0        0        0        0         0
## 4        0        0        0        0        0        0        0         0
## 5        0        0        0        0        0        0        0         0
## 6        0        0        0        0        0        0        0         0
##   event1_11 event1_0 category_1 category_2 category_3
## 1         0        1          1          0          0
## 2         0        1          1          0          0
## 3         0        1          1          0          0
## 4         0        1          1          0          0
## 5         0        1          1          0          0
## 6         0        1          1          0          0
```

Now, we will engineer features from Order data. One set of important features are is the revenue made by a customer on the last order of the same product category as well as and the number number of items ordered the last time. I have created those features below


```r
order_agg<- order_agg %>% arrange(custno,prodcat1,orderdate,) %>% group_by(custno, prodcat1) %>% mutate(cum_rev=cumsum(revenue), cum_num_prod=cumsum(num_prod))


order_agg$cumrev_till_prev_order <- order_agg$cum_rev - order_agg$revenue
order_agg$cumnum_prod_till_prev_order <- order_agg$cum_num_prod - order_agg$num_prod
order_agg <- order_agg %>% arrange(custno,prodcat1,orderdate) %>%
  group_by(custno,prodcat1) %>%
  mutate(rev_prev_ord = dplyr::lag(revenue, n = 1, default = 0),num_prod_prev_ord = dplyr::lag(num_prod, n = 1, default = 0))

order_agg[,c("revenue","num_prod","cum_rev","cum_num_prod")]<-NULL
summary(order_agg)
```

```
##      custno           ordno          orderdate                   prodcat1 
##  56     :   479   1974   :     5   Min.   :2016-01-01 05:05:14   1:29875  
##  2758   :   347   2794   :     5   1st Qu.:2016-09-25 19:15:03   2:53510  
##  1055   :   321   3657   :     5   Median :2017-06-17 17:01:47   3:36538  
##  1488   :   303   4577   :     5   Mean   :2017-06-24 14:26:40   4:28188  
##  1581   :   269   5614   :     5   3rd Qu.:2018-03-17 07:09:34   5: 9724  
##  3844   :   268   5949   :     5   Max.   :2019-01-02 23:54:58   7:21669  
##  (Other):177517   (Other):179474                                          
##  cumrev_till_prev_order cumnum_prod_till_prev_order  rev_prev_ord    
##  Min.   :    0.0        Min.   :  0.000             Min.   :   0.00  
##  1st Qu.:    0.0        1st Qu.:  0.000             1st Qu.:   0.00  
##  Median :    0.0        Median :  0.000             Median :   0.00  
##  Mean   :  445.5        Mean   :  5.976             Mean   :  45.24  
##  3rd Qu.:  387.6        3rd Qu.:  5.000             3rd Qu.:  83.79  
##  Max.   :22676.0        Max.   :305.000             Max.   :1578.89  
##                                                                      
##  num_prod_prev_ord
##  Min.   : 0.0000  
##  1st Qu.: 0.0000  
##  Median : 0.0000  
##  Mean   : 0.6045  
##  3rd Qu.: 1.0000  
##  Max.   :17.0000  
## 
```
The following four metrics are created.
*1. cum_rev_till_prev_order: This is the cumulative revenue of a customer till previous order
*2. cumnum_prod_till_prev_order: Cumulative number of items till previous order.
*3. rev_prev_order: revenue made from previous order
*4. num_prod_prev_order: Number of items ordered from previous order

Another important metric would be number of days till last order

```r
order_agg <- order_agg %>% arrange(custno,prodcat1,orderdate) %>%
  group_by(custno,prodcat1) %>%
  mutate(orderdate_prev_ord = dplyr::lag(orderdate, n = 1, default = NA))

order_agg$num_days_till_last_order <- difftime(order_agg$orderdate, order_agg$orderdate_prev_ord, units="days")

order_agg$orderdate_prev_ord<-NULL

order_agg$num_days_till_last_order <-as.numeric(order_agg$num_days_till_last_order)

summary(order_agg$num_days_till_last_order)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    9.65   35.19  100.02  127.97 1081.32   99244
```

The order aggregate data now looks like.

```r
head(order_agg,10)
```

```
## # A tibble: 10 x 9
## # Groups:   custno, prodcat1 [4]
##    custno ordno  orderdate           prodcat1 cumrev_till_prev_order
##    <fct>  <fct>  <dttm>              <fct>                     <dbl>
##  1 1      1      2017-06-12 08:27:59 1                           0  
##  2 2      70848  2016-01-26 14:14:21 1                           0  
##  3 2      112461 2016-01-26 14:23:21 1                          23.8
##  4 2      2      2016-03-29 13:04:27 1                         118. 
##  5 2      15645  2016-08-29 10:50:33 1                         132. 
##  6 2      78052  2017-05-02 12:24:02 1                         282. 
##  7 2      78052  2017-05-02 12:24:02 7                           0  
##  8 3      3      2017-01-31 19:16:49 1                           0  
##  9 3      119946 2017-02-27 19:34:25 1                          95.8
## 10 3      114698 2017-03-06 20:03:55 1                         210. 
## # ... with 4 more variables: cumnum_prod_till_prev_order <int>,
## #   rev_prev_ord <dbl>, num_prod_prev_ord <int>,
## #   num_days_till_last_order <dbl>
```


Now lets join the above data with order_agg data (order data aggregated at order-prodcat1 level). We will join order_agg with prodcat1 on the inequality. Since we are planning to predict prodcat1, we will put order_agg on the left and perform a left join.

```r
joined_df<-left_join(order_agg,online_spread_e2e1cat,by="custno") 
```

```
## Warning: Column `custno` joining factors with different levels, coercing to
## character vector
```

```r
joined_df[,13:36][is.na(joined_df[,13:36])] <- 0



joined_df$dt<- as.character(joined_df$dt)

joined_df$dt<-ifelse(joined_df$dt>joined_df$orderdate,NA,joined_df$dt)


joined_df$dt <- strptime(x = as.character(joined_df$dt),
                                format = "%Y-%m-%d %H:%M:%S")
joined_df$dt <- as.POSIXct(joined_df$dt, tz = "", format="%Y-%m-%d %H:%M:%S")

joined_df[c("session","visitor","event2_1","event2_2","event2_3","event2_4","event2_5","event2_6","event2_7","event2_8","event2_9","event2_10","event1_1","event1_2","event1_4","event1_5","event1_6","event1_7","event1_8","event1_9","event1_10","event1_11","event1_0","category_1","category_2","category_3")]<-lapply(joined_df[c("session","visitor","event2_1","event2_2","event2_3","event2_4","event2_5","event2_6","event2_7","event2_8","event2_9","event2_10","event1_1","event1_2","event1_4","event1_5","event1_6","event1_7","event1_8","event1_9","event1_10","event1_11","event1_0","category_1","category_2","category_3")], function(x) ifelse(is.na(joined_df$dt)==TRUE,NA,x))

joined_df<- joined_df %>% filter(orderdate>=dt | is.na(dt)==TRUE)
head(joined_df,20)
```

```
## # A tibble: 20 x 36
## # Groups:   custno, prodcat1 [5]
##    custno ordno  orderdate           prodcat1 cumrev_till_prev_order
##    <chr>  <fct>  <dttm>              <fct>                     <dbl>
##  1 1      1      2017-06-12 08:27:59 1                           0  
##  2 2      70848  2016-01-26 14:14:21 1                           0  
##  3 2      112461 2016-01-26 14:23:21 1                          23.8
##  4 2      2      2016-03-29 13:04:27 1                         118. 
##  5 2      15645  2016-08-29 10:50:33 1                         132. 
##  6 2      78052  2017-05-02 12:24:02 1                         282. 
##  7 2      78052  2017-05-02 12:24:02 7                           0  
##  8 3      3      2017-01-31 19:16:49 1                           0  
##  9 3      119946 2017-02-27 19:34:25 1                          95.8
## 10 3      114698 2017-03-06 20:03:55 1                         210. 
## 11 3      112224 2017-03-14 07:58:39 1                         267. 
## 12 3      98773  2017-03-23 10:29:42 1                         325. 
## 13 3      12236  2017-03-30 16:16:14 1                         424. 
## 14 4      67324  2017-06-29 21:20:08 1                           0  
## 15 4      115090 2017-06-30 14:01:15 1                          16.1
## 16 4      69265  2017-07-01 21:05:43 1                          16.7
## 17 4      42271  2017-07-26 21:30:54 1                          62.2
## 18 4      4      2017-08-14 21:22:10 1                         199. 
## 19 4      41424  2017-09-05 00:51:03 1                         233. 
## 20 4      96179  2017-09-07 18:29:30 1                         271. 
## # ... with 31 more variables: cumnum_prod_till_prev_order <int>,
## #   rev_prev_ord <dbl>, num_prod_prev_ord <int>,
## #   num_days_till_last_order <dbl>, session <int>, visitor <int>,
## #   dt <dttm>, event2_1 <dbl>, event2_2 <dbl>, event2_3 <dbl>,
## #   event2_4 <dbl>, event2_5 <dbl>, event2_6 <dbl>, event2_7 <dbl>,
## #   event2_8 <dbl>, event2_9 <dbl>, event2_10 <dbl>, event1_1 <dbl>,
## #   event1_2 <dbl>, event1_4 <dbl>, event1_5 <dbl>, event1_6 <dbl>,
## #   event1_7 <dbl>, event1_8 <dbl>, event1_9 <dbl>, event1_10 <dbl>,
## #   event1_11 <dbl>, event1_0 <dbl>, category_1 <dbl>, category_2 <dbl>,
## #   category_3 <dbl>
```
as you can see for a customer with a given orderno and prodcat1, only the online activity on or before the time of the order is joined. Now lets calculate the difference between orderdate and dt.
Note: We are also incluing the order data for which no online data is available


```r
joined_df$date_diff<- difftime(joined_df$orderdate,joined_df$dt,units = 'days')
head(joined_df,20)
```

```
## # A tibble: 20 x 37
## # Groups:   custno, prodcat1 [5]
##    custno ordno  orderdate           prodcat1 cumrev_till_prev_order
##    <chr>  <fct>  <dttm>              <fct>                     <dbl>
##  1 1      1      2017-06-12 08:27:59 1                           0  
##  2 2      70848  2016-01-26 14:14:21 1                           0  
##  3 2      112461 2016-01-26 14:23:21 1                          23.8
##  4 2      2      2016-03-29 13:04:27 1                         118. 
##  5 2      15645  2016-08-29 10:50:33 1                         132. 
##  6 2      78052  2017-05-02 12:24:02 1                         282. 
##  7 2      78052  2017-05-02 12:24:02 7                           0  
##  8 3      3      2017-01-31 19:16:49 1                           0  
##  9 3      119946 2017-02-27 19:34:25 1                          95.8
## 10 3      114698 2017-03-06 20:03:55 1                         210. 
## 11 3      112224 2017-03-14 07:58:39 1                         267. 
## 12 3      98773  2017-03-23 10:29:42 1                         325. 
## 13 3      12236  2017-03-30 16:16:14 1                         424. 
## 14 4      67324  2017-06-29 21:20:08 1                           0  
## 15 4      115090 2017-06-30 14:01:15 1                          16.1
## 16 4      69265  2017-07-01 21:05:43 1                          16.7
## 17 4      42271  2017-07-26 21:30:54 1                          62.2
## 18 4      4      2017-08-14 21:22:10 1                         199. 
## 19 4      41424  2017-09-05 00:51:03 1                         233. 
## 20 4      96179  2017-09-07 18:29:30 1                         271. 
## # ... with 32 more variables: cumnum_prod_till_prev_order <int>,
## #   rev_prev_ord <dbl>, num_prod_prev_ord <int>,
## #   num_days_till_last_order <dbl>, session <int>, visitor <int>,
## #   dt <dttm>, event2_1 <dbl>, event2_2 <dbl>, event2_3 <dbl>,
## #   event2_4 <dbl>, event2_5 <dbl>, event2_6 <dbl>, event2_7 <dbl>,
## #   event2_8 <dbl>, event2_9 <dbl>, event2_10 <dbl>, event1_1 <dbl>,
## #   event1_2 <dbl>, event1_4 <dbl>, event1_5 <dbl>, event1_6 <dbl>,
## #   event1_7 <dbl>, event1_8 <dbl>, event1_9 <dbl>, event1_10 <dbl>,
## #   event1_11 <dbl>, event1_0 <dbl>, category_1 <dbl>, category_2 <dbl>,
## #   category_3 <dbl>, date_diff <time>
```

We are assuming that only the last 30 days of online activity drives buying behaviour. Therefor we will only consider online activities and browsing category that occured 30 days before a transaction.


```r
joined_df1 <- joined_df %>% filter(date_diff<=30 | is.na(date_diff)==TRUE)
joined_df2 <- joined_df %>% filter(date_diff>30)
joined_df2[,13:36]=0
joined_df_new <- rbind(joined_df1,joined_df2)
joined_df_new[, 13:36][is.na(joined_df_new[, 13:36])] <- 0

head(joined_df_new,20)
```

```
## # A tibble: 20 x 37
## # Groups:   custno, prodcat1 [5]
##    custno ordno  orderdate           prodcat1 cumrev_till_prev_order
##    <chr>  <fct>  <dttm>              <fct>                     <dbl>
##  1 1      1      2017-06-12 08:27:59 1                           0  
##  2 2      70848  2016-01-26 14:14:21 1                           0  
##  3 2      112461 2016-01-26 14:23:21 1                          23.8
##  4 2      2      2016-03-29 13:04:27 1                         118. 
##  5 2      15645  2016-08-29 10:50:33 1                         132. 
##  6 2      78052  2017-05-02 12:24:02 1                         282. 
##  7 2      78052  2017-05-02 12:24:02 7                           0  
##  8 3      3      2017-01-31 19:16:49 1                           0  
##  9 3      119946 2017-02-27 19:34:25 1                          95.8
## 10 3      114698 2017-03-06 20:03:55 1                         210. 
## 11 3      112224 2017-03-14 07:58:39 1                         267. 
## 12 3      98773  2017-03-23 10:29:42 1                         325. 
## 13 3      12236  2017-03-30 16:16:14 1                         424. 
## 14 4      67324  2017-06-29 21:20:08 1                           0  
## 15 4      115090 2017-06-30 14:01:15 1                          16.1
## 16 4      69265  2017-07-01 21:05:43 1                          16.7
## 17 4      42271  2017-07-26 21:30:54 1                          62.2
## 18 4      4      2017-08-14 21:22:10 1                         199. 
## 19 4      41424  2017-09-05 00:51:03 1                         233. 
## 20 4      96179  2017-09-07 18:29:30 1                         271. 
## # ... with 32 more variables: cumnum_prod_till_prev_order <int>,
## #   rev_prev_ord <dbl>, num_prod_prev_ord <int>,
## #   num_days_till_last_order <dbl>, session <int>, visitor <int>,
## #   dt <dttm>, event2_1 <dbl>, event2_2 <dbl>, event2_3 <dbl>,
## #   event2_4 <dbl>, event2_5 <dbl>, event2_6 <dbl>, event2_7 <dbl>,
## #   event2_8 <dbl>, event2_9 <dbl>, event2_10 <dbl>, event1_1 <dbl>,
## #   event1_2 <dbl>, event1_4 <dbl>, event1_5 <dbl>, event1_6 <dbl>,
## #   event1_7 <dbl>, event1_8 <dbl>, event1_9 <dbl>, event1_10 <dbl>,
## #   event1_11 <dbl>, event1_0 <dbl>, category_1 <dbl>, category_2 <dbl>,
## #   category_3 <dbl>, date_diff <time>
```
What we have done above is for date difference greater than 30 days, we have changed the events flags (event2_1, event2_2 etc.) and to browsing category flags to 0 as we are not considering those events to have an impact on the buying behaviour. Now we will aggregate the data by taking the count of each events and categories. This count will be the number of times that event has happened (or browsing category used) in the past 30 days before a transaction.

Now we will aggregate the data to order-prodcat1 level


```r
df<- joined_df_new %>% group_by(custno,ordno,orderdate,prodcat1,cumrev_till_prev_order,cumnum_prod_till_prev_order,rev_prev_ord,num_prod_prev_ord,num_days_till_last_order) %>% 
  summarise(sum_category_1 = sum(category_1),
            sum_category_2 = sum(category_2),
            sum_category_3 = sum(category_3),
            sum_event1_1 = sum(event1_1),
            sum_event1_2 = sum(event1_2),
            sum_event1_4 = sum(event1_4),
            sum_event1_5 = sum(event1_5),
            sum_event1_6 = sum(event1_6),
            sum_event1_7 = sum(event1_7),
            sum_event1_8 = sum(event1_8),
            sum_event1_9 = sum(event1_9),
            sum_event1_10 = sum(event1_10),
            sum_event1_11 = sum(event1_11),
            sum_event2_1 = sum(event2_1),
            sum_event2_2 = sum(event2_2),
            sum_event2_3 = sum(event2_3),
            sum_event2_4 = sum(event2_4),
            sum_event2_5 = sum(event2_5),
            sum_event2_6 = sum(event2_6),
            sum_event2_7 = sum(event2_7),
            sum_event2_8 = sum(event2_8),
            sum_event2_9 = sum(event2_9),
            sum_event2_10 = sum(event2_10),
            avg_date_diff = mean(date_diff,na.rm = TRUE)
            )
```


```r
head(as.data.frame(df))
```

```
##   custno ordno           orderdate prodcat1 cumrev_till_prev_order
## 1      1     1 2017-06-12 08:27:59        1                   0.00
## 2     10    10 2017-04-09 23:38:53        2                   0.00
## 3    100   101 2017-11-22 04:52:43        1                 383.57
## 4    100   101 2017-11-22 04:52:43        2                 749.94
## 5    100  4234 2018-12-10 17:57:38        1                 862.94
## 6    100 21787 2018-07-23 18:43:40        2                 840.15
##   cumnum_prod_till_prev_order rev_prev_ord num_prod_prev_ord
## 1                           0         0.00                 0
## 2                           0         0.00                 0
## 3                           7        55.48                 1
## 4                           8        56.45                 1
## 5                          11       216.97                 2
## 6                          10        90.21                 2
##   num_days_till_last_order sum_category_1 sum_category_2 sum_category_3
## 1                       NA              0              0              0
## 2                       NA              0              0              0
## 3                 359.4787              0              0              0
## 4                 113.6654              0              0              0
## 5                 369.0288              0              0              0
## 6                 243.5354              0              0              0
##   sum_event1_1 sum_event1_2 sum_event1_4 sum_event1_5 sum_event1_6
## 1            0            0            0            0            0
## 2            0            0            0            0            0
## 3            0            0            0            0            0
## 4            0            0            0            0            0
## 5            0            0            0            0            0
## 6            0            0            0            0            0
##   sum_event1_7 sum_event1_8 sum_event1_9 sum_event1_10 sum_event1_11
## 1            0            0            0             0             0
## 2            0            0            0             0             0
## 3            0            0            0             0             0
## 4            0            0            0             0             0
## 5            0            0            0             0             0
## 6            0            0            0             0             0
##   sum_event2_1 sum_event2_2 sum_event2_3 sum_event2_4 sum_event2_5
## 1            0            0            0            0            0
## 2            0            0            0            0            0
## 3            0            0            0            0            0
## 4            0            0            0            0            0
## 5            0            0            0            0            0
## 6            0            0            0            0            0
##   sum_event2_6 sum_event2_7 sum_event2_8 sum_event2_9 sum_event2_10
## 1            0            0            0            0             0
## 2            0            0            0            0             0
## 3            0            0            0            0             0
## 4            0            0            0            0             0
## 5            0            0            0            0             0
## 6            0            0            0            0             0
##   avg_date_diff
## 1      NaN days
## 2 184.5084 days
## 3 570.4986 days
## 4 570.4986 days
## 5 954.0437 days
## 6 814.0340 days
```

Another metric we can include is revenue per product for cumuative value as well as previous orders

```r
df$cumrev_per_prod_till_prev_order <- ifelse(df$cumrev_till_prev_order==0,0,df$cumrev_till_prev_order/df$cumnum_prod_till_prev_order)

df$rev_per_prod_prev_ord <- ifelse(df$rev_prev_ord==0,0,df$rev_prev_ord/df$num_prod_prev_ord)
df[,c('cumrev_till_prev_order','cumnum_prod_till_prev_order','rev_prev_ord','num_prod_prev_ord')]<-NULL
```



```r
df$avg_date_diff<-as.numeric(df$avg_date_diff)
```

Breaking down orderdate to month, quarter, day-of-week, week of the year and hour

```r
df$order_month<-format(df$orderdate,"%m")
df$order_week_of_year<-strftime(df$orderdate,format="%W")
library(lubridate)
df$order_qtr<-quarter(df$orderdate)
df$order_day_of_week<-weekdays(df$orderdate,abbreviate = TRUE)
df$order_hour_of_day<-format(df$orderdate,"%H")

# converting the above features to factors
df[c('order_month','order_week_of_year','order_qtr','order_day_of_week','order_hour_of_day')] <- lapply(df[c('order_month','order_week_of_year','order_qtr','order_day_of_week','order_hour_of_day')], factor)
```


We have created the folllowing features by manipulating and combining order and online data.

```r
head(as.data.frame(df))
```

```
##   custno ordno           orderdate prodcat1 num_days_till_last_order
## 1      1     1 2017-06-12 08:27:59        1                       NA
## 2     10    10 2017-04-09 23:38:53        2                       NA
## 3    100   101 2017-11-22 04:52:43        1                 359.4787
## 4    100   101 2017-11-22 04:52:43        2                 113.6654
## 5    100  4234 2018-12-10 17:57:38        1                 369.0288
## 6    100 21787 2018-07-23 18:43:40        2                 243.5354
##   sum_category_1 sum_category_2 sum_category_3 sum_event1_1 sum_event1_2
## 1              0              0              0            0            0
## 2              0              0              0            0            0
## 3              0              0              0            0            0
## 4              0              0              0            0            0
## 5              0              0              0            0            0
## 6              0              0              0            0            0
##   sum_event1_4 sum_event1_5 sum_event1_6 sum_event1_7 sum_event1_8
## 1            0            0            0            0            0
## 2            0            0            0            0            0
## 3            0            0            0            0            0
## 4            0            0            0            0            0
## 5            0            0            0            0            0
## 6            0            0            0            0            0
##   sum_event1_9 sum_event1_10 sum_event1_11 sum_event2_1 sum_event2_2
## 1            0             0             0            0            0
## 2            0             0             0            0            0
## 3            0             0             0            0            0
## 4            0             0             0            0            0
## 5            0             0             0            0            0
## 6            0             0             0            0            0
##   sum_event2_3 sum_event2_4 sum_event2_5 sum_event2_6 sum_event2_7
## 1            0            0            0            0            0
## 2            0            0            0            0            0
## 3            0            0            0            0            0
## 4            0            0            0            0            0
## 5            0            0            0            0            0
## 6            0            0            0            0            0
##   sum_event2_8 sum_event2_9 sum_event2_10 avg_date_diff
## 1            0            0             0           NaN
## 2            0            0             0      184.5084
## 3            0            0             0      570.4986
## 4            0            0             0      570.4986
## 5            0            0             0      954.0437
## 6            0            0             0      814.0340
##   cumrev_per_prod_till_prev_order rev_per_prod_prev_ord order_month
## 1                         0.00000                 0.000          06
## 2                         0.00000                 0.000          04
## 3                        54.79571                55.480          11
## 4                        93.74250                56.450          11
## 5                        78.44909               108.485          12
## 6                        84.01500                45.105          07
##   order_week_of_year order_qtr order_day_of_week order_hour_of_day
## 1                 24         2               Mon                08
## 2                 14         2               Sun                23
## 3                 47         4               Wed                04
## 4                 47         4               Wed                04
## 5                 50         4               Mon                17
## 6                 30         3               Mon                18
```

The summary of the data is

```r
summary(df)
```

```
##     custno              ordno          orderdate                  
##  Length:179504      1974   :     5   Min.   :2016-01-01 05:05:14  
##  Class :character   2794   :     5   1st Qu.:2016-09-25 19:15:03  
##  Mode  :character   3657   :     5   Median :2017-06-17 17:01:47  
##                     4577   :     5   Mean   :2017-06-24 14:26:40  
##                     5614   :     5   3rd Qu.:2018-03-17 07:09:34  
##                     5949   :     5   Max.   :2019-01-02 23:54:58  
##                     (Other):179474                                
##  prodcat1  num_days_till_last_order sum_category_1     sum_category_2   
##  1:29875   Min.   :   0.00          Min.   : 0.00000   Min.   : 0.0000  
##  2:53510   1st Qu.:   9.65          1st Qu.: 0.00000   1st Qu.: 0.0000  
##  3:36538   Median :  35.19          Median : 0.00000   Median : 0.0000  
##  4:28188   Mean   : 100.02          Mean   : 0.07668   Mean   : 0.3121  
##  5: 9724   3rd Qu.: 127.97          3rd Qu.: 0.00000   3rd Qu.: 0.0000  
##  7:21669   Max.   :1081.32          Max.   :25.00000   Max.   :32.0000  
##            NA's   :99244                                                
##  sum_category_3    sum_event1_1       sum_event1_2      sum_event1_4    
##  Min.   : 0.000   Min.   : 0.00000   Min.   :0.00000   Min.   :0.00000  
##  1st Qu.: 0.000   1st Qu.: 0.00000   1st Qu.:0.00000   1st Qu.:0.00000  
##  Median : 0.000   Median : 0.00000   Median :0.00000   Median :0.00000  
##  Mean   : 1.205   Mean   : 0.07388   Mean   :0.03828   Mean   :0.03738  
##  3rd Qu.: 1.000   3rd Qu.: 0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000  
##  Max.   :55.000   Max.   :12.00000   Max.   :9.00000   Max.   :8.00000  
##                                                                         
##   sum_event1_5       sum_event1_6      sum_event1_7      sum_event1_8    
##  Min.   :0.000000   Min.   :0.00000   Min.   :0.00000   Min.   :0.00000  
##  1st Qu.:0.000000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000  
##  Median :0.000000   Median :0.00000   Median :0.00000   Median :0.00000  
##  Mean   :0.003755   Mean   :0.02283   Mean   :0.02489   Mean   :0.01826  
##  3rd Qu.:0.000000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000  
##  Max.   :4.000000   Max.   :4.00000   Max.   :8.00000   Max.   :8.00000  
##                                                                          
##   sum_event1_9      sum_event1_10      sum_event1_11      sum_event2_1    
##  Min.   :0.000000   Min.   :0.000000   Min.   :0.00000   Min.   : 0.0000  
##  1st Qu.:0.000000   1st Qu.:0.000000   1st Qu.:0.00000   1st Qu.: 0.0000  
##  Median :0.000000   Median :0.000000   Median :0.00000   Median : 0.0000  
##  Mean   :0.006345   Mean   :0.002201   Mean   :0.01613   Mean   : 0.1134  
##  3rd Qu.:0.000000   3rd Qu.:0.000000   3rd Qu.:0.00000   3rd Qu.: 0.0000  
##  Max.   :3.000000   Max.   :2.000000   Max.   :7.00000   Max.   :10.0000  
##                                                                           
##   sum_event2_2      sum_event2_3      sum_event2_4     sum_event2_5    
##  Min.   :0.00000   Min.   : 0.0000   Min.   : 0.000   Min.   :0.00000  
##  1st Qu.:0.00000   1st Qu.: 0.0000   1st Qu.: 0.000   1st Qu.:0.00000  
##  Median :0.00000   Median : 0.0000   Median : 0.000   Median :0.00000  
##  Mean   :0.02092   Mean   : 0.2485   Mean   : 0.202   Mean   :0.06925  
##  3rd Qu.:0.00000   3rd Qu.: 0.0000   3rd Qu.: 0.000   3rd Qu.:0.00000  
##  Max.   :5.00000   Max.   :14.0000   Max.   :14.000   Max.   :7.00000  
##                                                                        
##   sum_event2_6      sum_event2_7      sum_event2_8      sum_event2_9    
##  Min.   :0.00000   Min.   : 0.0000   Min.   : 0.0000   Min.   : 0.0000  
##  1st Qu.:0.00000   1st Qu.: 0.0000   1st Qu.: 0.0000   1st Qu.: 0.0000  
##  Median :0.00000   Median : 0.0000   Median : 0.0000   Median : 0.0000  
##  Mean   :0.04667   Mean   : 0.5932   Mean   : 0.2288   Mean   : 0.0656  
##  3rd Qu.:0.00000   3rd Qu.: 1.0000   3rd Qu.: 0.0000   3rd Qu.: 0.0000  
##  Max.   :8.00000   Max.   :23.0000   Max.   :15.0000   Max.   :14.0000  
##                                                                         
##  sum_event2_10      avg_date_diff     cumrev_per_prod_till_prev_order
##  Min.   :0.000000   Min.   :   0.00   Min.   :  0.00                 
##  1st Qu.:0.000000   1st Qu.:  75.36   1st Qu.:  0.00                 
##  Median :0.000000   Median : 204.83   Median :  0.00                 
##  Mean   :0.006212   Mean   : 254.28   Mean   : 33.41                 
##  3rd Qu.:0.000000   3rd Qu.: 390.34   3rd Qu.: 72.40                 
##  Max.   :6.000000   Max.   :1095.07   Max.   :149.98                 
##                     NA's   :29613                                    
##  rev_per_prod_prev_ord  order_month    order_week_of_year order_qtr
##  Min.   :  0.00        01     :18526   01     :  5089     1:46963  
##  1st Qu.:  0.00        05     :16640   48     :  5053     2:46829  
##  Median :  0.00        12     :16408   49     :  4751     3:45956  
##  Mean   : 33.47        03     :16101   09     :  4591     4:39756  
##  3rd Qu.: 67.78        09     :15724   47     :  4414              
##  Max.   :150.00        06     :15556   36     :  4387              
##                        (Other):80549   (Other):151219              
##  order_day_of_week order_hour_of_day
##  Fri:15476         19     : 14212   
##  Mon:25323         18     : 13804   
##  Sat:24936         20     : 12312   
##  Sun:41578         17     : 11324   
##  Thu:21252         12     : 10465   
##  Tue:23498         13     : 10457   
##  Wed:27441         (Other):106930
```

###3. Feature Selection

Lets look at the correlation between cumrev_per_prod_till_prev_order and rev_per_prod_prev_ord.

```r
cor(df[,!names(df) %in% c("custno","ordno","orderdate","prodcat1","order_month","order_day_of_week","order_week_of_year","order_qtr","order_hour_of_day")])
```

```
##                                 num_days_till_last_order sum_category_1
## num_days_till_last_order                               1             NA
## sum_category_1                                        NA     1.00000000
## sum_category_2                                        NA     0.05975836
## sum_category_3                                        NA     0.05260110
## sum_event1_1                                          NA     0.11519467
## sum_event1_2                                          NA     0.05190649
## sum_event1_4                                          NA     0.10420820
## sum_event1_5                                          NA     0.03355437
## sum_event1_6                                          NA     0.05581213
## sum_event1_7                                          NA     0.07045872
## sum_event1_8                                          NA     0.03299169
## sum_event1_9                                          NA     0.06742637
## sum_event1_10                                         NA     0.02922822
## sum_event1_11                                         NA     0.05451642
## sum_event2_1                                          NA     0.16400344
## sum_event2_2                                          NA     0.04884802
## sum_event2_3                                          NA     0.14995912
## sum_event2_4                                          NA     0.15237110
## sum_event2_5                                          NA     0.10477098
## sum_event2_6                                          NA     0.04255567
## sum_event2_7                                          NA     0.21068381
## sum_event2_8                                          NA     0.18515614
## sum_event2_9                                          NA     0.11550660
## sum_event2_10                                         NA     0.05779337
## avg_date_diff                                         NA             NA
## cumrev_per_prod_till_prev_order                       NA     0.03737429
## rev_per_prod_prev_ord                                 NA     0.03192756
##                                 sum_category_2 sum_category_3 sum_event1_1
## num_days_till_last_order                    NA             NA           NA
## sum_category_1                      0.05975836     0.05260110   0.11519467
## sum_category_2                      1.00000000     0.07901509   0.22927557
## sum_category_3                      0.07901509     1.00000000   0.30293379
## sum_event1_1                        0.22927557     0.30293379   1.00000000
## sum_event1_2                        0.09216206     0.28816102   0.01454894
## sum_event1_4                        0.09576677     0.23244340   0.01704493
## sum_event1_5                        0.18529400     0.08942683   0.04491075
## sum_event1_6                        0.15496074     0.19627540   0.11837647
## sum_event1_7                        0.23373840     0.18079784   0.09322193
## sum_event1_8                        0.12766962     0.09466138   0.03762944
## sum_event1_9                        0.15114544     0.06032940   0.02045415
## sum_event1_10                       0.02385869     0.09058968   0.03318439
## sum_event1_11                       0.14847478     0.18215765   0.01259576
## sum_event2_1                        0.39534755     0.36162396   0.32771739
## sum_event2_2                        0.29485586     0.08132352   0.16819145
## sum_event2_3                        0.28656827     0.69703151   0.19241359
## sum_event2_4                        0.34116828     0.57082973   0.21609841
## sum_event2_5                        0.28599343     0.32745477   0.13628671
## sum_event2_6                        0.15267308     0.34292059   0.06951080
## sum_event2_7                        0.37149552     0.79573575   0.37683835
## sum_event2_8                        0.25965403     0.63490593   0.17223948
## sum_event2_9                        0.19156168     0.38873940   0.15633985
## sum_event2_10                       0.14661643     0.10350555   0.01087952
## avg_date_diff                               NA             NA           NA
## cumrev_per_prod_till_prev_order     0.09659084     0.10296331   0.08923517
## rev_per_prod_prev_ord               0.08114139     0.09057785   0.07746783
##                                 sum_event1_2 sum_event1_4 sum_event1_5
## num_days_till_last_order                  NA           NA           NA
## sum_category_1                   0.051906492  0.104208203  0.033554375
## sum_category_2                   0.092162057  0.095766765  0.185293999
## sum_category_3                   0.288161024  0.232443398  0.089426830
## sum_event1_1                     0.014548941  0.017044927  0.044910752
## sum_event1_2                     1.000000000  0.106593784  0.009136500
## sum_event1_4                     0.106593784  1.000000000  0.008942886
## sum_event1_5                     0.009136500  0.008942886  1.000000000
## sum_event1_6                     0.043475175  0.045010854  0.017995510
## sum_event1_7                     0.005957926  0.022378707  0.036413875
## sum_event1_8                     0.021141806  0.016567662  0.034946981
## sum_event1_9                     0.003642405  0.012833633  0.196932767
## sum_event1_10                    0.009662797  0.038373035  0.023348874
## sum_event1_11                    0.047494832  0.042077314  0.076340648
## sum_event2_1                     0.126342528  0.078807816  0.096687948
## sum_event2_2                     0.022814858  0.032483246  0.085843617
## sum_event2_3                     0.193812238  0.171011851  0.089392936
## sum_event2_4                     0.180511178  0.087272039  0.105170291
## sum_event2_5                     0.101470000  0.083240478  0.041504383
## sum_event2_6                     0.067632969  0.068699315  0.033706739
## sum_event2_7                     0.327082131  0.326789177  0.158035173
## sum_event2_8                     0.140534211  0.128928045  0.072376612
## sum_event2_9                     0.093674460  0.053667104  0.034756614
## sum_event2_10                    0.017897243  0.007294684  0.176119546
## avg_date_diff                             NA           NA           NA
## cumrev_per_prod_till_prev_order  0.010570714  0.017304208  0.035784992
## rev_per_prod_prev_ord            0.007987318  0.015210939  0.030221063
##                                 sum_event1_6 sum_event1_7 sum_event1_8
## num_days_till_last_order                  NA           NA           NA
## sum_category_1                   0.055812133  0.070458724  0.032991685
## sum_category_2                   0.154960744  0.233738403  0.127669616
## sum_category_3                   0.196275401  0.180797837  0.094661384
## sum_event1_1                     0.118376472  0.093221932  0.037629442
## sum_event1_2                     0.043475175  0.005957926  0.021141806
## sum_event1_4                     0.045010854  0.022378707  0.016567662
## sum_event1_5                     0.017995510  0.036413875  0.034946981
## sum_event1_6                     1.000000000  0.039636173  0.041811518
## sum_event1_7                     0.039636173  1.000000000  0.031112866
## sum_event1_8                     0.041811518  0.031112866  1.000000000
## sum_event1_9                     0.014583815  0.015925018  0.014960042
## sum_event1_10                    0.016737109  0.049604709  0.008740903
## sum_event1_11                    0.025817024  0.019213420  0.018179601
## sum_event2_1                     0.138671178  0.147040927  0.067910687
## sum_event2_2                     0.057584136  0.072103152  0.027882213
## sum_event2_3                     0.144491018  0.143533072  0.097118603
## sum_event2_4                     0.125912701  0.163673961  0.116403423
## sum_event2_5                     0.083644601  0.150389508  0.037910115
## sum_event2_6                     0.049668450  0.034505390  0.033354643
## sum_event2_7                     0.264960597  0.270992280  0.116866673
## sum_event2_8                     0.122953320  0.131704969  0.098336034
## sum_event2_9                     0.087029808  0.106293839  0.054326345
## sum_event2_10                    0.009332018  0.017093671  0.022601455
## avg_date_diff                             NA           NA           NA
## cumrev_per_prod_till_prev_order  0.022992106  0.048528785  0.013275841
## rev_per_prod_prev_ord            0.019222640  0.039821325  0.009511783
##                                 sum_event1_9 sum_event1_10 sum_event1_11
## num_days_till_last_order                  NA            NA            NA
## sum_category_1                   0.067426374   0.029228220    0.05451642
## sum_category_2                   0.151145437   0.023858692    0.14847478
## sum_category_3                   0.060329405   0.090589681    0.18215765
## sum_event1_1                     0.020454153   0.033184391    0.01259576
## sum_event1_2                     0.003642405   0.009662797    0.04749483
## sum_event1_4                     0.012833633   0.038373035    0.04207731
## sum_event1_5                     0.196932767   0.023348874    0.07634065
## sum_event1_6                     0.014583815   0.016737109    0.02581702
## sum_event1_7                     0.015925018   0.049604709    0.01921342
## sum_event1_8                     0.014960042   0.008740903    0.01817960
## sum_event1_9                     1.000000000   0.004387925    0.02605872
## sum_event1_10                    0.004387925   1.000000000    0.02053569
## sum_event1_11                    0.026058724   0.020535692    1.00000000
## sum_event2_1                     0.099686753   0.037515416    0.07193240
## sum_event2_2                     0.141832536   0.056367806    0.01081409
## sum_event2_3                     0.070386038   0.065190966    0.11819904
## sum_event2_4                     0.061545179   0.043629189    0.11121592
## sum_event2_5                     0.056381737   0.025509788    0.04451130
## sum_event2_6                     0.046663616   0.015732748    0.06082702
## sum_event2_7                     0.104482673   0.099699785    0.26412166
## sum_event2_8                     0.067028737   0.051424380    0.11974492
## sum_event2_9                     0.042963772   0.033770146    0.04548187
## sum_event2_10                    0.071174611   0.004239591    0.45808392
## avg_date_diff                             NA            NA            NA
## cumrev_per_prod_till_prev_order  0.031206540   0.008508288    0.01007391
## rev_per_prod_prev_ord            0.027600647   0.007865215    0.01005484
##                                 sum_event2_1 sum_event2_2 sum_event2_3
## num_days_till_last_order                  NA           NA           NA
## sum_category_1                    0.16400344   0.04884802   0.14995912
## sum_category_2                    0.39534755   0.29485586   0.28656827
## sum_category_3                    0.36162396   0.08132352   0.69703151
## sum_event1_1                      0.32771739   0.16819145   0.19241359
## sum_event1_2                      0.12634253   0.02281486   0.19381224
## sum_event1_4                      0.07880782   0.03248325   0.17101185
## sum_event1_5                      0.09668795   0.08584362   0.08939294
## sum_event1_6                      0.13867118   0.05758414   0.14449102
## sum_event1_7                      0.14704093   0.07210315   0.14353307
## sum_event1_8                      0.06791069   0.02788221   0.09711860
## sum_event1_9                      0.09968675   0.14183254   0.07038604
## sum_event1_10                     0.03751542   0.05636781   0.06519097
## sum_event1_11                     0.07193240   0.01081409   0.11819904
## sum_event2_1                      1.00000000   0.16491709   0.26790031
## sum_event2_2                      0.16491709   1.00000000   0.08511606
## sum_event2_3                      0.26790031   0.08511606   1.00000000
## sum_event2_4                      0.24067090   0.07451285   0.38841942
## sum_event2_5                      0.33921453   0.14850369   0.22180114
## sum_event2_6                      0.20138925   0.10505722   0.23357347
## sum_event2_7                      0.29528358   0.08976346   0.55632798
## sum_event2_8                      0.23102153   0.08022814   0.43606937
## sum_event2_9                      0.15307721   0.06487092   0.21722918
## sum_event2_10                     0.06973166   0.01221502   0.09121469
## avg_date_diff                             NA           NA           NA
## cumrev_per_prod_till_prev_order   0.09227581   0.04056525   0.08875024
## rev_per_prod_prev_ord             0.07749117   0.03459262   0.07729935
##                                 sum_event2_4 sum_event2_5 sum_event2_6
## num_days_till_last_order                  NA           NA           NA
## sum_category_1                    0.15237110   0.10477098   0.04255567
## sum_category_2                    0.34116828   0.28599343   0.15267308
## sum_category_3                    0.57082973   0.32745477   0.34292059
## sum_event1_1                      0.21609841   0.13628671   0.06951080
## sum_event1_2                      0.18051118   0.10147000   0.06763297
## sum_event1_4                      0.08727204   0.08324048   0.06869931
## sum_event1_5                      0.10517029   0.04150438   0.03370674
## sum_event1_6                      0.12591270   0.08364460   0.04966845
## sum_event1_7                      0.16367396   0.15038951   0.03450539
## sum_event1_8                      0.11640342   0.03791011   0.03335464
## sum_event1_9                      0.06154518   0.05638174   0.04666362
## sum_event1_10                     0.04362919   0.02550979   0.01573275
## sum_event1_11                     0.11121592   0.04451130   0.06082702
## sum_event2_1                      0.24067090   0.33921453   0.20138925
## sum_event2_2                      0.07451285   0.14850369   0.10505722
## sum_event2_3                      0.38841942   0.22180114   0.23357347
## sum_event2_4                      1.00000000   0.17797305   0.12239740
## sum_event2_5                      0.17797305   1.00000000   0.27849308
## sum_event2_6                      0.12239740   0.27849308   1.00000000
## sum_event2_7                      0.47066984   0.22327941   0.20742510
## sum_event2_8                      0.36524952   0.19044655   0.18095639
## sum_event2_9                      0.30811130   0.11103355   0.07453614
## sum_event2_10                     0.09254652   0.02866873   0.02601103
## avg_date_diff                             NA           NA           NA
## cumrev_per_prod_till_prev_order   0.09585444   0.06096073   0.01976141
## rev_per_prod_prev_ord             0.08400979   0.05189747   0.01556599
##                                 sum_event2_7 sum_event2_8 sum_event2_9
## num_days_till_last_order                  NA           NA           NA
## sum_category_1                    0.21068381   0.18515614   0.11550660
## sum_category_2                    0.37149552   0.25965403   0.19156168
## sum_category_3                    0.79573575   0.63490593   0.38873940
## sum_event1_1                      0.37683835   0.17223948   0.15633985
## sum_event1_2                      0.32708213   0.14053421   0.09367446
## sum_event1_4                      0.32678918   0.12892804   0.05366710
## sum_event1_5                      0.15803517   0.07237661   0.03475661
## sum_event1_6                      0.26496060   0.12295332   0.08702981
## sum_event1_7                      0.27099228   0.13170497   0.10629384
## sum_event1_8                      0.11686667   0.09833603   0.05432635
## sum_event1_9                      0.10448267   0.06702874   0.04296377
## sum_event1_10                     0.09969979   0.05142438   0.03377015
## sum_event1_11                     0.26412166   0.11974492   0.04548187
## sum_event2_1                      0.29528358   0.23102153   0.15307721
## sum_event2_2                      0.08976346   0.08022814   0.06487092
## sum_event2_3                      0.55632798   0.43606937   0.21722918
## sum_event2_4                      0.47066984   0.36524952   0.30811130
## sum_event2_5                      0.22327941   0.19044655   0.11103355
## sum_event2_6                      0.20742510   0.18095639   0.07453614
## sum_event2_7                      1.00000000   0.49205418   0.30352810
## sum_event2_8                      0.49205418   1.00000000   0.25329970
## sum_event2_9                      0.30352810   0.25329970   1.00000000
## sum_event2_10                     0.12694000   0.07973688   0.02847559
## avg_date_diff                             NA           NA           NA
## cumrev_per_prod_till_prev_order   0.10435709   0.07829894   0.11248283
## rev_per_prod_prev_ord             0.09129023   0.06764350   0.09764213
##                                 sum_event2_10 avg_date_diff
## num_days_till_last_order                   NA            NA
## sum_category_1                    0.057793373            NA
## sum_category_2                    0.146616429            NA
## sum_category_3                    0.103505548            NA
## sum_event1_1                      0.010879520            NA
## sum_event1_2                      0.017897243            NA
## sum_event1_4                      0.007294684            NA
## sum_event1_5                      0.176119546            NA
## sum_event1_6                      0.009332018            NA
## sum_event1_7                      0.017093671            NA
## sum_event1_8                      0.022601455            NA
## sum_event1_9                      0.071174611            NA
## sum_event1_10                     0.004239591            NA
## sum_event1_11                     0.458083923            NA
## sum_event2_1                      0.069731659            NA
## sum_event2_2                      0.012215024            NA
## sum_event2_3                      0.091214686            NA
## sum_event2_4                      0.092546519            NA
## sum_event2_5                      0.028668733            NA
## sum_event2_6                      0.026011026            NA
## sum_event2_7                      0.126940002            NA
## sum_event2_8                      0.079736877            NA
## sum_event2_9                      0.028475585            NA
## sum_event2_10                     1.000000000            NA
## avg_date_diff                              NA             1
## cumrev_per_prod_till_prev_order   0.015234987            NA
## rev_per_prod_prev_ord             0.014600962            NA
##                                 cumrev_per_prod_till_prev_order
## num_days_till_last_order                                     NA
## sum_category_1                                      0.037374286
## sum_category_2                                      0.096590835
## sum_category_3                                      0.102963309
## sum_event1_1                                        0.089235169
## sum_event1_2                                        0.010570714
## sum_event1_4                                        0.017304208
## sum_event1_5                                        0.035784992
## sum_event1_6                                        0.022992106
## sum_event1_7                                        0.048528785
## sum_event1_8                                        0.013275841
## sum_event1_9                                        0.031206540
## sum_event1_10                                       0.008508288
## sum_event1_11                                       0.010073913
## sum_event2_1                                        0.092275805
## sum_event2_2                                        0.040565252
## sum_event2_3                                        0.088750244
## sum_event2_4                                        0.095854445
## sum_event2_5                                        0.060960725
## sum_event2_6                                        0.019761409
## sum_event2_7                                        0.104357094
## sum_event2_8                                        0.078298936
## sum_event2_9                                        0.112482827
## sum_event2_10                                       0.015234987
## avg_date_diff                                                NA
## cumrev_per_prod_till_prev_order                     1.000000000
## rev_per_prod_prev_ord                               0.880044013
##                                 rev_per_prod_prev_ord
## num_days_till_last_order                           NA
## sum_category_1                            0.031927560
## sum_category_2                            0.081141387
## sum_category_3                            0.090577852
## sum_event1_1                              0.077467828
## sum_event1_2                              0.007987318
## sum_event1_4                              0.015210939
## sum_event1_5                              0.030221063
## sum_event1_6                              0.019222640
## sum_event1_7                              0.039821325
## sum_event1_8                              0.009511783
## sum_event1_9                              0.027600647
## sum_event1_10                             0.007865215
## sum_event1_11                             0.010054840
## sum_event2_1                              0.077491173
## sum_event2_2                              0.034592618
## sum_event2_3                              0.077299347
## sum_event2_4                              0.084009789
## sum_event2_5                              0.051897473
## sum_event2_6                              0.015565994
## sum_event2_7                              0.091290228
## sum_event2_8                              0.067643502
## sum_event2_9                              0.097642128
## sum_event2_10                             0.014600962
## avg_date_diff                                      NA
## cumrev_per_prod_till_prev_order           0.880044013
## rev_per_prod_prev_ord                     1.000000000
```
This is a very high correlation.

```r
cor(df$rev_per_prod_prev_ord,df$cumrev_per_prod_till_prev_order)
```

```
## [1] 0.880044
```
Therefore, we will choose only one metric as a predictor, which is rev_per_prod_prev_ord.

```r
df$cumrev_per_prod_till_prev_order <- NULL
```

For categorical variables, we will perform the chi-square test of association. We will compare various columns derived from orderdate. 

```r
#"custno","ordno","orderdate","prodcat1","order_month",
chisq.test(df$order_month,df$order_qtr)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  df$order_month and df$order_qtr
## X-squared = 538510, df = 33, p-value < 2.2e-16
```

```r
chisq.test(df$order_day_of_week,df$order_hour_of_day)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  df$order_day_of_week and df$order_hour_of_day
## X-squared = 2343.5, df = 138, p-value < 2.2e-16
```

```r
chisq.test(df$order_day_of_week,df$order_week_of_year)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  df$order_day_of_week and df$order_week_of_year
## X-squared = 13783, df = 318, p-value < 2.2e-16
```
since the p-value is < 2.2e-16 is less than the cut-off value of 0.05, we can reject the null hypothesis in favor of alternative hypothesis and conclude, that the variables are dependent to each other. Therefore, we will only consider one variable for moedeling, which is order_month and delete the rest.


```r
df[,c("order_day_of_week","order_week_of_year","order_qtr","order_hour_of_day")]<-NULL

#removing id columns
df[,c("custno","ordno","orderdate")]<-NULL
```
We have removed combination of highly correlated variables and highly associated variable. During the model building phase we will encounter some variables that are not significant predictors to the response variable. In those steps, we will still be performing feature selection by removing insignificant predictors and keeping significant ones.

##4. Model Designing and sampling

We have prepared the data that now we can use to fit a model. We are trying to fit a model which predicts what product category-1 is a user most likely to buy based on his past online-activity as well as his buying behaviour. We will split the data into training and test sample. We will use the training sample to fit different classification models and test sample to measure the performance of the model. The train-test split is 70-30 split, i.e., 70% of the data is used for training and 30% for testing.


```r
## 70% of the sample size
smp_size <- floor(0.70 * nrow(df))

## set the seed to make your partition reproducible
set.seed(1234)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]
```

Our response mode is prodcat1. We need to make sure that all the classes of prodcat1 are present in train and test samples

```r
table(train$prodcat1)
```

```
## 
##     1     2     3     4     5     7 
## 20958 37549 25483 19725  6735 15202
```


```r
table(test$prodcat1)
```

```
## 
##     1     2     3     4     5     7 
##  8917 15961 11055  8463  2989  6467
```

In the next section, we will use different modelling techniques to fit a multi-class classifcation models. We will use the following algorithms
*1. Logistic regression
*2. Random Forest Regression

Logistic regression works well for linear relationships and has high interpretability, whereas Random Forest can fit non-linear relationships at the cost of interpretation.

The metric we will use to determine the best model is test-AUC (Area under the curve of the ROC curve for test sample)

##5. Model Generation and Evaluation


```r
#remove unnecessary objects to free-up space
rm(list=ls()[! ls() %in% c("df","train","test","online","order")])
gc()
```

```
##            used  (Mb) gc trigger   (Mb)   max used   (Mb)
## Ncells  3741627 199.9   12584350  672.1   21654566 1156.5
## Vcells 38106321 290.8  902416991 6884.9 1127983495 8605.9
```

In this step we will fit different models on the training sample and predict for the test sample 

### Logistic Regrssion
We will fit a mutinomial logistic regression model. We will ignore num_days_till_last_order and avg_date_diff in logistic regression as they have NA values. We could impute those values but given the time constraint, we will choose to ignore them. 

```r
train1<- train
test1<- test
train1[,c("num_days_till_last_order","avg_date_diff")]<-NULL
test1[,c("num_days_till_last_order","avg_date_diff")]<-NULL

library(nnet)
# Fit the model
glm <- nnet::multinom(prodcat1 ~., data = train1)
```

```
## # weights:  222 (180 variable)
## initial  value 225138.160827 
## iter  10 value 209815.533936
## iter  20 value 205119.752836
## iter  30 value 204321.142561
## iter  40 value 204067.811070
## iter  50 value 203700.943905
## iter  60 value 202405.934709
## iter  70 value 202102.056221
## iter  80 value 201805.121196
## iter  90 value 201463.660808
## iter 100 value 201388.633742
## final  value 201388.633742 
## stopped after 100 iterations
```

```r
# Summarize the model
summary(glm)
```

```
## Call:
## nnet::multinom(formula = prodcat1 ~ ., data = train1)
## 
## Coefficients:
##   (Intercept) sum_category_1 sum_category_2 sum_category_3 sum_event1_1
## 2  0.92685719    -0.25976008    -0.21846016    -0.15653215  -0.08278422
## 3 -0.01608086     0.55858920     0.59798482     0.52497554   0.20562568
## 4  0.09354858    -0.14542476    -0.23535552    -0.01287877  -0.35066917
## 5 -0.09783563     0.03098618     0.01213515     0.14495016  -0.03894853
## 7 -0.47879203     0.10261985     0.12321927     0.05740280   0.22817106
##   sum_event1_2 sum_event1_4 sum_event1_5 sum_event1_6 sum_event1_7
## 2 -0.008287228   0.08589900    0.3483203   0.07178375   0.16623964
## 3  0.007877831  -0.02182606    0.5742525   0.09367701   0.02787943
## 4  0.039188349   0.14924121   -0.2160810  -0.01427105  -0.05842807
## 5 -0.012121087   0.15994628    0.1169564  -0.03531872   0.01817052
## 7  0.018129963   0.02396771    0.3739307   0.09535578  -0.05755044
##   sum_event1_8 sum_event1_9 sum_event1_10 sum_event1_11 sum_event2_1
## 2    0.1702947   0.02029412    0.20154455    0.29036966  0.265228758
## 3    0.5000148   0.47598324    0.27961129   -0.06743240 -0.457797817
## 4    0.3526541   0.26509099   -0.39961027    0.08591697  0.210933364
## 5    0.5071345   0.67521085   -0.03899443    0.06919961 -0.031773671
## 7    0.3329135   0.35575033    0.15869189   -0.07209661  0.004402445
##   sum_event2_2 sum_event2_3 sum_event2_4 sum_event2_5 sum_event2_6
## 2   0.03010450  -0.19965515   0.16281669   0.23054718   0.18446050
## 3  -0.46889749  -0.59725872  -0.49968157  -0.51793239  -0.47668799
## 4  -0.33883230   0.17676008  -0.07520873   0.30360473   0.68110953
## 5  -0.13686801  -0.05680272  -0.15363637  -0.01972262   0.14761678
## 7  -0.08104179  -0.13424732  -0.05260009  -0.04518582  -0.05837584
##   sum_event2_7 sum_event2_8 sum_event2_9 sum_event2_10
## 2   0.12066793   0.23223625   0.26787968   0.158417789
## 3  -0.57219192  -0.44079642  -0.40219788  -0.564197358
## 4   0.07898073   0.13992636  -0.17966297   0.002290715
## 5  -0.14574022   0.03230797  -0.07199110  -0.597848602
## 7  -0.10786851   0.04617076   0.09463892  -0.265430886
##   rev_per_prod_prev_ord order_month02 order_month03 order_month04
## 2          0.0053273175    0.06670542   -0.45679105    -0.6132285
## 3          0.0071552592    0.28015000   -0.02144347    -0.4644083
## 4          0.0032106312   -0.15238535   -0.06834588    -0.7880644
## 5          0.0009114869   -1.19706345   -2.29136974    -3.0809798
## 7          0.0048534076    0.12131887   -0.15204343    -0.4647843
##   order_month05 order_month06 order_month07 order_month08 order_month09
## 2   -0.37591534    -0.1397130    0.15680028    0.08873367   -0.05275182
## 3   -0.02060058     0.3571499    0.53346173    0.52023102    0.27756253
## 4   -0.52247659     0.3878547    0.08487551   -0.02500962    0.27402639
## 5   -2.83136757    -2.2205060   -1.96490684   -1.50390169   -2.04718750
## 7   -0.06303049     0.2404515    0.31380264    0.35897002    0.11152224
##   order_month10 order_month11 order_month12
## 2   -0.86105906    -1.6318948   -1.22583565
## 3   -0.39333021    -1.1316142   -0.63581421
## 4   -0.92151454    -1.6003521   -1.05343164
## 5   -0.92900361    -0.9209117   -0.11145054
## 7   -0.06629529    -0.4029704   -0.03868497
## 
## Std. Errors:
##   (Intercept) sum_category_1 sum_category_2 sum_category_3 sum_event1_1
## 2  0.02945365     0.17988685     0.17922265     0.17902124   0.03476902
## 3  0.03393588     0.03838143     0.03580656     0.03552862   0.03130057
## 4  0.03353468     0.09035515     0.08901041     0.08852505   0.04016560
## 5  0.03582922     0.03272326     0.02350971     0.02113128   0.04332606
## 7  0.03878963     0.04114675     0.03837509     0.03808549   0.03365701
##   sum_event1_2 sum_event1_4 sum_event1_5 sum_event1_6 sum_event1_7
## 2   0.04343567   0.04584102    0.1786825   0.06164166   0.05155020
## 3   0.04323451   0.04819045    0.1654328   0.06104808   0.05080723
## 4   0.04235464   0.04569478    0.2270410   0.06646928   0.05944062
## 5   0.05688463   0.05848935    0.2592145   0.09005482   0.07406142
## 7   0.04832331   0.05311936    0.1847943   0.06780314   0.05758238
##   sum_event1_8 sum_event1_9 sum_event1_10 sum_event1_11 sum_event2_1
## 2   0.07101928    0.1366665     0.1949923    0.06984685   0.18072836
## 3   0.06691153    0.1229846     0.1916470    0.07820674   0.04422801
## 4   0.07303186    0.1446586     0.2266176    0.07769831   0.09302578
## 5   0.08587598    0.1685918     0.3068402    0.11487523   0.04243847
## 7   0.07535617    0.1364164     0.2244613    0.09060666   0.04783762
##   sum_event2_2 sum_event2_3 sum_event2_4 sum_event2_5 sum_event2_6
## 2   0.18900791   0.18014630   0.18014371   0.18180052   0.18463314
## 3   0.06451373   0.03974465   0.04044870   0.04928406   0.05686271
## 4   0.11676610   0.09038571   0.09104484   0.09507335   0.09726251
## 5   0.09207128   0.03185839   0.03446989   0.05159846   0.05848379
## 7   0.07171956   0.04298014   0.04384333   0.05366494   0.06322264
##   sum_event2_7 sum_event2_8 sum_event2_9 sum_event2_10
## 2   0.17925412   0.17959376   0.18214907     0.2094330
## 3   0.03731785   0.04041361   0.04874715     0.1178595
## 4   0.08934447   0.09055962   0.09726662     0.1503641
## 5   0.02674471   0.03299538   0.05096873     0.1970621
## 7   0.04013347   0.04360766   0.05263676     0.1395643
##   rev_per_prod_prev_ord order_month02 order_month03 order_month04
## 2          0.0002169191    0.04530423    0.04083369    0.03930674
## 3          0.0002276530    0.05078367    0.04551531    0.04568981
## 4          0.0002525343    0.05303935    0.04543052    0.04728159
## 5          0.0003425137    0.07165402    0.08381117    0.10439219
## 7          0.0002559565    0.05893696    0.05309151    0.05305669
##   order_month05 order_month06 order_month07 order_month08 order_month09
## 2    0.03960859    0.04359164    0.04377803    0.04450789    0.04296455
## 3    0.04462479    0.04782732    0.04815532    0.04870952    0.04775199
## 4    0.04688303    0.04730412    0.04997951    0.05126673    0.04738420
## 5    0.09946907    0.09460615    0.08901990    0.07707928    0.08703584
## 7    0.05139468    0.05493363    0.05560687    0.05584312    0.05537830
##   order_month10 order_month11 order_month12
## 2    0.05119891    0.04178776    0.04281439
## 3    0.05518960    0.04628534    0.04657055
## 4    0.06209392    0.05085691    0.04976748
## 5    0.06894489    0.04962812    0.04784662
## 7    0.06084246    0.04930314    0.05038297
## 
## Residual Deviance: 402777.3 
## AIC: 403137.3
```

Now we'll calculate Z score and p-Value for the variables in the model.

```r
z <- summary(glm)$coefficients/summary(glm)$standard.errors

p <- (1 - pnorm(abs(z), 0, 1))*2
p
```

```
##   (Intercept) sum_category_1 sum_category_2 sum_category_3 sum_event1_1
## 2 0.000000000     0.14873357    0.222870126   3.819127e-01 1.726682e-02
## 3 0.635599695     0.00000000    0.000000000   0.000000e+00 5.052136e-11
## 4 0.005277209     0.10751156    0.008190001   8.843306e-01 0.000000e+00
## 5 0.006321725     0.34368146    0.605731344   6.909806e-12 3.686723e-01
## 7 0.000000000     0.01263137    0.001323118   1.317571e-01 1.207567e-11
##   sum_event1_2 sum_event1_4 sum_event1_5 sum_event1_6 sum_event1_7
## 2    0.8486876  0.060951705 0.0512498907    0.2442081  0.001260558
## 3    0.8554166  0.650611737 0.0005181145    0.1249118  0.583191095
## 4    0.3548394  0.001090609 0.3412356557    0.8300002  0.325624493
## 5    0.8312631  0.006245192 0.6518488442    0.6949169  0.806190113
## 7    0.7075262  0.651841992 0.0430219338    0.1596166  0.317579050
##   sum_event1_8 sum_event1_9 sum_event1_10 sum_event1_11 sum_event2_1
## 2 1.649090e-02 8.819531e-01    0.30132202  3.221245e-05    0.1422252
## 3 7.860379e-14 1.087157e-04    0.14456744  3.885596e-01    0.0000000
## 4 1.373785e-06 6.687282e-02    0.07783843  2.688232e-01    0.0233614
## 5 3.517298e-09 6.201632e-05    0.89887406  5.469150e-01    0.4540382
## 7 9.967981e-06 9.111915e-03    0.47957268  4.262007e-01    0.9266750
##   sum_event2_2 sum_event2_3 sum_event2_4 sum_event2_5 sum_event2_6
## 2 8.734511e-01   0.26773468 3.660932e-01  0.204750584 3.177632e-01
## 3 3.643752e-13   0.00000000 0.000000e+00  0.000000000 0.000000e+00
## 4 3.710209e-03   0.05050998 4.087686e-01  0.001406208 2.509104e-12
## 5 1.371353e-01   0.07459038 8.306909e-06  0.702288756 1.160071e-02
## 7 2.584840e-01   0.00178730 2.302448e-01  0.399788556 3.558313e-01
##   sum_event2_7 sum_event2_8 sum_event2_9 sum_event2_10
## 2 5.008411e-01    0.1959696 1.413826e-01  4.494017e-01
## 3 0.000000e+00    0.0000000 2.220446e-16  1.692645e-06
## 4 3.766949e-01    0.1223149 6.472998e-02  9.878451e-01
## 5 5.056589e-08    0.3274978 1.578157e-01  2.414884e-03
## 7 7.193637e-03    0.2897016 7.218293e-02  5.719032e-02
##   rev_per_prod_prev_ord order_month02 order_month03 order_month04
## 2           0.000000000  1.409160e-01   0.000000000             0
## 3           0.000000000  3.457442e-08   0.637550332             0
## 4           0.000000000  4.065143e-03   0.132477285             0
## 5           0.007786981  0.000000e+00   0.000000000             0
## 7           0.000000000  3.954682e-02   0.004185934             0
##   order_month05 order_month06 order_month07 order_month08 order_month09
## 2     0.0000000  1.350432e-03  3.413508e-04  4.618905e-02  2.195225e-01
## 3     0.6443396  8.171241e-14  0.000000e+00  0.000000e+00  6.151504e-09
## 4     0.0000000  2.220446e-16  8.946884e-02  6.256678e-01  7.334729e-09
## 5     0.0000000  0.000000e+00  0.000000e+00  0.000000e+00  0.000000e+00
## 7     0.2200477  1.202539e-05  1.668839e-08  1.291347e-10  4.402782e-02
##   order_month10 order_month11 order_month12
## 2  0.000000e+00  0.000000e+00    0.00000000
## 3  1.026734e-12  0.000000e+00    0.00000000
## 4  0.000000e+00  0.000000e+00    0.00000000
## 5  0.000000e+00  0.000000e+00    0.01984163
## 7  2.758796e-01  2.220446e-16    0.44259505
```

Calculating the the odds ratio coefficient for each predictor 

```r
exp(coef(glm))
```

```
##   (Intercept) sum_category_1 sum_category_2 sum_category_3 sum_event1_1
## 2   2.5265562      0.7712366      0.8037555      0.8551040    0.9205498
## 3   0.9840478      1.7482044      1.8184506      1.6904175    1.2282933
## 4   1.0980639      0.8646549      0.7902898      0.9872038    0.7042167
## 5   0.9067979      1.0314713      1.0122091      1.1559820    0.9618002
## 7   0.6195313      1.1080701      1.1311324      1.0590823    1.2563002
##   sum_event1_2 sum_event1_4 sum_event1_5 sum_event1_6 sum_event1_7
## 2    0.9917470    1.0896963    1.4166860    1.0744230    1.1808560
## 3    1.0079089    0.9784104    1.7758026    1.0982050    1.0282717
## 4    1.0399663    1.1609530    0.8056701    0.9858303    0.9432461
## 5    0.9879521    1.1734478    1.1240704    0.9652977    1.0183366
## 7    1.0182953    1.0242572    1.4534364    1.1000502    0.9440743
##   sum_event1_8 sum_event1_9 sum_event1_10 sum_event1_11 sum_event2_1
## 2     1.185654     1.020501     1.2232907     1.3369216    1.3037292
## 3     1.648746     1.609596     1.3226156     0.9347909    0.6326754
## 4     1.422839     1.303550     0.6705813     1.0897158    1.2348301
## 5     1.660526     1.964447     0.9617561     1.0716501    0.9687258
## 7     1.395027     1.427251     1.1719768     0.9304410    1.0044122
##   sum_event2_2 sum_event2_3 sum_event2_4 sum_event2_5 sum_event2_6
## 2    1.0305622    0.8190131    1.1768210    1.2592889    1.2025695
## 3    0.6256917    0.5503181    0.6067238    0.5957511    0.6208362
## 4    0.7126019    1.1933447    0.9275499    1.3547335    1.9760690
## 5    0.8720853    0.9447804    0.8575838    0.9804706    1.1590686
## 7    0.9221552    0.8743738    0.9487594    0.9558199    0.9432954
##   sum_event2_7 sum_event2_8 sum_event2_9 sum_event2_10
## 2    1.1282502    1.2614177    1.3071898     1.1716556
## 3    0.5642872    0.6435237    0.6688484     0.5688165
## 4    1.0821835    1.1501891    0.8355518     1.0022933
## 5    0.8643822    1.0328355    0.9305392     0.5499936
## 7    0.8977456    1.0472532    1.0992619     0.7668754
##   rev_per_prod_prev_ord order_month02 order_month03 order_month04
## 2              1.005342     1.0689805     0.6333127    0.54159947
## 3              1.007181     1.3233283     0.9787848    0.62850687
## 4              1.003216     0.8586573     0.9339374    0.45472411
## 5              1.000912     0.3020800     0.1011278    0.04591425
## 7              1.004865     1.1289849     0.8589510    0.62827063
##   order_month05 order_month06 order_month07 order_month08 order_month09
## 2     0.6866605     0.8696078     1.1697620     1.0927896     0.9486154
## 3     0.9796102     1.4292500     1.7048237     1.6824163     1.3199086
## 4     0.5930500     1.4738156     1.0885815     0.9753005     1.3152495
## 5     0.0589322     0.1085542     0.1401689     0.2222613     0.1290975
## 7     0.9389148     1.2718232     1.3686196     1.4318539     1.1179786
##   order_month10 order_month11 order_month12
## 2     0.4227142     0.1955587     0.2935123
## 3     0.6748059     0.3225122     0.5295042
## 4     0.3979159     0.2018254     0.3487389
## 5     0.3949470     0.3981559     0.8945356
## 7     0.9358545     0.6683319     0.9620537
```

Calculate Variable importance (absolute value of t-statistic)

```r
library(caret)
varimp_glm<-as.data.frame(varImp(glm))
varimp_glm$predictor<-rownames(varimp_glm)
colnames(varimp_glm)[1]<-'importance'

varimp_glm %>% arrange(desc(importance))
```

```
##    importance             predictor
## 1  5.68774320         order_month11
## 2  5.41146528         order_month04
## 3  3.81339058         order_month05
## 4  3.34567502         order_month06
## 5  3.17120271         order_month10
## 6  3.06521701         order_month12
## 7  3.05384700         order_month07
## 8  2.98999357         order_month03
## 9  2.76305048         order_month09
## 10 2.49684603         order_month08
## 11 1.86301165          sum_event1_8
## 12 1.81762310         order_month02
## 13 1.79232953          sum_event1_9
## 14 1.62954085          sum_event1_5
## 15 1.58818535         sum_event2_10
## 16 1.54825064          sum_event2_6
## 17 1.18715492        sum_category_2
## 18 1.16472398          sum_event2_3
## 19 1.11699275          sum_event2_5
## 20 1.09738007        sum_category_1
## 21 1.07845244         sum_event1_10
## 22 1.05574408          sum_event2_2
## 23 1.02544930          sum_event2_7
## 24 1.01637055          sum_event2_9
## 25 0.97013605          sum_event2_1
## 26 0.94394345          sum_event2_4
## 27 0.90619866          sum_event1_1
## 28 0.89673941        sum_category_3
## 29 0.89143776          sum_event2_8
## 30 0.58501525         sum_event1_11
## 31 0.44088025          sum_event1_4
## 32 0.32826810          sum_event1_7
## 33 0.31040629          sum_event1_6
## 34 0.08560446          sum_event1_2
## 35 0.02145810 rev_per_prod_prev_ord
```
We can see that order month is the most important parameter. This is evident from the time series trends that we observed while exploring the data. Other important variables were sum_event1_8 ,sum_event1_8, and sum_event1_5 

Let's check for fitted values now.

```r
head(fitted(glm))
```

```
##           1         2         3          4          5          7
## 1 0.1362019 0.3347793 0.1507843 0.14873830 0.13157759 0.09791863
## 2 0.2078132 0.2037986 0.1576077 0.09417795 0.17682666 0.15977590
## 3 0.1982757 0.2971629 0.1782158 0.21250755 0.01799667 0.09584135
## 4 0.1406811 0.3090922 0.1978609 0.22767029 0.01384818 0.11084742
## 5 0.1918412 0.3069652 0.1847759 0.19673760 0.01759233 0.10208775
## 6 0.1406811 0.3090922 0.1978609 0.22767029 0.01384818 0.11084742
```


predicting on the test data

```r
predict_tst=as.data.frame(predict(glm,test1,type="probs"))
colnames(predict_tst)<-sapply(colnames(predict_tst), function(x) paste('logit_',x,sep = ""))
predict_tst$predicted_class<-apply(predict_tst,1,which.max)
predict_tst$predicted_class<-ifelse(predict_tst$predicted_class==6,7,predict_tst$predicted_class)
head(predict_tst)
```

```
##     logit_1   logit_2   logit_3    logit_4    logit_5   logit_7
## 1 0.1406811 0.3090922 0.1978609 0.22767029 0.01384818 0.1108474
## 2 0.3020623 0.1545535 0.1508060 0.09982670 0.12735199 0.1653995
## 3 0.1137407 0.2836578 0.3157333 0.12195955 0.01317366 0.1517349
## 4 0.1918412 0.3069652 0.1847759 0.19673760 0.01759233 0.1020877
## 5 0.1550755 0.2313376 0.2066006 0.09049224 0.14177102 0.1747231
## 6 0.1305176 0.3721515 0.2088617 0.16667300 0.01657632 0.1052199
##   predicted_class
## 1               2
## 2               1
## 3               3
## 4               2
## 5               2
## 6               2
```

```r
#test1=cbind(test1, predict_tst)
```

Creating confusion matrix for test sample

```r
tab<-table(predict_tst$predicted_class,test1$prodcat1)
library(caret) 
conf<-confusionMatrix(tab)
conf
```

```
## Confusion Matrix and Statistics
## 
##    
##         1     2     3     4     5     7
##   1  1744  1091  1018   617   919  1090
##   2  6219 13703  8312  6233  1550  4407
##   3   489   607  1155   326   176   590
##   4   336   446   419  1167   146   246
##   5   108    91   118   114   183   113
##   7    21    23    33     6    15    21
## 
## Overall Statistics
##                                           
##                Accuracy : 0.3337          
##                  95% CI : (0.3298, 0.3377)
##     No Information Rate : 0.2964          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.0945          
##  Mcnemar's Test P-Value : < 2.2e-16       
## 
## Statistics by Class:
## 
##                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 7
## Sensitivity           0.19558   0.8585  0.10448  0.13789 0.061224 0.003247
## Specificity           0.89463   0.2948  0.94887  0.96490 0.989305 0.997932
## Pos Pred Value        0.26918   0.3390  0.34550  0.42283 0.251719 0.176471
## Neg Pred Value        0.84858   0.8318  0.80400  0.85720 0.947181 0.880036
## Prevalence            0.16558   0.2964  0.20528  0.15715 0.055504 0.120088
## Detection Rate        0.03239   0.2545  0.02145  0.02167 0.003398 0.000390
## Detection Prevalence  0.12031   0.7506  0.06208  0.05125 0.013500 0.002210
## Balanced Accuracy     0.54510   0.5767  0.52668  0.55140 0.525265 0.500590
```
Class2 has high sensitivity, which means the model has a higher True Positive Rate for Class2. This means that the model does a good job of predicting whether a customer will will buy product category 2. At the same time it does a poor job of predicting whether a customer is not going to buy a product from prodcat2 or not. This is evident from the specificity of Class2

If we  we look at the sensitivity and specificity across all the other classes, we will see that the sensitivity values are low and specificity values are high. This means that, for the other classes the model does a better job identifying whether a customer is not going to buy a product in that class than predicting whether a customer is going to buy a product in that class



Calculating Test AUC of the ROC curve. The AUC is a measure of how well a model does a job of distinguishing between classes 

```r
library(HandTill2001)
auc(multcap(response=test1$prodcat1,predicted = predict(glm,test1,type="probs")))
```

```
## [1] 0.652249
```
This is a decent value. But it could be improved further by using complex models.

### Random Forest Classification



```r
# train1<- train
# test1<- test
# train1[,c("num_days_till_last_order","avg_date_diff")]<-NULL
# test1[,c("num_days_till_last_order","avg_date_diff")]<-NULL

library(randomForest)
```

```
## randomForest 4.6-14
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

```r
# Fit the model
set.seed(1234)
rf <- randomForest(prodcat1 ~., data = train1, ntree=500, mtry=round(sqrt(ncol(train)-1)), importance=TRUE)
# Summarize the model
rf
```

```
## 
## Call:
##  randomForest(formula = prodcat1 ~ ., data = train1, ntree = 500,      mtry = round(sqrt(ncol(train) - 1)), importance = TRUE) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 5
## 
##         OOB estimate of  error rate: 66.58%
## Confusion matrix:
##      1     2    3    4   5   7 class.error
## 1 4842 12678 1414 1321 359 344   0.7689665
## 2 3180 29391 2379 1889 279 431   0.2172628
## 3 2710 16849 3334 1560 289 741   0.8691677
## 4 1882 12550 1175 3672 278 168   0.8138403
## 5 2048  3063  419  563 472 170   0.9299183
## 7 2540  9327 1965  815 267 288   0.9810551
```



Calculate Variable importance

```r
varImp(rf)
```

```
##                                 1         2          3          4
## sum_category_1         -5.1193393 43.557678 -24.368299  -9.375528
## sum_category_2          5.3160834 67.981092 -18.337293 -11.836191
## sum_category_3         -0.2800313 56.653897 -39.581179  27.917861
## sum_event1_1           17.1193495 31.637741 -12.529545  -5.440176
## sum_event1_2            0.7737313 12.986533  -9.571350  -8.408507
## sum_event1_4           -2.0829410 14.300008  -6.252205 -11.700814
## sum_event1_5            4.8315502  9.508437   4.800939   8.999123
## sum_event1_6            6.0840557 17.179022 -14.475398  -7.191722
## sum_event1_7            0.6309928 12.988774  -4.969941 -15.301061
## sum_event1_8            1.6809914 26.299258 -11.317356 -10.896376
## sum_event1_9            3.8090363 15.657352  -1.720984  -2.980269
## sum_event1_10           4.3594627  3.671061   4.854107  -7.998542
## sum_event1_11           5.2903459 13.209633  -2.836639 -11.024353
## sum_event2_1            7.3919165 27.925765 -18.059779 -14.395289
## sum_event2_2            0.1331095 25.161970 -11.042390  -1.391763
## sum_event2_3          -18.2321937 75.808697 -28.973634  23.285029
## sum_event2_4            2.6477241 28.206531 -14.937120 -35.242783
## sum_event2_5            4.4391053 27.434754 -18.862426  -4.914085
## sum_event2_6            3.0745857 41.172743 -17.347414  28.037381
## sum_event2_7           -4.1280749 34.081676 -23.000965  -9.888816
## sum_event2_8            5.5807164 26.837586 -19.050343 -10.967575
## sum_event2_9            2.5006326 24.550648 -15.510322  -9.932935
## sum_event2_10           6.4064705  8.803003  -5.048304  -7.585508
## rev_per_prod_prev_ord  -8.2350514 29.043473  29.737574  38.254197
## order_month            39.6092151 67.915286  17.223029  60.407128
##                                 5           7
## sum_category_1         -2.3120335 -14.4477247
## sum_category_2         -0.1298790 -17.6503732
## sum_category_3         15.0713591 -38.6677828
## sum_event1_1           -3.7171077  -6.3085222
## sum_event1_2            0.3338238  -2.8592123
## sum_event1_4            6.9362477  -9.8811662
## sum_event1_5           10.6495981  -0.5754956
## sum_event1_6           13.7507759  -8.3775457
## sum_event1_7            3.5977528  -6.7350553
## sum_event1_8           15.3511229  -2.5753725
## sum_event1_9            1.7098612  -8.2161648
## sum_event1_10           0.9822861  -2.0048350
## sum_event1_11           2.3213302  -4.2730299
## sum_event2_1           -3.2902597 -18.8961765
## sum_event2_2            0.3898599 -12.1770148
## sum_event2_3           15.4449986 -23.8456466
## sum_event2_4           -6.7149926 -14.6255530
## sum_event2_5           -3.5118249 -14.1544791
## sum_event2_6            8.6371019 -13.9914822
## sum_event2_7           -0.3076361 -25.7277140
## sum_event2_8            1.9857983 -17.0012923
## sum_event2_9            4.9804470  -9.4825662
## sum_event2_10           3.5674212  -3.2029759
## rev_per_prod_prev_ord  -2.4392634   2.9193443
## order_month           106.0496162   8.1327850
```
Similar to what we have observed in the multinomial logistic regression, we can see that order month is the most important predictor.


predicting on the test data

```r
predict_tst_rf=as.data.frame(predict(rf,test1,type="prob"))
colnames(predict_tst_rf)<-sapply(colnames(predict_tst_rf), function(x) paste('rf_',x,sep = ""))
predict_tst_rf$predicted_class_rf<-apply(predict_tst_rf,1,which.max)
predict_tst_rf$predicted_class_rf<-ifelse(predict_tst_rf$predicted_class_rf==6,7,predict_tst_rf$predicted_class_rf)
head(predict_tst_rf)
```

```
##    rf_1  rf_2  rf_3  rf_4  rf_5  rf_7 predicted_class_rf
## 1 0.000 1.000 0.000 0.000 0.000 0.000                  2
## 2 0.862 0.056 0.020 0.046 0.008 0.008                  1
## 3 0.050 0.298 0.294 0.192 0.016 0.150                  2
## 4 0.002 0.998 0.000 0.000 0.000 0.000                  2
## 5 0.204 0.552 0.064 0.000 0.124 0.056                  2
## 6 0.002 0.998 0.000 0.000 0.000 0.000                  2
```

```r
#test1=cbind(test1, predict_tst_rf)
```

Creating confusion matrix for test sample

```r
tab_rf<-table(predict_tst_rf$predicted_class_rf,test1$prodcat1)
library(caret) 
conf_rf<-confusionMatrix(tab_rf)
conf_rf
```

```
## Confusion Matrix and Statistics
## 
##    
##         1     2     3     4     5     7
##   1  2014  1402  1189   787   945  1108
##   2  5430 12475  7323  5457  1323  3934
##   3   613  1024  1453   501   164   843
##   4   601   780   644  1538   278   331
##   5   111   112   132   104   195   117
##   7   148   168   314    76    84   134
## 
## Overall Statistics
##                                           
##                Accuracy : 0.3307          
##                  95% CI : (0.3267, 0.3347)
##     No Information Rate : 0.2964          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.1037          
##  Mcnemar's Test P-Value : < 2.2e-16       
## 
## Statistics by Class:
## 
##                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 7
## Sensitivity            0.2259   0.7816  0.13143  0.18173 0.065239 0.020721
## Specificity            0.8791   0.3807  0.92651  0.94197 0.988675 0.983328
## Pos Pred Value         0.2705   0.3471  0.31601  0.36865 0.252918 0.145022
## Neg Pred Value         0.8513   0.8054  0.80505  0.86061 0.947363 0.880347
## Prevalence             0.1656   0.2964  0.20528  0.15715 0.055504 0.120088
## Detection Rate         0.0374   0.2317  0.02698  0.02856 0.003621 0.002488
## Detection Prevalence   0.1382   0.6674  0.08538  0.07747 0.014317 0.017158
## Balanced Accuracy      0.5525   0.5811  0.52897  0.56185 0.526957 0.502024
```
The accuracy of the model is similar to the multinomial logisitic regression. Even the sensitivity and specificity across the classes is similar to multinomial logistic regression. Class 2 has high sensitivity and low specificity and all the other classes have low sensitivity and high specificity. 


Calculating Test AUC of the ROC curve

```r
library(HandTill2001)
auc(multcap(response=test$prodcat1,predicted = predict(rf,test1,type="prob")))
```

```
## [1] 0.5773988
```

The random forest model has better fit on the training data, as evident from the model summary, but it does not do a job better than multinomial logistic regression to seperate the classes. One possible reason could be that we did not choose that big of an ensemble. The number of trees that we choose in the forest were only 500. If we increase it, the performance will improve.


### Random Forest with hyper parameter tuning

We tune the hyperparameters of random forest model so that it can converge on a more optimal output. There are two hyper parameters of the random forest

*1. mtry: number of predictors randomly chosen for the decision trees in the forests
*2. ntree: total number of decision trees in the forest.

One caveat though, this process requires a lot of computation and takes some time to converge.

```r
# levels(train1$prodcat1)<-c("cat1","cat2","cat3","cat4","cat5","cat7")
# levels(test1$prodcat1)<-c("cat1","cat2","cat3","cat4","cat5","cat7")
```



```r
# library(tidyverse)
# library(caret)
# library(randomForest)
# customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
# customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
# customRF$grid <- function(x, y, len = NULL, search = "grid") {}
# customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
#   randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
# }
# customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
#   predict(modelFit, newdata)
# customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
#   predict(modelFit, newdata, type = "prob")
# customRF$sort <- function(x) x[order(x[,1]),]
# customRF$levels <- function(x) x$classes
```


```r
# control <- trainControl(method="cv", number=10, #repeats=3, 
#                         classProbs = TRUE)
# tunegrid <- expand.grid(.mtry=c(15:18), .ntree=c(500, 1000))
# set.seed(1234)
# custom <- train(prodcat1~., data=train1, method=customRF, metric="ROC", tuneGrid=tunegrid, trControl=control)
# summary(custom)
# plot(custom)
```

I have included the code but has not run it as it takes a lot of time to run and knit the report.

##6. Summary

To summarise, we had the customer order data as well as customer online activity data. We explored the data and found out that there is a seasonality in the revenue, number of orders, as well as the online activity. It peaks during Nov and Dec and drops during the months of Feb and Oct. We also saw that this trend is consitent across browsing categories as well as differnet online events. We also found out that most of the online activity is done on browsing category 3.

We also tried associating customer order data with online behaviour. While doing so, we made an assumption  
*1. Only the last 30 days of online activity drives buying behaviour for a customer.  
Any activity before that does not affect buying behaviour significantly. Based on this assumption, we have merged the online data with the order data. We were also able to derive som metrics like, count of each online event before the purchase, Number of days since last order, order month etc. We have used this metric to predict product category-1.

The predictive problem we are trying to solve is: Based on historical purchases and online beviour, what is the likelihood that a customer would purchase a product from a product-category1. To fit and measure the performance of the model, we first split the data into training and test sample. 70% of the data is used for training and 30% for measure its performance on unseen data. We have tried multinomial logistic regression, random forest, and random forest with hyper parameter tuning. Both the models performed decent on the test data. The models are good at identifying whether a customer is not going to buy from a particular product-category. The AUC value shows that the model does a decent job of separating classes in logistic regression. To improve the performance of the random forest model, we can perform hyperparameter tuning using a grid-search approach. This process takes a lot of time and computation capacity. I have shared the code for doing that.

With a better predicitve power, we can predict with high accuracy the product category where a customer is mostly likely to shop from. We can use this information to run targeted campaign or provide incentive to buy back from us.
