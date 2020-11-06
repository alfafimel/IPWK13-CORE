IPWK13-CORE - PART 2
================
Elizabeth Josephine
11/04/2020

# PROBLEM DEFINITION

## **a) Specifying the Question**

Perform clustering stating insights drawn from your analysis and
visualizations.

## **b) Defining the metrics for success**

Bivariates and univariate Exploratory data analysis Perform clustering
stating insights drawn from your analysis and visualizations. Upon
implementation, provide comparisons between the approaches learned this
week i.e. K-Means clustering vs Hierarchical clustering highlighting the
strengths and limitations of each approach in the context of your
analysis.  

## **c) Understanding the context**

Kira Plastinina is a Russian brand that is sold through a defunct chain
of retail stores in Russia, Ukraine, Kazakhstan, Belarus, China,
Philippines, and Armenia. The brand’s Sales and Marketing team would
like to understand their customer’s behavior from data that they have
collected over the past year. More specifically, they would like to
learn the characteristics of customer groups.

## **d) Recording the Experimental Design**

1.  Define the question, the metric for success, the context,
    experimental design taken.
2.  Read and explore the given dataset.
3.  Find and deal with outliers, anomalies, and missing data within the
    dataset.
4.  Perform univariate and bivariate analysis.
5.  Perform clustering stating insights drawn from your analysis and
    visualizations.

## **e) Relevance of the data**

The data used for this project is necessary for understanding their
customer’s behavior from data that they have collected over the past
year. More specifically, to learn the characteristics of customer
groups.

\[<http://bit.ly/EcommerceCustomersDataset>\].

# DATA ANALYSIS

## DATA SOURCING

``` r
# loading libraries
library(relaimpo)
```

    ## Loading required package: MASS

    ## Loading required package: boot

    ## Loading required package: survey

    ## Loading required package: grid

    ## Loading required package: Matrix

    ## Loading required package: survival

    ## 
    ## Attaching package: 'survival'

    ## The following object is masked from 'package:boot':
    ## 
    ##     aml

    ## 
    ## Attaching package: 'survey'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     dotchart

    ## Loading required package: mitools

    ## This is the global version of package relaimpo.

    ## If you are a non-US user, a version with the interesting additional metric pmvd is available

    ## from Ulrike Groempings web site at prof.beuth-hochschule.de/groemping.

``` r
library(data.table)
library(ggplot2) # Data visualization
library(ggthemes) # Plot themes
library(plotly) # Interactive data visualizations
```

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:MASS':
    ## 
    ##     select

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
library(dplyr) # Data manipulation
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following object is masked from 'package:MASS':
    ## 
    ##     select

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(psych) # Will be used for correlation visualization
```

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

    ## The following object is masked from 'package:boot':
    ## 
    ##     logit

``` r
# importing our data
# reading our data
df2 <- fread('http://bit.ly/EcommerceCustomersDataset')
df2
```

    ##        Administrative Administrative_Duration Informational
    ##     1:              0                       0             0
    ##     2:              0                       0             0
    ##     3:              0                      -1             0
    ##     4:              0                       0             0
    ##     5:              0                       0             0
    ##    ---                                                     
    ## 12326:              3                     145             0
    ## 12327:              0                       0             0
    ## 12328:              0                       0             0
    ## 12329:              4                      75             0
    ## 12330:              0                       0             0
    ##        Informational_Duration ProductRelated ProductRelated_Duration
    ##     1:                      0              1                0.000000
    ##     2:                      0              2               64.000000
    ##     3:                     -1              1               -1.000000
    ##     4:                      0              2                2.666667
    ##     5:                      0             10              627.500000
    ##    ---                                                              
    ## 12326:                      0             53             1783.791667
    ## 12327:                      0              5              465.750000
    ## 12328:                      0              6              184.250000
    ## 12329:                      0             15              346.000000
    ## 12330:                      0              3               21.250000
    ##        BounceRates  ExitRates PageValues SpecialDay Month OperatingSystems
    ##     1: 0.200000000 0.20000000    0.00000          0   Feb                1
    ##     2: 0.000000000 0.10000000    0.00000          0   Feb                2
    ##     3: 0.200000000 0.20000000    0.00000          0   Feb                4
    ##     4: 0.050000000 0.14000000    0.00000          0   Feb                3
    ##     5: 0.020000000 0.05000000    0.00000          0   Feb                3
    ##    ---                                                                    
    ## 12326: 0.007142857 0.02903061   12.24172          0   Dec                4
    ## 12327: 0.000000000 0.02133333    0.00000          0   Nov                3
    ## 12328: 0.083333333 0.08666667    0.00000          0   Nov                3
    ## 12329: 0.000000000 0.02105263    0.00000          0   Nov                2
    ## 12330: 0.000000000 0.06666667    0.00000          0   Nov                3
    ##        Browser Region TrafficType       VisitorType Weekend Revenue
    ##     1:       1      1           1 Returning_Visitor   FALSE   FALSE
    ##     2:       2      1           2 Returning_Visitor   FALSE   FALSE
    ##     3:       1      9           3 Returning_Visitor   FALSE   FALSE
    ##     4:       2      2           4 Returning_Visitor   FALSE   FALSE
    ##     5:       3      1           4 Returning_Visitor    TRUE   FALSE
    ##    ---                                                             
    ## 12326:       6      1           1 Returning_Visitor    TRUE   FALSE
    ## 12327:       2      1           8 Returning_Visitor    TRUE   FALSE
    ## 12328:       2      1          13 Returning_Visitor    TRUE   FALSE
    ## 12329:       2      3          11 Returning_Visitor   FALSE   FALSE
    ## 12330:       2      1           2       New_Visitor    TRUE   FALSE

## DATA CHECKING

``` r
# previewing the dataset
View(df2)
```

``` r
# previewing the column names
colnames(df2)
```

    ##  [1] "Administrative"          "Administrative_Duration"
    ##  [3] "Informational"           "Informational_Duration" 
    ##  [5] "ProductRelated"          "ProductRelated_Duration"
    ##  [7] "BounceRates"             "ExitRates"              
    ##  [9] "PageValues"              "SpecialDay"             
    ## [11] "Month"                   "OperatingSystems"       
    ## [13] "Browser"                 "Region"                 
    ## [15] "TrafficType"             "VisitorType"            
    ## [17] "Weekend"                 "Revenue"

``` r
# previewing the dataset
class(df2)
```

    ## [1] "data.table" "data.frame"

``` r
# previewing the datatypes of the dataset
sapply(df2, class)
```

    ##          Administrative Administrative_Duration           Informational 
    ##               "integer"               "numeric"               "integer" 
    ##  Informational_Duration          ProductRelated ProductRelated_Duration 
    ##               "numeric"               "integer"               "numeric" 
    ##             BounceRates               ExitRates              PageValues 
    ##               "numeric"               "numeric"               "numeric" 
    ##              SpecialDay                   Month        OperatingSystems 
    ##               "numeric"             "character"               "integer" 
    ##                 Browser                  Region             TrafficType 
    ##               "integer"               "integer"               "integer" 
    ##             VisitorType                 Weekend                 Revenue 
    ##             "character"               "logical"               "logical"

``` r
# previewing the head of the dataset
head(df2, n = 5)
```

    ##    Administrative Administrative_Duration Informational Informational_Duration
    ## 1:              0                       0             0                      0
    ## 2:              0                       0             0                      0
    ## 3:              0                      -1             0                     -1
    ## 4:              0                       0             0                      0
    ## 5:              0                       0             0                      0
    ##    ProductRelated ProductRelated_Duration BounceRates ExitRates PageValues
    ## 1:              1                0.000000        0.20      0.20          0
    ## 2:              2               64.000000        0.00      0.10          0
    ## 3:              1               -1.000000        0.20      0.20          0
    ## 4:              2                2.666667        0.05      0.14          0
    ## 5:             10              627.500000        0.02      0.05          0
    ##    SpecialDay Month OperatingSystems Browser Region TrafficType
    ## 1:          0   Feb                1       1      1           1
    ## 2:          0   Feb                2       2      1           2
    ## 3:          0   Feb                4       1      9           3
    ## 4:          0   Feb                3       2      2           4
    ## 5:          0   Feb                3       3      1           4
    ##          VisitorType Weekend Revenue
    ## 1: Returning_Visitor   FALSE   FALSE
    ## 2: Returning_Visitor   FALSE   FALSE
    ## 3: Returning_Visitor   FALSE   FALSE
    ## 4: Returning_Visitor   FALSE   FALSE
    ## 5: Returning_Visitor    TRUE   FALSE

``` r
# previewing the tail of the dataset
tail(df2, n = 5)
```

    ##    Administrative Administrative_Duration Informational Informational_Duration
    ## 1:              3                     145             0                      0
    ## 2:              0                       0             0                      0
    ## 3:              0                       0             0                      0
    ## 4:              4                      75             0                      0
    ## 5:              0                       0             0                      0
    ##    ProductRelated ProductRelated_Duration BounceRates  ExitRates PageValues
    ## 1:             53                1783.792 0.007142857 0.02903061   12.24172
    ## 2:              5                 465.750 0.000000000 0.02133333    0.00000
    ## 3:              6                 184.250 0.083333333 0.08666667    0.00000
    ## 4:             15                 346.000 0.000000000 0.02105263    0.00000
    ## 5:              3                  21.250 0.000000000 0.06666667    0.00000
    ##    SpecialDay Month OperatingSystems Browser Region TrafficType
    ## 1:          0   Dec                4       6      1           1
    ## 2:          0   Nov                3       2      1           8
    ## 3:          0   Nov                3       2      1          13
    ## 4:          0   Nov                2       2      3          11
    ## 5:          0   Nov                3       2      1           2
    ##          VisitorType Weekend Revenue
    ## 1: Returning_Visitor    TRUE   FALSE
    ## 2: Returning_Visitor    TRUE   FALSE
    ## 3: Returning_Visitor    TRUE   FALSE
    ## 4: Returning_Visitor   FALSE   FALSE
    ## 5:       New_Visitor    TRUE   FALSE

``` r
# checking the structure of the data
str(df2)
```

    ## Classes 'data.table' and 'data.frame':   12330 obs. of  18 variables:
    ##  $ Administrative         : int  0 0 0 0 0 0 0 1 0 0 ...
    ##  $ Administrative_Duration: num  0 0 -1 0 0 0 -1 -1 0 0 ...
    ##  $ Informational          : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Informational_Duration : num  0 0 -1 0 0 0 -1 -1 0 0 ...
    ##  $ ProductRelated         : int  1 2 1 2 10 19 1 1 2 3 ...
    ##  $ ProductRelated_Duration: num  0 64 -1 2.67 627.5 ...
    ##  $ BounceRates            : num  0.2 0 0.2 0.05 0.02 ...
    ##  $ ExitRates              : num  0.2 0.1 0.2 0.14 0.05 ...
    ##  $ PageValues             : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ SpecialDay             : num  0 0 0 0 0 0 0.4 0 0.8 0.4 ...
    ##  $ Month                  : chr  "Feb" "Feb" "Feb" "Feb" ...
    ##  $ OperatingSystems       : int  1 2 4 3 3 2 2 1 2 2 ...
    ##  $ Browser                : int  1 2 1 2 3 2 4 2 2 4 ...
    ##  $ Region                 : int  1 1 9 2 1 1 3 1 2 1 ...
    ##  $ TrafficType            : int  1 2 3 4 4 3 3 5 3 2 ...
    ##  $ VisitorType            : chr  "Returning_Visitor" "Returning_Visitor" "Returning_Visitor" "Returning_Visitor" ...
    ##  $ Weekend                : logi  FALSE FALSE FALSE FALSE TRUE FALSE ...
    ##  $ Revenue                : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
# checking the dimension/shape of the data
dim(df2) # 12330 rows and 18 columns
```

    ## [1] 12330    18

``` r
colnames(df2)
```

    ##  [1] "Administrative"          "Administrative_Duration"
    ##  [3] "Informational"           "Informational_Duration" 
    ##  [5] "ProductRelated"          "ProductRelated_Duration"
    ##  [7] "BounceRates"             "ExitRates"              
    ##  [9] "PageValues"              "SpecialDay"             
    ## [11] "Month"                   "OperatingSystems"       
    ## [13] "Browser"                 "Region"                 
    ## [15] "TrafficType"             "VisitorType"            
    ## [17] "Weekend"                 "Revenue"

``` r
# selecting needed columns
df3 <- subset(df2, select = c("ProductRelated", "ProductRelated_Duration", "PageValues", "Month", "VisitorType", "Weekend"))
colnames(df3)
```

    ## [1] "ProductRelated"          "ProductRelated_Duration"
    ## [3] "PageValues"              "Month"                  
    ## [5] "VisitorType"             "Weekend"

## DATA CLEANING

### Missing Values

``` r
# checking for missing values
sum(is.na(df3))# there are no missing values in the data
```

    ## [1] 28

``` r
# displaying all rows from the dataset which don't contain any missing values 
df4 <- na.omit(df3)
df4
```

    ##        ProductRelated ProductRelated_Duration PageValues Month
    ##     1:              1                0.000000    0.00000   Feb
    ##     2:              2               64.000000    0.00000   Feb
    ##     3:              1               -1.000000    0.00000   Feb
    ##     4:              2                2.666667    0.00000   Feb
    ##     5:             10              627.500000    0.00000   Feb
    ##    ---                                                        
    ## 12312:             53             1783.791667   12.24172   Dec
    ## 12313:              5              465.750000    0.00000   Nov
    ## 12314:              6              184.250000    0.00000   Nov
    ## 12315:             15              346.000000    0.00000   Nov
    ## 12316:              3               21.250000    0.00000   Nov
    ##              VisitorType Weekend
    ##     1: Returning_Visitor   FALSE
    ##     2: Returning_Visitor   FALSE
    ##     3: Returning_Visitor   FALSE
    ##     4: Returning_Visitor   FALSE
    ##     5: Returning_Visitor    TRUE
    ##    ---                          
    ## 12312: Returning_Visitor    TRUE
    ## 12313: Returning_Visitor    TRUE
    ## 12314: Returning_Visitor    TRUE
    ## 12315: Returning_Visitor   FALSE
    ## 12316:       New_Visitor    TRUE

### Duplicates

``` r
# checking for duplicates
duplicated_rows <- df4[duplicated(df4),]
duplicated_rows # there are 740 duplicates in the data
```

    ##      ProductRelated ProductRelated_Duration PageValues Month       VisitorType
    ##   1:              1                   -1.00          0   Feb Returning_Visitor
    ##   2:              1                   -1.00          0   Feb Returning_Visitor
    ##   3:              1                   -1.00          0   Feb Returning_Visitor
    ##   4:              1                   -1.00          0   Feb Returning_Visitor
    ##   5:              1                   -1.00          0   Feb Returning_Visitor
    ##  ---                                                                          
    ## 736:              3                    0.00          0   Nov Returning_Visitor
    ## 737:              1                    0.00          0   Dec Returning_Visitor
    ## 738:              2                    0.00          0   Nov Returning_Visitor
    ## 739:              2                    0.00          0   Nov Returning_Visitor
    ## 740:              3                   21.25          0   Nov       New_Visitor
    ##      Weekend
    ##   1:   FALSE
    ##   2:   FALSE
    ##   3:   FALSE
    ##   4:    TRUE
    ##   5:   FALSE
    ##  ---        
    ## 736:   FALSE
    ## 737:    TRUE
    ## 738:   FALSE
    ## 739:   FALSE
    ## 740:    TRUE

``` r
# showing these unique items and assigning to a variable unique_items below
#df4 <- df3[!duplicated(df3), ]
df5 <- unique(df4)
df5
```

    ##        ProductRelated ProductRelated_Duration PageValues Month
    ##     1:              1                0.000000    0.00000   Feb
    ##     2:              2               64.000000    0.00000   Feb
    ##     3:              1               -1.000000    0.00000   Feb
    ##     4:              2                2.666667    0.00000   Feb
    ##     5:             10              627.500000    0.00000   Feb
    ##    ---                                                        
    ## 11572:             16              503.000000    0.00000   Nov
    ## 11573:             53             1783.791667   12.24172   Dec
    ## 11574:              5              465.750000    0.00000   Nov
    ## 11575:              6              184.250000    0.00000   Nov
    ## 11576:             15              346.000000    0.00000   Nov
    ##              VisitorType Weekend
    ##     1: Returning_Visitor   FALSE
    ##     2: Returning_Visitor   FALSE
    ##     3: Returning_Visitor   FALSE
    ##     4: Returning_Visitor   FALSE
    ##     5: Returning_Visitor    TRUE
    ##    ---                          
    ## 11572: Returning_Visitor   FALSE
    ## 11573: Returning_Visitor    TRUE
    ## 11574: Returning_Visitor    TRUE
    ## 11575: Returning_Visitor    TRUE
    ## 11576: Returning_Visitor   FALSE

### Checking for outliers

``` r
# visualizing any existing outliers using a boxplot
#df1 <- subset(df, select = c("Daily Time Spent on Site", "Age", "Daily Internet Usage", "Male", "Clicked on Ad"))
df6 <- df5 %>% select_if(is.numeric)
boxplot(df6)
```

![](IPWK13_CORE_UNSUPERVISED_LEARNING_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
# there are several outliers in the data and removing them renders the data un-usable for analysis
# i will be working with the data that has outliers
```

# EXPLORATORY DATA ANALYSIS

## Univariate Analysis

### descriptive statistics

``` r
summary(df2)
```

    ##  Administrative   Administrative_Duration Informational   
    ##  Min.   : 0.000   Min.   :  -1.00         Min.   : 0.000  
    ##  1st Qu.: 0.000   1st Qu.:   0.00         1st Qu.: 0.000  
    ##  Median : 1.000   Median :   8.00         Median : 0.000  
    ##  Mean   : 2.318   Mean   :  80.91         Mean   : 0.504  
    ##  3rd Qu.: 4.000   3rd Qu.:  93.50         3rd Qu.: 0.000  
    ##  Max.   :27.000   Max.   :3398.75         Max.   :24.000  
    ##  NA's   :14       NA's   :14              NA's   :14      
    ##  Informational_Duration ProductRelated   ProductRelated_Duration
    ##  Min.   :  -1.00        Min.   :  0.00   Min.   :   -1.0        
    ##  1st Qu.:   0.00        1st Qu.:  7.00   1st Qu.:  185.0        
    ##  Median :   0.00        Median : 18.00   Median :  599.8        
    ##  Mean   :  34.51        Mean   : 31.76   Mean   : 1196.0        
    ##  3rd Qu.:   0.00        3rd Qu.: 38.00   3rd Qu.: 1466.5        
    ##  Max.   :2549.38        Max.   :705.00   Max.   :63973.5        
    ##  NA's   :14             NA's   :14       NA's   :14             
    ##   BounceRates         ExitRates         PageValues        SpecialDay     
    ##  Min.   :0.000000   Min.   :0.00000   Min.   :  0.000   Min.   :0.00000  
    ##  1st Qu.:0.000000   1st Qu.:0.01429   1st Qu.:  0.000   1st Qu.:0.00000  
    ##  Median :0.003119   Median :0.02512   Median :  0.000   Median :0.00000  
    ##  Mean   :0.022152   Mean   :0.04300   Mean   :  5.889   Mean   :0.06143  
    ##  3rd Qu.:0.016684   3rd Qu.:0.05000   3rd Qu.:  0.000   3rd Qu.:0.00000  
    ##  Max.   :0.200000   Max.   :0.20000   Max.   :361.764   Max.   :1.00000  
    ##  NA's   :14         NA's   :14                                           
    ##     Month           OperatingSystems    Browser           Region     
    ##  Length:12330       Min.   :1.000    Min.   : 1.000   Min.   :1.000  
    ##  Class :character   1st Qu.:2.000    1st Qu.: 2.000   1st Qu.:1.000  
    ##  Mode  :character   Median :2.000    Median : 2.000   Median :3.000  
    ##                     Mean   :2.124    Mean   : 2.357   Mean   :3.147  
    ##                     3rd Qu.:3.000    3rd Qu.: 2.000   3rd Qu.:4.000  
    ##                     Max.   :8.000    Max.   :13.000   Max.   :9.000  
    ##                                                                      
    ##   TrafficType    VisitorType         Weekend         Revenue       
    ##  Min.   : 1.00   Length:12330       Mode :logical   Mode :logical  
    ##  1st Qu.: 2.00   Class :character   FALSE:9462      FALSE:10422    
    ##  Median : 2.00   Mode  :character   TRUE :2868      TRUE :1908     
    ##  Mean   : 4.07                                                     
    ##  3rd Qu.: 4.00                                                     
    ##  Max.   :20.00                                                     
    ## 

``` r
# from these summaries, very few people visited the brand website during the weekends as compared to the weekdays.
# the revenue collected from the brand website was little considering that, of the total count of rows and input, only 1908 rendered a 'TRUE' in the revenue section while more than 10,000 entries rendered no revenue. 
```

``` r
# descriptive statistics
# these summaries will provide us with the measures of central tendencies and the measures of dispersion of the numerical columns
describe(df5)
```

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to max; returning -Inf

    ##                         vars     n    mean      sd median trimmed    mad min
    ## ProductRelated             1 11576   33.70   45.21  20.00   24.58  19.27   0
    ## ProductRelated_Duration    2 11576 1272.01 1950.12 671.59  893.09 771.39  -1
    ## PageValues                 3 11576    6.27   19.10   0.00    1.54   0.00   0
    ## Month*                     4 11576    6.17    2.39   7.00    6.36   1.48   1
    ## VisitorType*               5 11576    2.71    0.70   3.00    2.88   0.00   1
    ## Weekend                    6 11576     NaN      NA     NA     NaN     NA Inf
    ##                              max    range  skew kurtosis    se
    ## ProductRelated            705.00   705.00  4.29    30.35  0.42
    ## ProductRelated_Duration 63973.52 63974.52  7.20   134.21 18.13
    ## PageValues                361.76   361.76  6.19    61.75  0.18
    ## Month*                     10.00     9.00 -0.83    -0.39  0.02
    ## VisitorType*                3.00     2.00 -1.99     1.98  0.01
    ## Weekend                     -Inf     -Inf    NA       NA    NA

``` r
# the columns returning null values are those with 'character' datatypes
```

### Univariate Graphical

``` r
# creating a boxplot graph for the variable all the numerical variables
boxplot(df6, ylab = 'frequency', main = 'boxplot for numerical variables')
```

![](IPWK13_CORE_UNSUPERVISED_LEARNING_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
# fetching the columns
ProductRelated <- df6$ProductRelated

# fetching the frequency distribution
ProductRelated_frequency <- table(ProductRelated)

# plotting the bargraph
barplot(ProductRelated_frequency,  xlab = 'ProductRelated', ylab = 'frequency',  main = 'barplot on customer visits to the ProductRelated pages')
```

![](IPWK13_CORE_UNSUPERVISED_LEARNING_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
# fetching the columns
ProductRelated_Duration <- df6$ProductRelated_Duration

# fetching the frequency distribution
ProductRelated_Duration_frequency <- table(ProductRelated_Duration)

# plotting the bargraph
barplot(ProductRelated_Duration_frequency,  xlab = 'ProductRelated_Duration', ylab = 'frequency',  main = 'barplot on duration of customer visits to the ProductRelated pages')
```

![](IPWK13_CORE_UNSUPERVISED_LEARNING_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
# fetching the columns
hist(df6$ProductRelated)
```

![](IPWK13_CORE_UNSUPERVISED_LEARNING_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
# fetching the columns
hist(df6$ProductRelated_Duration)
```

![](IPWK13_CORE_UNSUPERVISED_LEARNING_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
# fetching the columns
hist(df6$PageValues)
```

![](IPWK13_CORE_UNSUPERVISED_LEARNING_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

## Bivariate analysis

``` r
# finding the covariance of the numerical variables in df2
covariance <- df5 %>% select_if(is.numeric)
cov(covariance)
```

    ##                         ProductRelated ProductRelated_Duration PageValues
    ## ProductRelated              2043.67420               75563.101   37.18262
    ## ProductRelated_Duration    75563.10148             3802973.668 1514.59772
    ## PageValues                    37.18262                1514.598  364.83994

``` r
# finding the correlation coefficients
cor(covariance)
```

    ##                         ProductRelated ProductRelated_Duration PageValues
    ## ProductRelated              1.00000000               0.8571213 0.04306088
    ## ProductRelated_Duration     0.85712129               1.0000000 0.04066160
    ## PageValues                  0.04306088               0.0406616 1.00000000

``` r
# creating a scatterplot
df2$`Revenue` <- as.factor(df2$`Revenue`)
ggplot(df2, aes(x=`ProductRelated`,y=`ProductRelated_Duration`, color= `Revenue`)) + geom_point()
```

    ## Warning: Removed 14 rows containing missing values (geom_point).

![](IPWK13_CORE_UNSUPERVISED_LEARNING_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->


# IMPLEMENTATION OF THE SOLUTION

## UNSUPERVISED LEARNING

### K-MEANS CLUSTERING

``` r
# Normalizing the dataset so that no particular attribute 
# has more impact on clustering algorithm than others.
# ---
# 
normalize <- function(x){
  return ((x-min(x)) / (max(x)-min(x)))
}
```

``` r
df6$ProductRelated<- normalize(df6$ProductRelated)
df6$ProductRelated_Duration<- normalize(df6$ProductRelated_Duration)
df6$PageValues<- normalize(df6$PageValues)
head(df6)
```

    ##    ProductRelated ProductRelated_Duration PageValues
    ## 1:    0.001418440            1.563122e-05          0
    ## 2:    0.002836879            1.016029e-03          0
    ## 3:    0.001418440            0.000000e+00          0
    ## 4:    0.002836879            5.731448e-05          0
    ## 5:    0.014184397            9.824223e-03          0
    ## 6:    0.026950355            2.426226e-03          0

``` r
# Applying the K-means clustering algorithm with no. of centroids(k)=3
result<- kmeans(df6,3) 
# Previewing the no. of records in each cluster
result$size 
```

    ## [1]   658 10032   886

``` r
# Getting the value of cluster center datapoint value(3 centers for k=3)
# ---
# 
result$centers 
```

    ##   ProductRelated ProductRelated_Duration  PageValues
    ## 1     0.04379055              0.01881582 0.187263442
    ## 2     0.03311248              0.01379485 0.006538671
    ## 3     0.21702768              0.08981467 0.013440616

``` r
# Getting the cluster vector that shows the cluster where each record falls
result$cluster
```

    ##     [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2
    ##    [37] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##    [73] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##   [109] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##   [145] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 1 2 2 2 2 2 3 2 2 1 1 1 2 2 2 2 2 2 2 2
    ##   [181] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##   [217] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 1 2 2 2 2 1
    ##   [253] 2 2 2 2 2 2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##   [289] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##   [325] 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##   [361] 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2
    ##   [397] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##   [433] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2
    ##   [469] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##   [505] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##   [541] 2 2 2 2 2 1 2 2 2 2 2 1 2 3 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##   [577] 2 2 2 1 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##   [613] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 3 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##   [649] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##   [685] 2 2 1 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##   [721] 2 2 2 2 2 2 2 2 2 2 2 3 2 2 1 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##   [757] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1
    ##   [793] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 1
    ##   [829] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##   [865] 2 2 2 2 2 2 3 2 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2
    ##   [901] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##   [937] 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 1 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2
    ##   [973] 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [1009] 2 3 2 2 1 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3
    ##  [1045] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [1081] 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2
    ##  [1117] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [1153] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2
    ##  [1189] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [1225] 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1
    ##  [1261] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2
    ##  [1297] 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [1333] 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 1 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [1369] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [1405] 2 2 3 2 3 2 2 2 2 2 2 2 2 1 2 2 2 2 1 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2
    ##  [1441] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [1477] 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [1513] 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2
    ##  [1549] 2 2 1 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 3 2
    ##  [1585] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [1621] 2 2 2 2 2 2 1 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2
    ##  [1657] 2 2 3 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2
    ##  [1693] 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [1729] 2 2 1 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [1765] 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [1801] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2
    ##  [1837] 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2
    ##  [1873] 2 2 2 2 2 1 2 1 2 2 3 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 3 2 2 3 2 2 2
    ##  [1909] 2 2 2 3 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 3 2 2 2 2
    ##  [1945] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [1981] 2 2 2 2 2 2 2 2 2 2 2 1 3 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2
    ##  [2017] 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [2053] 3 2 3 2 2 1 2 2 3 2 2 2 2 2 2 2 2 2 1 2 2 2 1 2 2 2 2 2 2 2 2 2 3 2 2 2
    ##  [2089] 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [2125] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [2161] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2
    ##  [2197] 1 2 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [2233] 2 2 2 3 2 2 2 2 2 2 1 2 1 2 1 2 2 2 2 2 2 2 1 2 2 2 2 2 1 2 3 2 2 2 2 2
    ##  [2269] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2
    ##  [2305] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 3 2 2 2 3 2
    ##  [2341] 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 3 2 2 2 2 2 2 2 1 3 2 2 1 2 1 2
    ##  [2377] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2
    ##  [2413] 3 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [2449] 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [2485] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2
    ##  [2521] 2 2 2 2 2 2 2 2 3 2 2 1 2 2 2 2 2 2 2 2 1 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2
    ##  [2557] 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 3 3 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [2593] 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 3 2 2 2 2 2
    ##  [2629] 1 2 3 2 2 2 3 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2
    ##  [2665] 2 2 2 1 2 1 2 2 2 2 2 2 2 2 2 2 3 2 3 1 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2
    ##  [2701] 2 2 2 2 2 2 2 2 2 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 3 2
    ##  [2737] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [2773] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [2809] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1
    ##  [2845] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 1 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [2881] 2 2 2 1 2 2 3 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 1 3 2 2 2 1 2
    ##  [2917] 2 2 1 2 2 2 2 2 1 2 3 2 2 2 2 1 2 2 2 2 2 2 2 3 2 2 2 2 1 2 2 2 2 2 2 2
    ##  [2953] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 3 2 1 2
    ##  [2989] 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 3
    ##  [3025] 2 2 2 1 3 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 1 2 2 2 2 2 2
    ##  [3061] 2 3 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 2 2 2 2
    ##  [3097] 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 3 2 3 2 2 2 2 3 2 2 2 2 2 2 2 3 2 2 2
    ##  [3133] 2 2 2 2 2 2 2 2 2 1 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2
    ##  [3169] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [3205] 3 2 2 2 2 2 2 2 3 2 2 1 2 2 2 2 2 2 2 2 3 2 3 2 2 1 2 2 2 2 2 2 2 2 2 2
    ##  [3241] 2 2 1 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [3277] 2 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2
    ##  [3313] 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [3349] 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2
    ##  [3385] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2
    ##  [3421] 2 2 2 2 2 2 2 2 1 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 1 3 2 2 2 2 2
    ##  [3457] 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 3 3
    ##  [3493] 2 3 2 2 2 2 2 1 2 1 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [3529] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [3565] 2 2 2 2 2 2 2 2 2 1 2 3 2 1 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 3 2 2 1 2 2
    ##  [3601] 2 2 2 2 2 2 2 2 2 2 2 3 2 3 2 2 2 2 1 2 2 1 2 2 1 2 2 1 2 2 2 2 3 2 2 2
    ##  [3637] 2 2 2 2 1 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 1 2
    ##  [3673] 1 1 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2
    ##  [3709] 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 3 3 2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2
    ##  [3745] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 1 2 3 3 2 2 2 2
    ##  [3781] 2 1 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 3 2 2 2 2 2 3 2 2 1
    ##  [3817] 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 2 2 2 3 1 2 2 2 2 2 2 2
    ##  [3853] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [3889] 2 2 2 2 3 2 2 2 1 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [3925] 2 2 2 2 1 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 1 2 2 2 1 2 2 2 2 2 2 2 2
    ##  [3961] 3 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [3997] 2 1 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2
    ##  [4033] 2 3 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2
    ##  [4069] 1 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 1 2 2 2 2 2 2 2 2 2
    ##  [4105] 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 1 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [4141] 2 3 2 2 2 2 3 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 1 2 1 2 2 2 2 2
    ##  [4177] 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [4213] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 2 2 2 2 1 2 1 2 2 2 2
    ##  [4249] 2 3 2 2 2 2 2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 3 2 2 2 2 2 3
    ##  [4285] 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2
    ##  [4321] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [4357] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [4393] 2 2 2 2 2 2 2 2 2 2 2 3 2 1 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [4429] 2 2 2 2 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [4465] 2 2 2 2 2 2 2 2 2 2 1 2 2 2 3 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [4501] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 3 2
    ##  [4537] 2 2 3 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 3 3 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2
    ##  [4573] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [4609] 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 3 2 1 2 2 2 1 2 2 2 2 2 2 2 2 3 2 2 2 2 2
    ##  [4645] 2 2 2 2 2 2 3 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2
    ##  [4681] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 1
    ##  [4717] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 3 3 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2
    ##  [4753] 2 2 2 3 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [4789] 3 2 2 2 2 2 2 2 2 1 2 2 2 2 1 2 2 2 2 3 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2
    ##  [4825] 2 2 2 1 3 2 2 2 2 2 2 2 2 2 2 3 2 2 1 2 2 2 2 1 2 1 2 2 2 2 1 2 2 2 2 2
    ##  [4861] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [4897] 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2
    ##  [4933] 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 1 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2
    ##  [4969] 3 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [5005] 1 2 2 2 2 2 2 1 2 2 2 3 2 2 2 2 3 1 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 1 2
    ##  [5041] 2 1 2 1 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 1 2 2 3 2
    ##  [5077] 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 3 1 2 2 2 2 2 2 2 2
    ##  [5113] 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 3 2 3 2 2 2 2 2 2 2 2 2 2 3 3 2 2 3
    ##  [5149] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2
    ##  [5185] 3 3 3 2 2 2 2 1 2 2 2 2 2 2 1 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 3 2 2 2 2 2
    ##  [5221] 2 3 2 2 2 2 3 2 2 2 2 2 2 2 2 3 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [5257] 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 3 3 2 2 2 2 2 1
    ##  [5293] 2 2 2 2 2 2 3 2 2 2 3 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 3
    ##  [5329] 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2
    ##  [5365] 2 2 2 2 1 2 2 2 2 2 1 2 3 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 3 2 2 2 2 1 2 2
    ##  [5401] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [5437] 2 3 2 3 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 3 3 2 2 2 2 2 2 2 2
    ##  [5473] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2
    ##  [5509] 2 2 2 2 1 2 2 2 2 3 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 3 2
    ##  [5545] 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 3 2 2 2
    ##  [5581] 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 3 2 2 3
    ##  [5617] 2 2 3 1 2 2 1 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 3 2 2 2
    ##  [5653] 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2
    ##  [5689] 3 3 2 2 2 2 2 2 2 2 2 2 1 2 2 2 3 2 1 2 3 2 3 2 2 3 2 2 2 2 2 2 2 2 2 3
    ##  [5725] 2 2 2 2 2 2 2 2 2 2 1 2 2 3 2 2 3 2 3 2 1 2 1 2 1 2 2 2 2 3 1 2 2 2 2 1
    ##  [5761] 2 2 2 2 2 1 2 2 2 2 2 2 3 2 1 2 2 2 2 2 2 1 1 2 3 2 2 2 2 2 2 3 2 2 2 2
    ##  [5797] 2 2 2 2 1 2 2 2 2 3 2 2 2 1 2 2 2 2 3 2 2 2 2 1 2 3 2 2 2 2 2 3 2 2 2 2
    ##  [5833] 2 2 2 2 2 2 2 3 2 2 2 2 3 2 3 2 2 2 1 2 2 2 3 1 2 2 2 2 2 1 2 2 2 3 2 2
    ##  [5869] 2 2 2 2 2 3 2 2 2 2 3 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1
    ##  [5905] 2 3 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 3 2 2 2 2 2 2 2
    ##  [5941] 2 1 2 2 2 2 3 2 2 1 2 2 2 2 2 2 2 2 2 1 2 1 2 2 3 3 2 2 2 2 2 2 1 2 1 2
    ##  [5977] 2 2 2 2 2 2 2 2 1 2 3 3 2 2 2 2 1 2 2 3 2 3 2 2 2 2 2 2 3 2 2 2 3 2 2 2
    ##  [6013] 2 2 3 3 2 2 1 2 2 2 2 2 2 2 2 2 2 3 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2
    ##  [6049] 2 2 2 3 2 2 2 2 2 2 2 3 2 1 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 3
    ##  [6085] 2 1 2 2 3 2 2 2 2 2 2 3 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2
    ##  [6121] 1 2 3 2 2 2 2 2 2 3 2 2 2 3 2 2 2 2 3 2 3 2 2 2 3 2 2 2 2 2 1 1 1 3 1 2
    ##  [6157] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 3
    ##  [6193] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 3 2 2 2 2 1 2 2 2 2 3 1 2 1 2 3 2
    ##  [6229] 2 2 1 2 2 2 2 1 2 2 2 2 2 3 2 1 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1
    ##  [6265] 2 2 2 2 3 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 1 3 2 2 3 1 2 2 2 3 2 2 2 2 2
    ##  [6301] 1 2 3 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 3 2 2 1 2 2 3 2 2 2 3 2 1 2 2 2 2
    ##  [6337] 2 2 2 2 2 2 2 1 2 2 2 1 2 2 3 3 2 2 2 3 2 2 2 2 2 2 2 2 3 2 3 2 2 2 1 2
    ##  [6373] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 3 2 2 2 2 2 3 2 2 2 2 2 2 2 2 1 2
    ##  [6409] 2 2 2 2 2 2 3 2 2 2 2 2 2 1 2 2 1 2 2 2 1 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2
    ##  [6445] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 3 3 2 2 2 1 3 2 2 2 1 2 2 2 1 2 2 2
    ##  [6481] 2 2 1 3 2 2 3 2 3 2 2 2 2 2 2 1 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [6517] 2 2 2 3 2 2 2 3 3 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2
    ##  [6553] 1 2 2 2 2 2 2 2 2 2 1 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2
    ##  [6589] 2 3 3 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 1 2 1 2 2 1 2 2 2 3 2 2 2 2 3 2 2 1
    ##  [6625] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 3 2 2 1 2 2
    ##  [6661] 2 2 2 2 2 2 3 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 3 2 1 2 2 2 2 2 2
    ##  [6697] 2 2 2 2 3 2 1 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 3 1 2 2 2 2 2 2 2 3 2 2
    ##  [6733] 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 3 2
    ##  [6769] 2 2 2 2 2 1 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 1
    ##  [6805] 2 2 2 2 2 3 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 2 2 2 3 2 1 2 2 3 2
    ##  [6841] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3
    ##  [6877] 2 2 1 3 2 2 2 1 1 2 2 3 2 2 2 2 2 2 3 2 2 2 2 2 2 1 1 3 2 2 2 2 2 2 2 2
    ##  [6913] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 3 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 3
    ##  [6949] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 1 2
    ##  [6985] 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [7021] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [7057] 2 2 2 2 2 2 2 2 2 2 2 1 2 2 1 1 2 2 2 1 2 2 2 2 2 2 2 2 3 1 2 3 1 1 2 2
    ##  [7093] 2 1 2 2 2 2 2 3 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 3 2 2 1
    ##  [7129] 3 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [7165] 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 3 1 2 2 2 2 2 2 2 2 1 2 2 2 2
    ##  [7201] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 2 2 2 2 2 3 2 2 2 2 2 2 2 1 2 2 2 2
    ##  [7237] 2 2 2 2 2 2 2 2 2 2 1 2 3 2 2 2 2 2 2 2 2 1 2 2 2 1 2 2 2 3 2 2 2 2 3 3
    ##  [7273] 3 2 2 2 2 2 2 2 2 1 3 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 3 2 2 3 2 2 2 1
    ##  [7309] 2 3 3 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 1 2 1 2 2 1 2 2 2
    ##  [7345] 2 2 2 2 2 2 2 2 1 1 2 2 2 3 2 2 2 1 2 1 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2
    ##  [7381] 2 2 2 2 2 2 2 2 2 3 3 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [7417] 2 2 2 2 2 3 2 2 2 3 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 1 2 2
    ##  [7453] 2 2 2 2 1 2 1 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2
    ##  [7489] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 1 2 2 2
    ##  [7525] 2 2 2 2 2 2 2 2 1 2 2 2 2 2 1 2 2 2 2 2 3 2 2 1 2 2 2 2 2 2 2 2 2 2 2 3
    ##  [7561] 2 2 2 3 2 2 2 2 3 2 1 2 3 2 2 1 2 2 2 2 2 2 2 1 3 3 2 1 2 2 2 2 2 2 2 2
    ##  [7597] 2 2 2 2 2 2 2 2 2 2 2 3 3 1 3 2 2 1 2 2 3 2 2 2 1 2 3 2 2 2 2 2 3 3 2 2
    ##  [7633] 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [7669] 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 3 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 1
    ##  [7705] 2 2 2 2 2 2 2 2 2 2 2 3 2 3 2 2 2 2 2 2 2 3 2 2 2 1 2 1 2 3 2 2 1 2 2 2
    ##  [7741] 2 2 2 2 2 3 2 2 1 2 2 2 2 3 3 2 2 2 3 2 2 3 3 3 2 3 2 2 3 2 2 2 2 1 2 2
    ##  [7777] 2 1 2 2 2 2 2 1 2 2 2 3 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 3
    ##  [7813] 2 3 1 2 2 2 3 2 2 1 2 2 2 2 1 2 2 3 3 2 2 1 2 2 3 2 2 2 2 2 2 2 2 2 2 2
    ##  [7849] 2 2 2 3 2 2 3 2 1 3 2 1 2 2 2 1 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 1 2 2
    ##  [7885] 2 2 3 2 2 2 2 3 2 2 3 1 2 2 2 2 2 2 3 3 2 2 2 2 2 2 2 2 2 3 1 2 3 2 2 2
    ##  [7921] 2 2 2 3 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3
    ##  [7957] 2 3 2 2 2 2 2 3 3 2 2 2 2 2 2 2 1 2 2 3 2 2 2 2 3 2 2 2 2 2 2 3 2 2 3 1
    ##  [7993] 2 2 2 2 2 3 2 3 2 2 2 3 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2
    ##  [8029] 3 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 1 3 2 2 2 1 2 2 2 2 2 2
    ##  [8065] 2 2 2 2 2 2 2 3 2 3 2 2 2 2 2 3 2 2 2 3 2 2 2 1 2 3 1 2 2 2 2 2 2 2 1 2
    ##  [8101] 3 3 2 3 2 2 1 2 2 2 2 2 2 2 1 2 2 1 2 2 2 2 3 2 2 2 1 2 2 2 2 2 2 2 1 2
    ##  [8137] 1 3 2 1 2 3 2 2 2 3 3 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 3 2 3 2 1 2 2 2 2
    ##  [8173] 2 2 2 2 2 2 2 2 2 3 2 2 2 3 2 3 3 2 2 2 2 2 1 2 2 1 3 2 2 2 2 2 2 2 2 2
    ##  [8209] 2 2 2 2 2 2 2 2 3 2 2 2 2 1 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2
    ##  [8245] 2 3 2 2 3 2 2 1 2 2 2 1 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 3 2 2 2 2 2
    ##  [8281] 1 2 3 2 2 3 3 2 3 2 2 2 2 1 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 1 1
    ##  [8317] 2 2 2 2 2 2 2 2 2 2 3 2 2 2 3 3 1 2 2 3 1 3 2 2 2 2 2 2 2 2 1 2 2 2 2 2
    ##  [8353] 2 2 2 2 2 2 3 2 2 2 2 2 2 2 1 2 3 2 2 2 2 2 2 2 3 2 3 2 2 2 2 2 2 2 2 2
    ##  [8389] 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 1 3 3 2 2 3 2 2 2 2 2
    ##  [8425] 2 3 2 2 2 2 2 1 2 1 2 2 2 3 2 2 2 3 2 3 2 2 2 2 1 2 2 2 2 2 2 1 1 2 2 2
    ##  [8461] 2 2 2 2 2 3 2 2 2 2 2 2 3 2 2 2 2 2 3 2 2 1 2 2 2 2 2 3 2 1 2 2 2 3 3 2
    ##  [8497] 2 2 3 2 3 2 2 3 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3
    ##  [8533] 2 2 1 1 2 2 2 2 3 2 2 2 2 2 2 2 1 1 2 2 2 2 2 2 3 2 2 2 2 2 3 2 1 2 2 2
    ##  [8569] 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 1 2 2 2 2 2 2 3 2 2 2 3 2 2 2 3 2 2 3 2 2
    ##  [8605] 2 2 2 2 2 2 2 2 3 1 3 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 1 2 2 2 2 2 3
    ##  [8641] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 3 2 3 2 2 3 1 2 2 1 2 2
    ##  [8677] 3 2 3 1 2 2 2 3 2 3 2 2 1 2 2 2 2 3 2 2 2 2 3 2 2 2 2 2 2 2 2 1 2 2 2 2
    ##  [8713] 1 2 2 1 2 2 2 2 3 2 1 2 2 1 3 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 2 2 2
    ##  [8749] 3 2 2 2 2 2 2 2 3 2 2 2 3 2 3 2 2 2 3 2 2 2 2 2 3 2 2 2 2 2 2 2 3 2 2 2
    ##  [8785] 2 2 2 3 2 2 2 2 3 2 2 3 3 2 1 2 2 2 3 2 2 2 3 2 2 3 1 2 3 2 2 2 2 3 2 2
    ##  [8821] 2 3 2 3 3 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2
    ##  [8857] 2 2 2 3 1 2 2 2 2 2 2 2 2 2 2 2 2 2 3 1 2 2 2 2 2 1 2 2 2 2 2 2 2 2 3 3
    ##  [8893] 2 2 2 2 2 2 3 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2
    ##  [8929] 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 1 3 2 3 1 1 2 2 2 3 2 3 2 2 2 2
    ##  [8965] 2 2 2 3 3 2 2 2 2 3 2 2 2 2 2 2 3 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3
    ##  [9001] 3 3 2 2 3 2 2 2 3 3 2 2 3 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [9037] 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 3 2 2 1 2
    ##  [9073] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 2 3 3 2 2 2 2 2 2 2 1 2 2 2 2 2 3 3 2
    ##  [9109] 1 2 3 2 2 2 1 2 2 2 2 2 2 3 2 2 2 1 2 2 2 2 2 2 2 2 3 2 2 2 2 2 3 2 2 2
    ##  [9145] 2 2 2 2 2 2 2 2 2 3 2 2 2 3 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2
    ##  [9181] 2 2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 3 2 2 3 3 2 2 2 2 2 2 2 2 1 2 2 2 2
    ##  [9217] 2 2 2 2 2 2 2 2 3 2 3 2 2 3 2 2 2 2 2 2 2 3 2 2 2 2 2 1 2 2 2 3 2 2 2 3
    ##  [9253] 3 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 3 3 2 1 2 2 2 2 2 2 3 2 2 2 3 2 2 2
    ##  [9289] 2 2 3 1 2 3 2 2 1 2 2 2 2 3 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2
    ##  [9325] 2 2 2 2 2 2 2 2 2 2 3 1 2 2 2 3 2 2 1 2 2 2 3 1 2 1 2 2 3 3 2 2 2 2 3 2
    ##  [9361] 1 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [9397] 2 2 2 2 2 2 2 1 1 2 2 2 2 2 3 2 2 2 2 3 1 2 2 2 2 2 3 2 3 2 2 2 2 2 2 1
    ##  [9433] 2 2 3 2 3 2 2 2 1 2 2 3 1 2 2 2 2 3 2 2 2 2 2 2 2 2 2 3 2 2 2 3 2 3 2 2
    ##  [9469] 2 2 2 2 1 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ##  [9505] 1 1 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 3 3 2 2 2 2 2 2 3
    ##  [9541] 2 3 2 3 2 2 3 3 1 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 3 2 2 3 2 2 2 3 2 2
    ##  [9577] 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 3 2 2 2 2 2 2 2 2 2 3 2 2 2
    ##  [9613] 2 2 3 2 2 1 2 2 3 2 2 3 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3
    ##  [9649] 2 2 3 1 2 2 1 2 2 2 2 2 3 2 3 3 1 2 2 3 3 2 3 2 2 2 2 2 2 2 2 3 2 2 2 2
    ##  [9685] 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 3 2 2 2 2 2 2 2 2 2 3 1 2 2 2
    ##  [9721] 2 2 2 2 2 2 2 3 3 2 2 2 3 2 2 2 2 3 2 2 2 2 2 2 2 2 3 2 3 2 1 2 1 2 2 2
    ##  [9757] 2 2 2 2 2 2 2 2 1 2 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2
    ##  [9793] 2 2 2 2 2 1 2 1 2 2 2 1 3 2 1 2 2 2 2 2 2 3 2 1 2 1 2 2 2 2 2 3 2 2 2 3
    ##  [9829] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 3 1 2 2
    ##  [9865] 2 2 2 1 2 2 2 2 1 1 2 2 2 1 2 2 2 2 2 2 2 2 2 2 1 2 2 1 2 2 2 2 2 2 3 2
    ##  [9901] 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 3 2 2 2 3 2 2 2 2
    ##  [9937] 2 2 2 2 3 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 3 2 2 2 2 3 2 2 2
    ##  [9973] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 3 2 1 3 2 2 1 2 2 2 2 2 2 2 2 2 1 2 2
    ## [10009] 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 3 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 3 2 2
    ## [10045] 2 2 2 2 2 1 2 2 2 2 2 2 2 2 1 2 2 3 2 2 3 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2
    ## [10081] 2 3 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2
    ## [10117] 2 2 2 2 2 3 1 2 2 3 2 2 2 2 2 2 2 3 2 2 2 1 2 2 2 2 2 2 1 2 2 2 2 2 2 2
    ## [10153] 2 2 1 2 2 3 3 2 2 3 2 2 2 3 3 2 2 1 2 2 2 2 3 1 3 2 3 2 2 2 2 2 2 2 2 2
    ## [10189] 2 2 2 2 1 2 2 2 2 2 2 2 2 1 3 2 2 2 2 3 2 2 2 2 2 3 2 1 2 2 2 2 1 1 2 3
    ## [10225] 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 1 2 3 2 2 2 3 2 3 2 2 2
    ## [10261] 2 3 2 2 3 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 3 2 2 2 2 2 2
    ## [10297] 2 2 2 2 2 2 2 2 3 3 2 2 2 2 2 2 1 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 3 2
    ## [10333] 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 3 1 2 2 2 2 2 2 2 2 3 2 2 1
    ## [10369] 1 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 2 3 2 2 2 2 2 3 2 2 2 2 2 3 2 2 2 2 2 2
    ## [10405] 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 3 2 2 3 2 2 2 2 3 2 2 2 2 2 2 2 3 2 2 3 2
    ## [10441] 2 2 2 1 2 3 2 2 2 2 2 3 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 1 2
    ## [10477] 2 2 2 2 2 2 2 1 2 2 2 1 3 2 2 2 2 2 2 2 2 2 3 1 3 2 2 2 2 3 2 2 3 2 2 1
    ## [10513] 2 2 1 3 3 3 2 1 2 3 2 2 2 2 2 2 2 3 3 2 2 2 3 2 2 2 2 2 2 3 2 2 2 3 2 2
    ## [10549] 2 2 2 2 2 2 2 2 2 2 3 2 2 3 2 2 2 2 2 2 3 1 3 2 2 2 1 2 1 2 2 2 2 2 2 1
    ## [10585] 2 2 1 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 3 2 2 2 2 2 2 1 2 2 1 3
    ## [10621] 3 2 2 3 3 2 2 2 2 2 1 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 3 2
    ## [10657] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 3 2 1 1 2 2 3 2 2 1
    ## [10693] 2 2 2 2 2 2 3 2 3 2 2 2 2 1 3 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [10729] 2 2 3 2 1 2 2 2 1 2 3 2 2 2 2 2 2 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2
    ## [10765] 2 2 2 2 2 2 2 2 2 2 2 1 2 3 3 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 1 2 2 1 2
    ## [10801] 2 2 2 2 2 1 2 2 2 2 1 3 3 2 3 2 3 3 1 2 2 2 2 2 1 2 2 2 2 2 2 2 3 2 3 1
    ## [10837] 2 2 2 2 2 3 2 2 3 3 3 2 1 2 2 3 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 3 3 2
    ## [10873] 2 2 1 2 2 2 1 2 3 2 2 2 2 3 2 2 2 2 3 1 1 2 2 3 2 3 2 2 2 2 1 2 2 2 2 2
    ## [10909] 2 2 2 2 2 2 2 2 2 1 2 2 1 2 1 2 2 2 1 2 2 2 3 2 2 3 2 2 2 2 2 2 2 2 2 2
    ## [10945] 1 2 2 2 2 2 2 2 2 2 3 2 2 2 2 3 1 2 2 2 2 2 2 2 1 2 1 2 2 2 2 2 2 2 2 2
    ## [10981] 2 3 2 3 2 2 2 3 2 2 2 2 2 1 2 2 2 2 3 2 2 2 2 2 2 2 2 3 2 1 2 2 2 2 2 2
    ## [11017] 2 2 2 3 3 3 3 2 1 1 2 3 2 2 2 2 3 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 1 2
    ## [11053] 2 3 2 2 3 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 3 2 2 2 2 1
    ## [11089] 2 2 2 2 2 1 2 2 2 2 3 2 2 2 3 3 3 2 2 3 3 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2
    ## [11125] 3 3 3 3 3 3 2 2 1 2 2 2 2 3 2 3 2 2 2 1 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1
    ## [11161] 2 3 2 2 2 2 2 2 3 2 2 2 2 2 3 2 2 3 3 2 3 2 2 2 2 3 2 2 2 3 3 2 2 2 3 2
    ## [11197] 2 2 2 2 2 2 2 3 2 2 2 3 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 1 2 2 2 2 2 3 2 2
    ## [11233] 3 2 2 2 2 2 2 2 2 2 2 3 2 1 2 2 2 2 2 2 3 2 3 2 3 2 3 2 2 2 2 2 2 2 2 2
    ## [11269] 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2
    ## [11305] 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2
    ## [11341] 2 2 2 2 2 2 1 3 2 2 2 2 2 3 1 2 2 2 2 2 3 2 1 2 3 2 1 2 2 2 2 1 1 1 1 2
    ## [11377] 2 3 2 3 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 3 2 3 2 3 2 2 2 1 3 2 2 2 3 2 2
    ## [11413] 2 2 2 2 3 2 2 2 2 2 2 2 2 2 3 2 2 2 2 3 3 2 1 2 2 2 1 3 2 3 3 2 2 2 2 2
    ## [11449] 2 2 2 2 2 2 2 2 3 2 2 1 2 2 2 2 2 3 2 2 2 2 2 1 2 2 2 2 2 3 2 2 2 2 1 2
    ## [11485] 3 3 2 2 2 2 2 2 3 2 2 1 2 2 3 2 2 2 2 3 2 2 2 2 3 2 2 2 2 3 2 1 2 2 3 2
    ## [11521] 1 2 2 2 1 2 3 2 2 2 3 2 2 3 2 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2
    ## [11557] 2 2 3 3 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

``` r
# Visualizing the  clustering results
# ---
# 
par(mfrow = c(1,2), mar = c(5,4,2,2))

# Plotting to see how Ozone and Solar.R data points have been distributed in clusters
# ---
#
plot(df6[,1:2], col = result$cluster) 
```

![](IPWK13_CORE_UNSUPERVISED_LEARNING_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

``` r
# Verifying the results of clustering
# ---
# 
par(mfrow = c(2,2), mar = c(5,4,2,2))

# Plotting to see how Sepal.Length and Sepal.Width data points have been distributed in clusters
plot(df6[c(1,2)], col = result$cluster)
```

![](IPWK13_CORE_UNSUPERVISED_LEARNING_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

``` r
# Plotting to see how the data points have been distributed 
# originally as per "class" attribute in dataset
df6.class<- df6[, "PageValues"]
plot(df6[c(1,2)], col = df6.class)
```

    ## Warning in plot.xy(xy, type, ...): supplied color is neither numeric nor
    ## character
    
    ## Warning in plot.xy(xy, type, ...): supplied color is neither numeric nor
    ## character

    ## Warning in plot.xy(xy.coords(x, y), type = type, ...): supplied color is neither
    ## numeric nor character

    ## Warning in plot.xy(xy, type, ...): supplied color is neither numeric nor
    ## character

    ## Warning in plot.xy(xy.coords(x, y), type = type, ...): supplied color is neither
    ## numeric nor character

    ## Warning in plot.xy(xy, type, ...): supplied color is neither numeric nor
    ## character

    ## Warning in plot.xy(xy.coords(x, y), type = type, ...): supplied color is neither
    ## numeric nor character

    ## Warning in plot.xy(xy, type, ...): supplied color is neither numeric nor
    ## character
    
    ## Warning in plot.xy(xy, type, ...): supplied color is neither numeric nor
    ## character

    ## Warning in plot.xy(xy.coords(x, y), type = type, ...): supplied color is neither
    ## numeric nor character

    ## Warning in plot.xy(xy, type, ...): supplied color is neither numeric nor
    ## character

    ## Warning in plot.xy(xy.coords(x, y), type = type, ...): supplied color is neither
    ## numeric nor character

    ## Warning in plot.xy(xy, type, ...): supplied color is neither numeric nor
    ## character

    ## Warning in plot.xy(xy.coords(x, y), type = type, ...): supplied color is neither
    ## numeric nor character

    ## Warning in plot.xy(xy, type, ...): supplied color is neither numeric nor
    ## character

![](IPWK13_CORE_UNSUPERVISED_LEARNING_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

``` r
plot(df6[c(2,3)], col = result$cluster)
```

![](IPWK13_CORE_UNSUPERVISED_LEARNING_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-37-2.png)<!-- -->

``` r
plot(df6[c(2,3)], col = df6.class)
```

    ## Warning in plot.xy(xy, type, ...): supplied color is neither numeric nor
    ## character
    
    ## Warning in plot.xy(xy, type, ...): supplied color is neither numeric nor
    ## character

    ## Warning in plot.xy(xy.coords(x, y), type = type, ...): supplied color is neither
    ## numeric nor character

    ## Warning in plot.xy(xy, type, ...): supplied color is neither numeric nor
    ## character

    ## Warning in plot.xy(xy.coords(x, y), type = type, ...): supplied color is neither
    ## numeric nor character

    ## Warning in plot.xy(xy, type, ...): supplied color is neither numeric nor
    ## character

    ## Warning in plot.xy(xy.coords(x, y), type = type, ...): supplied color is neither
    ## numeric nor character

    ## Warning in plot.xy(xy, type, ...): supplied color is neither numeric nor
    ## character
    
    ## Warning in plot.xy(xy, type, ...): supplied color is neither numeric nor
    ## character

    ## Warning in plot.xy(xy.coords(x, y), type = type, ...): supplied color is neither
    ## numeric nor character

    ## Warning in plot.xy(xy, type, ...): supplied color is neither numeric nor
    ## character

    ## Warning in plot.xy(xy.coords(x, y), type = type, ...): supplied color is neither
    ## numeric nor character

    ## Warning in plot.xy(xy, type, ...): supplied color is neither numeric nor
    ## character

    ## Warning in plot.xy(xy.coords(x, y), type = type, ...): supplied color is neither
    ## numeric nor character

    ## Warning in plot.xy(xy, type, ...): supplied color is neither numeric nor
    ## character

![](IPWK13_CORE_UNSUPERVISED_LEARNING_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-37-3.png)<!-- -->

### HIERACHIAL CLUSTERING

``` r
# As we donât want the hierarchical clustering result to depend to an arbitrary variable unit, 
# we start by scaling the data using the R function scale() as follows
# ---
# 
df6 <- scale(df6)
head(df6)
```

    ##      ProductRelated ProductRelated_Duration PageValues
    ## [1,]     -0.7232527              -0.6522699 -0.3284082
    ## [2,]     -0.7011322              -0.6194514 -0.3284082
    ## [3,]     -0.7232527              -0.6527827 -0.3284082
    ## [4,]     -0.7011322              -0.6509024 -0.3284082
    ## [5,]     -0.5241685              -0.3304950 -0.3284082
    ## [6,]     -0.3250844              -0.5731893 -0.3284082

``` r
# First we use the dist() function to compute the Euclidean distance between observations, 
d <- dist(df6, method = "euclidean")

# We then hierarchical clustering using the Ward's method
res.hc <- hclust(d, method = "ward.D2" )
```

``` r
# Lastly, we plot the obtained dendrogram

plot(res.hc, cex = 0.6, hang = -1)
```

![](IPWK13_CORE_UNSUPERVISED_LEARNING_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->
