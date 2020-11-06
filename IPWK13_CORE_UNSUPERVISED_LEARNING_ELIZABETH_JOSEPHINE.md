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
analysis. \#\# **c) Understanding the context**

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
# selecting needed columns
df3 <- subset(df2, select = c("Informational_Duration", "ProductRelated_Duration", "PageValues", "SpecialDay",  "Month", "Region", "VisitorType", "Weekend", "Revenue"))
colnames(df3)
```

    ## [1] "Informational_Duration"  "ProductRelated_Duration"
    ## [3] "PageValues"              "SpecialDay"             
    ## [5] "Month"                   "Region"                 
    ## [7] "VisitorType"             "Weekend"                
    ## [9] "Revenue"

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

    ##        Informational_Duration ProductRelated_Duration PageValues SpecialDay
    ##     1:                      0                0.000000    0.00000          0
    ##     2:                      0               64.000000    0.00000          0
    ##     3:                     -1               -1.000000    0.00000          0
    ##     4:                      0                2.666667    0.00000          0
    ##     5:                      0              627.500000    0.00000          0
    ##    ---                                                                     
    ## 12312:                      0             1783.791667   12.24172          0
    ## 12313:                      0              465.750000    0.00000          0
    ## 12314:                      0              184.250000    0.00000          0
    ## 12315:                      0              346.000000    0.00000          0
    ## 12316:                      0               21.250000    0.00000          0
    ##        Month Region       VisitorType Weekend Revenue
    ##     1:   Feb      1 Returning_Visitor   FALSE   FALSE
    ##     2:   Feb      1 Returning_Visitor   FALSE   FALSE
    ##     3:   Feb      9 Returning_Visitor   FALSE   FALSE
    ##     4:   Feb      2 Returning_Visitor   FALSE   FALSE
    ##     5:   Feb      1 Returning_Visitor    TRUE   FALSE
    ##    ---                                               
    ## 12312:   Dec      1 Returning_Visitor    TRUE   FALSE
    ## 12313:   Nov      1 Returning_Visitor    TRUE   FALSE
    ## 12314:   Nov      1 Returning_Visitor    TRUE   FALSE
    ## 12315:   Nov      3 Returning_Visitor   FALSE   FALSE
    ## 12316:   Nov      1       New_Visitor    TRUE   FALSE

### Duplicates

``` r
# checking for duplicates
duplicated_rows <- df4[duplicated(df4),]
duplicated_rows # there are 622 duplicates in the data
```

    ##      Informational_Duration ProductRelated_Duration PageValues SpecialDay Month
    ##   1:                     -1                    -1.0          0        0.0   Feb
    ##   2:                      0                     0.0          0        0.0   Feb
    ##   3:                      0                     0.0          0        0.0   Feb
    ##   4:                      0                     0.0          0        0.4   Feb
    ##   5:                      0                     0.0          0        0.0   Feb
    ##  ---                                                                           
    ## 618:                      0                     0.0          0        0.0   Dec
    ## 619:                      0                     0.0          0        0.0   Nov
    ## 620:                      0                   284.5          0        0.0   Dec
    ## 621:                      0                     0.0          0        0.0   Nov
    ## 622:                      0                     0.0          0        0.0   Nov
    ##      Region       VisitorType Weekend Revenue
    ##   1:      4 Returning_Visitor   FALSE   FALSE
    ##   2:      1 Returning_Visitor   FALSE   FALSE
    ##   3:      1 Returning_Visitor   FALSE   FALSE
    ##   4:      1 Returning_Visitor   FALSE   FALSE
    ##   5:      1 Returning_Visitor   FALSE   FALSE
    ##  ---                                         
    ## 618:      6 Returning_Visitor    TRUE   FALSE
    ## 619:      1 Returning_Visitor   FALSE   FALSE
    ## 620:      3 Returning_Visitor   FALSE   FALSE
    ## 621:      4 Returning_Visitor   FALSE   FALSE
    ## 622:      4 Returning_Visitor   FALSE   FALSE

``` r
# showing these unique items and assigning to a variable unique_items below
#df4 <- df3[!duplicated(df3), ]
df5 <- unique(df4)
df5
```

    ##        Informational_Duration ProductRelated_Duration PageValues SpecialDay
    ##     1:                      0                0.000000    0.00000          0
    ##     2:                      0               64.000000    0.00000          0
    ##     3:                     -1               -1.000000    0.00000          0
    ##     4:                      0                2.666667    0.00000          0
    ##     5:                      0              627.500000    0.00000          0
    ##    ---                                                                     
    ## 11690:                      0             1783.791667   12.24172          0
    ## 11691:                      0              465.750000    0.00000          0
    ## 11692:                      0              184.250000    0.00000          0
    ## 11693:                      0              346.000000    0.00000          0
    ## 11694:                      0               21.250000    0.00000          0
    ##        Month Region       VisitorType Weekend Revenue
    ##     1:   Feb      1 Returning_Visitor   FALSE   FALSE
    ##     2:   Feb      1 Returning_Visitor   FALSE   FALSE
    ##     3:   Feb      9 Returning_Visitor   FALSE   FALSE
    ##     4:   Feb      2 Returning_Visitor   FALSE   FALSE
    ##     5:   Feb      1 Returning_Visitor    TRUE   FALSE
    ##    ---                                               
    ## 11690:   Dec      1 Returning_Visitor    TRUE   FALSE
    ## 11691:   Nov      1 Returning_Visitor    TRUE   FALSE
    ## 11692:   Nov      1 Returning_Visitor    TRUE   FALSE
    ## 11693:   Nov      3 Returning_Visitor   FALSE   FALSE
    ## 11694:   Nov      1       New_Visitor    TRUE   FALSE

### Checking for outliers

``` r
# visualizing any existing outliers using a boxplot
#df1 <- subset(df, select = c("Daily Time Spent on Site", "Age", "Daily Internet Usage", "Male", "Clicked on Ad"))
df6 <- df5 %>% select_if(is.numeric)
df6
```

    ##        Informational_Duration ProductRelated_Duration PageValues SpecialDay
    ##     1:                      0                0.000000    0.00000          0
    ##     2:                      0               64.000000    0.00000          0
    ##     3:                     -1               -1.000000    0.00000          0
    ##     4:                      0                2.666667    0.00000          0
    ##     5:                      0              627.500000    0.00000          0
    ##    ---                                                                     
    ## 11690:                      0             1783.791667   12.24172          0
    ## 11691:                      0              465.750000    0.00000          0
    ## 11692:                      0              184.250000    0.00000          0
    ## 11693:                      0              346.000000    0.00000          0
    ## 11694:                      0               21.250000    0.00000          0
    ##        Region
    ##     1:      1
    ##     2:      1
    ##     3:      9
    ##     4:      2
    ##     5:      1
    ##    ---       
    ## 11690:      1
    ## 11691:      1
    ## 11692:      1
    ## 11693:      3
    ## 11694:      1

``` r
# Check for outliers
#df5 <- df4 %>% select_if(is.numeric)%>%select(-c(Male, `Clicked on Ad`))
#lapply(df5, function(x) boxplot.stats(x)$out)
boxplot(df6)# there are outliers in the data
```

![](IPWK13_CORE_UNSUPERVISED_LEARNING_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->
\#\#\# dealing with outliers

``` r
# Drop outliers in area income
#boxplot.stats(df6)$out
#df6 <- df6[-which(df6 %in% outliers),]
#boxplot(df6)
```

# EXPLORATORY DATA ANALYSIS

## Univariate Analysis

### Measures of Central Tendency

``` r
colnames(df6)
```

    ## [1] "Informational_Duration"  "ProductRelated_Duration"
    ## [3] "PageValues"              "SpecialDay"             
    ## [5] "Region"

``` r
# descriptive statistics
# these summaries will provide us with the measures of central tendencies of the numerical columns
describe(df6)
```

    ##                         vars     n    mean      sd median trimmed    mad min
    ## Informational_Duration     1 11694   36.34  144.29      0    4.30   0.00  -1
    ## ProductRelated_Duration    2 11694 1258.85 1944.61    659  880.84 769.47  -1
    ## PageValues                 3 11694    6.21   19.01      0    1.49   0.00   0
    ## SpecialDay                 4 11694    0.06    0.20      0    0.00   0.00   0
    ## Region                     5 11694    3.17    2.41      3    2.81   2.97   1
    ##                              max    range skew kurtosis    se
    ## Informational_Duration   2549.38  2550.38 7.38    72.29  1.33
    ## ProductRelated_Duration 63973.52 63974.52 7.21   134.57 17.98
    ## PageValues                361.76   361.76 6.22    62.36  0.18
    ## SpecialDay                  1.00     1.00 3.27     9.69  0.00
    ## Region                      9.00     8.00 0.97    -0.19  0.02

``` r
summary(df6)
```

    ##  Informational_Duration ProductRelated_Duration   PageValues    
    ##  Min.   :  -1.00        Min.   :   -1.0         Min.   :  0.00  
    ##  1st Qu.:   0.00        1st Qu.:  230.2         1st Qu.:  0.00  
    ##  Median :   0.00        Median :  659.0         Median :  0.00  
    ##  Mean   :  36.34        Mean   : 1258.8         Mean   :  6.21  
    ##  3rd Qu.:   0.00        3rd Qu.: 1544.6         3rd Qu.:  0.00  
    ##  Max.   :2549.38        Max.   :63973.5         Max.   :361.76  
    ##    SpecialDay          Region     
    ##  Min.   :0.00000   Min.   :1.000  
    ##  1st Qu.:0.00000   1st Qu.:1.000  
    ##  Median :0.00000   Median :3.000  
    ##  Mean   :0.06258   Mean   :3.168  
    ##  3rd Qu.:0.00000   3rd Qu.:4.000  
    ##  Max.   :1.00000   Max.   :9.000

### Univariate Graphical

``` r
# creating a boxplot graph for the variable all the numerical variables
boxplot(df6)
```

![](IPWK13_CORE_UNSUPERVISED_LEARNING_ELIZABETH_JOSEPHINE_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

## Bivariate analysis

### Graphical Techniques

## Multivariate analysis

# IMPLEMENTATION OF THE SOLUTION

## UNSUPERVISED LEARNING

### K-MEANS CLUSTERING

### HIERACHIAL CLUSTERING

### DBSCAN CLUSTERING

# CHALLENGING THE SOLUTION

# FOLLOW-UP QUESTIONS
