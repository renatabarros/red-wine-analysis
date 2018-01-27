---
title: 'How to pick a good bottle: the chemistry of red wine and its influence on
  wine quality'
author: "Renata Barros"
date: "January 2018"
output:
  pdf_document: default
  html_document: default
---


### 1. Introduction

This script was created to analyse Portuguese red wine data from Cortez et al. (2009) and investigate relationships between chemical properties and quality scores.

Data info available at: https://s3.amazonaws.com/udacity-hosted-downloads/ud651/wineQualityInfo.txt


### 2. Exploratory Data Analysis

Data and summary statistics: the red wine dataset has 1599 samples and measurements of 12 different properties (X just represents the sample number). One of these properties, quality, is the one measuring the score from 0 to 10 the wine got from wine experts. I am curious to see how chemical properties vary between wines of different quality scores, and if some properties might influence the final score.


```r
# Load data
df <- read.csv("wineQualityReds.csv")

# See variables names
names(df)
```

```
##  [1] "X"                    "fixed.acidity"        "volatile.acidity"    
##  [4] "citric.acid"          "residual.sugar"       "chlorides"           
##  [7] "free.sulfur.dioxide"  "total.sulfur.dioxide" "density"             
## [10] "pH"                   "sulphates"            "alcohol"             
## [13] "quality"
```

```r
# Get summary stats
summary(df)
```

```
##        X          fixed.acidity   volatile.acidity  citric.acid   
##  Min.   :   1.0   Min.   : 4.60   Min.   :0.1200   Min.   :0.000  
##  1st Qu.: 400.5   1st Qu.: 7.10   1st Qu.:0.3900   1st Qu.:0.090  
##  Median : 800.0   Median : 7.90   Median :0.5200   Median :0.260  
##  Mean   : 800.0   Mean   : 8.32   Mean   :0.5278   Mean   :0.271  
##  3rd Qu.:1199.5   3rd Qu.: 9.20   3rd Qu.:0.6400   3rd Qu.:0.420  
##  Max.   :1599.0   Max.   :15.90   Max.   :1.5800   Max.   :1.000  
##  residual.sugar     chlorides       free.sulfur.dioxide
##  Min.   : 0.900   Min.   :0.01200   Min.   : 1.00      
##  1st Qu.: 1.900   1st Qu.:0.07000   1st Qu.: 7.00      
##  Median : 2.200   Median :0.07900   Median :14.00      
##  Mean   : 2.539   Mean   :0.08747   Mean   :15.87      
##  3rd Qu.: 2.600   3rd Qu.:0.09000   3rd Qu.:21.00      
##  Max.   :15.500   Max.   :0.61100   Max.   :72.00      
##  total.sulfur.dioxide    density             pH          sulphates     
##  Min.   :  6.00       Min.   :0.9901   Min.   :2.740   Min.   :0.3300  
##  1st Qu.: 22.00       1st Qu.:0.9956   1st Qu.:3.210   1st Qu.:0.5500  
##  Median : 38.00       Median :0.9968   Median :3.310   Median :0.6200  
##  Mean   : 46.47       Mean   :0.9967   Mean   :3.311   Mean   :0.6581  
##  3rd Qu.: 62.00       3rd Qu.:0.9978   3rd Qu.:3.400   3rd Qu.:0.7300  
##  Max.   :289.00       Max.   :1.0037   Max.   :4.010   Max.   :2.0000  
##     alcohol         quality     
##  Min.   : 8.40   Min.   :3.000  
##  1st Qu.: 9.50   1st Qu.:5.000  
##  Median :10.20   Median :6.000  
##  Mean   :10.42   Mean   :5.636  
##  3rd Qu.:11.10   3rd Qu.:6.000  
##  Max.   :14.90   Max.   :8.000
```


Plot matrix: for a first look at the data, a plot matrix was constructed with information about correlation between variables. 

A quick analysis of correlation results shows quality might be somewhat correlated to fixed acidity (negative correlation), alcohol, sulphates and citric acidity (positive correlation). The distribution of relevant parameters and correlation between variables will be further explored in the next items.



```r
# Load GGally to get the plot matrix
library(GGally)

# Plot matrix
ggpairs(df)
```

![plot of chunk Plot matrix](figure/Plot matrix-1.png)


Histograms: the distribution of quality scores and variables somewhat correlated with it were observed through histograms. The median for each variable considering all samples was plotted as well for reference.

Starting with the quality scores, the wines analysed have scores between 3 and 8 and most wines have quality 5-6 (scale 1 to 10). From the summary statistics, we know the mean is 5.6 and the median is 6.

The subsequent plots are faceted by quality score (3-8) so we can have a better view of properties changes according to score.

The distribution of fixed acidity shows most wines have 6-8 g/dm^3 tartaric acid, no matter which is the quality score.
For alcohol, it appears highest rates wines (7-8) have the percentage above the median.
The distribution of sulphates shows highest rated wines (score 7 and 8) have sulphates above the median for all samples, which is represented by the blue dashed line. Interestingly, a progressive shift in sulphates content from higher to lower than the median is observed from highest to lowest rated wines. This shift in peak distribution is not observed for total sulfur dioxide (SO2).



```r
# Load ggplot
library(ggplot2)

# Quality score histogram
ggplot(aes(x = quality), data = df) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 10, 1))
```

![plot of chunk Histograms](figure/Histograms-1.png)

```r
# Fixed acidity histogram
ggplot(aes(x = fixed.acidity), data = df) +
  geom_histogram(binwidth = 0.1) +
  scale_x_continuous(breaks = seq(0, 16, 1)) +
  facet_wrap(~ quality) +
  geom_vline(aes(xintercept = median(fixed.acidity)), color = "red", linetype = 2)
```

![plot of chunk Histograms](figure/Histograms-2.png)

```r
# Alcohol histogram
ggplot(aes(x = alcohol), data = df) +
  geom_histogram(binwidth = 0.05) +
  scale_x_continuous(breaks = seq(8, 16, 1)) +
  facet_wrap(~ quality) +
  geom_vline(aes(xintercept = median(alcohol)), color = "red", linetype = 2)
```

![plot of chunk Histograms](figure/Histograms-3.png)

```r
# Sulphates histogram
ggplot(aes(x = sulphates), data = df) +
  geom_histogram(binwidth = 0.01) +
  scale_x_continuous(breaks = seq(0, 2, 0.5)) +
  facet_wrap(~ quality) +
  geom_vline(aes(xintercept = median(sulphates)), color = "red", linetype = 2)
```

![plot of chunk Histograms](figure/Histograms-4.png)

```r
# Total SO2
ggplot(aes(x = total.sulfur.dioxide), data = df) +
  geom_histogram(binwidth = 2) +
  scale_x_continuous(breaks = seq(0, 300, 30)) +
  facet_wrap(~ quality) +
  geom_vline(aes(xintercept = median(total.sulfur.dioxide)), color = "red", linetype = 2)
```

![plot of chunk Histograms](figure/Histograms-5.png)

```r
# Citric acidity histogram
ggplot(aes(x = citric.acid), data = df) +
  geom_histogram(binwidth = 0.01) +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  facet_wrap(~ quality) +
  geom_vline(aes(xintercept = median(citric.acid)), color = "red", linetype = 2)
```

![plot of chunk Histograms](figure/Histograms-6.png)


Scatter plots: various scatter plots were tested to visualise the relationship between variables. Quality buckets were preferred to visualise data, as there is a great difference in data points from average scores to low and high scores. Colouring by quality buckets makes it possible to see changes in acidity, sulphates and alcohol according to wine quality.



```r
# Sulphates
ggplot(aes(x = sulphates, y = quality), data = df) +
  geom_point(alpha = 0.1)
```

![plot of chunk Scatter plots](figure/Scatter plots-1.png)

```r
# Alcohol
ggplot(aes(x = alcohol, y = quality), data = df) +
  geom_point(alpha = 0.1)
```

![plot of chunk Scatter plots](figure/Scatter plots-2.png)

```r
# Check quantity of samples per quality score
table(df$quality)
```

```
## 
##   3   4   5   6   7   8 
##  10  53 681 638 199  18
```

```r
# Score buckets = high score (7 + 8), average score (5 + 6), low score (3 + 4)
quality.bucket <- cut(df$quality, breaks = c(2, 4, 6, 8), right = T)

# Alcohol vs. sulphates
ggplot(aes(x = alcohol, y = sulphates), data = df) +
  geom_jitter(aes(color = quality.bucket), show.legend = T)
```

![plot of chunk Scatter plots](figure/Scatter plots-3.png)

```r
# Citric vs. volatile acidity
ggplot(aes(x = volatile.acidity, y = citric.acid), data = df) +
  geom_point(aes(color = quality.bucket), show.legend = T)
```

![plot of chunk Scatter plots](figure/Scatter plots-4.png)

```r
# Median alcohol varying with quality
ggplot(aes(x = quality, y = alcohol), data = df) +
  geom_jitter(aes(color = quality.bucket), alpha = 0.5) +
  geom_line(stat = "summary", fun.y = median, linetype = 2)
```

![plot of chunk Scatter plots](figure/Scatter plots-5.png)

```r
# Median sulphates varying with quality
ggplot(aes(x = quality, y = sulphates), data = df) +
  geom_jitter(aes(color = quality.bucket), alpha = 0.5) +
  geom_line(stat = "summary", fun.y = median, linetype = 2)
```

![plot of chunk Scatter plots](figure/Scatter plots-6.png)

```r
# Median fixed acidity varying with quality
ggplot(aes(x = quality, y = volatile.acidity), data = df) +
  geom_jitter(aes(color = quality.bucket), alpha = 0.5) +
  geom_line(stat = "summary", fun.y = median, linetype = 2)
```

![plot of chunk Scatter plots](figure/Scatter plots-7.png)

## 3. Final plots and summary

The three plots below are representative of the main properties that influence on the quality of red wine: sulphates content, which is lower for wines of lower quality; amount of alcohol, that increases around 2% from lowest to highest rated wines; and volatile and citric acidity, which have a negative correlation, decreasing and increasing (respectively) from worst to best wines. 



```r
library(RColorBrewer)

# Sulphates histogram
ggplot(aes(x = sulphates), data = df) +
  geom_histogram(binwidth = 0.01) +
  scale_x_continuous(breaks = seq(0, 2, 0.5)) +
  facet_wrap(~ quality) +
  geom_vline(aes(xintercept = median(sulphates)),
             color = "blue", linetype = 2, show.legend = T) +
  xlab("Sulphates content (g/dm3)") +
  ylab("Frequency") +
  ggtitle("A. Amount of sulphates in red wines per quality score") +
  labs(caption = "* Dashed line is the median for all samples")
```

![plot of chunk Final plots & Summary](figure/Final plots & Summary-1.png)

```r
# Alcohol varying with quality
ggplot(aes(x = quality, y = alcohol), data = df) +
  geom_jitter(aes(color = quality.bucket), alpha = 0.6) +
  geom_line(stat = "summary", fun.y = median, linetype = 2) +
  scale_color_brewer(palette = "Set1") +
  guides(color = guide_legend(reverse = T)) +
  xlab("Quality score") +
  ylab("Alcohol content (% by volume)") +
  ggtitle("B. Quality vs. amount of alcohol in red wines") +
  labs(color = "Quality range", caption = "* Dashed line is the median for all samples")
```

![plot of chunk Final plots & Summary](figure/Final plots & Summary-2.png)

```r
# Citric vs. volatile acidity
ggplot(aes(x = citric.acid, y = volatile.acidity), data = df) +
  geom_jitter(aes(color = quality.bucket), alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  guides(color = guide_legend(reverse = T)) +
  xlab("Citric acidity (g/dm3)") +
  ylab("Volatile acidity (g/dm3)") +
  ggtitle("C. Citric acidity vs. volatile acidity in red wines")
```

![plot of chunk Final plots & Summary](figure/Final plots & Summary-3.png)


## 4. Discussion

The increased amount of sulphates might contribute to the formation of SO2 and its action as an antimicrobial and antioxidant, as described by Cortez et al. (2009). However, as no significant changes in SO2 were observed when comparing different quality scores (see EDA histograms), it is possible that other properties of added sulphates have a positive impact on wine quality. There is a preference for stronger (higher % of alcohol) and more citric red wines among experts; highest volatile acidity, associated with the vinager taste in high quantities, is mostly observed in lowest quality wines.

This gives information to pick a good bottle of Portuguese red wine, but sulphate or acidity contents are not found in the wine bottles. Luckily, the amount of alcohol is always displayed, so let's see the probability of getting a high quality red wine (score 7-8) based on % of alcohol ranges. 

The plot below shows that the probability of getting high quality wines increases from 8 to 14% alcohol, and strongly decreases above 14%. The interval (12,14]% alcohol has the highest probabilities, around 50%.


```r
# Load dplyr
library(dplyr, warn.conflicts = F)

# Summary of alcohol data 
summary(df$alcohol)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    8.40    9.50   10.20   10.42   11.10   14.90
```

```r
# Define alcohol buckets 
alcohol.bucket <- cut(df$alcohol, breaks = c(8, 9, 10, 11, 12, 13, 14, 15), right = T)
table(alcohol.bucket)
```

```
## alcohol.bucket
##   (8,9]  (9,10] (10,11] (11,12] (12,13] (13,14] (14,15] 
##      37     710     444     267     118      22       1
```

```r
# Add quality and alcohol buckes as columns in the data framne 
df$alcohol.bucket <- alcohol.bucket
df$quality.bucket <- quality.bucket

# New data frame that separates data according to quality and alcohol buckets and probability of getting highest quality wines (scores 7-8)
alc.groups <- group_by(df, alcohol.bucket)
df.alc.qual <- summarise(alc.groups,
                         high.qual = sum(quality.bucket == "(6,8]"),
                         average.qual = sum(quality.bucket == "(4,6]"),
                         low.qual = sum(quality.bucket == "(2,4]"),
                         n = n(),
                         prob.high = high.qual / n)

# Probability of getting highest quality wines varying with alcohol buckets 
ggplot(aes(x = alcohol.bucket, y = prob.high * 100), data = df.alc.qual) +
  geom_point(color = "blue") +
  xlab("Range of alcohol content (% by volume)") +
  ylab("Probability (%)") +
  ggtitle("D. Probability of getting a high quality wine based on alcohol content")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)


## 5. Conclusion

If you are a red wine fan as I am, and want to align your taste to the experts', keep in mind that there is a high chance that you will get a good bottle of Portuguese red wine if you pick one with 12.1 to 14% alcohol (probability close to 50%). Definitely worth a shot!


## 6. Reference

P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. 2009. Modeling wine preferences by data mining from physicochemical properties. Decision Support Systems, 47(4):547-553.

