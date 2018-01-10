---
title: 'Lab 6: Contingency Tables & Tests of Independence'
author: "Ruhil"
date: '2018-01-10'
output:
  html_document:
    highlight: pygments
    keep_md: yes
    mathjax: http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML
    theme: flatly
    toc: yes
  pdf_document:
    toc: yes
---



## Contingency Tables
When we have two categorical variables we are often interested in testing whether the outcomes of Variable A are independent of the values of Variable B. For example, 

* with the Titanic data, one variable might be the passenger's sex (Male versus Female) and the other variable might be whether the passenger survived or not.  
* with the Malaria and egg removal data one of the variables was the group the birds were in (control versus egg removed) while the other variable was whether the bird got malaria or not.   


### The Titanic Data
Let us read in the Titanic data, clean it up a bit, and then setup a few contingency tables.


```r
library(titanic)
data(titanic_train)
names(titanic_train)
```

```
##  [1] "PassengerId" "Survived"    "Pclass"      "Name"        "Sex"        
##  [6] "Age"         "SibSp"       "Parch"       "Ticket"      "Fare"       
## [11] "Cabin"       "Embarked"
```

Notice the contents of the data-set ...

* PassengerId = Passenger ID  
* Survived Passenger = Survival Indicator (0 = survived; 1 = died) 
* Pclass Passenger = Class (1 = First class; 2 = Second class; 3 = Third class)  
* Name  = Name  
* Sex  = Sex  
* Age  = Age  
* SibSp  = Number of Siblings/Spouses Aboard  
* Parch  = Number of Parents/Children Aboard  
* Ticket  = Ticket Number  
* Fare  = Passenger Fare  
* Cabin  = Cabin  
* Embarked  = Port of Embarkation  (S = Southampton; C = Cherbourg; Q = Queenstown)  


Let us label `Survived` so the tables and charts are easier to read. 


```r
titanic_train$Survived = factor(titanic_train$Survived, levels = c(0, 1), labels = c("Died", 
    "Survived"))
titanic_train$Embarked = factor(titanic_train$Embarked, levels = c("C", "Q", 
    "S"), labels = c("Cherbourg", "Queenstown", "Southampton"))
```

Now we can construct a contingency table of `Sex` and `Survived`


```r
tab.1 = table(titanic_train$Survived, titanic_train$Sex)
tab.1
```

```
##           
##            female male
##   Died         81  468
##   Survived    233  109
```

```r
tab.2 = prop.table(tab.1, 2)
tab.2  # column proportions
```

```
##           
##               female      male
##   Died     0.2579618 0.8110919
##   Survived 0.7420382 0.1889081
```

Was there an association between the passenger's sex and survival? We can run a chi-square test on tab.1 but looking at the data it is quite obvious that men were more likely to died than were women.

> H0: A passenger's chance of surviving was independent of his/her sex  
> H0: A passenger's chance of surviving was NOT independent of his/her sex  


```r
chisq.test(tab.1, correct = FALSE)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  tab.1
## X-squared = 263.05, df = 1, p-value < 2.2e-16
```

The p-value is practically 0 so we can easily reject H0; a passenger's chance of surviving was not independent of their sex.  

### Calculating Odds and Odds-Ratios
What if we were asked to calculate the odds-ratio of survival for men? The first thing we'll have to do is to make sure the data meet the requirements of the `mosaic` package: Successes" should be located in column 1, and the treatment of interest should be located in row 2. The odds ratio is calculated as (Odds row 2) / (Odds row 1). The confidence interval is calculated from the log(OR) and back-transformed. The package also defaults to a `conf.level = 0.95`.

Here, success means "died" and the treatment of interest is "men". So we want men in row 2 and died as column 1. The easiest way to setup the data for mosaic would be to create a Matrix from `tab.1`. 


```r
M = matrix(c(81, 468, 233, 109), nrow = 2, dimnames = list(Sex = c("Female", 
    "Male"), Outcome = c("Died", "Survived")))
M
```

```
##         Outcome
## Sex      Died Survived
##   Female   81      233
##   Male    468      109
```

```r
library(mosaic)

mosaicplot(t(M), col = c("yellow", "firebrick"), main = "Mosaic plot", cex.axis = 0.75, 
    ylab = "Relative Frequency")
```

![](Lab06_files/figure-html/titanic5-1.png)<!-- -->

```r
oddsRatio(M, verbose = TRUE)
```

```
## 
## Odds Ratio
## 
## Proportions
## 	   Prop. 1:	 0.258 
## 	   Prop. 2:	 0.8111 
## 	 Rel. Risk:	 3.144 
## 
## Odds
## 	    Odds 1:	 0.3476 
## 	    Odds 2:	 4.294 
## 	Odds Ratio:	 12.35 
## 
## 95 percent confidence interval:
## 	 2.596 < RR < 3.809 
## 	 8.9 < OR < 17.14 
## NULL
```

```
## [1] 12.35066
```

The odds-ratio of a man surviving versus a woman dying was 12.35: 1 ... men were more than 12 times as likely as women to die in the sinking. The 95% confidence interval of this odds-ratio is 8.9 to 17.14.  

Note carefully how the `matrix()` command worked. It took the values we entered and slotted them into the first column, and then into the second column. We had specified there were two rows, and hence it fills two rows of the first column before filling the second column. This means we have to be careful about how the order in which we specify the values. If we mess-up here the odds-ratio will be incorrect. Notice also how we specified the labels for the columns and rows. This could have been achieved via the following set of commands as well:


```r
M = matrix(c(81, 468, 233, 109), nrow = 2)
rownames(M) = c("Female", "Male")
colnames(M) = c("Died", "Survived")
M
```

```
##        Died Survived
## Female   81      233
## Male    468      109
```


### Malaria and Egg-removal
Table 2.3-1 in Chapter 2 shows that of 35 birds in the control group 7 ended up with Malaria and of the 30 birds in the Egg-removal group 15 ended up with Malaria. Is the incidence of Malaria independent of whether eggs were removed or not? What is the odds-ratio of Malaria for the egg-removal group? What is the associated 95% confidence interval? 

We know the treatment of interest is the "egg-removal" group (should be in row 2) and success is "malaria" (should be in column 1). 


```r
M = matrix(c(7, 15, 28, 15), nrow = 2)
colnames(M) = c("Malaria", "No Malaria")
rownames(M) = c("Control group", "Egg-removal group")
M
```

```
##                   Malaria No Malaria
## Control group           7         28
## Egg-removal group      15         15
```

```r
chisq.test(M, correct = FALSE)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  M
## X-squared = 6.4931, df = 1, p-value = 0.01083
```

```r
fisher.test(M)
```

```
## 
## 	Fisher's Exact Test for Count Data
## 
## data:  M
## p-value = 0.01739
## alternative hypothesis: true odds ratio is not equal to 1
## 95 percent confidence interval:
##  0.0710521 0.8411113
## sample estimates:
## odds ratio 
##  0.2557923
```

```r
oddsRatio(M, verbose = TRUE)
```

```
## 
## Odds Ratio
## 
## Proportions
## 	   Prop. 1:	 0.2 
## 	   Prop. 2:	 0.5 
## 	 Rel. Risk:	 2.5 
## 
## Odds
## 	    Odds 1:	 0.25 
## 	    Odds 2:	 1 
## 	Odds Ratio:	 4 
## 
## 95 percent confidence interval:
## 	 1.177 < RR < 5.309 
## 	 1.339 < OR < 11.95 
## NULL
```

```
## [1] 4
```

Clearly the p-values is less than $\alpha=0.05$ so we can reject the usual H0 of independence (i.e., malaria is independent of egg-removal).  The odds-ratio is 4, and the 95% confidence interval is 1.339 and 11.95.  


### Aspirin and Cancer
Here success is "No Cancer" and the treatment of interest is "Aspirin". 

```r
M = matrix(c(18515, 18496, 1427, 1438), nrow = 2)
rownames(M) = c("Placebo", "Aspirin")
colnames(M) = c("No Cancer", "Cancer")
M
```

```
##         No Cancer Cancer
## Placebo     18515   1427
## Aspirin     18496   1438
```

```r
mosaicplot(t(M), col = c("brown", "green"), main = "Mosaic plot", cex.axis = 0.75, 
    ylab = "Relative Frequency")
```

![](Lab06_files/figure-html/cancer-1.png)<!-- -->

```r
chisq.test(M, correct = FALSE)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  M
## X-squared = 0.050383, df = 1, p-value = 0.8224
```

```r
fisher.test(M)
```

```
## 
## 	Fisher's Exact Test for Count Data
## 
## data:  M
## p-value = 0.8311
## alternative hypothesis: true odds ratio is not equal to 1
## 95 percent confidence interval:
##  0.9342128 1.0892376
## sample estimates:
## odds ratio 
##   1.008744
```

```r
oddsRatio(M, verbose = TRUE)
```

```
## 
## Odds Ratio
## 
## Proportions
## 	   Prop. 1:	 0.9284 
## 	   Prop. 2:	 0.9279 
## 	 Rel. Risk:	 0.9994 
## 
## Odds
## 	    Odds 1:	 12.97 
## 	    Odds 2:	 12.86 
## 	Odds Ratio:	 0.9913 
## 
## 95 percent confidence interval:
## 	 0.9939 < RR < 1.005 
## 	 0.9188 < OR < 1.07 
## NULL
```

```
## [1] 0.9913321
```

Here we fail to reject H0; whether one is given Aspirin or a Placebo has no association with the subjects getting cancer (or not). Notice the 95% confidence interval spans 1.    


### Choosing Between Tests
My recommendation would be to go with the `fisher.test()` regardless of sample size or whether the chi-square assumptions are violated or not. However, disciplines and sub-disciplines vary so if your field defaults to chi-square tests for large samples, Fisher's Exact test when assumptions are violated and samples are small, and G-tests when samples are large and the assumptions are violated, then read on below.

(1) __The chi-square test:__ So long as we have random samples, and no more than 20% of the cells in our table have expected frequencies less than 5, the `chisq.test()` would work fine.  In general, however, the chi-square test is an "approximate" test and hence not as powerful or accurate as exact tests.  
(2) __Fisher's Exact test:__ Hence these days, given the computing power at our disposal, one might as well default to the `fisher.test()`. Classically, of course, these tests would be recommended if the chi-square assumption was violated and we had a small sample.   
(3) __The G-test:__ $G-test$ is useful for large samples and when dealing with complex experimental research designs. How large? They say anything beyond a sample size of 1000. These tests can be used when the chi-square assumptions are violated. 


## Practice Problems

#### Problem 4
It has been hypothesized that the white rump of pigeons serves to distract predators like the peregrine falcons, and therefore it may be an adaptation to reduce predation. To test this, researchers followed the fate of 203 pigeons, 101 with white rumps and 102 with blue rumps. Nine of the white-rumped birds and 92 of the blue-rumped birds were killed by falcons. 

> (a) Show the results in a frequency table (with the response as the rows and the explanatory variable as the columns). What association is suggested?

The response is whether the bird was killed or not, and the explanatory variable is the color of the pigeon's rump.  


```r
rumps = read.csv(url("http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q04PigeonRumps.csv"))
head(rumps)
```

```
##   rumpColor survivalWithFalcon
## 1      blue           survived
## 2      blue           survived
## 3      blue           survived
## 4      blue           survived
## 5      blue           survived
## 6      blue           survived
```

```r
names(rumps)
```

```
## [1] "rumpColor"          "survivalWithFalcon"
```

```r
tab.p = table(rumps$survivalWithFalcon, rumps$rumpColor)
addmargins(tab.p)
```

```
##           
##            blue white Sum
##   killed     92     9 101
##   survived   10    92 102
##   Sum       102   101 203
```

```r
tab.p2 = prop.table(tab.p, 1)
tab.p2
```

```
##           
##                  blue      white
##   killed   0.91089109 0.08910891
##   survived 0.09803922 0.90196078
```

The table suggests that 91% of the blue-rumped pigeons were killed versus only 9% of the white-rumped pigeons.

> Do the two kinds of pigeons differ in their rate of capture by falcons? Carry out an appropriate test.

$H_0:$ Predation by falcons is independent of rump-color  
$H_A:$ Predation by falcons is not independent of rump-color  
$\alpha=0.05$  
Reject $H_0$ if $p-value \leq 0.05$  


```r
chisq.test(tab.p, correct = FALSE)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  tab.p
## X-squared = 134.13, df = 1, p-value < 2.2e-16
```

```r
fisher.test(tab.p)
```

```
## 
## 	Fisher's Exact Test for Count Data
## 
## data:  tab.p
## p-value < 2.2e-16
## alternative hypothesis: true odds ratio is not equal to 1
## 95 percent confidence interval:
##   33.70502 270.81972
## sample estimates:
## odds ratio 
##   89.50586
```

Regardless of the test used, we can safely Reject $H_0$ given that the $p-value \approx 0$. The data suggest that predation by falcons is not independent of rump-color

> What is the estimated odds ratio for capture of the two groups of pigeons? What is the 95% confidence interval for the odds ratios?

Note: For the `mosaic` library, "successes" (killed) should be located in column 1, and the treatment of interest (white-rumped pigeons) should be located in row 2. So we'll have to recreate the table. Note also that the odds ratio is calculated as $(\text{Odds row 2}) / (\text{Odds row 1})$.  


```r
tab.p  # The original table
```

```
##           
##            blue white
##   killed     92     9
##   survived   10    92
```

```r
tab.pp = table(rumps$rumpColor, rumps$survivalWithFalcon)
tab.pp  # The corrected table with 'killed' as column 1 and 'blue' as row 2
```

```
##        
##         killed survived
##   blue      92       10
##   white      9       92
```

Say you wanted to build up the table with the matrix command. This could be done as follows once we see the original table and extract the values we need.  


```r
tab.p  # to see the values in the four cells
```

```
##           
##            blue white
##   killed     92     9
##   survived   10    92
```

```r
M = matrix(c(92, 9, 10, 92), nrow = 2)
M
```

```
##      [,1] [,2]
## [1,]   92   10
## [2,]    9   92
```

```r
colnames(M) = c("Killed", "Survived")
rownames(M) = c("Blue", "White")
M
```

```
##       Killed Survived
## Blue      92       10
## White      9       92
```

Now we can calculate the odds-ratio and the confidence interval. We do it first with the matrix `M` and then with tab.pp. 


```r
library(mosaic)
oddsRatio(M, conf.level = 0.95, verbose = TRUE)
```

```
## 
## Odds Ratio
## 
## Proportions
## 	   Prop. 1:	 0.902 
## 	   Prop. 2:	 0.08911 
## 	 Rel. Risk:	 0.09879 
## 
## Odds
## 	    Odds 1:	 9.2 
## 	    Odds 2:	 0.09783 
## 	Odds Ratio:	 0.01063 
## 
## 95 percent confidence interval:
## 	 0.05279 < RR < 0.1849 
## 	 0.00413 < OR < 0.02738 
## NULL
```

```
## [1] 0.01063327
```

```r
oddsRatio(tab.pp, conf.level = 0.95, verbose = TRUE)
```

```
## 
## Odds Ratio
## 
## Proportions
## 	   Prop. 1:	 0.902 
## 	   Prop. 2:	 0.08911 
## 	 Rel. Risk:	 0.09879 
## 
## Odds
## 	    Odds 1:	 9.2 
## 	    Odds 2:	 0.09783 
## 	Odds Ratio:	 0.01063 
## 
## 95 percent confidence interval:
## 	 0.05279 < RR < 0.1849 
## 	 0.00413 < OR < 0.02738 
## NULL
```

```
## [1] 0.01063327
```

The estimated odds ratio is 0.0106 and the 95% confidence interval is $[0.00413, 0.02738]$. We can be 95% confident that for the population of pigeons, the odds of white-rumped pigeons being killed are anywhere between 0.413% to 2.73% lower than the odds of blue-rumped pigeons being killed. 

When odds-ratios are $< 1$ they are difficult to interpret. So a common solution used widely is to flip them. That is, if the odds-ratio is 0.01063 for white-rumped pigeons being killed, then the inverse must be the probability of blue-rumped pigeons being killed. The inverse would be just $\dfrac{1}{0.01063} = 94.07338$ ... the odds of blue-rumped pigeons being killed are 94 times those of white-rumped pigeons being killed. If we did this conversion for the confidence interval we would have:


```r
1/0.00413
```

```
## [1] 242.1308
```

```r
1/0.02738
```

```
## [1] 36.52301
```

#### Problem 7
Reed frogs, a species living in West Africa, have been observed hopping away from grass fires long before the heat of the fire reached the area they were in. This finding led to the hypothesis that the frogs might hear the fire and respond well before the fire reaches them. To test this hypothesis, researchers played three types of sound to samples of reed frogs and recorded their response. Twenty frogs were exposed to the sound of fire, 20 were exposed to the sound of the fire played backward (to control for the range of sound frequencies present in the real sound), and 20 were exposed to equally loud white noise. Of these 60 frogs, 18 hopped away from the sound of the fire, 6 hopped away from the sound of the fire played backward, and 0 hopped away from the sound of the white noise.

> (a) Illustrate these data with a frequency table. What association is suggested?


```r
frogs = read.csv(url("http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter09/chap09q07FrogsFire.csv"))
head(frogs)
```

```
##   sound direction
## 1  fire      away
## 2  fire      away
## 3  fire      away
## 4  fire      away
## 5  fire      away
## 6  fire      away
```

```r
names(frogs)
```

```
## [1] "sound"     "direction"
```

```r
tab.f = table(frogs$direction, frogs$sound)
tab.f
```

```
##           
##            backwards fire noise
##   away             6   18     0
##   not away        14    2    20
```

```r
tab.ff = prop.table(tab.f, 2)
tab.ff
```

```
##           
##            backwards fire noise
##   away           0.3  0.9   0.0
##   not away       0.7  0.1   1.0
```

The table suggests an association between the type of sound and frogs hopping away.

> Do the data provide evidence that reed frogs change their behavior in response to the sound of fire?

$H_0:$ Reed frogs do not change their behavior in response to the sound of fire  
$H_A:$ Reed frogs do change their behavior in response to the sound of fire  
$\alpha=0.05$; the usual decision rule applies


```r
fisher.test(tab.f)
```

```
## 
## 	Fisher's Exact Test for Count Data
## 
## data:  tab.f
## p-value = 2.328e-09
## alternative hypothesis: two.sided
```

```r
chisq.test(tab.f, correct = FALSE)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  tab.f
## X-squared = 35, df = 2, p-value = 2.511e-08
```

Regardless of the test used, the $p-value$ is almost 0 and so we can easily reject $H_0$. The data suggest that reed frogs do change their behavior in response to the sound of fire


#### Problem 12
Migraine with aura is a potentially debilitating condition, yet little is known about its causes. A case-control study compared 93 people who suffer from chronic migraine with aura to a sample of 93 healthy patients. The researchers used trans-esophageal echocardiography to look for cardiac shunts in all of these patients. (A cardiac shunt is a heart defect that causes blood to flow from the right to the left in the heart, causing poor oxygenation.) Forty-four of the migraine patients were found to have a cardiac shunt while only 16 of the people without migraine symptoms had this heart defect.

> (a) Is this an observational or experimental study?

It is an observational study

> (b) Show the association between migraines and cardiac shunts with a mosaic plot.


```r
M = matrix(c(49, 44, 77, 16), nrow = 2, dimnames = list(Treatment = c("No Shunt", 
    "Shunt"), Outcome = c("Migraines", "No Migraines")))
M
```

```
##           Outcome
## Treatment  Migraines No Migraines
##   No Shunt        49           77
##   Shunt           44           16
```

```r
mosaicplot(t(M), col = c("yellow", "firebrick"), main = "Mosaic plot", cex.axis = 0.75, 
    ylab = "Relative Frequency")
```

![](Lab06_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

> (c) How strong is the association between migraines and cardiac shunts? Calculate the odds ratio for migraine, comparing the patients with and without cardiac shunts.

Remember: You need the successes in column 1 and the treatment in row 2. Our matrix "M" meets these criteria.


```r
oddsRatio(M, verbose = TRUE)
```

```
## 
## Odds Ratio
## 
## Proportions
## 	   Prop. 1:	 0.3889 
## 	   Prop. 2:	 0.7333 
## 	 Rel. Risk:	 1.886 
## 
## Odds
## 	    Odds 1:	 0.6364 
## 	    Odds 2:	 2.75 
## 	Odds Ratio:	 4.321 
## 
## 95 percent confidence interval:
## 	 1.444 < RR < 2.462 
## 	 2.2 < OR < 8.488 
## NULL
```

```
## [1] 4.321429
```

The odds ratio is 4.321429

> (d) What is the 95% confidence interval for this odds ratio?

The 95% confidence interval is $[2.200, 8.488]$


### Your Turn

#### Problem 6
Female Australian redback spiders, *Latrodectus hasselti*, are about 50 times larger than the males and often cannibalize the males during mating. There may be a reason for this, however: Perhaps females are more likely to accept the sperm of a male she has eaten than that of a male who has escaped. Researchers watched the mating behavior of 32 virgin female redback spiders, recording whether each female ate her first mate and then whether she rejected the advances of a second male later placed in her vicinity. The data were as shown below:


```r
M = matrix(c(3, 6, 22, 1), nrow = 2)
rownames(M) = c("2nd Male accepted", "2nd Male rejected")
colnames(M) = c("1st Male cannibalized", "1st Male escaped")
M
```

```
##                   1st Male cannibalized 1st Male escaped
## 2nd Male accepted                     3               22
## 2nd Male rejected                     6                1
```

Do these data suggest any association between the fate of the first male and that of the second male? Conduct an appropriate test.


#### Problem 14
A "Mediterranean diet" (high in fish, olive oil, red wine, etc.) has been touted as a key to a long life. A study looked at the death rates of people according to whether their diet had a low, medium, or high component of food that characterizes the Mediterranean diet. To rule out other possible causes of shortened lives the study also recorded whether each person was a current smoker, former smoker, or had never smoked. Do these data suggest as association between diet and smoking? 


```r
M = matrix(c(2516, 3657, 2012, 2920, 4653, 1627, 2417, 3449, 1294), nrow = 3)
rownames(M) = c("Never Smoked", "Former Smoker", "Current Smoker")
colnames(M) = c("Low Diet", "Medium Diet", "High Diet")
M
```

```
##                Low Diet Medium Diet High Diet
## Never Smoked       2516        2920      2417
## Former Smoker      3657        4653      3449
## Current Smoker     2012        1627      1294
```

(a) Draw a mosaicplot of these data. What pattern does this plot show?


(b) Test whether there is an association between smoking and the diet.



