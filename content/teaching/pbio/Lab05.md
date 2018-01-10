---
title: 'Lab 5: The Chi-Square and the Poisson'
author: "Anirudh V. S. Ruhil"
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



# The $\chi^2$ Distribution 
The $\chi^2$ distribution is used with multinomial data (i.e., when the categorical variable has more than two categories) to test whether observed frequency counts differ from expected frequency counts. 

## The Goodness-of-Fit Test for a single variable
$H_0$: Proportions are all the same  
$H_A$: Proportions are \textit{not} all the same 

$$\chi^{2} = \displaystyle\sum\limits_{i}\dfrac{\left(Observed_i - Expected_i \right)^2}{Expected_i}$$ 

$\chi^2$ distributed with $\left( \text{no. of categories}-1 \right)$ degrees of freedom $(\textit{df})$ 

Reject $H_0$ if $P-value \leq \alpha$; Do not reject $H_0$ otherwise 

### Assumptions:

1. No category has expected frequencies $< 1$
2. No more than 20% of the categories should have expected frequencies $< 5$


#### The Steps for goodness-of-fit testing ...
(1) Setup $H_0$ and $H_A$
(2) Setup $\alpha$ and the Decision-Rule
(3) Calculate expected probabilities if these are not given
(4) See if $\chi^2$ assumptions are violated 
    (a) No category has expected frequencies $< 1$
    (b) No more than 20% of the categories should have expected frequencies $< 5$
(5) If either assumption is violated then you will need to collapse the categories and start again from Step (3)
(6) Conduct the test
(7) Adjust for having estimated $P(X)$ from the data by using $df= n - 1 - 1 = n-2$
(8) State your conclusion (in words)


# Some Practice
## Four Health Campaigns

$H_0:$ Proportions of the sample recalling any commercial are the same  
$H_A:$ Proportions of the sample recalling any commercial are NOT the same  

Set $\alpha$; I choose 0.05  

Decision Rule: Reject $H_0$ if $p-value \leq \alpha$; do not reject otherwise.


```r
observed = c(85, 95, 50, 70)
expected = c(75, 75, 75, 75)
tab.00 = cbind(observed, expected); tab.00 # just to see obs vs. exp
```

```
##      observed expected
## [1,]       85       75
## [2,]       95       75
## [3,]       50       75
## [4,]       70       75
```

```r
chisq.test(observed, p=c(0.25, 0.25, 0.25, 0.25))
```

```
## 
## 	Chi-squared test for given probabilities
## 
## data:  observed
## X-squared = 15.333, df = 3, p-value = 0.001553
```

The p-value is 0.001553 so we can easily reject $H_0$; the data suggest that recall rates differ across the four health campaigns. 

## M&Ms 

Here we are given the proportion of the colors the company would like in each bag. We have the observed frequencies as well. 

$H_0:$ The proportion of colors follows the specified distribution  
$H_A:$ The proportion of colors DOES NOT follow the specified distribution  

Set $\alpha$; I choose 0.05  

Decision Rule: Reject $H_0$ if $p-value \leq \alpha$; do not reject otherwise.


```r
expected = c(0.3, 0.2, 0.2, 0.1, 0.1, 0.1)
observed = c(177, 135, 79, 41, 36, 38)
chisq.test(observed, p=expected)
```

```
## 
## 	Chi-squared test for given probabilities
## 
## data:  observed
## X-squared = 29.514, df = 5, p-value = 1.838e-05
```

Given the low p-value we reject $H_0$; the data suggest that the color distribution is NOT what the company would like to see.

## Number of Boys
Is the sex of consecutive children independent in humans? Does having one boy change the probability of the next child also being a boy? We looked at this with decision trees in Chapter 5. Here we use data compiled by Rodgers and Doughty (2011) to test for independence -- whether the sex of the first child influences (or not) the sex of the second child. 


```r
families = read.csv(url("http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08e5NumberOfBoys.csv"))
head(families)
```

```
##   numberOfBoys
## 1            0
## 2            0
## 3            0
## 4            0
## 5            0
## 6            0
```

```r
tab.2 = table(families); tab.2
```

```
## families
##    0    1    2 
##  530 1332  582
```

$H_0: \text{ No. of Boys in two-child families follows the binomial distribution}$  

$H_A: \text{ No. of Boys in two-child families does not follow the binomial distribution}$  

We need to start by calculating the probability of a boy being born in any given randomly selected birth. Given the sample but no population information, we'll have to calculate this probability from the sample. Total number of boys = 2496. Total sample size is 4888 children. So the probability of a boy is:


```r
p.boy = 2496/4888; p.boy
```

```
## [1] 0.5106383
```

If this is the base probability of seeing a boy in any randomly chosen birth, how often would we see 0 boys, 1 boy, 2 boys? The binomial distribution can tell us:


```r
x = seq(0, 2, by=1) # create a sequence that goes from 0 boys to 1 boy to 2 boys
y = dbinom(x, 2, p = p.boy) # probability of seeing no boys, 1 boy, 2 boys
y 
```

```
## [1] 0.2394749 0.4997737 0.2607515
```

```r
sum(y) # Making sure the probabilities sum to 1
```

```
## [1] 1
```

```r
chisq.test(tab.2, p=y) # the actual chi-square test
```

```
## 
## 	Chi-squared test for given probabilities
## 
## data:  tab.2
## X-squared = 20.021, df = 2, p-value = 4.492e-05
```

The p-value is small so we would reject $H_0$ but we have to adjust the degrees of freedom. Why? Because we used up one additional degree of freedom by using the sample to calculate `p.boy`. The correct p-value is thus:


```r
1 - pchisq(20.021, df=1)
```

```
## [1] 7.659634e-06
```

Now we can safely reject $H_0$; the data suggest that the number of boys in two-child families does not follow the binomial distribution. 

What does this mean? This means that somehow the probability of having a son or daughter appears to vary across families, maybe having a boy does influence the chance of having another boy or a girl, or something else is going on here that violates one of the the assumptions of the binomial distribution -- that what happens in one trial has no bearing on what happens in the next trial. 


# The Poisson Distribution

The Poisson distribution describes the number of successes that will occur in space or time if the number of successes are independent across consecutive space or time and occur with equal probability in each space or time. It is a useful distribution if our goal is to test whether or not events occur randomly in space and/or time because if they do, then the Poisson distribution would characterize these events quite well.   

$$P(X) = \dfrac{e^{-\mu} \mu^{X}}{X!}; \text{where } X=0,1,2,3,\ldots, n; \text{and Mean }= \text{Variance } = \mu$$

where $X=$ the number of events in a given time interval or space; $\mu=$ the mean number of events per time interval or space; and $P(X)=$ the probability of observing exactly $X$ events in a given interval. 

For example: Hospital births occur on average at 1.8 births per hour. What is $P(X=4)$? 

$$P(X=4) = \dfrac{e^{-1.8}(1.8)^4}{4!} = 0.0723$$

## Key Assumptions

1. The probability of observing a single event over a small time interval (or space) is approximately proportional to the size of that time interval (or space) . 
2. The probability of two events occurring in the same narrow time interval (or space) is negligible. 
3. The probability of an event within a certain time interval (or space) does not change across different time intervals (or space) . 
4. The probability of an event in one time interval (or space) is independent of the probability of an event in any other non-overlapping time interval (or space). 


Note:  

* Clumping occurs when the mean is _less than_ the variance
* Dispersion occurs when the mean is _greater than_ the variance

## -  The Steps ...
(1) Setup $H_0$ and $H_A$
(2) Setup $\alpha$ and the Decision-Rule
(3) Calculate the mean $(\mu)$ 
(4) Use $\mu$ to calculate expected frequencies for counts = $0, 1, 2, 3, \cdots$
(5) See if $\chi^2$ assumptions are violated 
    (a) No category has expected frequencies $< 1$
    (b) No more than 20% of the categories should have expected frequencies $< 5$
(6) If either assumption is violated then you will need to collapse the categories and start again from Step (3)
(7) Conduct the test. 
(8) Adjust for having estimated $\mu$ from the data by using $df= n - 1 - 1 = n-2$
(9) State your conclusion (in words)

### An Example: Mass Extinctions in the Fossil Record
Do extinctions occur randomly through the long fossil record of Earth's history or are there periods in which the extinction rates are unusually high ("mass extinctions") compared with background rates? We can test this hypothesis via data on the number of recorded extinctions of marine invertebrate families in 76 time intervals. If mass extinctions are indeed random in time then the distribution should follow the Poisson, with any departures from the Poisson suggesting that something must have been going on.     

$$H_0: \text{Number of extinctions follow the Poisson distribution }$$  
$$H_A: \text{Number of extinctions DO NOT follow the Poisson distribution }$$



```r
extinct = read.csv(url("http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08e6MassExtinctions.csv"))
head(extinct)
```

```
##   numberOfExtinctions
## 1                   1
## 2                   1
## 3                   1
## 4                   1
## 5                   1
## 6                   1
```

```r
names(extinct)
```

```
## [1] "numberOfExtinctions"
```

```r
tab.A = table(extinct); tab.A # Note missing "0" here versus the text
```

```
## extinct
##  1  2  3  4  5  6  7  8  9 10 11 14 16 20 
## 13 15 16  7 10  4  2  1  2  1  1  1  2  1
```

```r
mean(extinct$numberOfExtinctions)
```

```
## [1] 4.210526
```

```r
plot(tab.A, ylim=c(0,20), xaxt="n", type="h", xlab="Number of Mass Extinctions", ylab="Frequency", col="firebrick", cex.axis=0.8)
at = seq(from = 0, to = 20, by = 1)
axis(side = 1, at = at, cex.axis=0.8)
```

![](Lab05_files/figure-html/extinctions1-1.png)<!-- -->

This is all good but we need to estimate the population mean $(\lambda)$ of mass extinctions. Since we only have these particular sample data to go by, that is precisely what we will use to get $\hat{\lambda}$ and treat this $\hat{\lambda} = \lambda$. 

As it turns out, $\lambda=$ ``4.2105263``

Given $\lambda$, we can now calculate the probability of seeing no mass extinctions, 1 mass extinctions, 2 mass extinctions, and so on if indeed mass extinctions follow a Poisson distribution with a mean of 4.210526.


```r
x = seq(0, 20, by=1)
x.d = dpois(x, lambda=4.210526); x.d # probs of 0, 1, 2, ... mass extinctions
```

```
##  [1] 1.483856e-02 6.247815e-02 1.315329e-01 1.846076e-01 1.943238e-01
##  [6] 1.636411e-01 1.148358e-01 6.907418e-02 3.635483e-02 1.700811e-02
## [11] 7.161307e-03 2.741170e-03 9.618139e-04 3.115187e-04 9.368982e-05
## [16] 2.629889e-05 6.920761e-06 1.714120e-06 4.009638e-07 8.885623e-08
## [21] 1.870657e-08
```

```r
sum(x.d) # making sure the probabilities sum to 1
```

```
## [1] 1
```

Thus, in a random sample of 76 successive time intervals we would expect to see 


```r
x.d*76 
```

```
##  [1] 1.127731e+00 4.748339e+00 9.996503e+00 1.403018e+01 1.476861e+01
##  [6] 1.243672e+01 8.727523e+00 5.249638e+00 2.762967e+00 1.292616e+00
## [11] 5.442593e-01 2.083289e-01 7.309786e-02 2.367542e-02 7.120426e-03
## [16] 1.998716e-03 5.259778e-04 1.302731e-04 3.047325e-05 6.753074e-06
## [21] 1.421700e-06
```

proportion of intervals with 0, 1, 2, 3, ... mass extinctions.

Let us calculate the expected frequencies:


```r
expected = dpois(x, 4.210526) * 76
expected
```

```
##  [1] 1.127731e+00 4.748339e+00 9.996503e+00 1.403018e+01 1.476861e+01
##  [6] 1.243672e+01 8.727523e+00 5.249638e+00 2.762967e+00 1.292616e+00
## [11] 5.442593e-01 2.083289e-01 7.309786e-02 2.367542e-02 7.120426e-03
## [16] 1.998716e-03 5.259778e-04 1.302731e-04 3.047325e-05 6.753074e-06
## [21] 1.421700e-06
```

```r
round(expected, digits=2)  # rounding to two decimal places
```

```
##  [1]  1.13  4.75 10.00 14.03 14.77 12.44  8.73  5.25  2.76  1.29  0.54
## [12]  0.21  0.07  0.02  0.01  0.00  0.00  0.00  0.00  0.00  0.00
```

Because the observed number of extinctions start thinning at the upper end of the distribution, violating the assumptions of $\chi^2$, we need to collapse the first two (0 and 1) and the top end of the distribution into an omnibus category that we can call "10 or more". We do so below:


```r
new.ME = c("0 or 1", "2", "3", "4", "5", "6", "7", "8 or more"); new.ME
```

```
## [1] "0 or 1"    "2"         "3"         "4"         "5"         "6"        
## [7] "7"         "8 or more"
```

```r
new.counts = c(tab.A[1], tab.A[2:7], sum(tab.A[8:14]) ) # collapsing
new.counts
```

```
##  1  2  3  4  5  6  7    
## 13 15 16  7 10  4  2  9
```

```r
names(new.counts) = new.ME # naming the new categories
new.counts # now seeing them with the labels for each category
```

```
##    0 or 1         2         3         4         5         6         7 
##        13        15        16         7        10         4         2 
## 8 or more 
##         9
```

```r
sum(new.counts) # To verify we didn't mess up somewhere and we still have 76 extinctions
```

```
## [1] 76
```

We aren't done yet. Since we have rearranged the grouping we will need to recalculate the expected frequencies.


```r
x = seq(0 , 20, by = 1)
new.probs = dpois(x, 4.210526); new.probs
```

```
##  [1] 1.483856e-02 6.247815e-02 1.315329e-01 1.846076e-01 1.943238e-01
##  [6] 1.636411e-01 1.148358e-01 6.907418e-02 3.635483e-02 1.700811e-02
## [11] 7.161307e-03 2.741170e-03 9.618139e-04 3.115187e-04 9.368982e-05
## [16] 2.629889e-05 6.920761e-06 1.714120e-06 4.009638e-07 8.885623e-08
## [21] 1.870657e-08
```

```r
prob.0or1 = sum(new.probs[1:2]); prob.0or1
```

```
## [1] 0.07731671
```

```r
prob.2to7 = new.probs[3:8]; prob.2to7
```

```
## [1] 0.13153293 0.18460761 0.19432379 0.16364107 0.11483583 0.06907418
```

```r
prob.8plus = 1 - sum(prob.0or1, prob.2to7); prob.8plus
```

```
## [1] 0.06466788
```

```r
probs = c(prob.0or1, prob.2to7, prob.8plus); probs
```

```
## [1] 0.07731671 0.13153293 0.18460761 0.19432379 0.16364107 0.11483583
## [7] 0.06907418 0.06466788
```

```r
sum(probs)
```

```
## [1] 1
```

Now we conduct the $\chi^2$ test:

```r
xsq = chisq.test(new.counts, p=probs, correct=FALSE); xsq
```

```
## Warning in chisq.test(new.counts, p = probs, correct = FALSE): Chi-squared
## approximation may be incorrect
```

```
## 
## 	Chi-squared test for given probabilities
## 
## data:  new.counts
## X-squared = 23.95, df = 7, p-value = 0.001163
```

```r
xsq$observed # Observed frequencies used in the test
```

```
##    0 or 1         2         3         4         5         6         7 
##        13        15        16         7        10         4         2 
## 8 or more 
##         9
```

```r
xsq$expected # Expected frequencies used in the test
```

```
##    0 or 1         2         3         4         5         6         7 
##  5.876070  9.996503 14.030178 14.768608 12.436721  8.727523  5.249638 
## 8 or more 
##  4.914759
```

You get a warning `Warning message: In chisq.test(new.counts, p = probs, correct = FALSE):` because one of the expected frequencies is less than 5 but we should be okay here since we need 20% of the cells to have expected frequencies $< 5$ or at least one cell with expected frequency $< 1$ before the  $\chi^2$ assumptions are unmet.

The $\chi^2$ value is 23.95 with a $p-Value = 0.001163$. This is again incorrect because of the extra degree of freedom used up when we were forced to calculate the mean of the Poisson process from the sample. The correct $p-value$ can be calculated with one less degree of freedom as:


```r
1 - pchisq(23.95, df=6)
```

```
## [1] 0.0005334336
```
Given the p-value we can easily reject H0; the data provide sufficient evidence to conclude that mass extinctions in the fossil record DO NOT fit a Poisson distribution.  


## Practice Problems

### World Cup Goals (Problem 4)
How much do skill differences determine the outcome of soccer matches in the World Cup? If the probability of a goal is the same for all teams and matches, and if each goal is independent of another, then the distribution of goals per game should approximate the Poisson distribution. The data at hand are given below:


```r
goals = read.csv("http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08q05WorldCup.csv")
```

(a) Plot the frequency distribution of goals per team.


```r
tab.goals = table(goals$score); tab.goals
```

```
## 
##  0  1  2  3  4  5  8 
## 37 47 27 13  2  1  1
```

```r
plot(tab.goals, ylim=c(0,50),
     type = "h", lwd=3, col="tomato4", 
     ylab = "Frequency", 
     xlab="Score",
     main = "Frequency Distribution of Scores",
     sub = "(in World Cup matches)",
     cex.main=0.99) 
```

![](Lab05_files/figure-html/goalsplot-1.png)<!-- -->

(b) What is the mean number of goals?


```r
mean.goals = mean(goals$score); mean.goals
```

```
## [1] 1.257812
```

The mean number of goals scored is `mean.goals`. 

(c) Test whether the Poisson distribution characterizes the distribution of goals per game. 

Note: If we collapse games with 4, 5, and 8 goals, respectively, into an omnibus "4 or more" category we should be able to get around the problem of "thin cells" at the top of the distribution. So the new categories will be:


```r
new.cats = c(tab.goals[1:4], sum(tab.goals[5:7]))
sum(new.cats) # to see if the sample size is correct
```

```
## [1] 128
```

```r
names(new.cats) = c("0", "1", "2", "3", "4 or more")
new.cats
```

```
##         0         1         2         3 4 or more 
##        37        47        27        13         4
```

Now we are fine since exactly 20% of our cells have expected frequencies $< 5$. 

$H_0:$ Goals per game follow the Poisson distribution  
$H_A:$ Goals per game DO NOT follow the Poisson distribution  

$\alpha=0.05$  

Reject $H_0$ if the p-value is $\leq \alpha$; do not reject $H_0$ otherwise


```r
x = seq(0, 5, by=1)
probs = dpois(x, lambda=mean.goals); probs
```

```
## [1] 0.284275199 0.357564899 0.224874799 0.094283445 0.029647724 0.007458256
```

```r
sum(probs) # checking the sum of probabilities
```

```
## [1] 0.9981043
```

```r
new.probs = c(probs[1:4], 1 - sum(probs[1:4])) # adjusting probabilities to sum to 1
new.probs # checking the new probabilities
```

```
## [1] 0.28427520 0.35756490 0.22487480 0.09428344 0.03900166
```

```r
expected = new.probs * 128
expected #expected number of 0, 1, 2, ... goals 
```

```
## [1] 36.387225 45.768307 28.783974 12.068281  4.992212
```

```r
xsq = chisq.test(new.cats, p=new.probs); xsq
```

```
## Warning in chisq.test(new.cats, p = new.probs): Chi-squared approximation
## may be incorrect
```

```
## 
## 	Chi-squared test for given probabilities
## 
## data:  new.cats
## X-squared = 0.42317, df = 4, p-value = 0.9805
```

```r
1 - pchisq(0.42317, df=3)
```

```
## [1] 0.9354156
```

The p-value is quite high so we fail to reject $H_0$; the data suggest that the distribution of goals in the World Cup follows the Poisson distribution. 

We can check our observed versus expected frequencies (if we wanted to be doubly sure):


```r
xsq$observed
```

```
##         0         1         2         3 4 or more 
##        37        47        27        13         4
```

```r
xsq$expected
```

```
##         0         1         2         3 4 or more 
## 36.387225 45.768307 28.783974 12.068281  4.992212
```

It is quite obvious that there is marginal drift between our observed frequencies and what we would have expected to see if goals were distributed Poisson.


# Practice Problems
### Falling Cats
A study of the Feline High-Rise Syndrome (FHRS) included data on the number of cats that fell in a given month. Can we infer that the rate of cats falling varies between months of the year? 


```r
cats = read.csv("http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08q21FallingCatsByMonth.csv")
tab.cats = table(cats$month); tab.cats
```

```
## 
##     April    August  December  February   January      July      June 
##        10        13         5         6         4        19        14 
##     March       May  November   October September 
##         8         9         7        12        12
```

The months are not ordered correctly. Let us fix this first:


```r
cats$month = ordered(cats$month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

tab.cats = table(cats$month); tab.cats
```

```
## 
##   January  February     March     April       May      June      July 
##         4         6         8        10         9        14        19 
##    August September   October  November  December 
##        13        12        12         7         5
```

(a) Plot the distribution of FHRS in these data.


(b) Test whether FHRS rate varies across the months of the year. 



### Snapdragons
In snapdragons, variation in flower colors is determined by a single gene. *RR* individuals are red, *Rr* (heterozygous) are pink, and *rr* are white. In a cross between heterozygous individual, the expected ratio of red-flowered:pink-flowered:white-flowered offspring is 1:2:1.  

(a) The results of such a cross were 10 red, 21 pink, and 9 white offspring. Do these results differ significantly from that expected by the 1:2:1 ratio? 


(b) In another experiment, you count 100 times as many flowers and end up with 1000 red, 2100 pink, and 900 white. Do these results differ significantly from that expected by the 1:2:1 ratio?


(c) Do the proportions observed in (a) and (b) differ? Did the results of your hypothesis test differ? Why or why not? 


### Shark Attacks
In recent years shark attacks have drawn attention every summer. The data below reflect the number of shark attacks per year for the 2005 through 2014 years:


```r
attacks = c(58, 59, 70, 55, 68, 82, 79, 83, 76, 72)
year = c(seq(2005, 2014, by=1))
sharks = cbind.data.frame(year, attacks)
```

(a) Plot the distribution of shark attacks. 


(b) If shark attacks occur randomly and independently across years, what distribution would shark attacks follow? 



### Hurricanes
The Saffir-Simpson scale rates hurricane on a five-point rating system, with Category 5 being the worst. The data given here http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08q19Hurricanes.csv reflect the number of Cat 3 hurricanes over a one-hundred year period in the U.S. 


(a) Based on these data, what is the mean number of severe hurricanes to hit the U.S. per year?



(b) What model would describe the distribution of hurricanes per year if hurricanes hit independently of each other and the probability of a hurricane were the same in every year?



(c) Test the fit of the model you identified in (b) to these data. Be sure to specify the hypotheses, the $\alpha$, and the decision rule. Once you conduct the test, state your decision and hence your conclusion (in words). 







