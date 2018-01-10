---
title: "Lab 4: The Binomial Distribution & Test"
author: "@ruhil"
date: "February 1, 2016"
output: 
  html_document:
    highlight: tango
    keep_md: yes
    mathjax: http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML
    self_contained: yes
    theme: flatly
    toc: yes
---



# Flipping coins again 
Say you are flipping a fair coin twice. You know you can end up with Heads or Tails on any given toss. The sample space would then be $S = \left\{ HH, HT, TH, TT \right\}$, i.e., the sample size is $n=4$. The probability of getting exactly one Tails is $\dfrac{2}{4} = 0.5$ and so is the probability of one Heads. What if we flipped the coin ten times? What would be the probability of exactly three Tails in ten flips of the coin? We could spell out the full sample space and then calculate the needed probability but that would be tedious. It would be easier to use a formula that does this calculation for us. 

$$
\begin{align} 	
P\left[X \text{ successes}\right] = \binom{n}{X}p^{X}\left(1 - p\right)^{n-X} \\
& \text{where } \binom{n}{x} = \dfrac{n!}{X!(n-X)!} \text{ and } \\
& n! = n \times (n-1) \times (n-2) \times \cdots \times 2 \times 1
\end{align}
$$

Let us define Success as a Tails resulting on the flip, and X being 3 Tails in ten flips of the coin (i.e., $n=10$). Let us also assume that it is a fair coin so that the probability of Tails on any single flip of the coin is $p=0.5$. Plugging these values into the equation will yield:

$$
\begin{align} 	
P\left[3 \text{ successes}\right] = \binom{10}{3}0.5^{3}\left(1 - 0.5 \right)^{10-3} \\
& = \dfrac{10 \times 9 \times 8 \times \cdots \times 1}{\left(3 \times 2 \times 1 \right) \left(7 \times 6 \times \cdots \times 1 \right)} \left(0.5 \right)^{3} \left(0.5 \right)^{7} \\
& = \dfrac{10 \times 9 \times 8 }{\left(3 \times 2 \times 1 \right)} \left(0.5 \right)^{3} \left(0.5 \right)^{7} \\
& = \left(10 \times 3 \times 4 \right)\left(0.5 \right)^{3} \left(0.5 \right)^{7} \\
& = \left(120\right) \left(0.125 \right) \left(0.0078125 \right) \\
& = 0.1171875
\end{align}
$$

Using R as a calculator we could solve the above formula as follows:


```r
n = 10
x = 3
p = 0.5
p.3tails = choose(n, x) * (p)^(x) * (1 - p)^{n - x}
p.3tails
```

```
## [1] 0.1171875
```

What about three or more Tails? We could calculate the the probability of exactly three Tails, four Tails, and so on through ten Tails, add these up, and we would have our answer ... all using the formula above. An R function, <code>rbinom()</code>, can do these calculations for us quite easily. Below we do so for the example above.  


```r
set.seed(13579)
n = 10000000
x = rbinom(n, 10, p=0.5)
tab.x = table(x); tab.x
```

```
## x
##       0       1       2       3       4       5       6       7       8 
##    9736   97255  438778 1169791 2051968 2462822 2050665 1171457  439391 
##       9      10 
##   98260    9877
```

```r
ptab.x = (tab.x / n); ptab.x
```

```
## x
##         0         1         2         3         4         5         6 
## 0.0009736 0.0097255 0.0438778 0.1169791 0.2051968 0.2462822 0.2050665 
##         7         8         9        10 
## 0.1171457 0.0439391 0.0098260 0.0009877
```

```r
plot(ptab.x, ylab="Probability", xlab="No. of Tails", xlim=c(0, 10), ylim=c(0, 0.25), col="firebrick") 
```

![](Lab04_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Note: If it is a fair coin then half the flips result in Tails ... this is the most likely outcome. Say you flip the coin 10 times and you end up with only 1 Tails. Could this by chance? Sure, because we can see that this could happen with some non-zero probability, just as we could also end up with no Tails or even with ten Tails! Strange outcomes can result, even if they occur very rarely, and by chance alone. As a result, we have to generate some rules that can guide us in determining when to suspect that something is wrong because of an unusual result and when to determine that the result isn't very unusual after all.  

Before we move on, however, let us see how we might calculate the probability of three or more Tails. 


```r
p.3plus = sum(ptab.x[c(4, 5, 6, 7, 8, 9, 10, 11)])
p.3plus
```

```
## [1] 0.9454231
```

Note how we did this. We asked R to add the probability of 3 Tails (which is the fourth element in <code>ptab.x</code>), 4 Tails, 5 Tails, and so on through to 10 Tails (the eleventh element in <code>ptab.x</code>). We could do this more succinctly:


```r
p.3plus = sum(ptab.x[4:11])
p.3plus
```

```
## [1] 0.9454231
```

What about the probability of two or fewer Tails?


```r
p.2less = sum(ptab.x[1:3])
p.2less
```

```
## [1] 0.0545769
```

Note that this is just $0.0009736 + 0.0097255 + 0.0438778$. 

What about the probability of more than two Tails? This should be just 1 minus the probability of two or fewer Tails. That is, 1 - 0.0545769, i.e., ``0.9454231``, exactly the same result we got when we calculated <code>p.3plus</code>. Note that we saw this last week: $P(A^c) = 1 - P(A)$.


# The Binomial Test
Standard Error: $SE_{\hat{p}} = \sqrt{\dfrac{\hat{p}\left(1-\hat{p} \right)}{n-1}}$

For the Agresti-Coull confidence interval:  
First calculate $p^{'} = \dfrac{X+2}{n+4}$  
CI is then given by: $p^{'} - z\sqrt{ \dfrac{p^{'} \left(1-p^{'} \right) } {n+4} } < p < p^{'} + z\sqrt{ \dfrac{p^{'} \left(1-p^{'} \right) } {n+4} }$ 


Alternatively, use the <code>binom</code> library as shown below:


```r
library(binom)
binom.test(x, n, p=?, alternative="?", conf.level=?)
binom.confint(x, n, p=?, alternative="?", conf.level=?, methods = "ac")
```

## Problem 7.4
In 1955, John Wayne played Genghis Khan in _The Conqueror_. The movie was filmed downwind from a site where 11 overground nuclear bomb tests had been conducted. Of the 220 crew on the movie, 91 ended up being diagnosed with cancer by the early 1980s. Population data record that only about 14% of people in the same age-group as the movie crew should, on average, have been diagnosed with cancer within the same time frame of exposure. 

(a) What is the best estimate of the probability of a member of the crew getting cancer within the study interval? 


```r
n = 220
x = 91
p.sample = x/n
p.sample
```

```
## [1] 0.4136364
```

The best estimate is ``0.4136364``

(b) What is the standard error of your estimate? What does this quantity measure?


```r
se.sample = sqrt( p.sample * ( (1 - p.sample) / (n - 1) ) )
se.sample
```

```
## [1] 0.03327904
```

The standard error, ``0.033279``, measures the standard deviation of the sampling distribution of the sample proportion.

(c) What is the 95% confidence interval for your probability estimate? Does this interval bracket the 14% population rate? Interpret your result. 


```r
p.prime = (x + 2) / (n + 4)
p.prime
```

```
## [1] 0.4151786
```

```r
ci.low = p.prime - (1.96 * sqrt( p.prime * (1 - p.prime) / (n + 4)))
ci.high = p.prime + (1.96 * sqrt( p.prime * (1 - p.prime) / (n + 4)))
ci.low; ci.high
```

```
## [1] 0.3506486
```

```
## [1] 0.4797085
```

The 95% confidence interval is ``0.4797085``. This interval does not include the 14% cancer rate for the population, suggesting that cancer rate for the movie crew was not the same as that for the population. 

(d) Test whether the cancer rate for the movie crew was different from that for the population. Use $\alpha = 0.05$ and calculate the Agresti-Coull confidence interval.

H0: The cancer rate for the movie crew is the same as that of the population (p = 0.14)  
HA: The cancer rate for the movie crew is NOT the same as that of the population  (p is not equal to 0.14)  


```r
library(binom)
binom.test(x, n, p=0.14, alternative="two.sided", conf.level=0.95)
```

```
## 
## 	Exact binomial test
## 
## data:  x and n
## number of successes = 91, number of trials = 220, p-value <
## 2.2e-16
## alternative hypothesis: true probability of success is not equal to 0.14
## 95 percent confidence interval:
##  0.3478469 0.4817857
## sample estimates:
## probability of success 
##              0.4136364
```

```r
binom.confint(x, n, p=0.14, alternative="two.sided", conf.level=0.95, methods = "ac")
```

```
##          method  x   n      mean     lower     upper
## 1 agresti-coull 91 220 0.4136364 0.3505683 0.4796687
```

The P-value is practically 0 so we can easily reject the null hypothesis (H0). The data suggest that the cancer rate for the movie crew was different from that of the population.


## Problem 7.9
With global warming, species might (a) evolve to better adapt to warmer climates, (b) move closer to the poles to experience the colder temperatures it had in the past, or (c) go extinct. A recent study of the range limits of European butterflies found that of 24 species that had changed their ranges in the last 100 years, 22 had moved farther north and only two had moved south. Test the hypothesis that the fraction of butterfly species moving to the north is different from the fraction moving south. Calculate the appropriate 95% confidence interval as well. 

H0: There is no difference in the fraction of species moving north or south (p = 0.5)  
HA: There is a difference in the fraction of species moving north or south (p is not equal to 0.5)

Let us set $\alpha=0.05$


```r
x = 22
n = 24
library(binom)
binom.test(x, n, p=0.5, alternative="two.sided", conf.level=0.95)
```

```
## 
## 	Exact binomial test
## 
## data:  x and n
## number of successes = 22, number of trials = 24, p-value =
## 3.588e-05
## alternative hypothesis: true probability of success is not equal to 0.5
## 95 percent confidence interval:
##  0.7300272 0.9897437
## sample estimates:
## probability of success 
##              0.9166667
```

```r
binom.confint(x, n, p=0.5, alternative="two.sided", conf.level=0.95, methods = "ac")
```

```
##          method  x  n      mean     lower    upper
## 1 agresti-coull 22 24 0.9166667 0.7299712 0.988382
```

The P-value is almost 0 and so we can reject the null hypothesis. The data suggest that the fraction of species moving north is not the same as the fraction moving south.


## Problem 7.14
A common perception is that people suffering from chronic illness may be able to delay their deaths until after a special upcoming event, like Christmas. Out of 12,028 deaths from cancer in either the week before or after Christmas, 6052 happened in the week before. 

(a) What is the best estimate of the proportion of deaths that occurred in the week before Christmas?


(b) What is the 95% confidence interval for this estimate?


(c) Conduct a test to see whether people are able to delay their deaths. Setup the hypotheses, use $\alpha = 0.01$, calculate the corresponding confidence interval, and state your conclusion.  



