# Model
Julian During  
2 Februar 2017  





# Model Basics

## What happens if you repeat the analysis of `sim2` using a model without an intercept. What happens to the model equation? What happens to the predictions?


```r
sim2
```

```
## # A tibble: 40 × 2
##        x          y
##    <chr>      <dbl>
## 1      a  1.9353632
## 2      a  1.1764886
## 3      a  1.2436855
## 4      a  2.6235489
## 5      a  1.1120381
## 6      a  0.8660030
## 7      a -0.9100875
## 8      a  0.7207628
## 9      a  0.6865540
## 10     a  2.0673079
## # ... with 30 more rows
```

```r
ggplot(sim2, aes(x = x, y = y)) +
  geom_point()
```

![](04_model_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mod2 <- lm(y ~ x - 1, data = sim2)

grid <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2)
grid
```

```
## # A tibble: 4 × 2
##       x     pred
##   <chr>    <dbl>
## 1     a 1.152166
## 2     b 8.116039
## 3     c 6.127191
## 4     d 1.910981
```

```r
ggplot(sim2, aes(x)) + 
  geom_point(aes(y = y)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)
```

![](04_model_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

* Nothing different happens

## For `sim4`, which of `mod1` and `mod2` is better? I think mod2 does a slightly better job at removing patterns, but it’s pretty subtle. Can you come up with a plot to support my claim?


```r
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)
sim4_res <- sim4 %>% 
  gather_residuals(mod1, mod2)

sim4_res %>% 
  ggplot(aes(x = x1, y = resid, color = x2)) + 
    geom_point() + 
    geom_smooth() +
    facet_grid(model ~ x2)
```

![](04_model_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## Model Buildung

## In the plot of `lcarat` vs. `lprice`, there are some bright vertical strips. What do they represent?



```r
diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))

ggplot(diamonds2, aes(x = lcarat, y = lprice)) + 
  geom_hex(bins = 50)
```

![](04_model_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
diamonds2 %>% 
  filter(lcarat > -2, lcarat < -1.5) 
```

```
## # A tibble: 10,273 × 12
##    carat       cut color clarity depth table price     x     y     z
##    <dbl>     <ord> <ord>   <ord> <dbl> <dbl> <int> <dbl> <dbl> <dbl>
## 1   0.29   Premium     I     VS2  62.4    58   334  4.20  4.23  2.63
## 2   0.31      Good     J     SI2  63.3    58   335  4.34  4.35  2.75
## 3   0.26 Very Good     H     SI1  61.9    55   337  4.07  4.11  2.53
## 4   0.30      Good     J     SI1  64.0    55   339  4.25  4.28  2.73
## 5   0.31     Ideal     J     SI2  62.2    54   344  4.35  4.37  2.71
## 6   0.32   Premium     E      I1  60.9    58   345  4.38  4.42  2.68
## 7   0.30     Ideal     I     SI2  62.0    54   348  4.31  4.34  2.68
## 8   0.30      Good     J     SI1  63.4    54   351  4.23  4.29  2.70
## 9   0.30      Good     J     SI1  63.8    56   351  4.23  4.26  2.71
## 10  0.30 Very Good     J     SI1  62.7    59   351  4.21  4.27  2.66
## # ... with 10,263 more rows, and 2 more variables: lprice <dbl>,
## #   lcarat <dbl>
```

* Many diamonds get a carat value rounded up to the next higher unit, so that 
they can be sold for more money

## If `log(price) = a_0 + a_1 * log(carat)`, what does that say about the relationship between `price` and `carat`?

* That the price is exponentnially growing with carat.

## Extract the diamonds that have very high and very low residuals. Is there anything unusual about these diamonds? Are the particularly bad or good, or do you think these are pricing errors?


```r
mod_diamond2 <- lm(
  lprice ~ lcarat + color + cut + clarity, 
  data = diamonds2
)

diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond2, "lresid2") 

diamonds2 %>% 
  top_n(lresid2, n = 5)
```

```
## # A tibble: 5 × 13
##   carat     cut color clarity depth table price     x     y     z
##   <dbl>   <ord> <ord>   <ord> <dbl> <dbl> <int> <dbl> <dbl> <dbl>
## 1  0.25    Fair     F     SI2  54.4    64  1013  4.30  4.23  2.32
## 2  0.25 Premium     G     SI2  59.0    60  1186  5.33  5.28  3.12
## 3  0.25 Premium     G     SI2  58.8    60  1186  5.33  5.28  3.12
## 4  0.29    Fair     F     SI1  55.8    60  1776  4.48  4.41  2.48
## 5  0.34    Fair     F      I1  55.8    62  2160  4.72  4.60  2.60
## # ... with 3 more variables: lprice <dbl>, lcarat <dbl>, lresid2 <dbl>
```

```r
diamonds2 %>% 
  top_n(lresid2, n = -5)
```

```
## # A tibble: 5 × 13
##   carat     cut color clarity depth table price     x     y     z   lprice
##   <dbl>   <ord> <ord>   <ord> <dbl> <dbl> <int> <dbl> <dbl> <dbl>    <dbl>
## 1  1.27 Premium     H     SI2  59.3    61  2845  7.12  7.05  4.20 11.47421
## 2  1.52    Good     E      I1  57.3    58  3105  7.53  7.42  4.28 11.60038
## 3  1.52    Good     E      I1  57.3    58  3105  7.53  7.42  4.28 11.60038
## 4  2.46 Premium     E     SI2  59.7    59 10470  8.82  8.76  5.25 13.35397
## 5  1.03    Fair     E      I1  78.2    54  1262  5.72  5.59  4.42 10.30150
## # ... with 2 more variables: lcarat <dbl>, lresid2 <dbl>
```

```r
ggplot(diamonds2, aes(x = lcarat, y = lprice)) + 
  geom_hex(bins = 50) + 
  geom_point(data = diamonds2 %>% top_n(lresid2, n = 10), color = "green") + 
  geom_point(data = diamonds2 %>% top_n(lresid2, n = -10), color = "red")
```

![](04_model_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
ggplot(diamonds2, aes(lcarat, lresid2)) +
  geom_point()
```

![](04_model_files/figure-html/unnamed-chunk-5-2.png)<!-- -->


* The diamonds, that we have understimated, seem to have a very similar carat 
number
* The overestimated diamonds don't have a similar carat number

## Does the final model, `mod_diamonds2`, do a good job of predicting diamond prices? Would you trust it to tell you how much to spend if you were buying a diamond?

* There is still some pattern in the residuals left. Maybe include shape information (brillants??)?
Formula: Gewicht in Karat = Durchmesser^3 * 0.0037

## Use your Google sleuthing skills to brainstorm why there were fewer than expected flights on Jan 20, May 26, and Sep 1. (Hint: they all have the same explanation.) How would these days generalise to another year?

* No idea

## Create a new variable that splits the wday variable into terms, but only for Saturdays, i.e. it should have Thurs, Fri, but Sat-summer, Sat-spring, Sat-fall. How does this model compare with the model with every combination of wday and term?

