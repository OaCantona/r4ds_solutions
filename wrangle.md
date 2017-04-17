# Wrangle
Julian During  
24 Februar 2017  




```r
library(tidyverse)
library(stringr)
```


# Tidy Data

## Using prose, describe how the variables and observations are organised in each of the sample tables.

* `table1`
Every observations is a row and every variable is a column.

* `table2`
The variable `type` determines which variable we are currently looking at.

* `table3`
`rate` is the only variable in this one. It is already calculated, so it's hard 
to figure out the meaning.

* `table4a`
The years are presented as columns in this one. The numbers represent the cases

* `table4b`
The years are presented as columns in this one. The numbers represent the cases

## Compute the rate for table2, and table4a + table4b. Which representation is easiest to work with? Which is hardest? Why?

How it should look like


```r
table1 %>% 
  mutate(rate = cases / population * 10000)
```

```
## # A tibble: 6 × 5
##       country  year  cases population     rate
##         <chr> <int>  <int>      <int>    <dbl>
## 1 Afghanistan  1999    745   19987071 0.372741
## 2 Afghanistan  2000   2666   20595360 1.294466
## 3      Brazil  1999  37737  172006362 2.193930
## 4      Brazil  2000  80488  174504898 4.612363
## 5       China  1999 212258 1272915272 1.667495
## 6       China  2000 213766 1280428583 1.669488
```

For table2


```r
table2 %>% 
  group_by(country, year) %>% 
  summarise(rate = count[type == "cases"] / count[type == "population"] * 10000)
```

```
## Source: local data frame [6 x 3]
## Groups: country [?]
## 
##       country  year     rate
##         <chr> <int>    <dbl>
## 1 Afghanistan  1999 0.372741
## 2 Afghanistan  2000 1.294466
## 3      Brazil  1999 2.193930
## 4      Brazil  2000 4.612363
## 5       China  1999 1.667495
## 6       China  2000 1.669488
```

For table 3: 


```r
table3 %>% 
  mutate(
    strings = str_split(rate, "/"), 
    cases = map_int(strings, ~ as.integer(.[1])), 
    population = map_int(strings, ~ as.integer(.[2]))) %>% 
  group_by(country, year) %>% 
  summarise(rate = cases / population * 10000)
```

```
## Source: local data frame [6 x 3]
## Groups: country [?]
## 
##       country  year     rate
##         <chr> <int>    <dbl>
## 1 Afghanistan  1999 0.372741
## 2 Afghanistan  2000 1.294466
## 3      Brazil  1999 2.193930
## 4      Brazil  2000 4.612363
## 5       China  1999 1.667495
## 6       China  2000 1.669488
```

For table 4a + 4b: 


```r
table4a_tidy <- table4a %>% 
  gather(key = year, value = cases, -country)

table4b_tidy <- table4b %>% 
  gather(key = year, value = population, -country)

table4a_tidy %>% 
  left_join(table4b_tidy) %>% 
  group_by(country, year) %>% 
  summarise(rate = cases / population * 10000)
```

```
## Joining, by = c("country", "year")
```

```
## Source: local data frame [6 x 3]
## Groups: country [?]
## 
##       country  year     rate
##         <chr> <chr>    <dbl>
## 1 Afghanistan  1999 0.372741
## 2 Afghanistan  2000 1.294466
## 3      Brazil  1999 2.193930
## 4      Brazil  2000 4.612363
## 5       China  1999 1.667495
## 6       China  2000 1.669488
```

## Recreate the plot showing change in cases over time using table2 instead of table1. What do you need to do first?


```r
table2 %>% 
  filter(type == "cases") %>% 
  ggplot(aes(year, count)) + 
    geom_line(aes(group = country), colour = "grey50") + 
    geom_point(aes(colour = country))
```

![](wrangle_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

First you have to include a filter, to show only `type == "cases"`

## Why are `gather()` and `spread()` not perfectly symmetrical? Carefully consider the following example: (Hint: look at the variable types and think about column names.) Both `spread()` and `gather()` have a convert argument. What does it do?


```r
stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>% 
  spread(key = year, value = return) %>% 
  gather(key = "year", value = "return", `2015`:`2016`)
```

```
## # A tibble: 4 × 3
##    half  year return
##   <dbl> <chr>  <dbl>
## 1     1  2015   1.88
## 2     2  2015   0.59
## 3     1  2016   0.92
## 4     2  2016   0.17
```

In the `spread` step, the year variable becomes the new column names. 
Therefore they get turned from integer into characters. 
If you want to prevent this behavior, you could use the `convert` parameter.

## Why does this code fail?


```r
#table4a %>% 
#  gather(1999, 2000, key = "year", value = "cases")
```

Because the years are not quoted. It is mandatory to do this, because these are
nonsyntactic names.

## Why does spreading this tibble fail? How could you add a new column to fix the problem?


```r
people <- tribble(
  ~name,             ~key,    ~value,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)

people %>% 
  mutate(id = c(1, 1, 2, 3, 3)) %>% 
  spread(key = key, value = value)
```

```
## # A tibble: 3 × 4
##              name    id   age height
## *           <chr> <dbl> <dbl>  <dbl>
## 1 Jessica Cordero     3    37    156
## 2   Phillip Woods     1    45    186
## 3   Phillip Woods     2    50     NA
```

Because you have duplicate indentifiers ("Phillip Woods" and "age"). 
By introducing a unique `id` column, one could fix this problem.

## Tidy the simple tibble below. Do you need to spread or gather it? What are the variables?


```r
preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)

preg %>% 
  gather(key = "sex", value = "number", -pregnant)
```

```
## # A tibble: 4 × 3
##   pregnant    sex number
##      <chr>  <chr>  <dbl>
## 1      yes   male     NA
## 2       no   male     20
## 3      yes female     10
## 4       no female     12
```

You need to gather it. The variables are pregnant, sex and number.

## What do the `extra` and `fill` arguments do in `separate()`? Experiment with the various options for the following two toy datasets.


```r
test1 <- tibble(x = c("a,b,c", "d,e,f,g", "h,i,j"))

test1 %>% 
  separate(x, c("one", "two", "three"), extra = "drop")
```

```
## # A tibble: 3 × 3
##     one   two three
## * <chr> <chr> <chr>
## 1     a     b     c
## 2     d     e     f
## 3     h     i     j
```

```r
test1 %>% 
  separate(x, c("one", "two", "three"), extra = "merge")
```

```
## # A tibble: 3 × 3
##     one   two three
## * <chr> <chr> <chr>
## 1     a     b     c
## 2     d     e   f,g
## 3     h     i     j
```

```r
test2 <- tibble(x = c("a,b,c", "d,e", "f,g,i"))

test2 %>% 
  separate(x, c("one", "two", "three"), fill = "right")
```

```
## # A tibble: 3 × 3
##     one   two three
## * <chr> <chr> <chr>
## 1     a     b     c
## 2     d     e  <NA>
## 3     f     g     i
```

```r
test2 %>% 
  separate(x, c("one", "two", "three"), fill = "left")
```

```
## # A tibble: 3 × 3
##     one   two three
## * <chr> <chr> <chr>
## 1     a     b     c
## 2  <NA>     d     e
## 3     f     g     i
```

## Both `unite()` and `separate()` have a `remove` argument. What does it do? Why would you set it to `FALSE`?

It determines, if the original column should be kept or removed. If you set it
to `FALSE`, the original column is kept.

## Compare and contrast `separate()` and `extract()`. Why are there three variations of separation (by position, by separator, and with groups), but only one unite?

Extract looks at existing groups (based on a regex) in the data. If not present
return `NA`.

## Compare and contrast the `fill` arguments to `spread()` and `complete()`.

The fill argument in `spread` will replace explicit missing values. `complete`
won't do that.

## What does the direction argument to fill() do

It determines if the missing values should be filled from "down" or "up".

## In this case study I set `na.rm = TRUE` just to make it easier to check that we had the correct values. Is this reasonable? Think about how missing values are represented in this dataset. Are there implicit missing values? What’s the difference between an `NA` and `zero`?

You should first check, that `NA` values are not explicit missing values. 
In this dataset, the explicit missing values are represented by 0.

# Strings

## In code that doesn't use stringr, you'll often see `paste()` and `paste0()`. What's the difference between the two functions? What stringr function are they equivalent to? How do the functions differ in their handling of `NA`?

* `paste0` automatically takes an empty string as `sep` argument
* str_c


```r
paste0("a", "b", NA, "d")
```

```
## [1] "abNAd"
```

```r
str_c("a", "b", NA, "d")
```

```
## [1] NA
```

* `str_c` converts to `NA`, if one string is `NA`

##In your own words, describe the difference between the `sep` and `collapse` arguments to `str_c()`.


```r
str_c(c("a", "b"), c("c", "d"), "hello", sep = ",") 
```

```
## [1] "a,c,hello" "b,d,hello"
```

```r
str_c(c("a", "b"), c("c", "d"), "hello", collapse = ",")
```

```
## [1] "achello,bdhello"
```

The `collapse` argument, collapses the different strings into one big string. 
`sep` doesn't.

## Use `str_length()` and `str_sub()` to extract the middle character from a string. What will you do if the string has an even number of characters?


```r
test1 <- "hallo"
test2 <- "test"

middle_character <- function(string) {
  middle <- (str_length(string) + 1) / 2
  str_sub(string, start = middle, end = middle)
}

middle_character("hallo")
```

```
## [1] "l"
```

```r
middle_character("test")
```

```
## [1] "e"
```

You have to decide, if you choose the left or right character from the middle.
You could do this by including an additional parameter in the above function.

## What does `str_wrap()` do? When might you want to use it?

It determines when to put text in a new line. You might use it, when you want a 
nicely formated text and only have a fixed width of space available.

## What does `str_trim()` do? What's the opposite of `str_trim()`?

It trims leading and trailing whitespaces from a string. 
The opposite is `str_pad`.

## Write a function that turns (e.g.) a vector `c("a", "b", "c")` into the string `a, b, and c`.Think carefully about what it should do if given a vector of length 0, 1, or 2.


```r
str_add_and <- function(string_vector) {
  if (length(string_vector) <= 1)
    return(string_vector)
  if (length(string_vector) == 2)
    return(str_c(string_vector, collapse = " and "))
  last_string <- string_vector[length(string_vector)]
  front_string <- string_vector[-length(string_vector)]
  str_c(str_c(front_string, collapse = ", "), last_string, sep = " and ")
}
str_add_and(c("a", "b", "c", "d"))
```

```
## [1] "a, b, c and d"
```

```r
str_add_and(c("a"))
```

```
## [1] "a"
```

```r
str_add_and(c("a", "b"))
```

```
## [1] "a and b"
```

```r
str_add_and(c())
```

```
## NULL
```

## Explain why each of these strings don't match a `\`: `"\"`, `"\\"`, `"\\\"`.

* It only uses the special ability of the backslash
* To create a regex, you need a string
* In the string, you also need to escape the backslash

## How would you match the sequence `"'\`


```r
str_view('aaaa\"\'\\bbbb', "\\\"\\\'\\\\")
```

<!--html_preserve--><div id="htmlwidget-6baf119921f804f2c790" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-6baf119921f804f2c790">{"x":{"html":"<ul>\n  <li>aaaa<span class='match'>\"'\\<\/span>bbbb<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

## What patterns will the regular expression `\..\..\..` match? How would you represent it as a string?


```r
str_view(".a.b.c", '\\..\\..\\..')
```

<!--html_preserve--><div id="htmlwidget-f7f141250408d53b46ec" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-f7f141250408d53b46ec">{"x":{"html":"<ul>\n  <li><span class='match'>.a.b.c<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

```r
writeLines('\\..\\..\\..')
```

```
## \..\..\..
```

## How would you match the literal string `"$^$"`?


```r
str_view("$^$", "\\$\\^\\$")
```

<!--html_preserve--><div id="htmlwidget-e6a346428359d6ec228d" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-e6a346428359d6ec228d">{"x":{"html":"<ul>\n  <li><span class='match'>$^$<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

## Given the corpus of common words in stringr::words, create regular expressions that find all words that:
    Start with “y”.
    End with “x”
    Are exactly three letters long. (Don’t cheat by using str_length()!)
    Have seven letters or more.


```r
words[str_detect(words, "^y")]
```

```
## [1] "year"      "yes"       "yesterday" "yet"       "you"       "young"
```


```r
words[str_detect(words, "$x")]
```

```
## character(0)
```


```r
words[str_detect(words, "^...$")]
```

```
##   [1] "act" "add" "age" "ago" "air" "all" "and" "any" "arm" "art" "ask"
##  [12] "bad" "bag" "bar" "bed" "bet" "big" "bit" "box" "boy" "bus" "but"
##  [23] "buy" "can" "car" "cat" "cup" "cut" "dad" "day" "die" "dog" "dry"
##  [34] "due" "eat" "egg" "end" "eye" "far" "few" "fit" "fly" "for" "fun"
##  [45] "gas" "get" "god" "guy" "hit" "hot" "how" "job" "key" "kid" "lad"
##  [56] "law" "lay" "leg" "let" "lie" "lot" "low" "man" "may" "mrs" "new"
##  [67] "non" "not" "now" "odd" "off" "old" "one" "out" "own" "pay" "per"
##  [78] "put" "red" "rid" "run" "say" "see" "set" "sex" "she" "sir" "sit"
##  [89] "six" "son" "sun" "tax" "tea" "ten" "the" "tie" "too" "top" "try"
## [100] "two" "use" "war" "way" "wee" "who" "why" "win" "yes" "yet" "you"
```


```r
words[str_detect(words, ".......$")]
```

```
##   [1] "absolute"    "account"     "achieve"     "address"     "advertise"  
##   [6] "afternoon"   "against"     "already"     "alright"     "although"   
##  [11] "america"     "another"     "apparent"    "appoint"     "approach"   
##  [16] "appropriate" "arrange"     "associate"   "authority"   "available"  
##  [21] "balance"     "because"     "believe"     "benefit"     "between"    
##  [26] "brilliant"   "britain"     "brother"     "business"    "certain"    
##  [31] "chairman"    "character"   "Christmas"   "colleague"   "collect"    
##  [36] "college"     "comment"     "committee"   "community"   "company"    
##  [41] "compare"     "complete"    "compute"     "concern"     "condition"  
##  [46] "consider"    "consult"     "contact"     "continue"    "contract"   
##  [51] "control"     "converse"    "correct"     "council"     "country"    
##  [56] "current"     "decision"    "definite"    "department"  "describe"   
##  [61] "develop"     "difference"  "difficult"   "discuss"     "district"   
##  [66] "document"    "economy"     "educate"     "electric"    "encourage"  
##  [71] "english"     "environment" "especial"    "evening"     "evidence"   
##  [76] "example"     "exercise"    "expense"     "experience"  "explain"    
##  [81] "express"     "finance"     "fortune"     "forward"     "function"   
##  [86] "further"     "general"     "germany"     "goodbye"     "history"    
##  [91] "holiday"     "hospital"    "however"     "hundred"     "husband"    
##  [96] "identify"    "imagine"     "important"   "improve"     "include"    
## [101] "increase"    "individual"  "industry"    "instead"     "interest"   
## [106] "introduce"   "involve"     "kitchen"     "language"    "machine"    
## [111] "meaning"     "measure"     "mention"     "million"     "minister"   
## [116] "morning"     "necessary"   "obvious"     "occasion"    "operate"    
## [121] "opportunity" "organize"    "original"    "otherwise"   "paragraph"  
## [126] "particular"  "pension"     "percent"     "perfect"     "perhaps"    
## [131] "photograph"  "picture"     "politic"     "position"    "positive"   
## [136] "possible"    "practise"    "prepare"     "present"     "pressure"   
## [141] "presume"     "previous"    "private"     "probable"    "problem"    
## [146] "proceed"     "process"     "produce"     "product"     "programme"  
## [151] "project"     "propose"     "protect"     "provide"     "purpose"    
## [156] "quality"     "quarter"     "question"    "realise"     "receive"    
## [161] "recognize"   "recommend"   "relation"    "remember"    "represent"  
## [166] "require"     "research"    "resource"    "respect"     "responsible"
## [171] "saturday"    "science"     "scotland"    "secretary"   "section"    
## [176] "separate"    "serious"     "service"     "similar"     "situate"    
## [181] "society"     "special"     "specific"    "standard"    "station"    
## [186] "straight"    "strategy"    "structure"   "student"     "subject"    
## [191] "succeed"     "suggest"     "support"     "suppose"     "surprise"   
## [196] "telephone"   "television"  "terrible"    "therefore"   "thirteen"   
## [201] "thousand"    "through"     "thursday"    "together"    "tomorrow"   
## [206] "tonight"     "traffic"     "transport"   "trouble"     "tuesday"    
## [211] "understand"  "university"  "various"     "village"     "wednesday"  
## [216] "welcome"     "whether"     "without"     "yesterday"
```

## Create regular expressions to find all words that:
    Start with a vowel.
    That only contain consonants. (Hint: thinking about matching “not”-vowels.)
    End with ed, but not with eed.
    End with ing or ise.


```r
words[str_detect(words, "^[aeiou]")]
```

```
##   [1] "a"           "able"        "about"       "absolute"    "accept"     
##   [6] "account"     "achieve"     "across"      "act"         "active"     
##  [11] "actual"      "add"         "address"     "admit"       "advertise"  
##  [16] "affect"      "afford"      "after"       "afternoon"   "again"      
##  [21] "against"     "age"         "agent"       "ago"         "agree"      
##  [26] "air"         "all"         "allow"       "almost"      "along"      
##  [31] "already"     "alright"     "also"        "although"    "always"     
##  [36] "america"     "amount"      "and"         "another"     "answer"     
##  [41] "any"         "apart"       "apparent"    "appear"      "apply"      
##  [46] "appoint"     "approach"    "appropriate" "area"        "argue"      
##  [51] "arm"         "around"      "arrange"     "art"         "as"         
##  [56] "ask"         "associate"   "assume"      "at"          "attend"     
##  [61] "authority"   "available"   "aware"       "away"        "awful"      
##  [66] "each"        "early"       "east"        "easy"        "eat"        
##  [71] "economy"     "educate"     "effect"      "egg"         "eight"      
##  [76] "either"      "elect"       "electric"    "eleven"      "else"       
##  [81] "employ"      "encourage"   "end"         "engine"      "english"    
##  [86] "enjoy"       "enough"      "enter"       "environment" "equal"      
##  [91] "especial"    "europe"      "even"        "evening"     "ever"       
##  [96] "every"       "evidence"    "exact"       "example"     "except"     
## [101] "excuse"      "exercise"    "exist"       "expect"      "expense"    
## [106] "experience"  "explain"     "express"     "extra"       "eye"        
## [111] "idea"        "identify"    "if"          "imagine"     "important"  
## [116] "improve"     "in"          "include"     "income"      "increase"   
## [121] "indeed"      "individual"  "industry"    "inform"      "inside"     
## [126] "instead"     "insure"      "interest"    "into"        "introduce"  
## [131] "invest"      "involve"     "issue"       "it"          "item"       
## [136] "obvious"     "occasion"    "odd"         "of"          "off"        
## [141] "offer"       "office"      "often"       "okay"        "old"        
## [146] "on"          "once"        "one"         "only"        "open"       
## [151] "operate"     "opportunity" "oppose"      "or"          "order"      
## [156] "organize"    "original"    "other"       "otherwise"   "ought"      
## [161] "out"         "over"        "own"         "under"       "understand" 
## [166] "union"       "unit"        "unite"       "university"  "unless"     
## [171] "until"       "up"          "upon"        "use"         "usual"
```


```r
words[str_detect(words, "^[^aeiou]+$")]
```

```
## [1] "by"  "dry" "fly" "mrs" "try" "why"
```



```r
words[str_detect(words, "[^e]ed$")]
```

```
## [1] "bed"     "hundred" "red"
```


```r
words[str_detect(words, "ing$|ise$")]
```

```
##  [1] "advertise" "bring"     "during"    "evening"   "exercise" 
##  [6] "king"      "meaning"   "morning"   "otherwise" "practise" 
## [11] "raise"     "realise"   "ring"      "rise"      "sing"     
## [16] "surprise"  "thing"
```

## Empirically verify the rule “i before e except after c”.


```r
words[str_detect(words, "ei|cie")]
```

```
## [1] "eight"   "either"  "receive" "science" "society" "weigh"
```

Rule does not apply?

## Is “q” always followed by a “u”?


```r
words[str_detect(words, "q[^u]")]
```

```
## character(0)
```

Yes.

## Write a regular expression that matches a word if it’s probably written in British English, not American English.

?

## Create a regular expression that will match telephone numbers as commonly written in your country.


```r
str_view(string = "07431 1234", "\\d+\\s\\d+")
```

<!--html_preserve--><div id="htmlwidget-49d18e18987e4d96b900" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-49d18e18987e4d96b900">{"x":{"html":"<ul>\n  <li><span class='match'>07431 1234<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

##Describe the equivalents of `?`, `+`, `*` in `{m,n}` form.

* `{0,1}`
* `{1,}`
* `{0,}`

##Describe in words what these regular expressions match: (read carefully to see if I’m using a regular expression or a string that defines a regular expression.)
    ^.*$
    "\\{.+\\}"
    \d{4}-\d{2}-\d{2}
    "\\\\{4}"
    
* Look for words, that are 0 or more characters long
* ?
* Error
* Look for words, where there are 4 backslashes

##Create regular expressions to find all words that:
    Start with three consonants.
    Have three or more vowels in a row.
    Have two or more vowel-consonant pairs in a row.
    

```r
words[str_detect(words, "^[^aeiou]{3}")]
```

```
##  [1] "Christ"    "Christmas" "dry"       "fly"       "mrs"      
##  [6] "scheme"    "school"    "straight"  "strategy"  "street"   
## [11] "strike"    "strong"    "structure" "system"    "three"    
## [16] "through"   "throw"     "try"       "type"      "why"
```


```r
words[str_detect(words, "[aeiou]{3,}")]
```

```
## [1] "beauty"   "obvious"  "previous" "quiet"    "serious"  "various"
```


```r
words[str_detect(words, "([aeiou][^aeiou]){2,}")]
```

```
##   [1] "absolute"    "agent"       "along"       "america"     "another"    
##   [6] "apart"       "apparent"    "authority"   "available"   "aware"      
##  [11] "away"        "balance"     "basis"       "become"      "before"     
##  [16] "begin"       "behind"      "benefit"     "business"    "character"  
##  [21] "closes"      "community"   "consider"    "cover"       "debate"     
##  [26] "decide"      "decision"    "definite"    "department"  "depend"     
##  [31] "design"      "develop"     "difference"  "difficult"   "direct"     
##  [36] "divide"      "document"    "during"      "economy"     "educate"    
##  [41] "elect"       "electric"    "eleven"      "encourage"   "environment"
##  [46] "europe"      "even"        "evening"     "ever"        "every"      
##  [51] "evidence"    "exact"       "example"     "exercise"    "exist"      
##  [56] "family"      "figure"      "final"       "finance"     "finish"     
##  [61] "friday"      "future"      "general"     "govern"      "holiday"    
##  [66] "honest"      "hospital"    "however"     "identify"    "imagine"    
##  [71] "individual"  "interest"    "introduce"   "item"        "jesus"      
##  [76] "level"       "likely"      "limit"       "local"       "major"      
##  [81] "manage"      "meaning"     "measure"     "minister"    "minus"      
##  [86] "minute"      "moment"      "money"       "music"       "nature"     
##  [91] "necessary"   "never"       "notice"      "okay"        "open"       
##  [96] "operate"     "opportunity" "organize"    "original"    "over"       
## [101] "paper"       "paragraph"   "parent"      "particular"  "photograph" 
## [106] "police"      "policy"      "politic"     "position"    "positive"   
## [111] "power"       "prepare"     "present"     "presume"     "private"    
## [116] "probable"    "process"     "produce"     "product"     "project"    
## [121] "proper"      "propose"     "protect"     "provide"     "quality"    
## [126] "realise"     "reason"      "recent"      "recognize"   "recommend"  
## [131] "record"      "reduce"      "refer"       "regard"      "relation"   
## [136] "remember"    "report"      "represent"   "result"      "return"     
## [141] "saturday"    "second"      "secretary"   "secure"      "separate"   
## [146] "seven"       "similar"     "specific"    "strategy"    "student"    
## [151] "stupid"      "telephone"   "television"  "therefore"   "thousand"   
## [156] "today"       "together"    "tomorrow"    "tonight"     "total"      
## [161] "toward"      "travel"      "unit"        "unite"       "university" 
## [166] "upon"        "visit"       "water"       "woman"
```


## Describe, in words, what these expressions will match:

* `(.)\1\1`

Nothing, because no escape character

* `"(.)(.)\\2\\1"`

Double letter, that is encapsulated with another letter

* `(..)\1`

Nothing, escape character missing

* `(.).\\1.\\1`

Expression with same letter at the start, end and middle and some filling 
characters between them

* `(.)(.)(.).*\\3\\2\\1`

Words, that start and end with the same characters, but in reverse order

## Construct regular expressions to match words that:

* Start and end with the same character.


```r
str_view(c("abbififa", "babbififa"), pattern = "^(.).*\\1$")
```

<!--html_preserve--><div id="htmlwidget-edffa2a5ecbd8f0cdccb" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-edffa2a5ecbd8f0cdccb">{"x":{"html":"<ul>\n  <li><span class='match'>abbififa<\/span><\/li>\n  <li>babbififa<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

* Contain a repeated pair of letters (e.g. “church” contains “ch” repeated twice.)


```r
str_view(c("cucumber", "church"), pattern = ".*(..).*\\1")
```

<!--html_preserve--><div id="htmlwidget-bf4afd4f3d9f9b85a6a3" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-bf4afd4f3d9f9b85a6a3">{"x":{"html":"<ul>\n  <li><span class='match'>cucu<\/span>mber<\/li>\n  <li><span class='match'>church<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

* Contain one letter repeated in at least three places (e.g. “eleven” contains three “e”s.)


```r
str_view(c("eleven", "pineapple", "football"), pattern = ".*(.).*\\1.*\\1.*")
```

<!--html_preserve--><div id="htmlwidget-a5246ab14321c23422aa" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-a5246ab14321c23422aa">{"x":{"html":"<ul>\n  <li><span class='match'>eleven<\/span><\/li>\n  <li><span class='match'>pineapple<\/span><\/li>\n  <li>football<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->




    Contain a repeated pair of letters (e.g. “church” contains “ch” repeated twice.)

    Contain one letter repeated in at least three places (e.g. “eleven” contains three “e”s.)


