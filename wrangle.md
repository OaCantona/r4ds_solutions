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

<!--html_preserve--><div id="htmlwidget-438467f8b72aa79946a4" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-438467f8b72aa79946a4">{"x":{"html":"<ul>\n  <li>aaaa<span class='match'>\"'\\<\/span>bbbb<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

## What patterns will the regular expression `\..\..\..` match? How would you represent it as a string?


```r
str_view(".a.b.c", '\\..\\..\\..')
```

<!--html_preserve--><div id="htmlwidget-a7f1343d5d75db12c290" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-a7f1343d5d75db12c290">{"x":{"html":"<ul>\n  <li><span class='match'>.a.b.c<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

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

<!--html_preserve--><div id="htmlwidget-d670a41a1c8880f5481f" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-d670a41a1c8880f5481f">{"x":{"html":"<ul>\n  <li><span class='match'>$^$<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

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

<!--html_preserve--><div id="htmlwidget-d233e4085216c3a74b5f" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-d233e4085216c3a74b5f">{"x":{"html":"<ul>\n  <li><span class='match'>07431 1234<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

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

<!--html_preserve--><div id="htmlwidget-64c5b9ca4d1d1b762dfe" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-64c5b9ca4d1d1b762dfe">{"x":{"html":"<ul>\n  <li><span class='match'>abbififa<\/span><\/li>\n  <li>babbififa<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

* Contain a repeated pair of letters (e.g. “church” contains “ch” repeated twice.)


```r
str_view(c("cucumber", "church"), pattern = ".*(..).*\\1")
```

<!--html_preserve--><div id="htmlwidget-3ad5a06d49375941fc0c" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-3ad5a06d49375941fc0c">{"x":{"html":"<ul>\n  <li><span class='match'>cucu<\/span>mber<\/li>\n  <li><span class='match'>church<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

* Contain one letter repeated in at least three places (e.g. “eleven” contains three “e”s.)


```r
str_view(c("eleven", "pineapple", "football"), pattern = ".*(.).*\\1.*\\1.*")
```

<!--html_preserve--><div id="htmlwidget-47296a9c9646a6a6cba8" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-47296a9c9646a6a6cba8">{"x":{"html":"<ul>\n  <li><span class='match'>eleven<\/span><\/li>\n  <li><span class='match'>pineapple<\/span><\/li>\n  <li>football<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


## For each of the following challenges, try solving it by using both a single regular expression, and a combination of multiple str_detect() calls.

* Find all words that start or end with x.


```r
words[str_detect(words, "(x$|^x)")]
```

```
## [1] "box" "sex" "six" "tax"
```

```r
words[str_detect(words, "^x") | str_detect(words, "x$")]
```

```
## [1] "box" "sex" "six" "tax"
```

* Find all words that start with a vowel and end with a consonant.


```r
words[str_detect(words, "^[aeiou].{0,}[^aeiou]$")]
```

```
##   [1] "about"       "accept"      "account"     "across"      "act"        
##   [6] "actual"      "add"         "address"     "admit"       "affect"     
##  [11] "afford"      "after"       "afternoon"   "again"       "against"    
##  [16] "agent"       "air"         "all"         "allow"       "almost"     
##  [21] "along"       "already"     "alright"     "although"    "always"     
##  [26] "amount"      "and"         "another"     "answer"      "any"        
##  [31] "apart"       "apparent"    "appear"      "apply"       "appoint"    
##  [36] "approach"    "arm"         "around"      "art"         "as"         
##  [41] "ask"         "at"          "attend"      "authority"   "away"       
##  [46] "awful"       "each"        "early"       "east"        "easy"       
##  [51] "eat"         "economy"     "effect"      "egg"         "eight"      
##  [56] "either"      "elect"       "electric"    "eleven"      "employ"     
##  [61] "end"         "english"     "enjoy"       "enough"      "enter"      
##  [66] "environment" "equal"       "especial"    "even"        "evening"    
##  [71] "ever"        "every"       "exact"       "except"      "exist"      
##  [76] "expect"      "explain"     "express"     "identify"    "if"         
##  [81] "important"   "in"          "indeed"      "individual"  "industry"   
##  [86] "inform"      "instead"     "interest"    "invest"      "it"         
##  [91] "item"        "obvious"     "occasion"    "odd"         "of"         
##  [96] "off"         "offer"       "often"       "okay"        "old"        
## [101] "on"          "only"        "open"        "opportunity" "or"         
## [106] "order"       "original"    "other"       "ought"       "out"        
## [111] "over"        "own"         "under"       "understand"  "union"      
## [116] "unit"        "university"  "unless"      "until"       "up"         
## [121] "upon"        "usual"
```

```r
words[str_detect(words, "^[aeiou]") & !str_detect(words, "[aeiou]$")]
```

```
##   [1] "about"       "accept"      "account"     "across"      "act"        
##   [6] "actual"      "add"         "address"     "admit"       "affect"     
##  [11] "afford"      "after"       "afternoon"   "again"       "against"    
##  [16] "agent"       "air"         "all"         "allow"       "almost"     
##  [21] "along"       "already"     "alright"     "although"    "always"     
##  [26] "amount"      "and"         "another"     "answer"      "any"        
##  [31] "apart"       "apparent"    "appear"      "apply"       "appoint"    
##  [36] "approach"    "arm"         "around"      "art"         "as"         
##  [41] "ask"         "at"          "attend"      "authority"   "away"       
##  [46] "awful"       "each"        "early"       "east"        "easy"       
##  [51] "eat"         "economy"     "effect"      "egg"         "eight"      
##  [56] "either"      "elect"       "electric"    "eleven"      "employ"     
##  [61] "end"         "english"     "enjoy"       "enough"      "enter"      
##  [66] "environment" "equal"       "especial"    "even"        "evening"    
##  [71] "ever"        "every"       "exact"       "except"      "exist"      
##  [76] "expect"      "explain"     "express"     "identify"    "if"         
##  [81] "important"   "in"          "indeed"      "individual"  "industry"   
##  [86] "inform"      "instead"     "interest"    "invest"      "it"         
##  [91] "item"        "obvious"     "occasion"    "odd"         "of"         
##  [96] "off"         "offer"       "often"       "okay"        "old"        
## [101] "on"          "only"        "open"        "opportunity" "or"         
## [106] "order"       "original"    "other"       "ought"       "out"        
## [111] "over"        "own"         "under"       "understand"  "union"      
## [116] "unit"        "university"  "unless"      "until"       "up"         
## [121] "upon"        "usual"
```


*Are there any words that contain at least one of each different vowel?


```r
words[str_detect(words, "a") & str_detect(words, "e") & str_detect(words, "i") & 
  str_detect(words, "o") & str_detect(words, "u")]
```

```
## character(0)
```

## What word has the highest number of vowels? What word has the highest proportion of vowels? (Hint: what is the denominator?)


```r
df_vowel_count <- tibble(
  words = words, 
  vowel_count = str_count(words, "[aeiou]"),
  word_length = str_length(words),
  prop = vowel_count / word_length
)

df_vowel_count %>% filter(vowel_count == max(vowel_count))
```

```
## # A tibble: 8 × 4
##         words vowel_count word_length      prop
##         <chr>       <int>       <int>     <dbl>
## 1 appropriate           5          11 0.4545455
## 2   associate           5           9 0.5555556
## 3   available           5           9 0.5555556
## 4   colleague           5           9 0.5555556
## 5   encourage           5           9 0.5555556
## 6  experience           5          10 0.5000000
## 7  individual           5          10 0.5000000
## 8  television           5          10 0.5000000
```

```r
df_vowel_count %>% arrange(desc(prop))
```

```
## # A tibble: 980 × 4
##     words vowel_count word_length      prop
##     <chr>       <int>       <int>     <dbl>
## 1       a           1           1 1.0000000
## 2    area           3           4 0.7500000
## 3    idea           3           4 0.7500000
## 4     age           2           3 0.6666667
## 5     ago           2           3 0.6666667
## 6     air           2           3 0.6666667
## 7     die           2           3 0.6666667
## 8     due           2           3 0.6666667
## 9     eat           2           3 0.6666667
## 10 europe           4           6 0.6666667
## # ... with 970 more rows
```


## In the previous example, you might have noticed that the regular expression matched “flickered”, which is not a colour. Modify the regex to fix the problem.


```r
sentences
```

```
##   [1] "The birch canoe slid on the smooth planks."               
##   [2] "Glue the sheet to the dark blue background."              
##   [3] "It's easy to tell the depth of a well."                   
##   [4] "These days a chicken leg is a rare dish."                 
##   [5] "Rice is often served in round bowls."                     
##   [6] "The juice of lemons makes fine punch."                    
##   [7] "The box was thrown beside the parked truck."              
##   [8] "The hogs were fed chopped corn and garbage."              
##   [9] "Four hours of steady work faced us."                      
##  [10] "Large size in stockings is hard to sell."                 
##  [11] "The boy was there when the sun rose."                     
##  [12] "A rod is used to catch pink salmon."                      
##  [13] "The source of the huge river is the clear spring."        
##  [14] "Kick the ball straight and follow through."               
##  [15] "Help the woman get back to her feet."                     
##  [16] "A pot of tea helps to pass the evening."                  
##  [17] "Smoky fires lack flame and heat."                         
##  [18] "The soft cushion broke the man's fall."                   
##  [19] "The salt breeze came across from the sea."                
##  [20] "The girl at the booth sold fifty bonds."                  
##  [21] "The small pup gnawed a hole in the sock."                 
##  [22] "The fish twisted and turned on the bent hook."            
##  [23] "Press the pants and sew a button on the vest."            
##  [24] "The swan dive was far short of perfect."                  
##  [25] "The beauty of the view stunned the young boy."            
##  [26] "Two blue fish swam in the tank."                          
##  [27] "Her purse was full of useless trash."                     
##  [28] "The colt reared and threw the tall rider."                
##  [29] "It snowed, rained, and hailed the same morning."          
##  [30] "Read verse out loud for pleasure."                        
##  [31] "Hoist the load to your left shoulder."                    
##  [32] "Take the winding path to reach the lake."                 
##  [33] "Note closely the size of the gas tank."                   
##  [34] "Wipe the grease off his dirty face."                      
##  [35] "Mend the coat before you go out."                         
##  [36] "The wrist was badly strained and hung limp."              
##  [37] "The stray cat gave birth to kittens."                     
##  [38] "The young girl gave no clear response."                   
##  [39] "The meal was cooked before the bell rang."                
##  [40] "What joy there is in living."                             
##  [41] "A king ruled the state in the early days."                
##  [42] "The ship was torn apart on the sharp reef."               
##  [43] "Sickness kept him home the third week."                   
##  [44] "The wide road shimmered in the hot sun."                  
##  [45] "The lazy cow lay in the cool grass."                      
##  [46] "Lift the square stone over the fence."                    
##  [47] "The rope will bind the seven books at once."              
##  [48] "Hop over the fence and plunge in."                        
##  [49] "The friendly gang left the drug store."                   
##  [50] "Mesh mire keeps chicks inside."                           
##  [51] "The frosty air passed through the coat."                  
##  [52] "The crooked maze failed to fool the mouse."               
##  [53] "Adding fast leads to wrong sums."                         
##  [54] "The show was a flop from the very start."                 
##  [55] "A saw is a tool used for making boards."                  
##  [56] "The wagon moved on well oiled wheels."                    
##  [57] "March the soldiers past the next hill."                   
##  [58] "A cup of sugar makes sweet fudge."                        
##  [59] "Place a rosebush near the porch steps."                   
##  [60] "Both lost their lives in the raging storm."               
##  [61] "We talked of the slide show in the circus."               
##  [62] "Use a pencil to write the first draft."                   
##  [63] "He ran half way to the hardware store."                   
##  [64] "The clock struck to mark the third period."               
##  [65] "A small creek cut across the field."                      
##  [66] "Cars and busses stalled in snow drifts."                  
##  [67] "The set of china hit, the floor with a crash."            
##  [68] "This is a grand season for hikes on the road."            
##  [69] "The dune rose from the edge of the water."                
##  [70] "Those words were the cue for the actor to leave."         
##  [71] "A yacht slid around the point into the bay."              
##  [72] "The two met while playing on the sand."                   
##  [73] "The ink stain dried on the finished page."                
##  [74] "The walled town was seized without a fight."              
##  [75] "The lease ran out in sixteen weeks."                      
##  [76] "A tame squirrel makes a nice pet."                        
##  [77] "The horn of the car woke the sleeping cop."               
##  [78] "The heart beat strongly and with firm strokes."           
##  [79] "The pearl was worn in a thin silver ring."                
##  [80] "The fruit peel was cut in thick slices."                  
##  [81] "The Navy attacked the big task force."                    
##  [82] "See the cat glaring at the scared mouse."                 
##  [83] "There are more than two factors here."                    
##  [84] "The hat brim was wide and too droopy."                    
##  [85] "The lawyer tried to lose his case."                       
##  [86] "The grass curled around the fence post."                  
##  [87] "Cut the pie into large parts."                            
##  [88] "Men strive but seldom get rich."                          
##  [89] "Always close the barn door tight."                        
##  [90] "He lay prone and hardly moved a limb."                    
##  [91] "The slush lay deep along the street."                     
##  [92] "A wisp of cloud hung in the blue air."                    
##  [93] "A pound of sugar costs more than eggs."                   
##  [94] "The fin was sharp and cut the clear water."               
##  [95] "The play seems dull and quite stupid."                    
##  [96] "Bail the boat, to stop it from sinking."                  
##  [97] "The term ended in late June that year."                   
##  [98] "A tusk is used to make costly gifts."                     
##  [99] "Ten pins were set in order."                              
## [100] "The bill as paid every third week."                       
## [101] "Oak is strong and also gives shade."                      
## [102] "Cats and dogs each hate the other."                       
## [103] "The pipe began to rust while new."                        
## [104] "Open the crate but don't break the glass."                
## [105] "Add the sum to the product of these three."               
## [106] "Thieves who rob friends deserve jail."                    
## [107] "The ripe taste of cheese improves with age."              
## [108] "Act on these orders with great speed."                    
## [109] "The hog crawled under the high fence."                    
## [110] "Move the vat over the hot fire."                          
## [111] "The bark of the pine tree was shiny and dark."            
## [112] "Leaves turn brown and yellow in the fall."                
## [113] "The pennant waved when the wind blew."                    
## [114] "Split the log with a quick, sharp blow."                  
## [115] "Burn peat after the logs give out."                       
## [116] "He ordered peach pie with ice cream."                     
## [117] "Weave the carpet on the right hand side."                 
## [118] "Hemp is a weed found in parts of the tropics."            
## [119] "A lame back kept his score low."                          
## [120] "We find joy in the simplest things."                      
## [121] "Type out three lists of orders."                          
## [122] "The harder he tried the less he got done."                
## [123] "The boss ran the show with a watchful eye."               
## [124] "The cup cracked and spilled its contents."                
## [125] "Paste can cleanse the most dirty brass."                  
## [126] "The slang word for raw whiskey is booze."                 
## [127] "It caught its hind paw in a rusty trap."                  
## [128] "The wharf could be seen at the farther shore."            
## [129] "Feel the heat of the weak dying flame."                   
## [130] "The tiny girl took off her hat."                          
## [131] "A cramp is no small danger on a swim."                    
## [132] "He said the same phrase thirty times."                    
## [133] "Pluck the bright rose without leaves."                    
## [134] "Two plus seven is less than ten."                         
## [135] "The glow deepened in the eyes of the sweet girl."         
## [136] "Bring your problems to the wise chief."                   
## [137] "Write a fond note to the friend you cherish."             
## [138] "Clothes and lodging are free to new men."                 
## [139] "We frown when events take a bad turn."                    
## [140] "Port is a strong wine with s smoky taste."                
## [141] "The young kid jumped the rusty gate."                     
## [142] "Guess the results from the first scores."                 
## [143] "A salt pickle tastes fine with ham."                      
## [144] "The just claim got the right verdict."                    
## [145] "These thistles bend in a high wind."                      
## [146] "Pure bred poodles have curls."                            
## [147] "The tree top waved in a graceful way."                    
## [148] "The spot on the blotter was made by green ink."           
## [149] "Mud was spattered on the front of his white shirt."       
## [150] "The cigar burned a hole in the desk top."                 
## [151] "The empty flask stood on the tin tray."                   
## [152] "A speedy man can beat this track mark."                   
## [153] "He broke a new shoelace that day."                        
## [154] "The coffee stand is too high for the couch."              
## [155] "The urge to write short stories is rare."                 
## [156] "The pencils have all been used."                          
## [157] "The pirates seized the crew of the lost ship."            
## [158] "We tried to replace the coin but failed."                 
## [159] "She sewed the torn coat quite neatly."                    
## [160] "The sofa cushion is red and of light weight."             
## [161] "The jacket hung on the back of the wide chair."           
## [162] "At that high level the air is pure."                      
## [163] "Drop the two when you add the figures."                   
## [164] "A filing case is now hard to buy."                        
## [165] "An abrupt start does not win the prize."                  
## [166] "Wood is best for making toys and blocks."                 
## [167] "The office paint was a dull sad tan."                     
## [168] "He knew the skill of the great young actress."            
## [169] "A rag will soak up spilled water."                        
## [170] "A shower of dirt fell from the hot pipes."                
## [171] "Steam hissed from the broken valve."                      
## [172] "The child almost hurt the small dog."                     
## [173] "There was a sound of dry leaves outside."                 
## [174] "The sky that morning was clear and bright blue."          
## [175] "Torn scraps littered the stone floor."                    
## [176] "Sunday is the best part of the week."                     
## [177] "The doctor cured him with these pills."                   
## [178] "The new girl was fired today at noon."                    
## [179] "They felt gay when the ship arrived in port."             
## [180] "Add the store's account to the last cent."                
## [181] "Acid burns holes in wool cloth."                          
## [182] "Fairy tales should be fun to write."                      
## [183] "Eight miles of woodland burned to waste."                 
## [184] "The third act was dull and tired the players."            
## [185] "A young child should not suffer fright."                  
## [186] "Add the column and put the sum here."                     
## [187] "We admire and love a good cook."                          
## [188] "There the flood mark is ten inches."                      
## [189] "He carved a head from the round block of marble."         
## [190] "She has st smart way of wearing clothes."                 
## [191] "The fruit of a fig tree is apple-shaped."                 
## [192] "Corn cobs can be used to kindle a fire."                  
## [193] "Where were they when the noise started."                  
## [194] "The paper box is full of thumb tacks."                    
## [195] "Sell your gift to a buyer at a good gain."                
## [196] "The tongs lay beside the ice pail."                       
## [197] "The petals fall with the next puff of wind."              
## [198] "Bring your best compass to the third class."              
## [199] "They could laugh although they were sad."                 
## [200] "Farmers came in to thresh the oat crop."                  
## [201] "The brown house was on fire to the attic."                
## [202] "The lure is used to catch trout and flounder."            
## [203] "Float the soap on top of the bath water."                 
## [204] "A blue crane is a tall wading bird."                      
## [205] "A fresh start will work such wonders."                    
## [206] "The club rented the rink for the fifth night."            
## [207] "After the dance they went straight home."                 
## [208] "The hostess taught the new maid to serve."                
## [209] "He wrote his last novel there at the inn."                
## [210] "Even the worst will beat his low score."                  
## [211] "The cement had dried when he moved it."                   
## [212] "The loss of the second ship was hard to take."            
## [213] "The fly made its way along the wall."                     
## [214] "Do that with a wooden stick."                             
## [215] "Lire wires should be kept covered."                       
## [216] "The large house had hot water taps."                      
## [217] "It is hard to erase blue or red ink."                     
## [218] "Write at once or you may forget it."                      
## [219] "The doorknob was made of bright clean brass."             
## [220] "The wreck occurred by the bank on Main Street."           
## [221] "A pencil with black lead writes best."                    
## [222] "Coax a young calf to drink from a bucket."                
## [223] "Schools for ladies teach charm and grace."                
## [224] "The lamp shone with a steady green flame."                
## [225] "They took the axe and the saw to the forest."             
## [226] "The ancient coin was quite dull and worn."                
## [227] "The shaky barn fell with a loud crash."                   
## [228] "Jazz and swing fans like fast music."                     
## [229] "Rake the rubbish up and then burn it."                    
## [230] "Slash the gold cloth into fine ribbons."                  
## [231] "Try to have the court decide the case."                   
## [232] "They are pushed back each time they attack."              
## [233] "He broke his ties with groups of former friends."         
## [234] "They floated on the raft to sun their white backs."       
## [235] "The map had an X that meant nothing."                     
## [236] "Whitings are small fish caught in nets."                  
## [237] "Some ads serve to cheat buyers."                          
## [238] "Jerk the rope and the bell rings weakly."                 
## [239] "A waxed floor makes us lose balance."                     
## [240] "Madam, this is the best brand of corn."                   
## [241] "On the islands the sea breeze is soft and mild."          
## [242] "The play began as soon as we sat down."                   
## [243] "This will lead the world to more sound and fury"          
## [244] "Add salt before you fry the egg."                         
## [245] "The rush for funds reached its peak Tuesday."             
## [246] "The birch looked stark white and lonesome."               
## [247] "The box is held by a bright red snapper."                 
## [248] "To make pure ice, you freeze water."                      
## [249] "The first worm gets snapped early."                       
## [250] "Jump the fence and hurry up the bank."                    
## [251] "Yell and clap as the curtain slides back."                
## [252] "They are men nho walk the middle of the road."            
## [253] "Both brothers wear the same size."                        
## [254] "In some forin or other we need fun."                      
## [255] "The prince ordered his head chopped off."                 
## [256] "The houses are built of red clay bricks."                 
## [257] "Ducks fly north but lack a compass."                      
## [258] "Fruit flavors are used in fizz drinks."                   
## [259] "These pills do less good than others."                    
## [260] "Canned pears lack full flavor."                           
## [261] "The dark pot hung in the front closet."                   
## [262] "Carry the pail to the wall and spill it there."           
## [263] "The train brought our hero to the big town."              
## [264] "We are sure that one war is enough."                      
## [265] "Gray paint stretched for miles around."                   
## [266] "The rude laugh filled the empty room."                    
## [267] "High seats are best for football fans."                   
## [268] "Tea served from the brown jug is tasty."                  
## [269] "A dash of pepper spoils beef stew."                       
## [270] "A zestful food is the hot-cross bun."                     
## [271] "The horse trotted around the field at a brisk pace."      
## [272] "Find the twin who stole the pearl necklace."              
## [273] "Cut the cord that binds the box tightly."                 
## [274] "The red tape bound the smuggled food."                    
## [275] "Look in the corner to find the tan shirt."                
## [276] "The cold drizzle will halt the bond drive."               
## [277] "Nine men were hired to dig the ruins."                    
## [278] "The junk yard had a mouldy smell."                        
## [279] "The flint sputtered and lit a pine torch."                
## [280] "Soak the cloth and drown the sharp odor."                 
## [281] "The shelves were bare of both jam or crackers."           
## [282] "A joy to every child is the swan boat."                   
## [283] "All sat frozen and watched the screen."                   
## [284] "ii cloud of dust stung his tender eyes."                  
## [285] "To reach the end he needs much courage."                  
## [286] "Shape the clay gently into block form."                   
## [287] "The ridge on a smooth surface is a bump or flaw."         
## [288] "Hedge apples may stain your hands green."                 
## [289] "Quench your thirst, then eat the crackers."               
## [290] "Tight curls get limp on rainy days."                      
## [291] "The mute muffled the high tones of the horn."             
## [292] "The gold ring fits only a pierced ear."                   
## [293] "The old pan was covered with hard fudge."                 
## [294] "Watch the log float in the wide river."                   
## [295] "The node on the stalk of wheat grew daily."               
## [296] "The heap of fallen leaves was set on fire."               
## [297] "Write fast, if you want to finish early."                 
## [298] "His shirt was clean but one button was gone."             
## [299] "The barrel of beer was a brew of malt and hops."          
## [300] "Tin cans are absent from store shelves."                  
## [301] "Slide the box into that empty space."                     
## [302] "The plant grew large and green in the window."            
## [303] "The beam dropped down on the workmen's head."             
## [304] "Pink clouds floated JTith the breeze."                    
## [305] "She danced like a swan, tall and graceful."               
## [306] "The tube was blown and the tire flat and useless."        
## [307] "It is late morning on the old wall clock."                
## [308] "Let's all join as we sing the last chorus."               
## [309] "The last switch cannot be turned off."                    
## [310] "The fight will end in just six minutes."                  
## [311] "The store walls were lined with colored frocks."          
## [312] "The peace league met to discuss their plans."             
## [313] "The rise to fame of a person takes luck."                 
## [314] "Paper is scarce, so write with much care."                
## [315] "The quick fox jumped on the sleeping cat."                
## [316] "The nozzle of the fire hose was bright brass."            
## [317] "Screw the round cap on as tight as needed."               
## [318] "Time brings us many changes."                             
## [319] "The purple tie was ten years old."                        
## [320] "Men think and plan and sometimes act."                    
## [321] "Fill the ink jar with sticky glue."                       
## [322] "He smoke a big pipe with strong contents."                
## [323] "We need grain to keep our mules healthy."                 
## [324] "Pack the records in a neat thin case."                    
## [325] "The crunch of feet in the snow was the only sound."       
## [326] "The copper bowl shone in the sun's rays."                 
## [327] "Boards will warp unless kept dry."                        
## [328] "The plush chair leaned against the wall."                 
## [329] "Glass will clink when struck by metal."                   
## [330] "Bathe and relax in the cool green grass."                 
## [331] "Nine rows of soldiers stood in line."                     
## [332] "The beach is dry and shallow at low tide."                
## [333] "The idea is to sew both edges straight."                  
## [334] "The kitten chased the dog down the street."               
## [335] "Pages bound in cloth make a book."                        
## [336] "Try to trace the fine lines of the painting."             
## [337] "Women form less than half of the group."                  
## [338] "The zones merge in the central part of town."             
## [339] "A gem in the rough needs work to polish."                 
## [340] "Code is used when secrets are sent."                      
## [341] "Most of the new is easy for us to hear."                  
## [342] "He used the lathe to make brass objects."                 
## [343] "The vane on top of the pole revolved in the wind."        
## [344] "Mince pie is a dish served to children."                  
## [345] "The clan gathered on each dull night."                    
## [346] "Let it burn, it gives us warmth and comfort."             
## [347] "A castle built from sand fails to endure."                
## [348] "A child's wit saved the day for us."                      
## [349] "Tack the strip of carpet to the worn floor."              
## [350] "Next Tuesday we must vote."                               
## [351] "Pour the stew from the pot into the plate."               
## [352] "Each penny shone like new."                               
## [353] "The man went to the woods to gather sticks."              
## [354] "The dirt piles were lines along the road."                
## [355] "The logs fell and tumbled into the clear stream."         
## [356] "Just hoist it up and take it away,"                       
## [357] "A ripe plum is fit for a king's palate."                  
## [358] "Our plans right now are hazy."                            
## [359] "Brass rings are sold by these natives."                   
## [360] "It takes a good trap to capture a bear."                  
## [361] "Feed the white mouse some flower seeds."                  
## [362] "The thaw came early and freed the stream."                
## [363] "He took the lead and kept it the whole distance."         
## [364] "The key you designed will fit the lock."                  
## [365] "Plead to the council to free the poor thief."             
## [366] "Better hash is made of rare beef."                        
## [367] "This plank was made for walking on."                      
## [368] "The lake sparkled in the red hot sun."                    
## [369] "He crawled with care along the ledge."                    
## [370] "Tend the sheep while the dog wanders."                    
## [371] "It takes a lot of help to finish these."                  
## [372] "Mark the spot with a sign painted red."                   
## [373] "Take two shares as a fair profit."                        
## [374] "The fur of cats goes by many names."                      
## [375] "North winds bring colds and fevers."                      
## [376] "He asks no person to vouch for him."                      
## [377] "Go now and come here later."                              
## [378] "A sash of gold silk will trim her dress."                 
## [379] "Soap can wash most dirt away."                            
## [380] "That move means the game is over."                        
## [381] "He wrote down a long list of items."                      
## [382] "A siege will crack the strong defense."                   
## [383] "Grape juice and water mix well."                          
## [384] "Roads are paved with sticky tar."                         
## [385] "Fake &ones shine but cost little."                        
## [386] "The drip of the rain made a pleasant sound."              
## [387] "Smoke poured out of every crack."                         
## [388] "Serve the hot rum to the tired heroes."                   
## [389] "Much of the story makes good sense."                      
## [390] "The sun came up to light the eastern sky."                
## [391] "Heave the line over the port side."                       
## [392] "A lathe cuts and trims any wood."                         
## [393] "It's a dense crowd in two distinct ways."                 
## [394] "His hip struck the knee of the next player."              
## [395] "The stale smell of old beer lingers."                     
## [396] "The desk was firm on the shaky floor."                    
## [397] "It takes heat to bring out the odor."                     
## [398] "Beef is scarcer than some lamb."                          
## [399] "Raise the sail and steer the ship northward."             
## [400] "The cone costs five cents on Mondays."                    
## [401] "A pod is what peas always grow in."                       
## [402] "Jerk the dart from the cork target."                      
## [403] "No cement will hold hard wood."                           
## [404] "We now have a new base for shipping."                     
## [405] "The list of names is carved around the base."             
## [406] "The sheep were led home by a dog."                        
## [407] "Three for a dime, the young peddler cried."               
## [408] "The sense of smell is better than that of touch."         
## [409] "No hardship seemed to keep him sad."                      
## [410] "Grace makes up for lack of beauty."                       
## [411] "Nudge gently but wake her now."                           
## [412] "The news struck doubt into restless minds."               
## [413] "Once we stood beside the shore."                          
## [414] "A chink in the wall allowed a draft to blow."             
## [415] "Fasten two pins on each side."                            
## [416] "A cold dip restores health and zest."                     
## [417] "He takes the oath of office each March."                  
## [418] "The sand drifts over the sill of the old house."          
## [419] "The point of the steel pen was bent and twisted."         
## [420] "There is a lag between thought and act."                  
## [421] "Seed is needed to plant the spring corn."                 
## [422] "Draw the chart with heavy black lines."                   
## [423] "The boy owed his pal thirty cents."                       
## [424] "The chap slipped into the crowd and was lost."            
## [425] "Hats are worn to tea and not to dinner."                  
## [426] "The ramp led up to the wide highway."                     
## [427] "Beat the dust from the rug onto the lawn."                
## [428] "Say it slow!y but make it ring clear."                    
## [429] "The straw nest housed five robins."                       
## [430] "Screen the porch with woven straw mats."                  
## [431] "This horse will nose his way to the finish."              
## [432] "The dry wax protects the deep scratch."                   
## [433] "He picked up the dice for a second roll."                 
## [434] "These coins will be needed to pay his debt."              
## [435] "The nag pulled the frail cart along."                     
## [436] "Twist the valve and release hot steam."                   
## [437] "The vamp of the shoe had a gold buckle."                  
## [438] "The smell of burned rags itches my nose."                 
## [439] "Xew pants lack cuffs and pockets."                        
## [440] "The marsh will freeze when cold enough."                  
## [441] "They slice the sausage thin with a knife."                
## [442] "The bloom of the rose lasts a few days."                  
## [443] "A gray mare walked before the colt."                      
## [444] "Breakfast buns are fine with a hot drink."                
## [445] "Bottles hold four kinds of rum."                          
## [446] "The man wore a feather in his felt hat."                  
## [447] "He wheeled the bike past. the winding road."              
## [448] "Drop the ashes on the worn old rug."                      
## [449] "The desk and both chairs were painted tan."               
## [450] "Throw out the used paper cup and plate."                  
## [451] "A clean neck means a neat collar."                        
## [452] "The couch cover and hall drapes were blue."               
## [453] "The stems of the tall glasses cracked and broke."         
## [454] "The wall phone rang loud and often."                      
## [455] "The clothes dried on a thin wooden rack."                 
## [456] "Turn on the lantern which gives us light."                
## [457] "The cleat sank deeply into the soft turf."                
## [458] "The bills were mailed promptly on the tenth of the month."
## [459] "To have is better than to wait and hope."                 
## [460] "The price is fair for a good antique clock."              
## [461] "The music played on while they talked."                   
## [462] "Dispense with a vest on a day like this."                 
## [463] "The bunch of grapes was pressed into wine."               
## [464] "He sent the figs, but kept the ripe cherries."            
## [465] "The hinge on the door creaked with old age."              
## [466] "The screen before the fire kept in the sparks."           
## [467] "Fly by night, and you waste little time."                 
## [468] "Thick glasses helped him read the print."                 
## [469] "Birth and death mark the limits of life."                 
## [470] "The chair looked strong but had no bottom."               
## [471] "The kite flew wildly in the high wind."                   
## [472] "A fur muff is stylish once more."                         
## [473] "The tin box held priceless stones."                       
## [474] "We need an end of all such matter."                       
## [475] "The case was puzzling to the old and wise."               
## [476] "The bright lanterns were gay on the dark lawn."           
## [477] "We don't get much money but we have fun."                 
## [478] "The youth drove with zest, but little skill."             
## [479] "Five years he lived with a shaggy dog."                   
## [480] "A fence cuts through the corner lot."                     
## [481] "The way to save money is not to spend much."              
## [482] "Shut the hatch before the waves push it in."              
## [483] "The odor of spring makes young hearts jump."              
## [484] "Crack the walnut with your sharp side teeth."             
## [485] "He offered proof in the form of a lsrge chart."           
## [486] "Send the stuff in a thick paper bag."                     
## [487] "A quart of milk is water for the most part."              
## [488] "They told wild tales to frighten him."                    
## [489] "The three story house was built of stone."                
## [490] "In the rear of the ground floor was a large passage."     
## [491] "A man in a blue sweater sat at the desk."                 
## [492] "Oats are a food eaten by horse and man."                  
## [493] "Their eyelids droop for want. of sleep."                  
## [494] "The sip of tea revives his tired friend."                 
## [495] "There are many ways to do these things."                  
## [496] "Tuck the sheet under the edge of the mat."                
## [497] "A force equal to that would move the earth."              
## [498] "We like to see clear weather."                            
## [499] "The work of the tailor is seen on each side."             
## [500] "Take a chance and win a china doll."                      
## [501] "Shake the dust from your shoes, stranger."                
## [502] "She was kind to sick old people."                         
## [503] "The dusty bench stood by the stone wall."                 
## [504] "The square wooden crate was packed to be shipped."        
## [505] "We dress to suit the weather of most days."               
## [506] "Smile when you say nasty words."                          
## [507] "A bowl of rice is free with chicken stew."                
## [508] "The water in this well is a source of good health."       
## [509] "Take shelter in this tent, but keep still."               
## [510] "That guy is the writer of a few banned books."            
## [511] "The little tales they tell are false."                    
## [512] "The door was barred, locked, and bolted as well."         
## [513] "Ripe pears are fit for a queen's table."                  
## [514] "A big wet stain was on the round carpet."                 
## [515] "The kite dipped and swayed, but stayed aloft."            
## [516] "The pleasant hours fly by much too soon."                 
## [517] "The room was crowded with a wild mob."                    
## [518] "This strong arm shall shield your honor."                 
## [519] "She blushed when he gave her a white orchid."             
## [520] "The beetle droned in the hot June sun."                   
## [521] "Press the pedal with your left foot."                     
## [522] "Neat plans fail without luck."                            
## [523] "The black trunk fell from the landing."                   
## [524] "The bank pressed for payment of the debt."                
## [525] "The theft of the pearl pin was kept secret."              
## [526] "Shake hands with this friendly child."                    
## [527] "The vast space stretched into the far distance."          
## [528] "A rich farm is rare in this sandy waste."                 
## [529] "His wide grin earned many friends."                       
## [530] "Flax makes a fine brand of paper."                        
## [531] "Hurdle the pit with the aid of a long pole."              
## [532] "A strong bid may scare your partner stiff."               
## [533] "Even a just cause needs power to win."                    
## [534] "Peep under the tent and see the clowns."                  
## [535] "The leaf drifts along with a slow spin."                  
## [536] "Cheap clothes are flashy but don??????t last."            
## [537] "A thing of small note can cause despair."                 
## [538] "Flood the mails with requests for this book."             
## [539] "A thick coat of black paint covered all."                 
## [540] "The pencil was cut to be sharp at both ends."             
## [541] "Those last words were a strong statement."                
## [542] "He wrote his name boldly at the top of tile sheet."       
## [543] "Dill pickles are sour but taste fine."                    
## [544] "Down that road is the way to the grain farmer."           
## [545] "Either mud or dust are found at all times."               
## [546] "The best method is to fix it in place with clips."        
## [547] "If you mumble your speech will be lost."                  
## [548] "At night the alarm roused him from a deep sleep."         
## [549] "Read just what the meter says."                           
## [550] "Fill your pack with bright trinkets for the poor."        
## [551] "The small red neon lamp went out."                        
## [552] "Clams are small, round, soft, and tasty."                 
## [553] "The fan whirled its round blades softly."                 
## [554] "The line where the edges join was clean."                 
## [555] "Breathe deep and smell the piny air."                     
## [556] "It matters not if he reads these words or those."         
## [557] "A brown leather bag hung from its strap."                 
## [558] "A toad and a frog are hard to tell apart."                
## [559] "A white silk jacket goes with any shoes."                 
## [560] "A break in the dam almost caused a flood."                
## [561] "Paint the sockets in the wall dull green."                
## [562] "The child crawled into the dense grass."                  
## [563] "Bribes fail where honest men work."                       
## [564] "Trample the spark, else the flames will spread."          
## [565] "The hilt. of the sword was carved with fine designs."     
## [566] "A round hole was drilled through the thin board."         
## [567] "Footprints showed the path he took up the beach."         
## [568] "She was waiting at my front lawn."                        
## [569] "A vent near the edge brought in fresh air."               
## [570] "Prod the old mule with a crooked stick."                  
## [571] "It is a band of steel three inches wide."                 
## [572] "The pipe ran almost the length of the ditch."             
## [573] "It was hidden from sight by a mass of leaves and shrubs." 
## [574] "The weight. of the package was seen on the high scale."   
## [575] "Wake and rise, and step into the green outdoors."         
## [576] "The green light in the brown box flickered."              
## [577] "The brass tube circled the high wall."                    
## [578] "The lobes of her ears were pierced to hold rings."        
## [579] "Hold the hammer near the end to drive the nail."          
## [580] "Next Sunday is the twelfth of the month."                 
## [581] "Every word and phrase he speaks is true."                 
## [582] "He put his last cartridge into the gun and fired."        
## [583] "They took their kids from the public school."             
## [584] "Drive the screw straight into the wood."                  
## [585] "Keep the hatch tight and the watch constant."             
## [586] "Sever the twine with a quick snip of the knife."          
## [587] "Paper will dry out when wet."                             
## [588] "Slide the catch back and open the desk."                  
## [589] "Help the weak to preserve their strength."                
## [590] "A sullen smile gets few friends."                         
## [591] "Stop whistling and watch the boys march."                 
## [592] "Jerk the cord, and out tumbles the gold."                 
## [593] "Slidc the tray across the glass top."                     
## [594] "The cloud moved in a stately way and was gone."           
## [595] "Light maple makes for a swell room."                      
## [596] "Set the piece here and say nothing."                      
## [597] "Dull stories make her laugh."                             
## [598] "A stiff cord will do to fasten your shoe."                
## [599] "Get the trust fund to the bank early."                    
## [600] "Choose between the high road and the low."                
## [601] "A plea for funds seems to come again."                    
## [602] "He lent his coat to the tall gaunt stranger."             
## [603] "There is a strong chance it will happen once more."       
## [604] "The duke left the park in a silver coach."                
## [605] "Greet the new guests and leave quickly."                  
## [606] "When the frost has come it is time for turkey."           
## [607] "Sweet words work better than fierce."                     
## [608] "A thin stripe runs down the middle."                      
## [609] "A six comes up more often than a ten."                    
## [610] "Lush fern grow on the lofty rocks."                       
## [611] "The ram scared the school children off."                  
## [612] "The team with the best timing looks good."                
## [613] "The farmer swapped his horse for a brown ox."             
## [614] "Sit on the perch and tell the others what to do."         
## [615] "A steep trail is painful for our feet."                   
## [616] "The early phase of life moves fast."                      
## [617] "Green moss grows on the northern side."                   
## [618] "Tea in thin china has a sweet taste."                     
## [619] "Pitch the straw through the door of the stable."          
## [620] "The latch on the beck gate needed a nail."                
## [621] "The goose was brought straight from the old market."      
## [622] "The sink is the thing in which we pile dishes."           
## [623] "A whiff of it will cure the most stubborn cold."          
## [624] "The facts don<U+0092>t always show who is right."         
## [625] "She flaps her cape as she parades the street."            
## [626] "The loss of the cruiser was a blow to the fleet."         
## [627] "Loop the braid to the left and then over."                
## [628] "Plead with the lawyer to drop the lost cause."            
## [629] "Calves thrive on tender spring grass."                    
## [630] "Post no bills on this office wall."                       
## [631] "Tear a thin sheet from the yellow pad."                   
## [632] "A cruise in warm waters in a sleek yacht is fun."         
## [633] "A streak of color ran down the left edge."                
## [634] "It was done before the boy could see it."                 
## [635] "Crouch before you jump or miss the mark."                 
## [636] "Pack the kits and don<U+0092>t forget the salt."          
## [637] "The square peg will settle in the round hole."            
## [638] "Fine soap saves tender skin."                             
## [639] "Poached eggs and tea must suffice."                       
## [640] "Bad nerves are jangled by a door slam."                   
## [641] "Ship maps are different from those for planes."           
## [642] "Dimes showered down from all sides."                      
## [643] "They sang the same tunes at each party."                  
## [644] "The sky in the west is tinged with orange red."           
## [645] "The pods of peas ferment in bare fields."                 
## [646] "The horse balked and threw the tall rider."               
## [647] "The hitch between the horse and cart broke."              
## [648] "Pile the coal high in the shed corner."                   
## [649] "The gold vase is both rare and costly."                   
## [650] "The knife was hung inside its bright sheath."             
## [651] "The rarest spice comes from the far East."                
## [652] "The roof should be tilted at a sharp slant."              
## [653] "A smatter of French is worse than none."                  
## [654] "The mule trod the treadmill day and night."               
## [655] "The aim of the contest is to raise a great fund."         
## [656] "To send it. now in large amounts is bad."                 
## [657] "There is a fine hard tang in salty air."                  
## [658] "Cod is the main business of the north shore."             
## [659] "The slab was hewn from heavy blocks of slat<U+0092>e."    
## [660] "Dunk the stale biscuits into strong drink."               
## [661] "Hang tinsel from both branches."                          
## [662] "Cap the jar with a tight brass cover."                    
## [663] "The poor boy missed the boat again."                      
## [664] "Be sure to set the lamp firmly in the hole."              
## [665] "Pick a card and slip it. under the pack."                 
## [666] "A round mat will cover the dull spot."                    
## [667] "The first part of the plan needs changing."               
## [668] "The good book informs of what we ought to know."          
## [669] "The mail comes in three batches per day."                 
## [670] "You cannot brew tea in a cold pot."                       
## [671] "Dots of light betrayed the black cat."                    
## [672] "Put the chart on the mantel and tack it down."            
## [673] "The night shift men rate extra pay."                      
## [674] "The red paper brightened the dim stage."                  
## [675] "See the player scoot to third base."                      
## [676] "Slide the bill between the two leaves."                   
## [677] "Many hands help get the job done."                        
## [678] "We don't like to admit our small faults."                 
## [679] "No doubt about the way the wind blows."                   
## [680] "Dig deep in the earth for pirate's gold."                 
## [681] "The steady drip is worse than a drenching rain."          
## [682] "A flat pack takes less luggage space."                    
## [683] "Green ice frosted the punch bowl."                        
## [684] "A stuffed chair slipped from the moving van."             
## [685] "The stitch will serve but needs to be shortened."         
## [686] "A thin book fits in the side pocket."                     
## [687] "The gloss on top made it unfit to read."                  
## [688] "The hail pattered on the burnt brown grass."              
## [689] "Seven seals were stamped on great sheets."                
## [690] "Our troops are set to strike heavy blows."                
## [691] "The store was jammed before the sale could start."        
## [692] "It was a bad error on the part of the new judge."         
## [693] "One step more and the board will collapse."               
## [694] "Take the match and strike it against your shoe."          
## [695] "The pot boiled, but the contents failed to jell."         
## [696] "The baby puts his right foot in his mouth."               
## [697] "The bombs left most of the town in ruins."                
## [698] "Stop and stare at the hard working man."                  
## [699] "The streets are narrow and full of sharp turns."          
## [700] "The pup jerked the leash as he saw a feline shape."       
## [701] "Open your book to the first page."                        
## [702] "Fish evade the net, and swim off."                        
## [703] "Dip the pail once and let it settle."                     
## [704] "Will you please answer that phone."                       
## [705] "The big red apple fell to the ground."                    
## [706] "The curtain rose and the show was on."                    
## [707] "The young prince became heir to the throne."              
## [708] "He sent the boy on a short errand."                       
## [709] "Leave now and you will arrive on time."                   
## [710] "The corner store was robbed last night."                  
## [711] "A gold ring will please most any girl."                   
## [712] "The long journey home took a year."                       
## [713] "She saw a cat in the neighbor's house."                   
## [714] "A pink shell was found on the sandy beach."               
## [715] "Small children came to see him."                          
## [716] "The grass and bushes were wet with dew."                  
## [717] "The blind man counted his old coins."                     
## [718] "A severe storm tore down the barn."                       
## [719] "She called his name many times."                          
## [720] "When you hear the bell, come quickly."
```

```r
colors <- c("red", "blue", "green", "orange", "yellow", "purple") 
color_match <- str_c("\\b(", str_c(colors, collapse = "|"), ")\\b")

sentences[str_count(sentences, color_match) > 1]
```

```
## [1] "It is hard to erase blue or red ink."          
## [2] "The sky in the west is tinged with orange red."
```

## From the Harvard sentences data, extract:

* The first word from each sentence.


```r
str_extract(sentences, "[(a-z)|(A-Z)]+")
```

```
##   [1] "The"        "Glue"       "It"         "These"      "Rice"      
##   [6] "The"        "The"        "The"        "Four"       "Large"     
##  [11] "The"        "A"          "The"        "Kick"       "Help"      
##  [16] "A"          "Smoky"      "The"        "The"        "The"       
##  [21] "The"        "The"        "Press"      "The"        "The"       
##  [26] "Two"        "Her"        "The"        "It"         "Read"      
##  [31] "Hoist"      "Take"       "Note"       "Wipe"       "Mend"      
##  [36] "The"        "The"        "The"        "The"        "What"      
##  [41] "A"          "The"        "Sickness"   "The"        "The"       
##  [46] "Lift"       "The"        "Hop"        "The"        "Mesh"      
##  [51] "The"        "The"        "Adding"     "The"        "A"         
##  [56] "The"        "March"      "A"          "Place"      "Both"      
##  [61] "We"         "Use"        "He"         "The"        "A"         
##  [66] "Cars"       "The"        "This"       "The"        "Those"     
##  [71] "A"          "The"        "The"        "The"        "The"       
##  [76] "A"          "The"        "The"        "The"        "The"       
##  [81] "The"        "See"        "There"      "The"        "The"       
##  [86] "The"        "Cut"        "Men"        "Always"     "He"        
##  [91] "The"        "A"          "A"          "The"        "The"       
##  [96] "Bail"       "The"        "A"          "Ten"        "The"       
## [101] "Oak"        "Cats"       "The"        "Open"       "Add"       
## [106] "Thieves"    "The"        "Act"        "The"        "Move"      
## [111] "The"        "Leaves"     "The"        "Split"      "Burn"      
## [116] "He"         "Weave"      "Hemp"       "A"          "We"        
## [121] "Type"       "The"        "The"        "The"        "Paste"     
## [126] "The"        "It"         "The"        "Feel"       "The"       
## [131] "A"          "He"         "Pluck"      "Two"        "The"       
## [136] "Bring"      "Write"      "Clothes"    "We"         "Port"      
## [141] "The"        "Guess"      "A"          "The"        "These"     
## [146] "Pure"       "The"        "The"        "Mud"        "The"       
## [151] "The"        "A"          "He"         "The"        "The"       
## [156] "The"        "The"        "We"         "She"        "The"       
## [161] "The"        "At"         "Drop"       "A"          "An"        
## [166] "Wood"       "The"        "He"         "A"          "A"         
## [171] "Steam"      "The"        "There"      "The"        "Torn"      
## [176] "Sunday"     "The"        "The"        "They"       "Add"       
## [181] "Acid"       "Fairy"      "Eight"      "The"        "A"         
## [186] "Add"        "We"         "There"      "He"         "She"       
## [191] "The"        "Corn"       "Where"      "The"        "Sell"      
## [196] "The"        "The"        "Bring"      "They"       "Farmers"   
## [201] "The"        "The"        "Float"      "A"          "A"         
## [206] "The"        "After"      "The"        "He"         "Even"      
## [211] "The"        "The"        "The"        "Do"         "Lire"      
## [216] "The"        "It"         "Write"      "The"        "The"       
## [221] "A"          "Coax"       "Schools"    "The"        "They"      
## [226] "The"        "The"        "Jazz"       "Rake"       "Slash"     
## [231] "Try"        "They"       "He"         "They"       "The"       
## [236] "Whitings"   "Some"       "Jerk"       "A"          "Madam"     
## [241] "On"         "The"        "This"       "Add"        "The"       
## [246] "The"        "The"        "To"         "The"        "Jump"      
## [251] "Yell"       "They"       "Both"       "In"         "The"       
## [256] "The"        "Ducks"      "Fruit"      "These"      "Canned"    
## [261] "The"        "Carry"      "The"        "We"         "Gray"      
## [266] "The"        "High"       "Tea"        "A"          "A"         
## [271] "The"        "Find"       "Cut"        "The"        "Look"      
## [276] "The"        "Nine"       "The"        "The"        "Soak"      
## [281] "The"        "A"          "All"        "ii"         "To"        
## [286] "Shape"      "The"        "Hedge"      "Quench"     "Tight"     
## [291] "The"        "The"        "The"        "Watch"      "The"       
## [296] "The"        "Write"      "His"        "The"        "Tin"       
## [301] "Slide"      "The"        "The"        "Pink"       "She"       
## [306] "The"        "It"         "Let"        "The"        "The"       
## [311] "The"        "The"        "The"        "Paper"      "The"       
## [316] "The"        "Screw"      "Time"       "The"        "Men"       
## [321] "Fill"       "He"         "We"         "Pack"       "The"       
## [326] "The"        "Boards"     "The"        "Glass"      "Bathe"     
## [331] "Nine"       "The"        "The"        "The"        "Pages"     
## [336] "Try"        "Women"      "The"        "A"          "Code"      
## [341] "Most"       "He"         "The"        "Mince"      "The"       
## [346] "Let"        "A"          "A"          "Tack"       "Next"      
## [351] "Pour"       "Each"       "The"        "The"        "The"       
## [356] "Just"       "A"          "Our"        "Brass"      "It"        
## [361] "Feed"       "The"        "He"         "The"        "Plead"     
## [366] "Better"     "This"       "The"        "He"         "Tend"      
## [371] "It"         "Mark"       "Take"       "The"        "North"     
## [376] "He"         "Go"         "A"          "Soap"       "That"      
## [381] "He"         "A"          "Grape"      "Roads"      "Fake"      
## [386] "The"        "Smoke"      "Serve"      "Much"       "The"       
## [391] "Heave"      "A"          "It"         "His"        "The"       
## [396] "The"        "It"         "Beef"       "Raise"      "The"       
## [401] "A"          "Jerk"       "No"         "We"         "The"       
## [406] "The"        "Three"      "The"        "No"         "Grace"     
## [411] "Nudge"      "The"        "Once"       "A"          "Fasten"    
## [416] "A"          "He"         "The"        "The"        "There"     
## [421] "Seed"       "Draw"       "The"        "The"        "Hats"      
## [426] "The"        "Beat"       "Say"        "The"        "Screen"    
## [431] "This"       "The"        "He"         "These"      "The"       
## [436] "Twist"      "The"        "The"        "Xew"        "The"       
## [441] "They"       "The"        "A"          "Breakfast"  "Bottles"   
## [446] "The"        "He"         "Drop"       "The"        "Throw"     
## [451] "A"          "The"        "The"        "The"        "The"       
## [456] "Turn"       "The"        "The"        "To"         "The"       
## [461] "The"        "Dispense"   "The"        "He"         "The"       
## [466] "The"        "Fly"        "Thick"      "Birth"      "The"       
## [471] "The"        "A"          "The"        "We"         "The"       
## [476] "The"        "We"         "The"        "Five"       "A"         
## [481] "The"        "Shut"       "The"        "Crack"      "He"        
## [486] "Send"       "A"          "They"       "The"        "In"        
## [491] "A"          "Oats"       "Their"      "The"        "There"     
## [496] "Tuck"       "A"          "We"         "The"        "Take"      
## [501] "Shake"      "She"        "The"        "The"        "We"        
## [506] "Smile"      "A"          "The"        "Take"       "That"      
## [511] "The"        "The"        "Ripe"       "A"          "The"       
## [516] "The"        "The"        "This"       "She"        "The"       
## [521] "Press"      "Neat"       "The"        "The"        "The"       
## [526] "Shake"      "The"        "A"          "His"        "Flax"      
## [531] "Hurdle"     "A"          "Even"       "Peep"       "The"       
## [536] "Cheap"      "A"          "Flood"      "A"          "The"       
## [541] "Those"      "He"         "Dill"       "Down"       "Either"    
## [546] "The"        "If"         "At"         "Read"       "Fill"      
## [551] "The"        "Clams"      "The"        "The"        "Breathe"   
## [556] "It"         "A"          "A"          "A"          "A"         
## [561] "Paint"      "The"        "Bribes"     "Trample"    "The"       
## [566] "A"          "Footprints" "She"        "A"          "Prod"      
## [571] "It"         "The"        "It"         "The"        "Wake"      
## [576] "The"        "The"        "The"        "Hold"       "Next"      
## [581] "Every"      "He"         "They"       "Drive"      "Keep"      
## [586] "Sever"      "Paper"      "Slide"      "Help"       "A"         
## [591] "Stop"       "Jerk"       "Slidc"      "The"        "Light"     
## [596] "Set"        "Dull"       "A"          "Get"        "Choose"    
## [601] "A"          "He"         "There"      "The"        "Greet"     
## [606] "When"       "Sweet"      "A"          "A"          "Lush"      
## [611] "The"        "The"        "The"        "Sit"        "A"         
## [616] "The"        "Green"      "Tea"        "Pitch"      "The"       
## [621] "The"        "The"        "A"          "The"        "She"       
## [626] "The"        "Loop"       "Plead"      "Calves"     "Post"      
## [631] "Tear"       "A"          "A"          "It"         "Crouch"    
## [636] "Pack"       "The"        "Fine"       "Poached"    "Bad"       
## [641] "Ship"       "Dimes"      "They"       "The"        "The"       
## [646] "The"        "The"        "Pile"       "The"        "The"       
## [651] "The"        "The"        "A"          "The"        "The"       
## [656] "To"         "There"      "Cod"        "The"        "Dunk"      
## [661] "Hang"       "Cap"        "The"        "Be"         "Pick"      
## [666] "A"          "The"        "The"        "The"        "You"       
## [671] "Dots"       "Put"        "The"        "The"        "See"       
## [676] "Slide"      "Many"       "We"         "No"         "Dig"       
## [681] "The"        "A"          "Green"      "A"          "The"       
## [686] "A"          "The"        "The"        "Seven"      "Our"       
## [691] "The"        "It"         "One"        "Take"       "The"       
## [696] "The"        "The"        "Stop"       "The"        "The"       
## [701] "Open"       "Fish"       "Dip"        "Will"       "The"       
## [706] "The"        "The"        "He"         "Leave"      "The"       
## [711] "A"          "The"        "She"        "A"          "Small"     
## [716] "The"        "The"        "A"          "She"        "When"
```

* All words ending in ing.


```r
ing_pattern <- "\\b[(a-z)|(A-z)]+ing\\b"
sentences_with_ing <- str_detect(sentences, ing_pattern)
str_extract_all(sentences[sentences_with_ing], ing_pattern, simplify = TRUE)
```

```
##       [,1]       
##  [1,] "spring"   
##  [2,] "evening"  
##  [3,] "morning"  
##  [4,] "winding"  
##  [5,] "living"   
##  [6,] "king"     
##  [7,] "Adding"   
##  [8,] "making"   
##  [9,] "raging"   
## [10,] "playing"  
## [11,] "sleeping" 
## [12,] "ring"     
## [13,] "glaring"  
## [14,] "sinking"  
## [15,] "dying"    
## [16,] "Bring"    
## [17,] "lodging"  
## [18,] "filing"   
## [19,] "making"   
## [20,] "morning"  
## [21,] "wearing"  
## [22,] "Bring"    
## [23,] "wading"   
## [24,] "swing"    
## [25,] "nothing"  
## [26,] "ring"     
## [27,] "morning"  
## [28,] "sing"     
## [29,] "sleeping" 
## [30,] "painting" 
## [31,] "king"     
## [32,] "walking"  
## [33,] "bring"    
## [34,] "bring"    
## [35,] "shipping" 
## [36,] "spring"   
## [37,] "ring"     
## [38,] "winding"  
## [39,] "puzzling" 
## [40,] "spring"   
## [41,] "landing"  
## [42,] "thing"    
## [43,] "waiting"  
## [44,] "whistling"
## [45,] "nothing"  
## [46,] "timing"   
## [47,] "thing"    
## [48,] "spring"   
## [49,] "changing" 
## [50,] "drenching"
## [51,] "moving"   
## [52,] "working"  
## [53,] "ring"
```

* All plurals.


```r
plural_pattern <- "\\b[(a-z)|(A-z)]+s\\b"
sentences_with_plural <- str_detect(sentences, plural_pattern)
str_extract_all(sentences[sentences_with_plural], plural_pattern, simplify = TRUE)
```

```
##        [,1]         [,2]       [,3]      [,4]     
##   [1,] "planks"     ""         ""        ""       
##   [2,] "days"       "is"       ""        ""       
##   [3,] "is"         "bowls"    ""        ""       
##   [4,] "lemons"     "makes"    ""        ""       
##   [5,] "was"        ""         ""        ""       
##   [6,] "hogs"       ""         ""        ""       
##   [7,] "hours"      "us"       ""        ""       
##   [8,] "stockings"  "is"       ""        ""       
##   [9,] "was"        ""         ""        ""       
##  [10,] "is"         ""         ""        ""       
##  [11,] "is"         ""         ""        ""       
##  [12,] "helps"      "pass"     ""        ""       
##  [13,] "fires"      ""         ""        ""       
##  [14,] "across"     ""         ""        ""       
##  [15,] "bonds"      ""         ""        ""       
##  [16,] "Press"      "pants"    ""        ""       
##  [17,] "was"        ""         ""        ""       
##  [18,] "was"        "useless"  ""        ""       
##  [19,] "gas"        ""         ""        ""       
##  [20,] "his"        ""         ""        ""       
##  [21,] "was"        ""         ""        ""       
##  [22,] "kittens"    ""         ""        ""       
##  [23,] "was"        ""         ""        ""       
##  [24,] "is"         ""         ""        ""       
##  [25,] "days"       ""         ""        ""       
##  [26,] "was"        ""         ""        ""       
##  [27,] "Sickness"   ""         ""        ""       
##  [28,] "grass"      ""         ""        ""       
##  [29,] "books"      ""         ""        ""       
##  [30,] "keeps"      "chicks"   ""        ""       
##  [31,] "leads"      "sums"     ""        ""       
##  [32,] "was"        ""         ""        ""       
##  [33,] "is"         "boards"   ""        ""       
##  [34,] "wheels"     ""         ""        ""       
##  [35,] "soldiers"   ""         ""        ""       
##  [36,] "makes"      ""         ""        ""       
##  [37,] "steps"      ""         ""        ""       
##  [38,] "lives"      ""         ""        ""       
##  [39,] "circus"     ""         ""        ""       
##  [40,] "across"     ""         ""        ""       
##  [41,] "Cars"       "busses"   "drifts"  ""       
##  [42,] "This"       "is"       "hikes"   ""       
##  [43,] "words"      ""         ""        ""       
##  [44,] "was"        ""         ""        ""       
##  [45,] "weeks"      ""         ""        ""       
##  [46,] "makes"      ""         ""        ""       
##  [47,] "strokes"    ""         ""        ""       
##  [48,] "was"        ""         ""        ""       
##  [49,] "was"        "slices"   ""        ""       
##  [50,] "factors"    ""         ""        ""       
##  [51,] "was"        ""         ""        ""       
##  [52,] "his"        ""         ""        ""       
##  [53,] "grass"      ""         ""        ""       
##  [54,] "parts"      ""         ""        ""       
##  [55,] "Always"     ""         ""        ""       
##  [56,] "costs"      "eggs"     ""        ""       
##  [57,] "was"        ""         ""        ""       
##  [58,] "seems"      ""         ""        ""       
##  [59,] "is"         "gifts"    ""        ""       
##  [60,] "pins"       ""         ""        ""       
##  [61,] "as"         ""         ""        ""       
##  [62,] "is"         "gives"    ""        ""       
##  [63,] "Cats"       "dogs"     ""        ""       
##  [64,] "glass"      ""         ""        ""       
##  [65,] "Thieves"    "friends"  ""        ""       
##  [66,] "improves"   ""         ""        ""       
##  [67,] "orders"     ""         ""        ""       
##  [68,] "was"        ""         ""        ""       
##  [69,] "Leaves"     ""         ""        ""       
##  [70,] "logs"       ""         ""        ""       
##  [71,] "is"         "parts"    "tropics" ""       
##  [72,] "his"        ""         ""        ""       
##  [73,] "things"     ""         ""        ""       
##  [74,] "lists"      "orders"   ""        ""       
##  [75,] "less"       ""         ""        ""       
##  [76,] "boss"       ""         ""        ""       
##  [77,] "its"        "contents" ""        ""       
##  [78,] "brass"      ""         ""        ""       
##  [79,] "is"         ""         ""        ""       
##  [80,] "its"        ""         ""        ""       
##  [81,] "is"         ""         ""        ""       
##  [82,] "times"      ""         ""        ""       
##  [83,] "leaves"     ""         ""        ""       
##  [84,] "plus"       "is"       "less"    ""       
##  [85,] "eyes"       ""         ""        ""       
##  [86,] "problems"   ""         ""        ""       
##  [87,] "Clothes"    ""         ""        ""       
##  [88,] "events"     ""         ""        ""       
##  [89,] "is"         ""         ""        ""       
##  [90,] "Guess"      "results"  "scores"  ""       
##  [91,] "tastes"     ""         ""        ""       
##  [92,] "thistles"   ""         ""        ""       
##  [93,] "poodles"    "curls"    ""        ""       
##  [94,] "was"        ""         ""        ""       
##  [95,] "was"        "his"      ""        ""       
##  [96,] "this"       ""         ""        ""       
##  [97,] "is"         ""         ""        ""       
##  [98,] "stories"    "is"       ""        ""       
##  [99,] "pencils"    ""         ""        ""       
## [100,] "pirates"    ""         ""        ""       
## [101,] "is"         ""         ""        ""       
## [102,] "is"         ""         ""        ""       
## [103,] "figures"    ""         ""        ""       
## [104,] "is"         ""         ""        ""       
## [105,] "does"       ""         ""        ""       
## [106,] "is"         "toys"     "blocks"  ""       
## [107,] "was"        ""         ""        ""       
## [108,] "actress"    ""         ""        ""       
## [109,] "pipes"      ""         ""        ""       
## [110,] "was"        "leaves"   ""        ""       
## [111,] "was"        ""         ""        ""       
## [112,] "scraps"     ""         ""        ""       
## [113,] "is"         ""         ""        ""       
## [114,] "pills"      ""         ""        ""       
## [115,] "was"        ""         ""        ""       
## [116,] "burns"      "holes"    ""        ""       
## [117,] "tales"      ""         ""        ""       
## [118,] "miles"      ""         ""        ""       
## [119,] "was"        "players"  ""        ""       
## [120,] "is"         "inches"   ""        ""       
## [121,] "has"        "clothes"  ""        ""       
## [122,] "is"         ""         ""        ""       
## [123,] "cobs"       ""         ""        ""       
## [124,] "is"         "tacks"    ""        ""       
## [125,] "tongs"      ""         ""        ""       
## [126,] "petals"     ""         ""        ""       
## [127,] "compass"    "class"    ""        ""       
## [128,] "Farmers"    ""         ""        ""       
## [129,] "was"        ""         ""        ""       
## [130,] "is"         ""         ""        ""       
## [131,] "is"         ""         ""        ""       
## [132,] "wonders"    ""         ""        ""       
## [133,] "hostess"    ""         ""        ""       
## [134,] "his"        ""         ""        ""       
## [135,] "his"        ""         ""        ""       
## [136,] "loss"       "was"      ""        ""       
## [137,] "its"        ""         ""        ""       
## [138,] "wires"      ""         ""        ""       
## [139,] "taps"       ""         ""        ""       
## [140,] "is"         ""         ""        ""       
## [141,] "was"        "brass"    ""        ""       
## [142,] "writes"     ""         ""        ""       
## [143,] "Schools"    "ladies"   ""        ""       
## [144,] "was"        ""         ""        ""       
## [145,] "fans"       ""         ""        ""       
## [146,] "ribbons"    ""         ""        ""       
## [147,] "his"        "ties"     "groups"  "friends"
## [148,] "backs"      ""         ""        ""       
## [149,] "Whitings"   "nets"     ""        ""       
## [150,] "ads"        "buyers"   ""        ""       
## [151,] "rings"      ""         ""        ""       
## [152,] "makes"      "us"       ""        ""       
## [153,] "this"       "is"       ""        ""       
## [154,] "islands"    "is"       ""        ""       
## [155,] "as"         "as"       ""        ""       
## [156,] "This"       ""         ""        ""       
## [157,] "funds"      "its"      ""        ""       
## [158,] "is"         ""         ""        ""       
## [159,] "gets"       ""         ""        ""       
## [160,] "as"         "slides"   ""        ""       
## [161,] "brothers"   ""         ""        ""       
## [162,] "his"        ""         ""        ""       
## [163,] "houses"     "bricks"   ""        ""       
## [164,] "Ducks"      "compass"  ""        ""       
## [165,] "flavors"    "drinks"   ""        ""       
## [166,] "pills"      "less"     "others"  ""       
## [167,] "pears"      ""         ""        ""       
## [168,] "is"         ""         ""        ""       
## [169,] "miles"      ""         ""        ""       
## [170,] "seats"      "fans"     ""        ""       
## [171,] "is"         ""         ""        ""       
## [172,] "spoils"     ""         ""        ""       
## [173,] "is"         "cross"    ""        ""       
## [174,] "binds"      ""         ""        ""       
## [175,] "ruins"      ""         ""        ""       
## [176,] "shelves"    "crackers" ""        ""       
## [177,] "is"         ""         ""        ""       
## [178,] "his"        "eyes"     ""        ""       
## [179,] "needs"      ""         ""        ""       
## [180,] "is"         ""         ""        ""       
## [181,] "apples"     "hands"    ""        ""       
## [182,] "crackers"   ""         ""        ""       
## [183,] "curls"      "days"     ""        ""       
## [184,] "tones"      ""         ""        ""       
## [185,] "fits"       ""         ""        ""       
## [186,] "was"        ""         ""        ""       
## [187,] "leaves"     "was"      ""        ""       
## [188,] "His"        "was"      "was"     ""       
## [189,] "was"        "hops"     ""        ""       
## [190,] "cans"       "shelves"  ""        ""       
## [191,] "clouds"     ""         ""        ""       
## [192,] "was"        "useless"  ""        ""       
## [193,] "is"         ""         ""        ""       
## [194,] "as"         "chorus"   ""        ""       
## [195,] "minutes"    ""         ""        ""       
## [196,] "walls"      "frocks"   ""        ""       
## [197,] "discuss"    "plans"    ""        ""       
## [198,] "takes"      ""         ""        ""       
## [199,] "is"         ""         ""        ""       
## [200,] "was"        "brass"    ""        ""       
## [201,] "as"         "as"       ""        ""       
## [202,] "brings"     "us"       "changes" ""       
## [203,] "was"        "years"    ""        ""       
## [204,] "sometimes"  ""         ""        ""       
## [205,] "contents"   ""         ""        ""       
## [206,] "mules"      ""         ""        ""       
## [207,] "records"    ""         ""        ""       
## [208,] "was"        ""         ""        ""       
## [209,] "rays"       ""         ""        ""       
## [210,] "Boards"     "unless"   ""        ""       
## [211,] "Glass"      ""         ""        ""       
## [212,] "grass"      ""         ""        ""       
## [213,] "rows"       "soldiers" ""        ""       
## [214,] "is"         ""         ""        ""       
## [215,] "is"         "edges"    ""        ""       
## [216,] "Pages"      ""         ""        ""       
## [217,] "lines"      ""         ""        ""       
## [218,] "less"       ""         ""        ""       
## [219,] "zones"      ""         ""        ""       
## [220,] "needs"      ""         ""        ""       
## [221,] "is"         "secrets"  ""        ""       
## [222,] "is"         "us"       ""        ""       
## [223,] "brass"      "objects"  ""        ""       
## [224,] "is"         ""         ""        ""       
## [225,] "gives"      "us"       ""        ""       
## [226,] "fails"      ""         ""        ""       
## [227,] "us"         ""         ""        ""       
## [228,] "woods"      "sticks"   ""        ""       
## [229,] "piles"      "lines"    ""        ""       
## [230,] "logs"       ""         ""        ""       
## [231,] "is"         ""         ""        ""       
## [232,] "plans"      ""         ""        ""       
## [233,] "Brass"      "rings"    "natives" ""       
## [234,] "takes"      ""         ""        ""       
## [235,] "seeds"      ""         ""        ""       
## [236,] "is"         ""         ""        ""       
## [237,] "This"       "was"      ""        ""       
## [238,] "wanders"    ""         ""        ""       
## [239,] "takes"      ""         ""        ""       
## [240,] "shares"     "as"       ""        ""       
## [241,] "cats"       "goes"     "names"   ""       
## [242,] "winds"      "colds"    "fevers"  ""       
## [243,] "asks"       ""         ""        ""       
## [244,] "dress"      ""         ""        ""       
## [245,] "means"      "is"       ""        ""       
## [246,] "items"      ""         ""        ""       
## [247,] "Roads"      ""         ""        ""       
## [248,] "ones"       ""         ""        ""       
## [249,] "heroes"     ""         ""        ""       
## [250,] "makes"      ""         ""        ""       
## [251,] "cuts"       "trims"    ""        ""       
## [252,] "ways"       ""         ""        ""       
## [253,] "His"        ""         ""        ""       
## [254,] "lingers"    ""         ""        ""       
## [255,] "was"        ""         ""        ""       
## [256,] "takes"      ""         ""        ""       
## [257,] "is"         ""         ""        ""       
## [258,] "costs"      "cents"    "Mondays" ""       
## [259,] "is"         "peas"     "always"  ""       
## [260,] "names"      "is"       ""        ""       
## [261,] "is"         ""         ""        ""       
## [262,] "makes"      ""         ""        ""       
## [263,] "news"       "restless" "minds"   ""       
## [264,] "pins"       ""         ""        ""       
## [265,] "restores"   ""         ""        ""       
## [266,] "takes"      ""         ""        ""       
## [267,] "drifts"     ""         ""        ""       
## [268,] "was"        ""         ""        ""       
## [269,] "is"         ""         ""        ""       
## [270,] "is"         ""         ""        ""       
## [271,] "lines"      ""         ""        ""       
## [272,] "his"        "cents"    ""        ""       
## [273,] "was"        ""         ""        ""       
## [274,] "Hats"       ""         ""        ""       
## [275,] "robins"     ""         ""        ""       
## [276,] "mats"       ""         ""        ""       
## [277,] "This"       "his"      ""        ""       
## [278,] "protects"   ""         ""        ""       
## [279,] "coins"      "his"      ""        ""       
## [280,] "rags"       "itches"   ""        ""       
## [281,] "pants"      "cuffs"    "pockets" ""       
## [282,] "lasts"      "days"     ""        ""       
## [283,] "buns"       ""         ""        ""       
## [284,] "Bottles"    "kinds"    ""        ""       
## [285,] "his"        ""         ""        ""       
## [286,] "ashes"      ""         ""        ""       
## [287,] "chairs"     ""         ""        ""       
## [288,] "means"      ""         ""        ""       
## [289,] "drapes"     ""         ""        ""       
## [290,] "stems"      "glasses"  ""        ""       
## [291,] "clothes"    ""         ""        ""       
## [292,] "gives"      "us"       ""        ""       
## [293,] "bills"      ""         ""        ""       
## [294,] "is"         ""         ""        ""       
## [295,] "is"         ""         ""        ""       
## [296,] "this"       ""         ""        ""       
## [297,] "grapes"     "was"      ""        ""       
## [298,] "figs"       "cherries" ""        ""       
## [299,] "sparks"     ""         ""        ""       
## [300,] "glasses"    ""         ""        ""       
## [301,] "limits"     ""         ""        ""       
## [302,] "is"         ""         ""        ""       
## [303,] "priceless"  "stones"   ""        ""       
## [304,] "was"        ""         ""        ""       
## [305,] "lanterns"   ""         ""        ""       
## [306,] "years"      ""         ""        ""       
## [307,] "cuts"       ""         ""        ""       
## [308,] "is"         ""         ""        ""       
## [309,] "waves"      ""         ""        ""       
## [310,] "makes"      "hearts"   ""        ""       
## [311,] "is"         ""         ""        ""       
## [312,] "tales"      ""         ""        ""       
## [313,] "was"        ""         ""        ""       
## [314,] "was"        ""         ""        ""       
## [315,] "Oats"       ""         ""        ""       
## [316,] "eyelids"    ""         ""        ""       
## [317,] "revives"    "his"      ""        ""       
## [318,] "ways"       "things"   ""        ""       
## [319,] "is"         ""         ""        ""       
## [320,] "shoes"      ""         ""        ""       
## [321,] "was"        ""         ""        ""       
## [322,] "was"        ""         ""        ""       
## [323,] "dress"      "days"     ""        ""       
## [324,] "words"      ""         ""        ""       
## [325,] "is"         ""         ""        ""       
## [326,] "this"       "is"       ""        ""       
## [327,] "this"       ""         ""        ""       
## [328,] "is"         "books"    ""        ""       
## [329,] "tales"      ""         ""        ""       
## [330,] "was"        "as"       ""        ""       
## [331,] "pears"      ""         ""        ""       
## [332,] "was"        ""         ""        ""       
## [333,] "hours"      ""         ""        ""       
## [334,] "was"        ""         ""        ""       
## [335,] "This"       ""         ""        ""       
## [336,] "Press"      ""         ""        ""       
## [337,] "plans"      ""         ""        ""       
## [338,] "was"        ""         ""        ""       
## [339,] "hands"      "this"     ""        ""       
## [340,] "is"         "this"     ""        ""       
## [341,] "His"        "friends"  ""        ""       
## [342,] "makes"      ""         ""        ""       
## [343,] "needs"      ""         ""        ""       
## [344,] "clowns"     ""         ""        ""       
## [345,] "drifts"     ""         ""        ""       
## [346,] "clothes"    ""         ""        ""       
## [347,] "mails"      "requests" "this"    ""       
## [348,] "was"        "ends"     ""        ""       
## [349,] "words"      ""         ""        ""       
## [350,] "his"        ""         ""        ""       
## [351,] "pickles"    ""         ""        ""       
## [352,] "is"         ""         ""        ""       
## [353,] "times"      ""         ""        ""       
## [354,] "is"         "clips"    ""        ""       
## [355,] "says"       ""         ""        ""       
## [356,] "trinkets"   ""         ""        ""       
## [357,] "Clams"      ""         ""        ""       
## [358,] "its"        "blades"   ""        ""       
## [359,] "edges"      "was"      ""        ""       
## [360,] "matters"    "reads"    "words"   ""       
## [361,] "its"        ""         ""        ""       
## [362,] "goes"       "shoes"    ""        ""       
## [363,] "sockets"    ""         ""        ""       
## [364,] "grass"      ""         ""        ""       
## [365,] "Bribes"     ""         ""        ""       
## [366,] "flames"     ""         ""        ""       
## [367,] "was"        "designs"  ""        ""       
## [368,] "was"        ""         ""        ""       
## [369,] "Footprints" ""         ""        ""       
## [370,] "was"        ""         ""        ""       
## [371,] "is"         "inches"   ""        ""       
## [372,] "was"        "mass"     "leaves"  "shrubs" 
## [373,] "was"        ""         ""        ""       
## [374,] "outdoors"   ""         ""        ""       
## [375,] "brass"      ""         ""        ""       
## [376,] "lobes"      "ears"     "rings"   ""       
## [377,] "is"         ""         ""        ""       
## [378,] "speaks"     "is"       ""        ""       
## [379,] "his"        ""         ""        ""       
## [380,] "kids"       ""         ""        ""       
## [381,] "gets"       "friends"  ""        ""       
## [382,] "boys"       ""         ""        ""       
## [383,] "tumbles"    ""         ""        ""       
## [384,] "across"     "glass"    ""        ""       
## [385,] "was"        ""         ""        ""       
## [386,] "makes"      ""         ""        ""       
## [387,] "stories"    ""         ""        ""       
## [388,] "funds"      "seems"    ""        ""       
## [389,] "his"        ""         ""        ""       
## [390,] "is"         ""         ""        ""       
## [391,] "guests"     ""         ""        ""       
## [392,] "has"        "is"       ""        ""       
## [393,] "words"      ""         ""        ""       
## [394,] "runs"       ""         ""        ""       
## [395,] "comes"      ""         ""        ""       
## [396,] "rocks"      ""         ""        ""       
## [397,] "looks"      ""         ""        ""       
## [398,] "his"        ""         ""        ""       
## [399,] "others"     ""         ""        ""       
## [400,] "is"         ""         ""        ""       
## [401,] "moves"      ""         ""        ""       
## [402,] "moss"       "grows"    ""        ""       
## [403,] "has"        ""         ""        ""       
## [404,] "was"        ""         ""        ""       
## [405,] "is"         "dishes"   ""        ""       
## [406,] "facts"      "always"   "is"      ""       
## [407,] "flaps"      "as"       "parades" ""       
## [408,] "loss"       "was"      ""        ""       
## [409,] "Calves"     "grass"    ""        ""       
## [410,] "bills"      "this"     ""        ""       
## [411,] "waters"     "is"       ""        ""       
## [412,] "was"        ""         ""        ""       
## [413,] "miss"       ""         ""        ""       
## [414,] "kits"       ""         ""        ""       
## [415,] "saves"      ""         ""        ""       
## [416,] "eggs"       ""         ""        ""       
## [417,] "nerves"     ""         ""        ""       
## [418,] "maps"       "planes"   ""        ""       
## [419,] "Dimes"      "sides"    ""        ""       
## [420,] "tunes"      ""         ""        ""       
## [421,] "is"         ""         ""        ""       
## [422,] "pods"       "peas"     "fields"  ""       
## [423,] "is"         ""         ""        ""       
## [424,] "was"        "its"      ""        ""       
## [425,] "comes"      ""         ""        ""       
## [426,] "is"         ""         ""        ""       
## [427,] "is"         ""         ""        ""       
## [428,] "amounts"    "is"       ""        ""       
## [429,] "is"         ""         ""        ""       
## [430,] "is"         "business" ""        ""       
## [431,] "was"        "blocks"   ""        ""       
## [432,] "biscuits"   ""         ""        ""       
## [433,] "branches"   ""         ""        ""       
## [434,] "brass"      ""         ""        ""       
## [435,] "needs"      ""         ""        ""       
## [436,] "informs"    ""         ""        ""       
## [437,] "comes"      "batches"  ""        ""       
## [438,] "Dots"       ""         ""        ""       
## [439,] "leaves"     ""         ""        ""       
## [440,] "hands"      ""         ""        ""       
## [441,] "faults"     ""         ""        ""       
## [442,] "blows"      ""         ""        ""       
## [443,] "is"         ""         ""        ""       
## [444,] "takes"      "less"     ""        ""       
## [445,] "needs"      ""         ""        ""       
## [446,] "fits"       ""         ""        ""       
## [447,] "gloss"      ""         ""        ""       
## [448,] "grass"      ""         ""        ""       
## [449,] "seals"      "sheets"   ""        ""       
## [450,] "troops"     "blows"    ""        ""       
## [451,] "was"        ""         ""        ""       
## [452,] "was"        ""         ""        ""       
## [453,] "contents"   ""         ""        ""       
## [454,] "puts"       "his"      "his"     ""       
## [455,] "bombs"      "ruins"    ""        ""       
## [456,] "streets"    "turns"    ""        ""       
## [457,] "as"         ""         ""        ""       
## [458,] "was"        ""         ""        ""       
## [459,] "was"        ""         ""        ""       
## [460,] "was"        ""         ""        ""       
## [461,] "grass"      "bushes"   ""        ""       
## [462,] "his"        "coins"    ""        ""       
## [463,] "his"        "times"    ""        ""
```

## Find all words that come after a “number” like “one”, “two”, “three” etc. Pull out both the number and the word.


```r
numbers <- c("one", "two", "three", "four", "five", "six", "seven", "eight", 
  "nine", "ten")
numbers_match <- str_c(numbers, collapse = "|")
numbers_word_match <- str_c("(", numbers_match, ") ([^ ]+)")

sentences[str_detect(sentences, numbers_word_match)] %>% 
  str_extract_all(numbers_word_match, simplify = TRUE)
```

```
##       [,1]            [,2]        
##  [1,] "ten served"    ""          
##  [2,] "one over"      ""          
##  [3,] "seven books"   ""          
##  [4,] "two met"       ""          
##  [5,] "two factors"   ""          
##  [6,] "one and"       ""          
##  [7,] "three lists"   ""          
##  [8,] "seven is"      ""          
##  [9,] "two when"      ""          
## [10,] "one floor."    ""          
## [11,] "ten inches."   ""          
## [12,] "one with"      ""          
## [13,] "one war"       ""          
## [14,] "one button"    ""          
## [15,] "six minutes."  ""          
## [16,] "ten years"     ""          
## [17,] "one in"        ""          
## [18,] "ten chased"    ""          
## [19,] "one like"      ""          
## [20,] "two shares"    ""          
## [21,] "two distinct"  ""          
## [22,] "one costs"     "five cents"
## [23,] "ten two"       ""          
## [24,] "five robins."  ""          
## [25,] "four kinds"    ""          
## [26,] "one rang"      ""          
## [27,] "ten him."      ""          
## [28,] "three story"   ""          
## [29,] "ten by"        ""          
## [30,] "one wall."     ""          
## [31,] "three inches"  ""          
## [32,] "ten your"      ""          
## [33,] "six comes"     "ten than"  
## [34,] "one before"    ""          
## [35,] "three batches" ""          
## [36,] "two leaves."   ""
```

## Find all contractions. Separate out the pieces before and after the apostrophe


```r
contractions_match <- "([^ ]+)'([^ ]+)"
sentences[str_detect(sentences, contractions_match)] %>% 
  str_extract_all(contractions_match, simplify = TRUE)
```

```
##       [,1]        
##  [1,] "It's"      
##  [2,] "man's"     
##  [3,] "don't"     
##  [4,] "store's"   
##  [5,] "workmen's" 
##  [6,] "Let's"     
##  [7,] "sun's"     
##  [8,] "child's"   
##  [9,] "king's"    
## [10,] "It's"      
## [11,] "don't"     
## [12,] "queen's"   
## [13,] "don't"     
## [14,] "pirate's"  
## [15,] "neighbor's"
```

## Replace all forward slashes in a string with backslashes.


```r
test <- "sdfasdf/test"
str_detect(test, "/")
```

```
## [1] TRUE
```

```r
str_replace(test, "\\/", "\\")
```

```
## [1] "sdfasdftest"
```

## Implement a simple version of `str_to_lower()` using `replace_all()`.


```r
duju_to_lower <- function(string) {
  duju_letters <- letters
  names(duju_letters) <- toupper(letters)
  str_replace_all(string, duju_letters)
}

duju_to_lower("AAbisEEWaioT")
```

```
## [1] "aabiseewaiot"
```

## Switch the first and last letters in words. Which of those strings are still words?


```r
words_switched <- str_replace(words, "(^.)(.{0,})(.$)", "\\3\\2\\1")
words_switched[str_detect(words, words_switched)]
```

```
##  [1] "a"          "america"    "area"       "dad"        "dead"      
##  [6] "depend"     "educate"    "else"       "encourage"  "engine"    
## [11] "europe"     "evidence"   "example"    "excuse"     "exercise"  
## [16] "expense"    "experience" "eye"        "health"     "high"      
## [21] "knock"      "level"      "local"      "nation"     "non"       
## [26] "rather"     "refer"      "remember"   "serious"    "stairs"    
## [31] "test"       "tonight"    "transport"  "treat"      "trust"     
## [36] "window"     "yesterday"
```

## Split up a string like "apples, pears, and bananas" into individual components


```r
"apples, pears, and bananas" %>% 
  str_split(pattern = ", |\\s") %>% 
  .[[1]]
```

```
## [1] "apples"  "pears"   "and"     "bananas"
```

## Why is it better to split up by `boundary("word")` than `" "`?

For example there could be two whitespaces.

## What does splitting with an empty string ("") do? Experiment, and then read the documentation.


```r
str_split("Hallo was passiert dann?", pattern = "")
```

```
## [[1]]
##  [1] "H" "a" "l" "l" "o" " " "w" "a" "s" " " "p" "a" "s" "s" "i" "e" "r"
## [18] "t" " " "d" "a" "n" "n" "?"
```

```r
#?str_split
```

Same as `boundary("character")`

## How would you find all strings containing `\` with `regex()` vs. with `fixed()`?


```r
str_subset(c("a\\b", "ab"), "\\\\")
```

```
## [1] "a\\b"
```

```r
#> [1] "a\\b"
str_subset(c("a\\b", "ab"), fixed("\\"))
```

```
## [1] "a\\b"
```

```r
#> [1] "a\\b"
```

## What are the five most common words in sentences?


```r
df_word_count <- tibble(
  sentence = sentences, 
  words = str_extract_all(sentence, boundary("word"))) %>% 
  unnest(words) %>% 
  mutate(words = str_to_lower(words)) %>% 
  group_by(words) %>% 
  summarise(anz = n()) %>% 
  arrange(desc(anz)) %>% 
  top_n(anz, n = 5)

df_word_count
```

```
## # A tibble: 5 × 2
##   words   anz
##   <chr> <int>
## 1   the   751
## 2     a   202
## 3    of   132
## 4    to   123
## 5   and   118
```

##Find the stringi functions that:

* Count the number of words.


```r
stringi::stri_count_words("Hallo wie geht es dir?")
```

```
## [1] 5
```

* Find duplicated strings.


```r
stringi::stri_duplicated(c(
  "Hallo wie Hallo wie geht es dir", "Hallo", "Tschüss", "Hallo"))
```

```
## [1] FALSE FALSE FALSE  TRUE
```

* Generate random text.


```r
stringi::stri_rand_strings(n = 5, length = 12)
```

```
## [1] "GWOyiAebHdew" "CEMSr4lYqRd6" "6WVxH7oCt2K5" "leINOlinI76S"
## [5] "AzEpcQHDLFz2"
```

## How do you control the language that stri_sort() uses for sorting?

* Through its `locale` argument


