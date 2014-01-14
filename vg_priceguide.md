Videogame pricing data
========================================================

Videogame consumers may wonder what kind of game they should "invest" in.  What kind of games and what kind of condition should they be kept in?




```r
price.guide <- read.csv("price-guide.csv", na.strings = c(""))
head(price.guide)
```

```
##      id console.name       product.name loose.price cib.price new.price
## 1 15087   Atari 2600    3-D Tic-Tac-Toe       $2.99    $19.98    $39.95
## 2 15088   Atari 2600            32 in 1      $27.99      <NA>    $38.22
## 3 15089   Atari 2600             9 to 5        <NA>      <NA>      <NA>
## 4 15090   Atari 2600          Acid Drop       $9.94      <NA>      <NA>
## 5 15094   Atari 2600          Adventure       $4.99    $23.99    $49.99
## 6 15095   Atari 2600 Adventures of Tron       $4.49    $12.99    $26.95
##                genre release.date
## 1             Puzzle         <NA>
## 2              Other         <NA>
## 3              Other         <NA>
## 4 Action & Adventure         <NA>
## 5 Action & Adventure         <NA>
## 6 Action & Adventure         <NA>
```

First some definitions:  
- ID is just a generic ID for the game
- console name is the name for the console
- product name is the name of the game or accessory
- loose price is the price of the product without case or manual
- cib price is the price of the product with case and manual
- new price is the price of the the new unopened product
- genre is the genre of the product
- release date is the release date of the product

______________________________________
Next we do some cleanup of the data, fixing the dates as well as removing the '$' sign from the prices.



```r
price.guide$release.date = as.Date(price.guide$release.date)

price.guide$loose.price = as.numeric(gsub("\\$", "", price.guide$loose.price))
price.guide$cib.price = as.numeric(gsub("\\$", "", price.guide$cib.price))
price.guide$new.price = as.numeric(gsub("\\$", "", price.guide$new.price))
```


We can see from the plots below that the prices are a bit skewed.


```r
plot(price.guide$loose.price)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-31.png) 

```r
plot(price.guide$new.price)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-32.png) 

```r
summary(price.guide)
```

```
##        id               console.name            product.name  
##  Min.   : 1964   Playstation 2:1997   Monopoly        :   13  
##  1st Qu.: 6715   Nintendo DS  :1751   Spiderman       :   13  
##  Median :12602   Xbox 360     :1402   Wheel of Fortune:   12  
##  Mean   :14416   Wii          :1349   Jeopardy        :   11  
##  3rd Qu.:19394   Playstation  :1317   Chessmaster     :    9  
##  Max.   :34522   Playstation 3:1162   Madden 2007     :    9  
##                  (Other)      :9692   (Other)         :18603  
##   loose.price      cib.price       new.price     
##  Min.   :    0   Min.   :    0   Min.   :     0  
##  1st Qu.:    3   1st Qu.:    6   1st Qu.:    10  
##  Median :    6   Median :   10   Median :    18  
##  Mean   :   20   Mean   :   23   Mean   :    63  
##  3rd Qu.:   10   3rd Qu.:   18   3rd Qu.:    35  
##  Max.   :23100   Max.   :33433   Max.   :105000  
##  NA's   :401     NA's   :1402    NA's   :1119    
##                 genre       release.date       
##  Action & Adventure:8558   Min.   :1977-01-01  
##  Other             :1797   1st Qu.:2000-09-01  
##  Racing            :1480   Median :2005-10-01  
##  Sports            :1215   Mean   :2004-02-12  
##  RPG               : 847   3rd Qu.:2009-05-01  
##  (Other)           :4772   Max.   :2013-11-05  
##  NA's              :   1   NA's   :2868
```


___________________________________________
Therefore we do the following log transformations


```r
price.guide$logloose = log(price.guide$loose.price)
price.guide$logcib = log(price.guide$cib.price)
price.guide$lognew = log(price.guide$new.price)
```

A panel equation that we will be using later

```r
p.regression = function(x, y) {
    panel.xyplot(x, y)
    panel.abline(lm(y ~ x))
}

library(lattice)
```



```r
xyplot(logcib ~ logloose | console.name, data = price.guide, panel = p.regression)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

This graph shows, somewhat unsurprisingly, that as loose prices increase, cib prices also increase. 

___________________________________________


```r
xyplot(lognew ~ release.date | genre, data = price.guide, panel = p.regression)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

This plot is a bit more interesting.  If we ignore music and controllers, we see that generally the older games have a higher new price than younger ones.  However, this does not seem to be the case for FPS (first person shooter) games

```r
xyplot(lognew ~ release.date | genre, data = subset(price.guide, genre == "FPS"), 
    panel = p.regression)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-81.png) 

```r
xyplot(logloose ~ release.date | console.name, data = subset(price.guide, genre == 
    "FPS"), panel = p.regression)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-82.png) 

A reason that newer FPS games seem to enjoy higher new prices could be that FPSs have become much more popular in recent years than in years past.

____________________________________________
Finally lets define a spread of the new price - the loose price

```r
price.guide$spread = price.guide$lognew - price.guide$logloose
summary(price.guide$spread)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    -6.5     0.5     1.1     1.2     1.7     8.5    1163
```

```r
boxplot(price.guide$spread)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

So for the most part there is not that much of a spread between the new and loose prices


_______________________________________________


Over time, is there one genre or console where new games are prized much more than loose games?

No, it seems that over time that spread decreases, this makes sense as older games in mint condition are considered more valuable

```r
xyplot(spread ~ release.date | genre, data = price.guide, panel = p.regression)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-101.png) 

```r

xyplot(spread ~ release.date, data = price.guide, panel = p.regression)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-102.png) 

```r

xyplot(spread ~ release.date | console.name, data = price.guide, panel = p.regression)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-103.png) 


The Atari 400 on a further look

```r
xyplot(spread ~ release.date, data = subset(price.guide, console.name == "Atari 400"), 
    panel = p.regression)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 


________________________________________
A conclusion to this is to 'invest' in new FPS games
