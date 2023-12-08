# Class 5: Data visualization with ggplot
Sanket Garg (A59026686)

## Using GGPLOT

``` r
plot(cars)
```

![](class05_files/figure-commonmark/unnamed-chunk-1-1.png)

to use ggplot2, we first need to install it on our computers. To do
this, we will use the function ‘install.packages()’.

to use ggplot, I need to spell out at least 3 things: - data (the stuff
I want to plot as a data.frame) - aesthetics (aes() values - how the
data map to the plot) - geoms (how I want things drawn)

``` r
library(ggplot2)
ggplot(cars) + 
  aes(x = speed, y = dist) + 
  geom_point() +
  geom_smooth(method='lm')
```

    `geom_smooth()` using formula = 'y ~ x'

![](class05_files/figure-commonmark/unnamed-chunk-2-1.png)

``` r
  ggplot(cars) + 
  aes(x = speed, y = dist) + 
  geom_point() + 
labs(title="Speed and Stopping Distances of cars", x="Speed (MPH)", y="Stopping          Distance(ft)",subtitle = "Your informative subtitle text here",caption="Dataset: 'cars'")+
  geom_smooth(method = 'lm', se=FALSE) + 
  theme_bw()
```

    `geom_smooth()` using formula = 'y ~ x'

![](class05_files/figure-commonmark/unnamed-chunk-3-1.png)

How many genes are upregulated?

``` r
url <- "https://bioboot.github.io/bimm143_S20/class-material/up_down_expression.txt"
genes <- read.delim(url)
head(genes)
```

            Gene Condition1 Condition2      State
    1      A4GNT -3.6808610 -3.4401355 unchanging
    2       AAAS  4.5479580  4.3864126 unchanging
    3      AASDH  3.7190695  3.4787276 unchanging
    4       AATF  5.0784720  5.0151916 unchanging
    5       AATK  0.4711421  0.5598642 unchanging
    6 AB015752.4 -3.6808610 -3.5921390 unchanging

``` r
ncol(genes)
```

    [1] 4

``` r
table(genes$State)
```


          down unchanging         up 
            72       4997        127 

``` r
round(table(genes$State)/nrow(genes)*100,2)
```


          down unchanging         up 
          1.39      96.17       2.44 

Making a scatter plot for the Genes dataset

``` r
  ggplot(genes) + 
  aes(x=Condition1, y=Condition2) + 
  geom_point()
```

![](class05_files/figure-commonmark/unnamed-chunk-5-1.png)

Changing the color of the datapoints based on their State:

``` r
p <- ggplot(genes) + 
  aes(x=Condition1, y=Condition2, col=State) + 
  geom_point()
p
```

![](class05_files/figure-commonmark/unnamed-chunk-6-1.png)

Adding title and changing the x and y labels:

``` r
p + 
  scale_color_manual(values = c("blue","gray", "red")) +
  labs(title = "Gene Expression Changes Upon Drug Treatment", x="Control (No Drug)", y="Drug Treatment")
```

![](class05_files/figure-commonmark/unnamed-chunk-7-1.png)

Downloading the gapminder dataset:

``` r
url <- "https://raw.githubusercontent.com/jennybc/gapminder/master/inst/extdata/gapminder.tsv"
gapminder <- read.delim(url)
library(dplyr)
```

    Warning: package 'dplyr' was built under R version 4.3.2


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
gapminder_2007 <- gapminder %>% filter(year==2007)
```

Plotting the gapminder dataset:

``` r
ggplot(gapminder_2007) +
  aes(x=gdpPercap,y=lifeExp) +
  geom_point(alpha=0.4)
```

![](class05_files/figure-commonmark/unnamed-chunk-9-1.png)

Controlling the aesthetics using the population and the continent
aspects of the table:

``` r
ggplot(gapminder_2007) +
  aes(x=gdpPercap,y=lifeExp, color=continent, size=pop) +
  geom_point(alpha=0.4)
```

![](class05_files/figure-commonmark/unnamed-chunk-10-1.png)

Plotting the scatter plot by using population aspect for color

``` r
ggplot(gapminder_2007) +
  aes(x=gdpPercap,y=lifeExp, color=pop) +
  geom_point(alpha=0.4)
```

![](class05_files/figure-commonmark/unnamed-chunk-11-1.png)

Controlling the size of the data points:

``` r
ggplot(gapminder_2007) +
  aes(x=gdpPercap,y=lifeExp, size=pop) +
  geom_point(alpha=0.4) +
  scale_size_area(max_size = 10)
```

![](class05_files/figure-commonmark/unnamed-chunk-12-1.png)

Plotting for 1957

``` r
gapminder_1957 <- gapminder %>% filter(year==1957)
ggplot(gapminder_1957)+
  aes(x=gdpPercap, y=lifeExp, color = continent, size = pop) +
  geom_point(alpha = 0.7) + 
  scale_size_area(max_size = 15)
```

![](class05_files/figure-commonmark/unnamed-chunk-13-1.png)

Comparing 1957 and 2007:

``` r
gapminder_1957 <- gapminder %>% filter(year==1957 | year==2007)
ggplot(gapminder_1957)+
  aes(x=gdpPercap, y=lifeExp, color = continent, size = pop) +
  geom_point(alpha = 0.7) + 
  scale_size_area(max_size = 10) +
  facet_wrap(~year)
```

![](class05_files/figure-commonmark/unnamed-chunk-14-1.png)
