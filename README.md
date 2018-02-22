# Reserving for non life insurance

This package is a light compilation of useful code for non life reserving in insurance.

> Be careful if you are using it. It was mainly developed for self-teaching and is not as reliable as the ChainLadder package that already exists. Use it to your own risks.

## Installing the package

You can install the package by using the `r install_github` function from the **devtools** package.

```r
library(devtools)
devtools::install_github("arnaudbu/ReservingLad", dependencies = TRUE)
```

The warnings are normal. They just come from some conflicts between the libraries.

## Type of data

The triangles must be entered under the format of a numeric matrix, with NAs used for unavailable values. A triangle is provided as an example:

```r
data(triangleExampleEngland)
triangleExampleEngland
```

```r
        1       2       3       4       5       6       7       8       9      10
1  357848 1124788 1735330 2218270 2745596 3319994 3466336 3606286 3833515 3901463
2  352118 1236139 2170033 3353322 3799067 4120063 4647867 4914039 5339085      NA
3  290507 1292306 2218525 3235179 3985995 4132918 4628910 4909315      NA      NA
4  310608 1418858 2195047 3757447 4029929 4381982 4588268      NA      NA      NA
5  443160 1136350 2128333 2897821 3402672 3873311      NA      NA      NA      NA
6  396132 1333217 2180715 2985752 3691712      NA      NA      NA      NA      NA
7  440832 1288463 2419861 3483130      NA      NA      NA      NA      NA      NA
8  359480 1421128 2864498      NA      NA      NA      NA      NA      NA      NA
9  376686 1363294      NA      NA      NA      NA      NA      NA      NA      NA
10 344014      NA      NA      NA      NA      NA      NA      NA      NA      NA
```
## Triangle Manipulation

### Create Triangle

The function `Payments2Triangle` can convert a list of payment to a cumulated triangle for reserving purpose. Check the help for more information.

### Cumulate

```r
decTriangle <- Decumulate(triangleExampleEngland)
cumTriangle <- Cumulate(triangleExampleEngland)
cumTriangle
```

### Decumulate

```r
decTriangle <- Decumulate(triangleExampleEngland)
decTriangle
```

```r
        1       2       3       4      5      6      7      8      9    10
1  357848  766940  610542  482940 527326 574398 146342 139950 227229 67948
2  352118  884021  933894 1183289 445745 320996 527804 266172 425046    NA
3  290507 1001799  926219 1016654 750816 146923 495992 280405     NA    NA
4  310608 1108250  776189 1562400 272482 352053 206286     NA     NA    NA
5  443160  693190  991983  769488 504851 470639     NA     NA     NA    NA
6  396132  937085  847498  805037 705960     NA     NA     NA     NA    NA
7  440832  847631 1131398 1063269     NA     NA     NA     NA     NA    NA
8  359480 1061648 1443370      NA     NA     NA     NA     NA     NA    NA
9  376686  986608      NA      NA     NA     NA     NA     NA     NA    NA
10 344014      NA      NA      NA     NA     NA     NA     NA     NA    NA
```

## Hypothesis testing

### First Mack hypothesis

Plot to show the possibility to use the development factors

```r
MackFirstHyp(triangleExampleEngland)
```

### Second Mack hypothesis

Test the independance of the residuals

```r
MackSecondHyp(triangleExampleEngland)
```

### Chain Ladder hypothesis

Test the independance of residuals regarding the calendar year.

```r
ChainLadderHyp(triangleExampleEngland)
```

## Chain Ladder

```r
outputCL <- ChainLadder(triangleExampleEngland)
outputCL
```

## Bornhuetter Fergusson

```r
ultimateClaims <- c(3901463,5433719,5378826,5297906,4858200,5111171,5660771,6784799,5642266,4969825)
outputBF <- BornFerg(triangleExampleEngland, ultimateClaims)
outputBF
```

## Mack 93 variance estimation

```r
error <- Mack93Variance(triangleExampleEngland)
error
```
## Stochastic evaluations

### Poisson bootstraping

```r
bcl <- BootstrapChainLadder(triangleExampleEngland, 1000)
mean(bcl$ibnr)
bcl$predictionError
```

### Mack bootstraping

```r
bm <- BootstrapMack(triangleExampleEngland, 1000)
mean(bm$ibnr)
bm$predictionError
```
