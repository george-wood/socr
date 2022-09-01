
<!-- README.md is generated from README.Rmd. Please edit that file -->

# socr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of socr is to provide a general framework for modeling tracking
and event data in soccer.

## Installation

Install the development version of socr:

``` r
# install.packages("devtools")
devtools::install_github("george-wood/socr")
```

(Please note that installing won’t work while this repository is
private.)

## Usage

An example using the [Metrica Sports sample tracking
data](https://github.com/metrica-sports/sample-data):

``` r
library(socr)

data(metrica_tracking)

# coerce to a tracking object
trk <- as_tracking(metrica_tracking)
print(trk)
#> Key: <entity, team, time>
#>          period   team  entity    time       x       y
#>           <int> <char>  <char>   <num>   <num>   <num>
#>       1:      1   ball    ball    0.04 54.5664 30.9672
#>       2:      1   ball    ball    0.08 59.5740 32.5248
#>       3:      1   ball    ball    0.12 64.4592 34.0448
#>       4:      1   ball    ball    0.16 66.4152 33.7848
#>       5:      1   ball    ball    0.20 66.6144 32.4560
#>      ---                                              
#> 1639161:      1   home player9 2850.56 58.3908 84.0000
#> 1639162:      1   home player9 2850.60 58.3908 84.0000
#> 1639163:      1   home player9 2850.64 58.3908 84.0000
#> 1639164:      1   home player9 2850.68 58.3908 84.0000
#> 1639165:      1   home player9 2850.72 58.3908 84.0000

# coerce to a position object
p <- as_position(trk[time == 10])
print(p, n = 5)
#> <position[23]>
#> t: 10.00 e: ball <ball> xy: 41.0, 78.0    
#> t: 10.00 e: player1 <home> xy: 38.1, 76.0 
#> t: 10.00 e: player10 <home> xy: 48.4, 55.6
#> t: 10.00 e: player11 <home> xy: 5.0, 40.1 
#> t: 10.00 e: player15 <away> xy: 57.6, 36.1
#> ... with 18 more positions

# calculate displacement between players
displacement(p,
             from = c("player1", "player2"),
             to = c("player1", "player2", "ball"))
#> , , 10, x
#> 
#>         player1 player2     ball
#> player1   0.000  11.034  -2.9292
#> player2 -11.034   0.000 -13.9632
#> 
#> , , 10, y
#> 
#>          player1 player2     ball
#> player1   0.0000 14.3096  -2.0648
#> player2 -14.3096  0.0000 -16.3744
#> 
#> , , 10, xy
#> 
#>          player1  player2      ball
#> player1  0.00000 18.06969  3.583798
#> player2 18.06969  0.00000 21.519571
```
