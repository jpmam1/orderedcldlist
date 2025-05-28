
# orderedcldlist

**`orderedcldlist`** is an R package for creating compact letter displays (CLDs) in a user-specified order.

## Features

- Takes the output from a Tukey HSD test, modified to include a "Comparison" column and assigns letters denoting statistical significance in a user-specified order.

## 📦 Installation

``` r
# Development version (from local source or GitHub)
# install.packages("devtools")
devtools::install_github("jpmam1/orderedcldlist")
```

## 🛠️ Usage

``` r
library(orderedcldlist)

set.seed(12345)
df <- data.frame(program = rep(c("AA", "BB", "CC", "DD"), each = 10),
                 weight_loss = c(runif(10, 0, 10),
                                 runif(10, 0, 4),
                                 runif(10, 1, 7),
                                 runif(10, 0, 3)),
                 group = factor(sample(c(0,1), replace = TRUE, size = 10)))
model = aov(weight_loss ~ program * group, data = df)
summary(model)
TUK = TukeyHSD(model, ordered = TRUE)
TUK = as.data.frame(TUK[[1]])
HSD = data.frame(Comparison=row.names(TUK),
                 diff=TUK$diff, lwr=TUK$lwr, upr=TUK$upr, p.adj=TUK$`p adj`)
HSD

desired_order2 = c("DD", "BB", "CC", "AA")
orderedcldlist(data = HSD,
               comparison = "Comparison",
               p.value = "p.adj",
               threshold = 0.05,
               desired_order = desired_order2)
```

## 📊 Example Output

``` r
#>   Group Letter MonoLetter
#> 1    DD      a        a  
#> 2    BB      a        a  
#> 3    CC      b         b 
#> 4    AA      c          c
```

## 📄 License

GPLv3

