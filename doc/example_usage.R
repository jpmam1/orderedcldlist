## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
devtools::install()

## ----setup--------------------------------------------------------------------
#> This function takes the output of TukeyHSD post-hoc test, modified to include
#> a "Comparison" column, e.g.

## ----example------------------------------------------------------------------
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


desired_order = c("BB", "AA", "CC", "DD")
orderedcldlist(data = HSD,
               comparison = "Comparison",
               p.value = "p.adj",
               threshold = 0.05,
               desired_order = desired_order)

desired_order = c("DD", "BB", "CC", "AA")
orderedcldlist(data = HSD,
               comparison = "Comparison",
               p.value = "p.adj",
               threshold = 0.05,
               desired_order = desired_order)


