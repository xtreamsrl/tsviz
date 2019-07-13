# Dataset selection

The addin looks into your global environment and selects all `data.frame` which have:
 - at least one column of type Date
 - at least one column of type numeric

To get started, try:
```
library(tsviz)
prices <- crypto_prices
```
and run the addin.
