## ggjiterbump

Special thanks to:

[ggbump (GitHub)](https://github.com/davidsjoberg/ggbump/tree/master)

### Example

Install:

```r
devtools::install_github("abikoushi/ggjitterbump")
```

Example:

```r
library(ggjitterbump)
library(tidyr)
library(dplyr)

dfiris <- mutate(iris, id=row_number()) %>%
  pivot_longer(1:4, names_to = "variable")

ggplot(dfiris, aes(x = variable, y = log(value), group = id, colour=Species)) +
  geom_jitterbump(linewidth = 0.1)+
  geom_point(alpha = 0.1)+
  guides(colour = guide_legend(override.aes = list(alpha=0.7, linewidth = 0.8)))+
  theme_bw(16)
```

![](https://github.com/abikoushi/ggjitterbump/blob/main/example/iris_bump.png)
