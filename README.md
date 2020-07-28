# shapvis

![R-CMD-check](https://github.com/6chaoran/shapvis/workflows/R-CMD-check/badge.svg)

visualization functions for shap values from h2o package `h2o.predict_contributions`


## Installation

```r
install.packages('remotes')
remotes::install_github('6chaoran/shapvis')
```

## Usage

`shap_values_mtcars` is the resulting data.frame from `h2o.predict_contribution` 
using `mtcars` as input data.

```r
library(shapvis)

# variable importance
h2o.shap.varimp(shap_values_mtcars)
```
|variable | shap.value.mean| top_n|
|:--------|---------------:|-----:|
|wt       |       2.1189705|     1|
|cyl      |       1.3284089|     2|
|disp     |       1.1661675|     3|
|hp       |       0.7075942|     4|
|qsec     |       0.3712314|     5|
|drat     |       0.1967453|     6|
|carb     |       0.0928105|     7|
|gear     |       0.0845765|     8|
|am       |       0.0078222|     9|
|vs       |       0.0073782|    10|

```r
h2o.shap.varimp_plot(shap_values_mtcars, mtcars, plot_type = 'bar')
```
![](https://raw.githubusercontent.com/6chaoran/shapvis/master/images/varimp_plot_bar.png)

```r
h2o.shap.varimp_plot(shap_values_mtcars, mtcars, plot_type = 'dot')
```
![](https://raw.githubusercontent.com/6chaoran/shapvis/master/images/varimp_plot_dot.png)

```r
# pdp plots
plots <- list()
for(i in c('cyl','disp','hp','wt')){
 log <- ifelse(i == 'disp', TRUE, FALSE)
 plots[[i]] <- h2o.shap.dependency_plot(shap_values_mtcars, mtcars, i, log)
}
ggpubr::ggarrange(plotlist = plots, ncol = 2, nrow = 2)
```
![](https://raw.githubusercontent.com/6chaoran/shapvis/master/images/dependency_plot.png)

```r
# waterfall plot
h2o.shap.waterfall_plot(shap_values_mtcars, X = mtcars, idx = 1, type = 'bar', max.display = 5)
```
![](https://raw.githubusercontent.com/6chaoran/shapvis/master/images/waterfall_plot_bar.png)

```r
# added new arrow chart
h2o.shap.waterfall_plot(shap_values_mtcars, X = mtcars, idx = 1, type = 'arrow')
```
![](https://raw.githubusercontent.com/6chaoran/shapvis/master/images/waterfall_plot_arrow.png)


## Shap values generation

from `h2o.predict_contributions`

```r
h2o.init() 
hex <- as.h2o(mtcars) 
model <- h2o.gbm(x = setdiff(colnames(mtcars), 'mpg'), 
                 y = mpg',
                 min_rows = 3, 
                 training_frame = hex, 
                 nfolds = 3) 
shap_values_mtcars <- h2o.predict_contributions(model, hex)
```

### Shap values example

original dataset - `mtcars`:

|                  |  mpg| cyl| disp|  hp| drat|    wt|  qsec| vs| am| gear| carb|
|:-----------------|----:|---:|----:|---:|----:|-----:|-----:|--:|--:|----:|----:|
|Mazda RX4         | 21.0|   6|  160| 110| 3.90| 2.620| 16.46|  0|  1|    4|    4|
|Mazda RX4 Wag     | 21.0|   6|  160| 110| 3.90| 2.875| 17.02|  0|  1|    4|    4|
|Datsun 710        | 22.8|   4|  108|  93| 3.85| 2.320| 18.61|  1|  1|    4|    1|
|Hornet 4 Drive    | 21.4|   6|  258| 110| 3.08| 3.215| 19.44|  1|  0|    3|    1|
|Hornet Sportabout | 18.7|   8|  360| 175| 3.15| 3.440| 17.02|  0|  0|    3|    2|
|Valiant           | 18.1|   6|  225| 105| 2.76| 3.460| 20.22|  1|  0|    3|    1|

shap values - `shap_values_mtcars`:

|   cyl|  disp|   hp|  drat|    wt|  qsec|    vs|    am|  gear|  carb| BiasTerm|
|-----:|-----:|----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|--------:|
|  1.57| -0.46| 0.15|  0.21| -0.52|  0.02| -0.02|  0.01| -0.04| -0.08|    20.09|
|  1.43| -0.47| 0.19|  0.22| -0.44|  0.22| -0.02|  0.01| -0.04| -0.09|    20.09|
|  2.06| -0.34| 1.69|  0.07| -0.42| -0.45|  0.01|  0.01| -0.06|  0.05|    20.09|
|  1.49| -0.40| 0.14|  0.06| -0.31|  0.20|  0.02|  0.00| -0.03|  0.11|    20.09|
| -0.85| -0.21| 0.20|  0.14| -1.59|  0.79|  0.00| -0.01| -0.04|  0.19|    20.09|
|  1.22| -0.46| 0.01| -0.33| -2.35| -0.16|  0.01|  0.00| -0.02|  0.06|    20.09|
