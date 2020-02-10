# ROGER is an R package for the automatic classification of galaxies using their phase-space information.

# Prerequisites

This software make an extensive usage of the following packages that must be installed: ```caret, randomForest, kernlab```

You can install this packages inside an R-session with:

```R
install.packages(c('caret', 'randomForest', 'kernlab'))
``` 

# Installation

You can install the pratto package directly from your R session using the ```install_github``` function from the ```devtools``` package.

``` R
library('devtools')
install_github('MartindelosRios/ROGER')
```

# Example

``` R
# Loading the ROGER library.
library('ROGER')

# Loading the data
data('testset')

# Let's see the structure of this dataset
str(testset)

# Let's keep only with the 'r' and 'v' columns that will be used to predict, and
# save the real classification for future comparison.

cat        <- testset[, c(4,5)]
real_class <- testset$flag1

# Let's predict the proabability of being of each class using our ML
pred_prob <- get_class(cat, knn)
```
In the [Examples](https://github.com/Martindelosrios/ROGER/tree/master/Examples) section you can find more
examples!
# Authors
Héctor J. Martínez (IATE-OAC-UNC)


Martín de los Rios (ICTP-SAIFR/IFT-UNESP) <a itemprop="sameAs"  href="https://orcid.org/0000-0003-2190-2196" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"> <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>


Valeria Coenda (IATE-OAC-UNC)


Hernán Muriel (IATE-OAC-UNC)


Andrés N. Ruiz (IATE-OAC-UNC)


Cristian Vega (CCT-UNLP)


Sofía Cora (CCT-UNLP)


