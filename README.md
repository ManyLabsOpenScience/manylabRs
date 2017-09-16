
```{r,echo=FALSE}
library(plyr)
library(broom)
```

## Install the package

Several ways to install the package.

### Source from GitHub

Use the code below to install the `manylabRs` package directly from [GitHub](https://github.com/ManyLabsOpenScience/manylabRs).

```{r, eval=FALSE}
library(devtools)
install_github("ManyLabsOpenScience/manylabRs")
```

### Download tarball from GitHub

First [download the tarball](https://github.com/ManyLabsOpenScience/manylabRs/pkg/), then install the package locally through the RStudio package installer: `Tools` >> `Install Packages...`


## Main function

The main function to inspect is `get.analyses()`.  

It will take one or more take analysis (`studies`) from the `masteRkey` sheet and an indication of whether the analysis is:
1. `global` - will disregard the clusters in the data and use all valid caes for analyses, both `primary` and `secondary` analyses have a `global` variant.
2. `primary`- target analysis of replication study conducted for each lab seperately.
3. `secondary` - additional analyses conducted for each lab seperately.
4. `order` - presentation order analyses disregard the clusters int he data, each order is analysed seperately

> Have a look at [`saveConsole.R`](https://github.com/ManyLabsOpenScience/manylabRs/blob/master/inst/saveConsole.R) which calls the `testScript()` function and creates a log file with lots of info about the analysis steps.


The example below runs a global analysis for `Huang.1`
```{r}
library(manylabRs)
library(tidyverse)

df <- get.analyses(studies = 1, analysis.type = 1)
```

The object `df` contains two named lists:^[the names correspond to the analysis name in the `masteRkey`]

### `raw.case`

This list contains dataframes with the relevant variables for each analysis, but before the analysis specific variable functions (`varfun`) are applied. There is a Boolean variable `case.include` which indicates whther a case is valid and should be included for analysis.

```{r}
df$raw.case$Huang.1
```

### `aggregated`    

The dataframe in `aggregated` contains the data as is was analysed, after the `varfun` is applied.

```{r}
df$aggregated$Huang.1
```



## Other algorithms

+ Get information from `masteRkey` on the analyses to run: `get.info()`
+ Get a data filter based on exclusion criteria: `get.chain()`
+ Select the appropriate variables: `get.sourceData()`
+ Apply the analysis-specific variable function: `varfun.ABC.#()`
+ Apply the analysis listed in column `stat.test` to the data
+ Organise the output `get.desriptives()`
+ Calculate confidence intervals for effect sizes `any2any()`
+ Return the ouput





## Other algorithms

+ Get information from `masteRkey` on the analyses to run: `get.info()`
+ Get a data filter based on exclusion criteria: `get.chain()`
+ Select the appropriate variables: `get.sourceData()`
+ Apply the analysis-specific variable function: `varfun.ABC.#()`
+ Apply the analysis listed in column `stat.test` to the data
+ Organise the output `get.desriptives()`
+ Calculate confidence intervals for effect sizes `any2any()`
+ Return the ouput



