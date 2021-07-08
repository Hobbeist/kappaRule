# `kappaRule`: A parallel implementation of the RuleFit algorithm by Friedman and Popescue

# Installation

```{r}
devtools::install_github("Hobbeist/ruleFit")
```

# How to

As an example, we use the `PimaIndianDiabetes` dataset from the `mlbench` package.

## Get the `mlbench` package

```{r}
install.packages("mlbench")
library(mlbench)

data("PimaIndianDiabetes")
```

## Prepare the data

```{r}
library(rsample)

# Re-coding the outcome variable
diabetes <- PimaIndianDiabetes %>%
         mutate(diabetes = as.factor(ifelse(diabetes %in% "neg", 0, 1)))


# Train and Test set
set.seed(456)
split <- initial_split(diabetes)
train <- training(split)
test  <- testing(split)

```

## Run the `ruleFit` model

```{r}
rf_results <- 
  ruleFit(data = train,
          y = "diabetes",
          ntree = 2000,
          max.depth = 3,
          rule.filter = 10,
          rule.extract.cores = parallel::detectCores(),
          kappa.cores = parallel::detectCores()
  )

```


## `ruleFit` results object

```{r}


```


# LICENSE
The contents of this repository are distributed under the MIT license. See below for details:

```
The MIT License (MIT)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

```