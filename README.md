# `kappaRule`: A parallel implementation of the RuleFit algorithm by Friedman and Popescue

# Installation

```r
devtools::install_github("Hobbeist/ruleFit")
```

# How to

As an example, we use the `PimaIndianDiabetes` dataset from the `mlbench` package.

## Get the `mlbench` package

```r
install.packages("mlbench")
library(mlbench)

data("PimaIndianDiabetes")
```

## Prepare the data

```r
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

```r
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

```r
=========================================


  _____       _      ______ _ _
 |  __ \     | |    |  ____(_) |
 | |__) |   _| | ___| |__   _| |_
 |  _  / | | | |/ _ \  __| | | __|
 | | \ \ |_| | |  __/ |    | | |_
 |_|  \_\__,_|_|\___|_|    |_|\__|



========== Final RuleFit model ==========

  Number of rules : 5 
  Top Rule : rule_1240 
  Prediction finished after : 5.650231 mins 

=========================================

Top predictors with performance:

    features        coefs                                         rules
1  rule_1240  0.571078463  pedigree >= 0.211 & age < 59.5 & age >= 24.5
2  rule_9001  0.509554892 mass >= 26.7 & age >= 28.5 & pedigree < 1.178
3   pedigree  0.359358593                                          <NA>
4  rule_2543  0.266639832   insulin < 210 & age >= 26.5 & mass >= 26.35
5  rule_9354  0.263752536    age < 59.5 & pregnant < 13.5 & age >= 28.5
6  rule_1825  0.123884775      age >= 32.5 & age >= 29.5 & mass >= 26.3
7       mass  0.051551334                                          <NA>
8    glucose  0.031630574                                          <NA>
9   pressure -0.009338871                                          <NA>
10  pregnant  0.008273054                                          <NA>

=========================================

Model call: 
kappaRule(data = train, y = "diabetes", ntree = 2000, max.depth = 3, 
    rule.filter = 10, rule.extract.cores = 64, kappa.cores = 64)

```


## Predict

```r
predict.ruleFit(test_data = test, 
                model = rf_results,
                predict = "class")
```

Results:

```r
[1] 0 1 0 0 0 0 1 1 1 0 0 1 0 0 0 0 0 0 0 0 1 0 1 0 0 0 1 0 0 1 0 0 0 0 0 0 0 1 0 0 1 0 1 1 0 1 0 0 1 1 1 1 1 0 0 1 0 1 0 0 0 0 0 0 0 1 1 1 0
 [70] 1 1 1 0 1 0 0 0 1 0 0 1 0 0 0 0 0 0 1 0 1 1 0 0 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 1 0 0 0 0 0 0 1 0 0 0 0 1 0 1 0 1 0 0 1 0 0 0 1 1
[139] 0 1 1 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 0 1 0 1 0 0 1 1 0 1 1 1 0 0 1 1 0 0 0 0 0 1 1 0 0 0 1 1 1 0 0 0 0
Levels: 0 1
```


## Evaluate

```r
kappaRule_metrics(test_data = test, 
                  model = rf_results)
```

Output:

```r
# A tibble: 13 x 3
   Measure              Train_Metrics Test_Metrics
   <chr>                        <dbl>        <dbl>
 1 accuracy                     0.780        0.766
 2 kappa                        0.467        0.490
 3 sensitivity                  0.783        0.802
 4 specificity                  0.769        0.697
 5 pos_pred_value               0.921        0.835
 6 neg_pred_value               0.508        0.648
 7 precision                    0.921        0.835
 8 recall                       0.783        0.802
 9 f1                           0.846        0.818
10 prevalence                   0.774        0.656
11 detection_rate               0.606        0.526
12 detection_prevalence         0.658        0.630
13 balanced_accuracy            0.776        0.749
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
