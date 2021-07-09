#' Class specific functions
#'
#' @param x, an object of class ruleFit
#' @export
print.ruleFit <- function(x){

  if(!class(x) =="ruleFit"){
    stop("Argument 'x' should be of class 'ruleFit'")
  }

  cat("=========================================\n")
  cat("\n")

  cat(c("", "  _____       _      ______ _ _", " |  __ \\     | |    |  ____(_) |",
        " | |__) |   _| | ___| |__   _| |_", " |  _  / | | | |/ _ \\  __| | | __|",
        " | | \\ \\ |_| | |  __/ |    | | |_", " |_|  \\_\\__,_|_|\\___|_|    |_|\\__|",
        "", ""), sep="\n")

  cat("\n")
  cat("========== Final RuleFit model ==========\n")


  cat("\n  Number of rules :", length(grep("rule", x$varimp$features)),
      "\n  Top Rule :", as.character(x$varimp[grep("rule", x$varimp$features),"features"][[1]]),
      "\n  Prediction finished after :", x$duration[[1]], units(x$duration), "\n", sep = " ")

  cat("\n")

  cat("=========================================\n")

  cat("\n")
  cat("Top predictors with performance:\n")

  cat("\n")
  print(x$varimp)

  cat("\n")

  cat("=========================================\n")

  cat("\n")
  cat("Model call: \n")
  print(x$rulefit_call)
  cat("\n")
}

#' Predict function for varppRule
#'
#' @param test_data a test data set
#' @param model the stored kappaRule model
#' @param predict if the prediction should be probability or class
#'
#' @import dplyr
#' @import glmnet
#'
#' @export
predict.ruleFit <- function(model,
                            test_data,
                            predict=c("probability", "class")){

  rule_desc <- model$rules_to_apply

  test_2 <- test
  for(i in names(rule_desc)) {

    test_2 <- cbind(test_2, (test %>%
                               dplyr::mutate(!!names(rule_desc[i]) := ifelse(eval(parse(text=as.character(rule_desc[i]))),
                                                                      model$positive[i],
                                                                      model$alternative[i])) %>%
                               dplyr::select(!!names(rule_desc[i])))
    )


  }

  test_mat <- Matrix::as.matrix(test_2 %>% select(-{{y}}))


  predictions <- as.factor(glmnet::predict.glmnet(rf_results$RuleFit,
                                   test_mat,
                                   s="lambda.min",
                                   type="class"))

  return(predictions)

}


#====================================================================================================================

#' Function to evaluate the model
#'
#' @param test_data test data to evaluate the model on
#' @param model saved kappaRule model

#' @importFrom broom tidy
#' @importFrom caret confusionMatrix
#'
#' @export
kappaRule_metrics <- function(test_data,
                              model,
                              measure = NULL){

  results_test <- broom::tidy(caret::confusionMatrix(table(test_data[,model$y],
                                           predict.ruleFit(test_data, model))))%>%
    dplyr::select(term, estimate) %>%
    dplyr::rename(Test_Metrics = estimate) %>%
    dplyr::rename(Measure = term) %>%
    dplyr::filter(!Measure %in% "mcnemar")

  results_train <- broom::tidy(caret::confusionMatrix(table(model$RuleData[,model$y],
                                                            predict(model$RuleFit,
                                                                    Matrix::as.matrix(model$RuleData %>%
                                                                                                      dplyr::select(!!-model$y)),
                                                                    type = "class"))))%>%
    dplyr::select(term, estimate) %>%
    dplyr::rename(Train_Metrics = estimate) %>%
    dplyr::rename(Measure = term) %>%
    dplyr::filter(!Measure %in% "mcnemar")


  return(as_tibble(cbind(results_train, results_test[,"Test_Metrics"])))

}



#====================================================================================================================

#' Function to return all rules ranked by variable importance
#' @param x an object of class varppRuleFit
#' @export
varImp <- function(x){

  x$varimp

}

#====================================================================================================================

#' Return model metrics for both Random Forest and RuleFit
#' @param actual values and predicted values of the model
#' @import pROC
#' @export
.threshold <- function(actual, predicted){
  suppressMessages(suppressWarnings(
    roc_obj <- pROC::roc(actual, predicted)))

  #get the "best" "threshold"
  # there are lots of other options for other metrics as well
  suppressMessages(suppressWarnings(
    cut_off <- pROC::coords(roc_obj, "best", "threshold")$threshold))
  cut_off

}

#====================================================================================================================

#' Return model metrics for both Random Forest and RuleFit
#' @param actual values and predicted values of the model
#' @import pROC
#' @export
.class_by_threshold <- function(actual, predicted) {

  suppressMessages(suppressWarnings(
    roc_obj <- pROC::roc(actual, predicted)))

  #get the "best" "threshold"
  # there are lots of other options for other metrics as well
  suppressMessages(suppressWarnings(
    cut_off <- pROC::coords(roc_obj, "best", "threshold")$threshold))

  tab <- table(predicted = ifelse(predicted > cut_off , 1, 0), original = actual)
  if(nrow(tab)!=ncol(tab)){

    missings      <- setdiff(colnames(tab),rownames(tab))
    missing_mat   <- mat.or.vec(nr = length(missings), nc = ncol(tab))
    tab           <- as.table(rbind(as.matrix(tab), missing_mat))
    rownames(tab) <- colnames(tab)
  }

  caret::confusionMatrix(tab)
}

#====================================================================================================================

#' Return model metrics for both Random Forest and RuleFit
#' @param x an object of class \code{varppRuleFit}
#' @import pROC
#' @importFrom caret confusionMatrix
#' @export
metrics <- function(x) {
  #========================================================================================
  # Model quality measures
  # 1. Random Forest predictions
  #========================================================================================

  if(!is.null(x$RandomForest$accuracy[,3])){

    RandomForestPrediction <- .class_by_threshold(x$RandomForest$accuracy[,2], x$RandomForest$accuracy[,3])

    #========================================================================================
    # 2. LASSO
    #========================================================================================

    RuleFitPrediction <-  .class_by_threshold(x$RuleFit$accuracy[,2], x$RuleFit$accuracy[,3])

    list(RandomForest = RandomForestPrediction, RuleFit = RuleFitPrediction)
  }else{

    RuleFitPrediction <-  .class_by_threshold(x$RuleFit$accuracy[,2], x$RuleFit$accuracy[,3])

    list(RuleFit=RuleFitPrediction)


  }
}


#====================================================================================================================

#' Return a table with model names, auPRC, PP100 and ntree of the model: only for two level bootstrap model
#' @param x an object of class \code{varppRuleFit}
#' @import precrec
#' @export
performance <- function(x, ntree=x$ntree){
  results_varpp <-
    evalmod(
      mmdata(scores=join_scores(x$RuleFit$accuracy$CADD_expression[!is.na(x$RuleFit$accuracy$CADD_expression)],
                                x$RuleFit$accuracy$CADD_raw_rankscore[!is.na(x$RuleFit$accuracy$CADD_raw_rankscore)],
                                chklen=FALSE),

             join_labels(x$RuleFit$accuracy$Pathogenic[!is.na(x$RuleFit$accuracy$CADD_expression)],
                         x$RuleFit$accuracy$Pathogenic[!is.na(x$RuleFit$accuracy$CADD_raw_rankscore)],
                         chklen=FALSE),
             modnames=c("RuleFit","CADD_raw_rankscore"),
             dsids=1:2)
    )


  model_results <-     auc(results_varpp) %>%
    filter(curvetypes %in% "PRC") %>%
    select(-c(dsids,curvetypes)) %>%
    mutate(PP100 = c(sum(x$RuleFit$accuracy[order(-(x$RuleFit$accuracy$CADD_expression)), ][1:100, "Pathogenic"])/100,
                     sum(x$RuleFit$accuracy[order(-(x$RuleFit$accuracy$CADD_raw_rankscore)), ][1:100, "Pathogenic"])/100)) %>%
    mutate(ntree = ntree) %>%
    rename(auPRC = aucs)

  model_results
}


#====================================================================================================================

#' Calculate kappa statistic
#' @param cross_table the confusion Matrix of predictions and actual data
#' @import precrec
#' @export
kappa_stats <- function(cross_table){

  diagonal.counts    <- diag(cross_table)
  N                  <- sum(cross_table)
  row.marginal.props <- rowSums(cross_table)/N
  col.marginal.props <- colSums(cross_table)/N

  # Compute kappa (k)
  Po <- sum(diagonal.counts)/N
  Pe <- sum(row.marginal.props*col.marginal.props)
  k <- (Po - Pe)/(1 - Pe)
  k
}

#====================================================================================================================


#' Prediction of single rules
#'
#' @param rulename is the name of one of the rules as returned by the varppRuleFit model
#' @param rulefit_results_object, a varppRuleFit object
#'
#' @export
selected_rule_performance <- function(rulename,
                                      rulefit_results_object
){

  data           = rulefit_results_object$RuleData
  predictions    = rulefit_results_object$RuleFit$accuracy[,1]
  predicted_data = subset(data, data$GeneVariant %in% predictions)

  tab <- table(rule = predicted_data[,rulename], Pathogenic=predicted_data[,"Pathogenic"])

  if(nrow(tab)!=ncol(tab)){

    missings      <- setdiff(colnames(tab),rownames(tab))
    missing_mat   <- mat.or.vec(nr = length(missings), nc = ncol(tab))
    tab           <- as.table(rbind(as.matrix(tab), missing_mat))
    rownames(tab) <- colnames(tab)
  }

  caret::confusionMatrix(tab)

}

#====================================================================================================================


#' Extract rules from ranger trees
#'
#' This function returns rules based on the decision trees built in ranger. It depends on the function varpp
#'
#' @param rf_results the results fro mthe ranger tree generation within the varpp function
#'
#' @return a named vector of rules
#' @import tidypredict
#' @import stringr
#' @export
.extract_ranger_rules <- function(rf_results){

  RULES <- gsub('\n', '',gsub('\"', '', tidypredict::tidypredict_fit(rf_results), ','))
  RULES <- gsub('case_when\\(','',RULES)
  RULES <- gsub('\\)','',RULES)


  rules <- unlist(strsplit(as.character(unlist(RULES)), ','))
  rules <- gsub("\\s+", " ", rules) # This means one or more spaces
  rules <- gsub('~ 1', '~ "1"', rules)
  rules <- gsub('~ 0', '~ "0"', rules)
  rules <- stringr::str_trim(rules)
  rules
}

#====================================================================================================================


#' Simulate data to test the ruleFit function
#'
#' @param n the number of instances
#' @param p the number of variables
#' @param seed the seed, to create reproducible results
#'
#' @return a simulated data set with a binary outcome variable and 35 predictors that are associated with the outcome
#
#' @export
simulate_data <- function(n=1000,
                          p = 100,
                          seed = 777){

  n = n
  p = p
  #no influence : 65

  #xij ∼ U(0, 1)
  set.seed(seed)
  x <- NULL
  for( i in 1:p){

    x <- cbind(x,runif(n, min = 0, max = 1))

  }

  x <- data.frame(x)
  names(x) <- paste0("x", 1:dim(x)[2])

  # Create noise variable
  e <- rnorm(n, mean = 0, sd =2)

  y <-  10*(exp(x[,1]^2) * exp(x[,2]^2) * exp(x[,3]^2) * exp(x[,4]^2) * exp(x[,5]^2)) + rowSums(x[,6:35] + e)

  y_bin <- ifelse(y > median(y), 1, 0)


  x$y <- y_bin
  x$index <- c(1:dim(x)[1])
  x <- x[,c("index", "y", names(x)[grep("x", names(x))])]
  x$index.1 <- NULL

  x
}


#====================================================================================================================

