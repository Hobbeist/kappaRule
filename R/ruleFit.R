#' The RuleFit function
#'
#' This is rulefit in general. When not using the VARPP framework, this function allows to perform rulefit on any data set with a
#' binary outcome.
#'
#' @param data
#' @param y
#' @param ntree number of trees to be built, defaults to 200
#' @param max.depth maximum tree depth, defaults to 3
#' @param rule.filter filter the top n rules based on kappa statistic. If NULL, the rules are filter above a kappa of 0.05
#' @param rule.extract.cores number of cores for parallel, defaults to 4. This is specifically for the varpp rule extract step (less memory hungry than the cv.glmnet step)
#' @param kappa.cores number of cores used for the rule filtering by kappa. This needs to be seperate, as it is quite memory intensive when the input + rule data is very large. Defaults to 2.
#' This defaults to TRUE
#' @return A list of predictions for the outcome. Further, a variable importance list for all rules and variables tested.
#'
#' @import doMC
#' @import doParallel
#' @import parallel
#' @import ranger
#' @import foreach
#' @import caret
#' @import glmnet
#' @import tidyverse
#' @import magrittr
#' @importFrom parallel detectCores
#' @importFrom caret trainControl train nearZeroVar
#' @import lattice
#' @import ggplot2
#' @import grid
#' @import precrec
#' @import DT
#' @import rmarkdown
#' @import knitr
#' @import pander
#' @import tidyr
#' @import tidyselect
#' @export
ruleFit <- function(data,
                     y,
                     ntree = 200,
                     max.depth = 3,
                     rule.filter = 10,
                     bootstrap.rounds = 100,
                     rule.extract.cores = 64,
                     kappa.cores = 24
                    ){


  #========================================================================================
  # Setup
  #========================================================================================
  # Record the call and return it with the results later
  CALL = match.call()

  # Time measurment
  start_time <- Sys.time()

  # # Function checklist and stop commands for missing values
  if(is.null(data))
    stop("Data is missing")

  if(is.null(y))
    stop("Outcome is missing")



  #========================================================================================
  # Step I: Random Forest and rule extraction
  #========================================================================================


    message(paste0("Extracting rules from ",ntree, " trees, with a max.depth of ", max.depth, ", using ", rule.extract.cores, " cores."))

      # Bootstrap rounds is a new addition as I try to save the dat_boot oject for
      # the lasso step; the lasso step currently does the exact same sampling as we do here, which should not be necessary.
      # There is, however, an additional element of randomness in the model when doing the sampling again later. Maybe revise?
      # VARPP and extract the rules


  outcome <- rlang::ensym(y)
  model   <- rlang::inject(ranger(!!outcome ~ .,data = data, num.trees =  ntree, max.depth = max.depth, num.threads = 64))
  rules   <- .extract_ranger_rules(model)
  names(rules) <- paste0("rule_",1:length(rules))




  #========================================================================================
  # CREATING RULE VARIABLES
  #========================================================================================

  rules_data = list()

  message("Creating rule variables...")
  rule_start_time <- Sys.time()

  # Create the values that the rules take (0 or 1, this is part of the extracted "rule sentence")
  positive    = as.vector(unlist(mclapply(rules, function(x) if(grepl('\"1\"', x) %in% TRUE) return(1) else(return(0)) )))
  alternative = as.vector(unlist(mclapply(rules, function(x) if(grepl('\"1\"', x) %in% TRUE) return(0) else(return(1)) )))

  # remove the ~ 0 and  ~1 parts of the rules to make them evaluatable
  rules = unlist(mclapply(rules, function(x) str_split(x, " ~")[[1]][1]))


    cl <- makeCluster((rule.extract.cores/2))
    registerDoParallel(cl)

    rules_data <- foreach( i = 1:length(rules)) %dopar% {
      library(tidyverse)
      data %>%
        mutate(!!rules[i] := ifelse(eval(parse(text=rules[i])), positive[i], alternative[i])) %>%
        select(!!rules[i])

    }
    stopCluster(cl)

    # Name the positive and alternative variables like the rules, so they can be applied in the predict function
    names(positive)    <- names(rules)
    names(alternative) <- names(rules)


  rule_end_time   = Sys.time()
  rule_time_taken = rule_end_time - rule_start_time


  message(paste0("Rules created after ", paste(round(rule_time_taken,2), units(rule_time_taken), sep=" ")))

  # This will combine the rules created in the above loop
  rule_dat = do.call(cbind, rules_data)
  rm(rules_data)#, positive, alternative)
  gc()

  # Remove duplicated columns
  rule_dat  <- rule_dat[!duplicated(as.list(rule_dat))]

  # Save the total number of Rules
  total_rules_without_duplicates = dim(rule_dat)[2]

  #===================================================================================
  # FILTERING RULES: ONLY 0, ONLY 1 and KAPPA filter
  #===================================================================================
  message(paste0("Rules before filtering: ", total_rules_without_duplicates))
  message("Filtering rules...")

  # Maybe do the conditional zero variance thing here?
  # Remove Rules that are only 0 or only 1 (based on the condition if all tested rules return FALSE, don't remove as ther is nothing to remove )
  if(length(which((as.numeric(unlist(lapply(rule_dat, function(x) var(x, na.rm=T)))) %in% 0) %in% FALSE)) < 1){

  rule_dat <- rule_dat[,-which(as.numeric(unlist(lapply(rule_dat, function(x) var(x)))) %in% 0)]

  }

    patho = data[,y]


  if(is.null(rule.filter)){

    rule_kappas <- unlist(parallel::mclapply(rule_dat, function(x){
      kappa_stats(table(x, patho))
    }, mc.cores = kappa.cores, mc.allow.recursive = TRUE))

    # Filtering based on KAPPA
    rule_kappas <- sort(rule_kappas)

  message(paste0("All ",dim(rule_dat)[2], " rules are used for LASSO..."))

  }else{
  if(rule.filter%%1 == 0){
      # Remove all rules with a Kappa value lower than 0.1
      rule_kappas <- unlist(parallel::mclapply(rule_dat, function(x){
        kappa_stats(table(x, patho))
        }, mc.cores = kappa.cores, mc.allow.recursive = TRUE))

      rule_kappas <- sort(rule_kappas)[(length(rule_kappas)-rule.filter+1):length(rule_kappas)]
      rule_dat <- rule_dat[,names(rule_kappas)]
      message(paste0(dim(rule_dat)[2], " rules are used for LASSO..."))
  }

      if(!rule.filter%%1 == 0){

        # Remove all rules with a Kappa value lower than rule.filter
        rule_kappas <- unlist(parallel::mclapply(rule_dat, function(x){
          kappa_stats(table(x, patho))
          }, mc.cores = kappa.cores, mc.allow.recursive = TRUE))

        rule_dat <- rule_dat[,-which(rule_kappas < rule.filter)]
        message(paste0(dim(rule_dat)[2], " rules are used for LASSO..."))

      }

    }

  rm(patho)
  gc()

  # Save the total number of Rules
  total_rules_after_var_removal = dim(rule_dat)[2]


  if(total_rules_after_var_removal == 0)
    stop("No rules had a kappa value > 0.05. Modelling stopped")

  data        <- cbind(data, rule_dat)
  data_dimensions <- dim(data$dat)

  # Remove unnecessary data
  rm(rule_dat)
  gc()

  #========================================================================================
  # Step II: glmnet
  #========================================================================================

  message("Starting LASSO...")


  Y  <- as.factor(data[,y])
  X  <- Matrix::as.matrix(data %>% select(-{{y}}))


  modelling_start_time <- Sys.time()

  cl <- makeCluster((rule.extract.cores/2))

  model_glmnet <- cv.glmnet(X,
                            Y,
                            alpha = 1,
                            family = "binomial",
                            parallel = TRUE,
                            standardize= TRUE
                            )
  stopCluster(cl)




  modelling_end_time   = Sys.time()
  modelling_time_taken = modelling_end_time - modelling_start_time
  message(paste0("Modelling finished after ", paste(round(modelling_time_taken,2), units(modelling_time_taken), sep=" ")))


  # Predict on the data
  predictions <- as.factor(predict(model_glmnet,
                                   Matrix::as.matrix(data %>% select(-{{y}})),
                                   s="lambda.min",
                                   type="class"))


  Coefs <- coef(model_glmnet, s="lambda.min")

  Results <- data.frame(
    features = Coefs@Dimnames[[1]][ which(Coefs[,1] != 0 ) ], #intercept included
    coefs    = Coefs              [ which(Coefs[,1] != 0 ) ]  #intercept included
  )

  Results$rules <- rules[as.character(Results$features)]
  Results %>%
    arrange(desc(abs(coefs)))  %>% filter(!features %in% "(Intercept)") -> varimp



  # Calculate Kappa statistic for every rule and sort the rule slot by that
  # SELECTED_RULES, the subset of the LASSO results with "rule" in the name:
  # as.character(LASSO$varimp$Variable[grep("rule",LASSO$varimp$Variable)])

  rules_to_apply <- names(data)[grep("rule", names(data))]
  rules_to_apply <- rules[rules_to_apply]

  message("LASSO finished! Cleaning up and preparing results.")

  end_time   = Sys.time()
  time_taken = end_time - start_time


          results        <- list(RuleFit = model_glmnet,
                                  y=y,
                                 RuleData = data,
                                 varimp = varimp,
                                 duration = time_taken,
                                 ntree = ntree,
                                 maxdepth = max.depth,
                                 rulefit_call = CALL,
                                 total_rules_without_duplicates = total_rules_without_duplicates,
                                 total_rules_after_var_removal = total_rules_after_var_removal,
                                 data_dimensions = data_dimensions,
                                 rule_kappas = rule_kappas,
                                 alternative = alternative,
                                 positive = positive,
                                 rules_to_apply = rules_to_apply
                                 )


  class(results) <- "ruleFit"


  #========================================================================================
  # Return the results
  #========================================================================================

  results

}
