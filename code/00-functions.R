# UTILS: some functions used in this project

get_marfx_lm <- function(model, model_dataset, type = "likelihood",
                         cat_variables = NULL, model_formula = NULL) {
  library(MASS)
  library(nnet)
  library(dplyr)
  
  # - pulling key coefficients and data
  pe <- coef(model) # point estimates
  vc <- vcov(model)
  se <- sqrt(diag(vc))
  
  # - simulate Betas
  sims <- 10000
  simbetas <- MASS::mvrnorm(sims, pe, vc) 
  
  # - initializing the results dataset
  marf_res <- NULL
  
  # - pulling the outcome and explanatory variables from the model formula. 
  #   Detecting any interactions and saving the names as well
  yname <- as.character(model$terms[[2]])
  predictors <- attr(model$terms, "term.labels")
  
  # - pulling the right side of the model formula to create model matrices 
  #   in the future (know which categorical variables need to be transformed to
  #   dummies)
  if (is.null(model_formula)) {
    right_side <- paste0("~",
                         strsplit(as.character(model$call), split = "~")[[2]][2])
  } else {
    right_side <- paste0("~", strsplit(as.character(model_formula), split = "~")[[3]])
  }
  # 
  # - a version of the datset only with the variables of interest (and removing
  #   any rows that have an NA for any of those variables). Pulling the actual
  #   outcome values as well.
  full_data <- na.omit(model_dataset[,c(yname, predictors)])
  X <- full_data[,predictors]
  Y <- full_data[,yname]
  
  # - create a generic scenario (all variables at their minimum --dummies-- or
  #   average --numeric-- value)
  gen_scen <- NULL
  for (j in 1:ncol(X)) {
    if (colnames(X[j]) %in% cat_variables) {
      # - if it's a factor: giving it an NA for now
      gen_scen <- c(gen_scen, NA)
    } else {
      # - if dummy variable, calculate the minimum (so 1)
      if (length(table(X[j])) > 2) {
        gen_scen <- c(gen_scen, as.matrix(mean(X[,j])))
      } else {
        # - if numeric, calculate average value
        gen_scen <- c(gen_scen, as.matrix(min(X[,j])))
      }
    }
  }
  gen_scen <- t(as.data.frame(gen_scen))
  gen_scen <- data.frame(gen_scen)
  colnames(gen_scen) <- colnames(X)
  rownames(gen_scen) <- NULL
  
  # - a list of variables for which to calculate the marginal effects in a first
  #   step: excluding factor variables and interactions
  variables <- predictors[which(!(predictors %in% 
                                    c(cat_variables)))]
  
  # - iterating through the selected predictors and calculating the marginal 
  #   effect on the outcome of going from minimum to maximum value
  #   -- NUMERIC VARIABLES --
  for (v in variables) {
    # - check if dummy variable or numeric
    if (length(table(X[,v])) > 2) {
      dummy <- FALSE
    } else {
      dummy <-TRUE
    }
    # - creating a 0 and 1 scenario for dummies, a mean and mean+1sd for numeric
    if (dummy) {
      gen_scen <- as.data.frame(gen_scen)
      scen0 <- scen1 <- gen_scen
      scen1[,v] <- max(X[,v])
    } else {
      gen_scen <- as.data.frame(gen_scen)
      scen0 <- scen1 <- gen_scen
      scen1[,v] <- mean(X[,v]) + sd(X[,v])
    }
    
    # - transforming the scenario to model.matrix format (transforming 
    #   categorical variables into dummies)
    for (cat_v in cat_variables) {
      scen0[,cat_v] <- factor(X[,cat_v][1], 
                              levels = levels(X[,cat_v]))
      scen1[,cat_v] <- scen0[,cat_v]
    }
    
    for (var in variables) {
      if (!(var %in% cat_variables)) {
        scen0[,var] <- as.numeric(as.character(scen0[,var]))
        scen1[,var] <- as.numeric(as.character(scen1[,var]))
      }
    }
    
    mmatrix0 <- model.matrix(formula(right_side), scen0)
    mmatrix1 <- model.matrix(formula(right_side), scen1)
    
    # - N predicted values for each scenario, and then taking the average 
    #   likelihood or the average differendce, and 95% CIs.
    
    yhats0 <- mmatrix0 %*% t(as.matrix(simbetas))
    yhats1 <- mmatrix1 %*% t(as.matrix(simbetas))
    
    if (type == "likelihood") {
      pred_values <- yhats1 / yhats0
    } else if (type == "difference") {
      pred_values <- yhats1 - yhats0
    }
    
    pe <- round(mean(pred_values), 4)
    lwr <- round(quantile(pred_values, probs = 0.025), 4)
    upr <- round(quantile(pred_values, probs = 0.975), 4)
    
    # - adding the marginal effect for this variable to the results dataset
    new_row <- data.frame(v, pe, lwr, upr)
    marf_res <- rbind(marf_res, new_row)
  }
  
  #   -- CATEGORICAL VARIABLES --
  for (v in cat_variables) {
    # - pull the reference class for this cat var
    full_data[,v] <- as.factor(full_data[,v])
    ref_class <- factor(levels(full_data[,v])[1], 
                        levels = levels(full_data[,v]))
    scen0 <- gen_scen
    
    # - set up 'random' values for the other categorical variables
    for (cat_v in cat_variables) {
      if (cat_v != v) {
        scen0[,cat_v] <- factor(X[,cat_v][1], 
                                levels = as.character(unique(X[,cat_v])))
      }
    }
    scen0[,v] <- ref_class
    
    # - calculate predictions for baseline ref class
    mmatrix0 <- model.matrix(formula(right_side), scen0)
    yhats0 <- mmatrix0 %*% t(as.matrix(simbetas))
    
    # - calculate now Pr and marginal effect for the other levels
    other_levels <- levels(full_data[,v])[2:length(levels(full_data[,v]))]
    for (lev in other_levels) {
      scen1 <- scen0
      scen1[,v] <- factor(lev, levels = levels(scen0[,v]))
      mmatrix1 <- model.matrix(formula(right_side), scen1)
      yhats1 <- mmatrix1 %*% t(as.matrix(simbetas))
      
      if (type == "likelihood") {
        pred_values <- yhats1 / yhats0
      } else if (type == "difference") {
        pred_values <- yhats1 - yhats0
      }
      
      pe <- round(mean(pred_values), 4)
      lwr <- round(quantile(pred_values, probs = 0.025), 4)
      upr <- round(quantile(pred_values, probs = 0.975), 4)
      
      # - adding the marginal effect for this variable to the results dataset
      new_row <- data.frame(v = paste0(v, ":", lev), pe, lwr, upr)
      marf_res <- rbind(marf_res, new_row)
    }
  }
  return(marf_res)
}


get_marfx_logistic <- function(model, model_dataset, type = "likelihood",
                               cat_variables = NULL) {
  library(MASS)
  library(nnet)
  library(dplyr)
  
  # - pulling key coefficients and data
  pe <- coef(model) # point estimates
  vc <- vcov(model)
  se <- sqrt(diag(vc))
  
  # - simulate Betas
  sims <- 1000
  simbetas <- MASS::mvrnorm(sims, pe, vc) 
  
  # - initializing the results dataset
  marf_res <- NULL
  
  # - pulling the outcome and explanatory variables from the model formula. 
  #   Detecting any interactions and saving the names as well
  yname <- strsplit(as.character(model$formula), split = "~")[[2]]
  predictors <- unique(strsplit(
    strsplit(as.character(model$formula), split = "~")[[3]], 
    split = " \\+ | \\* "
  )[[1]])
  interactions_pre <- strsplit(
    strsplit(as.character(model$formula), split = "~")[[3]], 
    split = " \\+ "
  )[[1]]
  interactions <- interactions_pre[which(grepl(" * ", interactions_pre))]
  inter_variables <- strsplit(interactions, split = " \\* ")
  
  # - pulling the right side of the model formula to create model matrices 
  #   in the future (know which categorical variables need to be transformed to
  #   dummies)
  right_side <- paste0("~",
                       strsplit(as.character(model$formula), split = "~")[[3]])
  
  # - a version of the datset only with the variables of interest (and removing
  #   any rows that have an NA for any of those variables). Pulling the actual
  #   outcome values as well.
  full_data <- na.omit(model_dataset[,c(yname, predictors)])
  X <- full_data[,predictors]
  Y <- full_data[,yname]
  
  # - create a generic scenario (all variables at their average value)
  # - create a generic scenario (all variables at their minimum --dummies-- or
  #   average --numeric-- value)
  gen_scen <- NULL
  for (j in 1:ncol(X)) {
    if (colnames(X[j]) %in% cat_variables) {
      # - if it's a factor: giving it an NA for now
      gen_scen <- c(gen_scen, NA)
    } else {
      # - if dummy variable, calculate the minimum (so 1)
      if (length(table(X[j])) > 2) {
        gen_scen <- c(gen_scen, as.matrix(mean(X[,j])))
      } else {
        # - if numeric, calculate average value
        gen_scen <- c(gen_scen, as.matrix(min(X[,j])))
      }
    }
  }
  gen_scen <- t(as.data.frame(gen_scen))
  gen_scen <- data.frame(gen_scen)
  colnames(gen_scen) <- colnames(X)
  rownames(gen_scen) <- NULL
  
  # - a list of variables for which to calculate the marginal effects in a first
  #   step: excluding factor variables and interactions
  variables <- predictors[which(!(predictors %in% 
                                    c(cat_variables)))]
  
  # - iterating through the selected predictors and calculating the marginal 
  #   effect on the outcome of going from minimum to maximum value
  #   -- NUMERIC VARIABLES --
  for (v in variables) {
    # - check if dummy variable or numeric
    if (length(table(X[,v])) > 2) {
      dummy <- FALSE
    } else {
      dummy <-TRUE
    }
    # - creating a 0 and 1 scenario for dummies, a mean and mean+1sd for numeric
    if (dummy) {
      gen_scen <- as.data.frame(gen_scen)
      scen0 <- scen1 <- gen_scen
      scen1[,v] <- max(X[,v])
    } else {
      gen_scen <- as.data.frame(gen_scen)
      scen0 <- scen1 <- gen_scen
      scen1[,v] <- mean(X[,v]) + sd(X[,v])
    }
    
    # - transforming the scenario to model.matrix format (transforming 
    #   categorical variables into dummies)
    for (cat_v in cat_variables) {
      scen0[,cat_v] <- factor(X[,cat_v][1], 
                              levels = levels(X[,cat_v]))
      scen1[,cat_v] <- scen0[,cat_v]
    }
    
    for (var in variables) {
      if (!(var %in% cat_variables)) {
        scen0[,var] <- as.numeric(as.character(scen0[,var]))
        scen1[,var] <- as.numeric(as.character(scen1[,var]))
      }
    }
    
    mmatrix0 <- model.matrix(formula(right_side), scen0)
    mmatrix1 <- model.matrix(formula(right_side), scen1)
    
    # - N predicted values for each scenario, and then taking the average 
    #   likelihood or the average differendce, and 95% CIs.
    
    yhats0 <- exp(mmatrix0 %*% t(as.matrix(simbetas)))
    yhats1 <- exp(mmatrix1 %*% t(as.matrix(simbetas)))
    
    if (type == "likelihood") {
      pred_values <- yhats1 / yhats0
    } else if (type == "difference") {
      pred_values <- yhats1 - yhats0
    }
    
    pe <- round(mean(pred_values), 4)
    lwr <- round(quantile(pred_values, probs = 0.025), 4)
    upr <- round(quantile(pred_values, probs = 0.975), 4)
    
    # - adding the marginal effect for this variable to the results dataset
    new_row <- data.frame(v, pe, lwr, upr)
    marf_res <- rbind(marf_res, new_row)
  }
  
  #   -- CATEGORICAL VARIABLES --
  for (v in cat_variables) {
    # - pull the reference class for this cat var
    full_data[,v] <- as.factor(full_data[,v])
    ref_class <- factor(levels(full_data[,v])[1], 
                        levels = levels(full_data[,v]))
    scen0 <- gen_scen
    
    # - set up 'random' values for the other categorical variables
    for (cat_v in cat_variables) {
      if (cat_v != v) {
        scen0[,cat_v] <- factor(X[,cat_v][1], 
                                levels = as.character(unique(X[,cat_v])))
      }
    }
    scen0[,v] <- ref_class
    
    # - calculate Pr baseline ref class
    mmatrix0 <- model.matrix(formula(right_side), scen0)
    yhats0 <- exp(mmatrix0 %*% t(as.matrix(simbetas)))
    
    # - calculate now Pr and marginal effect for the other levels
    other_levels <- levels(full_data[,v])[2:length(levels(full_data[,v]))]
    for (lev in other_levels) {
      scen1 <- scen0
      scen1[,v] <- factor(lev, levels = levels(scen0[,v]))
      mmatrix1 <- model.matrix(formula(right_side), scen1)
      yhats1 <- exp(mmatrix1 %*% t(as.matrix(simbetas)))
      
      if (type == "likelihood") {
        pred_values <- yhats1 / yhats0
      } else if (type == "difference") {
        pred_values <- yhats1 - yhats0
      }
      
      pe <- round(mean(pred_values), 4)
      lwr <- round(quantile(pred_values, probs = 0.025), 4)
      upr <- round(quantile(pred_values, probs = 0.975), 4)
      
      # - adding the marginal effect for this variable to the results dataset
      new_row <- data.frame(v = paste0(v, ":", lev), pe, lwr, upr)
      marf_res <- rbind(marf_res, new_row)
    }
  }
  
  # - finally, calculating the marginal effects for interacted variables
  genscen <- scen0
  
  for (inter in inter_variables) {
    if ("MRef" %in% inter) {
      inter <- c("Majority", "MRef")
    }
    v1 <- inter[1]
    v2 <- inter[2]
    # - for each value of the first dummy
    for (i in 0:1) {
      scen_i <- genscen
      # - calculate marginal effect of the second dummy going form 0 to 1
      scen_i0 <- scen_i1 <- scen_i
      scen_i0[,v1] <- scen_i1[,v1] <-  i
      scen_i1[,v2] <- 1
      
      mmatrix_i0 <- model.matrix(formula(right_side), scen_i0)
      mmatrix_i1 <- model.matrix(formula(right_side), scen_i1)      
      
      yhats0 <- exp(mmatrix_i0 %*% t(as.matrix(simbetas)))
      yhats1 <- exp(mmatrix_i1 %*% t(as.matrix(simbetas)))
      
      if (type == "likelihood") {
        pred_values <- yhats1 / yhats0
      } else if (type == "difference") {
        pred_values <- yhats1 - yhats0
      }
      
      pe <- round(mean(pred_values), 4)
      lwr <- round(quantile(pred_values, probs = 0.025), 4)
      upr <- round(quantile(pred_values, probs = 0.975), 4)
      
      # - adding the marginal effect for this variable to the results dataset
      new_row <- data.frame(v = paste0(v1, i, " (", v2, ")"), pe, lwr, upr)
      marf_res <- rbind(marf_res, new_row)
    }
    
  }
  return(marf_res)
}


