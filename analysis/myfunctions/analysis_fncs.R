## Functions used to analyze the data following our Registered Report
## (see Appendix E in https://osf.io/26fbh/)


###########################################################
## Global parameters etc. to be used by other functions or script
###########################################################

# ensure target directory for saving figures exists
my_figs_path <- "myfigures/"
dir.create(my_figs_path)

# specify certain plotting parameters to be used as defaults
my_dodge_w <- .2
my_jitter_w <- .2
my_alpha <- .3
my_dotsize <- 1
my_linesize <- 1


###########################################################
## General data manipulation or convenience functions
###########################################################

# save ggplot with global specifications
my_ggsave <- function(
  fig_name,
  mywidth  = 4,
  myheight = 4,
  myplot = last_plot(),
  type = ".png",
  target_folder = my_figs_path
  ) {
  fname <- paste0(target_folder, fig_name, type)
  ggsave(filename = fname, plot = myplot, width = mywidth, height = myheight)
}

# Add nuisance variable "preced_error" (binary) which is 1 if an error was made
# on any of the preceding words in a trial, 0 otherwise:
preced_error <- function(x) {
  cumsum <- cumsum(x)
  shift_cumsum <- c(0, cumsum[1 : (length(cumsum) - 1)])
  out <- ifelse(shift_cumsum > 0, 1, 0)
  out
}


###########################################################
## Plots
###########################################################

# Plot raw data
plot_raw_data <- function(df, mytitle = NULL, excl_CONTROL = TRUE) {

  # Choose more descriptive names for factor levels:
  df$block_type <- factor(df$block_type, labels = c("arm\nmovements", "control", "leg\nmovements"))

  if (excl_CONTROL) {
    df <- df %>% filter(block_type != "control")
  }

  # compute subject means
  df_sbj <- df %>%
    group_by(subject, block_type, word_type) %>%
    summarise(prop_error = mean(error)) %>%
    mutate(block_type = factor(block_type))  # to remove "control" if not used

  # basic plot
  p <- ggplot(
    data = df_sbj,
    aes(x = block_type, y = prop_error, color = word_type, shape = word_type)
    ) +
    geom_point(
      alpha = my_alpha,
      position = position_jitterdodge(
        jitter.width = my_jitter_w, jitter.height = 0, dodge.width = my_dodge_w,
        seed = 6477
        )
      ) +
    stat_summary(
      fun.data = "mean_cl_boot", position = position_dodge(width = my_dodge_w),
      size = my_dotsize
    ) +
    xlab("Interference type") +
    ylab("Proportion of errors") +
    labs(color = "Word type", shape = "Word type") +
    guides(color = guide_legend(override.aes = list(size = .75))) +
    ylim(0, 1) +
    ggtitle(mytitle) +
    theme(plot.title = element_text(hjust = 0.5))
  # add lines between conditions if CONTROL is excluded
  if (excl_CONTROL) {
    p <- p +
      stat_summary(
        aes(x = as.numeric(block_type)),
        fun = mean, 
        geom = "line",
        position = position_dodge(width = my_dodge_w),
        size = my_linesize
        )
  }
  p
}


# A convenience function tailored to the specific models fitted here
# The input fm should be the FULL model of the output of analysis_pipe().
# See https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html
plot_mymodel <- function(fm, mytitle) {
  sjPlot::plot_model(
  fm,  # the first element is the FULL model
  transform = NULL, 
  prob.outer = .95, 
  vline.color = "gray",
  order.terms = c(1, 2, 6, 3:5),
  axis.title = "Log-Odds of error",
  # axis.lim = c(-.5, .5),
  title = mytitle,
  axis.labels = c(  # Careful: the labels are blindly applied from bottom to top!
    "Preceding error in trial",
    "Word position in trial",
    "Trial in experiment",
    "Interference-by-word type interaction",
    "Word type (arm- vs. leg-related)",
    "Interference type (arm vs. leg movements)"
    ),
  ) +
  # https://github.com/strengejacke/sjPlot/issues/440
  ylim(-.25, .5)
}


# plot the interaction effect
plot_interaction <- function(fm, mytitle) {
  plot_model(
    fm, type = "pred", terms = c("block_type", "word_type"),
    title = mytitle,
    axis.title = "Probability of error"
    )
}


###########################################################
## Analysis pipeline
###########################################################

# Create a function that streamlines data processing and model fitting:
analysis_pipe <- function(
  df,             # expects data frame in a specific format
  repBF = FALSE,  # compute replication BF?
  # if repBF=TRUE, repBF_post should be a vector of 2 values, consisting of
  # the mean and SD of the posterior for the critical interaction effect
  # based on the re-analysis of the original data
  repBF_post = NULL  
  ) {
  
  # to be used for filename
  df_name <- deparse(substitute(df))
  if (repBF) { df_name <- paste(df_name, "repBF", sep = "_") }
  # name of the file where results are saved (or have been saved if run earlier)
  file_name <- paste0("mymodels/results_brms_", df_name, ".rds")

  # If model list has been saved to disk, load it and **RETURN**,
  # otherwise fit the models
  if (file.exists(file_name)) {
    cat("The list of models", file_name, "exists on disk, so we load it...\n\n")
    return(readRDS(file_name))
  } else {
    cat("The list of models", file_name, "doesn't exist on disk, so we will fit it.",
        "\nThis will take some time. Don't let your computer go to sleep!\n\n")    
  }

  # coding scheme: contrast code factors and standardize numerical predictors
  cat("Coding scheme:\n\n")
  df$block_type <- factor(df$block_type)
  contrasts(df$block_type) <- contr.sum(2)
  colnames(contrasts(df$block_type)) <- "arm_vs_leg"
  print("df$block_type")
  print(contrasts(df$block_type))
  cat("\n")
  
  df$word_type <- factor(df$word_type)
  contrasts(df$word_type) <- contr.sum(2)
  colnames(contrasts(df$word_type)) <- "arm_vs_leg"
  print("df$word_type")
  print(contrasts(df$word_type))
  cat("\n")
  
  df$trial_exp_z <- scale(df$trial_exp)
  df$word_pos_z <- scale(df$word_pos)
  df$preced_error_z <- scale(df$preced_error)
  
  # Formula of the full model:
  formula_full <- as.formula(paste(
    "error ~",  # DV
    "1 + block_type * word_type +",  # critical manipulations and interaction
    "trial_exp_z + word_pos_z + preced_error_z +",  # nuisance predictors
    "(1 + block_type * word_type | subject) + (1 + block_type | verb)"  # maximal random structure
  ))
  cat("FULL model formula:\n\n")
  print(formula_full)
  # Formula of the null model (remove the population-level interaction)
  formula_null <- update(formula_full, ~ . - block_type : word_type)
  cat("\n\nNULL model formula:\n\n")
  print(formula_null)
  
  # Priors:
  # As default, specify weakly informative priors: N(0,sigma^2 = 4) for
  # population-level (fixed) effects
  # NB: In Stan a normal distribution is specified with sigma (*not* sigma^2), see
  # https://mc-stan.org/docs/2_18/functions-reference/normal-distribution.html
  # and
  # https://stackoverflow.com/questions/52893379/stan-in-r-standard-deviation-or-variance-in-normal-distribution

  # Print to screen the priors that can be specified for full and null models:
  cat("\nThe following priors can be specified for the FULL model:\n\n")
  print(get_prior(formula_full, df))
  cat("\nThe following priors can be specified for the NULL model:\n\n")
  print(get_prior(formula_null, df))

  # Set weakly informative priors by default
  myprior <- set_prior("normal(0, 2)", class = "b")  
  # But if repBF=T, then replace prior for critical interaction with values
  # from the posterior of our SP13 re-analysis:
  if (repBF) {
  myprior_repBF <- c(
    myprior,
    set_prior(
      paste("normal(", repBF_post[1], ",", repBF_post[2], ")", sep = ""),
      class = "b", 
      coef = "block_typearm_vs_leg:word_typearm_vs_leg"
      )
    ) 
  }

  # Print priors to screen
  cat("\nThe following priors will be used (NB: repBF = ", repBF, "):\n\n", sep = "")
  if (repBF) {
    print(myprior_repBF)
  } else {
    print(myprior)
  }

  # fit null model (without interaction):
  # NB: The prior for interaction is not used in the null model:
  bfm_null <- brm(
    formula = formula_null,
    data = df,
    prior = myprior,
    family = "bernoulli",
    iter = 20000, warmup = 2000, chains = 4,  # https://discourse.mc-stan.org/t/bayes-factor-using-brms/4469/3
    save_all_pars = TRUE  # necessary for brms::bayes_factor() later
  )
  # Model *with* interaction
  bfm_full <- update(
    bfm_null,
    formula = formula_full,
    prior = if (repBF) { myprior_repBF } else { myprior },
    )

  # pack models and data into a list and give sensible names to each object
  out <- list(bfm_full, bfm_null, df)
  names(out) <- paste(df_name, c("bfm_full", "bfm_null", "dataset"), sep = "_")

  # save list to disk:
  saveRDS(out, file = file_name)
  
  out
}


# A wrapper for our BF analysis such that the Bayes factor analysis with bridge
# sampling is only run if it is not already present on disk:
my_BF_wrapper <- function(
  model_list  # should be a list as output by analysis_pipe()
  ) {

  # to be used for filename
  model_list_name <- deparse(substitute(model_list))
  # name of the file where results are saved (or have been saved if run earlier)
  file_name <- paste0("mymodels/", model_list_name, "_BF.rds")
  print(file_name)

  # If the BF has already been saved to disk, load it, otherwise run it:
  if (file.exists(file_name)) {
    cat("The BF analysis", file_name, "exists on disk, so we load it...\n\n")
    return(readRDS(file_name))
  } else {
    cat("The BF analysis", file_name, "doesn't exist on disk, so we will run it.",
        "\nThis may take some time (30+ min).\n\n")    
  }

  # Run the BF analysis with bridge sampling
  BF <- brms::bayes_factor(model_list[[1]], model_list[[2]])
  # save it to disk
  saveRDS(BF, file = file_name)
  # return it
  BF
}


###########################################################
## Quality check / positive control (effect of serial position)
###########################################################

# In our pre-registration we wrote we would use lme4 for our positive control
# testing for an effect of serial position on recall. This function does this
# analysis using lme4, as pre-registered.

positive_control_lme4 <- function(df) {  # expects data frame in a specific format, incl control condition

  # to be used for filename
  df_name <- deparse(substitute(df))
  # name of the file where results are saved (or have been saved if run earlier)
  file_name <- paste0("mymodels/fm_lme4_positive_control_", df_name, ".rds")

  # If model list has been saved to disk, load it and **RETURN**,
  # otherwise fit the models
  if (file.exists(file_name)) {
    cat("The model", file_name, "exists on disk, so we load it...\n\n")
    return(readRDS(file_name))
  } else {
    cat("The model", file_name, "doesn't exist on disk, so we will fit it.",
        "\nThis will take some time. Don't let your computer go to sleep!\n\n")    
  }

  ## coding scheme: contrast code factors and standardize numerical predictors
  cat("Coding scheme:\n\n")
  
  # block_type: set CONTROL as the reference condition for sum contrasts
  df$block_type <- factor(df$block_type, levels = c("ARM", "LEG"))
  contrasts(df$block_type) <- contr.sum(2)
  colnames(contrasts(df$block_type)) <- "arm_vs_leg"
  # verify result
  print("df$block_type")
  print(contrasts(df$block_type))
  cat("\n")

  # sum contrasts for word type
  df$word_type <- factor(df$word_type)
  contrasts(df$word_type) <- contr.sum(2)
  colnames(contrasts(df$word_type)) <- "arm_vs_leg"
  print("df$word_type")
  print(contrasts(df$word_type))
  cat("\n")

  # scale the other predictors
  df$trial_exp_z <- scale(df$trial_exp)
  df$word_pos_z <- scale(df$word_pos)
  df$preced_error_z <- scale(df$preced_error)
  
  # Formula of the full model (as pre-registered):
  formula_full <- as.formula(paste(
    "error ~",
    "word_pos_z + trial_exp_z + preced_error_z + word_type + block_type +",
    "(1 + word_pos_z | subject) + (1 | verb)"
  ))
  cat("FULL model formula:\n\n")
  print(formula_full)

  ## fit glmer model
  fm_posit_control <- glmer(
    formula = formula_full,
    data = df,
    family = "binomial"
  )

  # save fm to disk:
  saveRDS(fm_posit_control, file = file_name)
  
fm_posit_control
}


###########################################################
## Comparison to control condition
###########################################################

# This function almost replicates the analysis_pipe() function, but it is used
# to analyze the data including the control condition (seen as the reference);
# we don't have a "null" model, only the full model
analysis_control <- function(df) {  # expects data frame in a specific format, incl control condition

  # to be used for filename
  df_name <- deparse(substitute(df))
  # name of the file where results are saved (or have been saved if run earlier)
  file_name <- paste0("mymodels/results_brms_control_", df_name, ".rds")

  # If model list has been saved to disk, load it and **RETURN**,
  # otherwise fit the models
  if (file.exists(file_name)) {
    cat("The model", file_name, "exists on disk, so we load it...\n\n")
    return(readRDS(file_name))
  } else {
    cat("The model", file_name, "doesn't exist on disk, so we will fit it.",
        "\nThis will take some time. Don't let your computer go to sleep!\n\n")    
  }

  ## coding scheme: contrast code factors and standardize numerical predictors
  cat("Coding scheme:\n\n")
  
  # block_type: set CONTROL as the reference condition for sum contrasts
  df$block_type <- factor(df$block_type, levels = c("ARM", "LEG", "CONTROL"))
  contrasts(df$block_type) <- contr.sum(3)
  colnames(contrasts(df$block_type)) <- c("arm_vs_control", "leg_vs_control")
  # verify result
  print("df$block_type")
  print(contrasts(df$block_type))
  cat("\n")

  # sum contrasts for word type
  df$word_type <- factor(df$word_type)
  contrasts(df$word_type) <- contr.sum(2)
  colnames(contrasts(df$word_type)) <- "arm_vs_leg"
  print("df$word_type")
  print(contrasts(df$word_type))
  cat("\n")

  # scale the other predictors
  df$trial_exp_z <- scale(df$trial_exp)
  df$word_pos_z <- scale(df$word_pos)
  df$preced_error_z <- scale(df$preced_error)
  
  # Formula of the full model:
  formula_full <- as.formula(paste(
    "error ~",  # DV
    "1 + block_type * word_type +",  # critical manipulations and interaction
    "trial_exp_z + word_pos_z + preced_error_z +",  # nuisance predictors
    "(1 + block_type * word_type | subject) + (1 + block_type | verb)"  # maximal random structure
  ))
  cat("FULL model formula:\n\n")
  print(formula_full)
  
  # Priors:
  # As default, specify weakly informative priors: N(0,sigma^2 = 4) for
  # population-level (fixed) effects
  # NB: In Stan a normal distribution is specified with sigma (*not* sigma^2), see
  # https://mc-stan.org/docs/2_18/functions-reference/normal-distribution.html
  # and
  # https://stackoverflow.com/questions/52893379/stan-in-r-standard-deviation-or-variance-in-normal-distribution

  # Print to screen the priors that can be specified for full model:
  cat("\nThe following priors can be specified for the FULL model:\n\n")
  print(get_prior(formula_full, df))

  # Set weakly informative priors
  myprior <- set_prior("normal(0, 2)", class = "b")  

  # Print priors to screen
  cat("\nThe following priors will be used:\n\n")
  print(myprior)

  # fit model:
  bfm_full <- brm(
    formula = formula_full,
    data = df,
    prior = myprior,
    family = "bernoulli",
    iter = 20000, warmup = 2000, chains = 4  # https://discourse.mc-stan.org/t/bayes-factor-using-brms/4469/3
  )

  # save model to disk:
  saveRDS(bfm_full, file = file_name)
  
  bfm_full
}
