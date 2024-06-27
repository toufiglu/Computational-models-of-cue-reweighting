calculate_luce_choice <- function(data) {
  # apply Luce's choice rule to get the probability of p for each of the two test tokens in each iteration
  r.iterations <- lapply(data$plot_data, function(list_data) {
    mean_act_p <- list_data$mean.act.p
    mean_act_b <- list_data$mean.act.b
    mean_act_p / (mean_act_p + mean_act_b)
  })
  
  # calculate the mean over the 10 iterations.
  r.mean <- sapply(1:2, function(index) {
    mean(sapply(r.iterations, function(x) x[index]))
  })
  return(r.mean)
}

extract_trajectory <- function(data) {
  d.iterations <- lapply(seq_along(data$w), function(i) {
    table <- data$w[[i]]
    Iteration = i
    lowf0 <- table["normalised.f0199", "p"] * 199 + table["normalised.vot22", "p"] * 22
    highf0 <- table["normalised.f0269", "p"] * 269 + table["normalised.vot22", "p"] * 22
    
    return(data.frame(Iteration, lowf0, highf0))
  })
  combined_df <- do.call(rbind, d.iterations)
  return(combined_df)
}


format_training_data <- function(data) {
  data <- data %>%
    left_join(f0_reference, by='f0') %>% 
    left_join(VOT_reference, by='VOT') %>% 
    mutate(
      Frequency = 1,
      Outcomes = case_when(
        VOT %in% c(-20, -10, 0) ~ "b",
        TRUE ~ "p"
      ),
      Cues = paste(paste0("normalised.vot", VOT_centered), 
                   paste0("normalised.f0", f0_centered), sep = "_"),
    ) %>% 
    dplyr::select(Cues, Outcomes, Frequency) %>% 
    slice(rep(1:n(), each = Frequency)) %>% 
    dplyr::select(-Frequency)
}

Rescorla = function (cuesOutcomes = train,
                     Lambda=1,
                     rate,
                     w) 
{   
  cues = unique(unlist(strsplit(cuesOutcomes$Cues, "_")))
  outcomes = unique(unlist(strsplit(cuesOutcomes$Outcomes, "_")))
  
  w=w
  
  preTrained=FALSE
  if (all(!is.na(w))==TRUE){preTrained=TRUE}
  if (preTrained==TRUE) {outcomes = colnames(w)}
  if (preTrained==FALSE) 
  { 
    w = matrix(0, length(cues), length(outcomes)) 
    rownames(w)=cues
    colnames(w)=outcomes
  }
  theCues = strsplit(cuesOutcomes$Cues, "_")
  theOutcomes = strsplit(cuesOutcomes$Outcomes, "_")
  
  if (preTrained==FALSE) {knownOutcomes = NULL}
  if (preTrained==TRUE) {knownOutcomes = outcomes}
  
  for (i in 1:nrow(cuesOutcomes)) {
    cs = theCues[[i]]
    ou = theOutcomes[[i]] 
    toAdd = ou[!is.element(ou, knownOutcomes)]
    knownOutcomes = c(knownOutcomes, toAdd)
    Vtotal = colSums(w[cs,ou,drop=FALSE])
    w[cs,ou] = t(t(w[cs,ou]) + (Lambda-Vtotal)*rate)
    otherou = knownOutcomes[!is.element(knownOutcomes, ou)]
    
    if (!is.null(otherou)) {
      Vtotal = colSums(w[cs,otherou,drop=FALSE])
      w[cs,otherou] = t(t(w[cs,otherou]) + (0-Vtotal)*rate)
    }
  }
  return(w)
}

Run_Rescorla <- function(exposure, w_pre, rate) 
{
  train = exposure
  train$Cues<-as.character(train$Cues)
  train$Outcomes<-as.character(train$Outcomes)
  
  # record the results for each round.
  plot_round <- list()
  w_round <- list()
  
  # The Rescorla-Wagner model has substantial randomness due to the order of presentation. Therefore, following Harmon et al., 2019's implementation, we also ran the model 1000 times and take an averaged responses. To maintain reproducible results, we set seed before each of the outer loop as well as the inner loop.
  
  for (round in 1:10) {
    w <- if (round == 1) w_pre else w_round[[round - 1]]
    act.p <- matrix(NA, numTestStim, maxRep)
    act.b <- matrix(NA, numTestStim, maxRep)
    
    set.seed(123+round)
    train<-train[sample(nrow(train)),]
    
    for (nRep in 1:maxRep)
    {
      set.seed(1000 * round + nRep)
      train<-train[sample(nrow(train)),]
      if (nRep==1) {w<-Rescorla(train,rate=rate,w=w)}
      else {w<-(w*(nRep-1)+Rescorla(train,rate=rate,w=w))/nRep}
      
      
      #activation of p and b for each test stimulus
      for (i in 1:numTestStim)
      {
        act.p[i,nRep]<-sum(w[testStim[i,],"p"])  
        act.b[i,nRep]<-sum(w[testStim[i,],"b"])   
      }
      
    }
    
    #following Harmon and colleagues, we limit activation to be between 0 and 1
    act.p[act.p<0]<-0
    act.b[act.b<0]<-0
    act.p[act.p>1]<-1
    act.b[act.b>1]<-1
    
    #Calculating the mean activation of p and b across replications
    #Putting it into the plot data
    plot_round[[round]] <- list(
      mean.act.p = apply(act.p, 1, mean),
      sd.act.p = apply(act.p, 1, sd),
      mean.act.b = apply(act.b, 1, mean),
      sd.act.b = apply(act.b, 1, sd)
    )
    
    w_round[[round]]=w
    
  }
  return(list(plot_data = plot_round, w=w_round))
}


add_condition <- function(df, name) {
  df$condition <- name
  return(df)
}

get_trajectory_list <- function(results) {
  d.trajectory <- lapply(names(results), function(name) {
    add_condition(results[[name]], name)
  })
  
  combined_df <- do.call(rbind, d.trajectory)
  return(combined_df)
}


apply_ccure <- function(x, data) {
  require(lme4)
  
  x - predict(lmer(x ~ 1 + (1 | Talker), data = data), random.only = T)
}

# format exposure data for the ideal adaptor
get_exposure_data <- function(input_data, Condition, n.subject=1) {
  input_data <- input_data %>%
    left_join(f0_reference, by='f0') %>%
    left_join(VOT_reference, by='VOT') %>% 
    mutate(Condition=Condition,
           Item.Type=Condition,
           Item.Category = case_when(
             VOT %in% c(-20, -10, 0) ~ "/b/",
             TRUE ~ "/p/")) %>%
    dplyr::select(-VOT, -f0, -f0_Mel) %>%
    rename(VOT = VOT_centered,
           f0_Mel = f0_centered) %>% 
    crossing(Subject = factor(1:n.subject)) %>%
    mutate(Subject = paste(Condition, Subject, sep = "."),
           Phase = 'training',
           x = map2(VOT, f0_Mel, ~ c("VOT" = .x, "f0_Mel" = .y))) %>% 
    mutate(ItemID=as.character(row_number()))
  
  names(input_data$x) <- NULL
  
  return(input_data)
}

# a function used to deal with sequential models
model_for_sequential <- function(representation.model) {
  representation.model <- representation.model %>% 
    mutate_at(vars(starts_with("prior_")), ~as.numeric(as.character(.x))) %>%
    filter(Item.Intended_category == "/p/") %>% 
    select(prior_kappa, prior_nu, posterior) %>%
    unnest(posterior)
}

# the following two functions will be used for processing results of the ideal adaptor and the normalization change model.
rep_processing <- function(variable) {
  data <- get(variable, envir = .GlobalEnv)
  results <- 
    data %>%
    bind_rows() %>% 
    mutate(prob.p = ifelse(Item.Intended_category =="/p/", response, 1-response),
           normalised_f0 = ifelse(Item.Intended_category =="/p/", 269, 199),
           normalised_f0 = factor(normalised_f0)) %>% 
    group_by(Condition, prior_kappa, prior_nu, Item.Intended_category, normalised_f0) %>% # normalised_f0 is essentially determined by Item.Intended_category. It is included here to make sure that the column remains, which will be used for plotting below.
    summarise(mean_prob_p = mean(prob.p), .groups = 'drop') %>% 
    rename(prob.p=mean_prob_p)
  
  return(results)
}

norm_processing <- function(variable) {
  data <- get(variable, envir = .GlobalEnv)
  results <- 
    data %>%
    bind_rows() %>% 
    mutate(prob.p = ifelse(Item.Intended_category =="/p/", response, 1-response),
           normalised_f0 = ifelse(Item.Intended_category =="/p/", 269, 199),
           normalised_f0 = factor(normalised_f0)) %>% 
    group_by(Condition, prior_kappa.normalization, Item.Intended_category, normalised_f0) %>% 
    summarise(mean_prob_p = mean(prob.p), .groups = 'drop') %>% 
    rename(prob.p=mean_prob_p)
  
  return(results)
}

# visualization function
basic_IH_result_plot <- function(data) {
  data %>%
    mutate(Levels = ifelse(normalised_f0 == 199, "Low f0", "High f0")) %>% 
    ggplot(aes(x = block, y = prob.p, group = Levels)) +
    list(
      geom_point(size = 2, aes(color = Levels)),
      scale_color_manual(values = c("Low f0" = "orange", "High f0" = "blue")),
      coord_cartesian(ylim = c(0, 1)),
      scale_y_continuous(breaks = c(0, 0.5, 1)),
      scale_x_discrete(labels= c("canonical", "neutral", "reversed")),
      xlab("Exposure condition"),
      ylab("Proportion of /p/ responses"),
      theme(
        legend.key.height= duo.panel.key,
        legend.position = "top", 
        axis.text.x = element_text(hjust=1, size = 20, angle = 90),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        legend.text = element_text (size = 20),
        legend.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 20),
        strip.text.x = element_text(size= 20, face = "bold"),
        strip.text.y = element_text(size= 20, face = "bold"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey", size = 0.5),
        panel.grid.minor = element_line(color = "lightgrey", size = 0.25))
    )
}

get_trajectory_results <- function(canonical, neutral, reversed) {
  output <- 
    rbind(bind_rows(canonical), bind_rows(neutral), bind_rows(reversed)) %>% 
    unnest_wider(x) %>%
    mutate(response = ifelse(category=="/b/", 1-response, response)) %>% 
    mutate(f0=ifelse(f0_Mel=="269", "highf0", "lowf0")) %>% 
    mutate(Iteration=case_when(str_starts(Subject, "canonical") ~ Iteration,
                               str_starts(Subject, "neutral") ~ Iteration + 10,
                               str_starts(Subject, "reversed") ~ Iteration + 20))
  
  return(output)
}

plot_response_trajectory <- function(data) {
  suppressWarnings({
    plot <- ggplot(data, aes(x = Iteration, y = response, color = f0)) +
      facet_grid(prior_kappa ~ prior_nu) +
      geom_line() +
      geom_point() +
      scale_x_discrete(labels = ifelse(data$block == "canonical", "c", "r")) +
      labs(title = paste("The trajectory of /p/ responses in IH11\n according to the prediction of the ideal adaptor model"),
           x = "iteration",
           y = "posterior probability",
           color = "f0") +
      scale_color_manual(values = c("highf0" = "blue", "lowf0" = "orange"))
    
    return(plot)
  })
}



update_normalization_and_categorize_test <- function(
    prior,
    mu_0 = first(prior_marginal_VOT_f0_stats$x_mean),
    kappa.normalization,
    exposure,
    test
) {
  
  # Get normalization parameters for exposure data
  exposure.normalization <- 
    exposure %>%
    summarise(
      x_N = length(x),
      x_mean = list(colMeans(reduce(x, rbind))),
      x_cov = list(cov(reduce(x, rbind))))
  
  # Apply normalization based on exposure to test
  mu_inferred <- 1 / 
    (kappa.normalization + exposure.normalization$x_N[[1]]) * 
    (kappa.normalization * mu_0 + exposure.normalization$x_N[[1]] * exposure.normalization$x_mean[[1]])
  
  test %<>%
    mutate(
      x_unnormalized = x,
      x = map(x, ~ .x - (mu_inferred - mu_0)))
  
  test %>%
    select(x_unnormalized, x, Item.Intended_category) %>%
    nest(x = x, Item.Intended_category = c(x, Item.Intended_category, x_unnormalized)) %>%
    mutate(posterior = list(prior)) %>%
    
    # Don't add test again since it's already in the data
    add_test_and_categorize(test = NULL)
}


get_normalization_models_for_plot <- function(
    prior_kappa.normalization = c(4, 16, 64, 256, 1024),
    d.IH.exposure,
    normalization.prior,
    type
){
  all_normalizations <- list()  # Create an empty list to store results from each iteration
  Condition = d.IH.exposure$Condition[[1]]
  
  if (RESET_MODELS || !file.exists(get_path(paste0("../models/d.IH.results.normalization_", example_label, "_", Condition, "_", type, ".rds")))){
    
    for(i in 1:10){  # Loop to iterate 10 times
      set.seed(100+i)
      d.IH.exposure<-d.IH.exposure[sample(nrow(d.IH.exposure)),]
      
      if(i == 1){
        normalization.adaptor <-  normalization.prior
      } else {
        normalization.adaptor <- all_normalizations[[i-1]]
      }
      
      
      normalization.adaptor <- normalization.adaptor %>% 
        filter(Item.Intended_category == "/p/") %>% 
        select(prior_kappa.normalization, posterior) %>% 
        rename(prior=posterior)
      
      normalization.pred <- 
        d.IH.exposure %>%
        nest(data = -c(Condition, Subject)) %>%
        crossing(prior_kappa.normalization = normalization.adaptor$prior_kappa.normalization,
                 prior = normalization.adaptor$prior) %>%
        mutate(prior_kappa.normalization = as.numeric(as.character(prior_kappa.normalization))) %>% 
        group_by(Condition, Subject, prior_kappa.normalization) %>% 
        group_modify(
          ~ update_normalization_and_categorize_test(
            prior = .x$prior[[1]],
            kappa.normalization = .y$prior_kappa.normalization,
            exposure = .x$data[[1]],
            test = d.IH.test)) %>%
        mutate_at(vars(starts_with("prior_")), ~factor(.x)) %>%
        mutate_at(vars(starts_with("prior_")), fct_rev) %>%
        ungroup()
      
      all_normalizations[[i]] <- normalization.pred
    }
    saveRDS(all_normalizations, get_path(paste0("../models/d.IH.results.normalization_", example_label, "_", Condition, "_", type, ".rds")))
  } else {
    all_normalizations <- readRDS(get_path(paste0("../models/d.IH.results.normalization_", example_label, "_", Condition, "_", type, ".rds")))
    
  }
  
  
  return(all_normalizations)
}

