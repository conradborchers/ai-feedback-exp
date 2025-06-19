get_student_params <- function(m_rasch, param_name='theta_s1') {
  if (is.na(m_rasch)) {
    return(NA)
  }
  res <- ranef(m_rasch)$student %>%
    rownames_to_column('student') %>%
    tibble() %>%
    janitor::clean_names() %>%
    rename("{param_name}" := intercept)
  return(res)
}

split_items_in_half <- function(d_irt) {
  items <- d_irt$item %>% unique()
  n_items <- length(items)
  item_indices <- sample(1:n_items, n_items, replace=FALSE)
  half1_indices <- item_indices[1:floor(n_items / 2)]
  half2_indices <- item_indices[(floor(n_items / 2) + 1):n_items]
  half1_items <- items[half1_indices]
  half2_items <- items[half2_indices]
  return(list(half1_items=half1_items, half2_items=half2_items))
}


split_items_in_half_random <- function(d_irt, max_combs = 1000) {
  items <- unique(d_irt$item)
  n_items <- length(items)
  half_size <- floor(n_items / 2)
  splits <- list()
  
  seen_splits <- new.env(hash = TRUE)  # Store seen splits for uniqueness
  
  for (i in 1:max_combs) {
    repeat {
      half1_items <- sample(items, half_size)
      half2_items <- setdiff(items, half1_items)
      key <- paste(sort(half1_items), collapse = "-")  # Unique key for the split
      
      # Ensure uniqueness
      if (!exists(key, envir = seen_splits)) {
        assign(key, TRUE, envir = seen_splits)
        splits[[length(splits) + 1]] <- list(half1_items = half1_items, half2_items = half2_items)
        break
      }
    }
  }
  
  return(splits)
}

fit_rasch <- function(d_irt) {
  # First solution is rank deficient as time is explained by item, needs to be nested
  #m_rasch <- lme4::glmer(response ~ item + (1 | student) + time, d_irt, family='binomial', verbose=0, nAGQ=0)
  if (length(unique(d_irt$response))==1) { # constant response
    return(NA)
  }
  #print(d_irt)
  if ((length(unique(d_irt$item))>1) & length(unique(d_irt$time))>1) {
    m_rasch <- lme4::glmer(factor(response) ~ (1 | time:item) + (1 | student), d_irt, family='binomial', verbose=0, nAGQ=0)
  } else {
    if (length(unique(d_irt$time))>1) {
      m_rasch <- lme4::glmer(factor(response) ~ (1 | time) + (1 | student), d_irt, family='binomial', verbose=0, nAGQ=0)
    } else {
      m_rasch <- lme4::glmer(factor(response) ~ (1 | student), d_irt, family='binomial', verbose=0, nAGQ=0)
    } 
  }
  #m_rasch <- lme4::glmer(response ~ item + (1 | student), d_irt, family='binomial', verbose=0, nAGQ=0)
  return(m_rasch)
}

compute_split_half <- function(d_irt, split) {
  params1 <- d_irt %>%
    filter(item %in% split$half1_items) %>%
    fit_rasch() %>%
    get_student_params(param_name='theta_s1')
  params2 <- d_irt %>%
    filter(item %in% split$half2_items) %>%
    fit_rasch() %>%
    get_student_params(param_name='theta_s2')
  if (!(is.data.frame(params1) & is.data.frame(params2))) {
    return(NA)
  }
  params <- inner_join(params1, params2, by='student')
  relia <- cor(params$theta_s1, params$theta_s2, use='pairwise.complete.obs')
  return(relia)
}

compute_split_half_reliability_n_combs <- function(d_irt, max_combs=1000) {
  splits <- split_items_in_half_random(d_irt, max_combs)
  relias <- pbsapply(splits, function(x) compute_split_half(d_irt, x))
  m <- round(mean(relias, na.rm=TRUE), 3)
  ci <- round(quantile(relias, c(0.025, 0.975), na.rm=TRUE), 3)
  ans <- paste(m, ' [', ci[1], ', ', ci[2], ']', sep='')
  return(ans)
}

run_all_splits <- function(d_irt, max_combs=100) {
  i <- 0
  res <- list()
  items <- d_irt$item %>% unique()
  print(items)
  for (remove_item in items) {
    cat(remove_item, '\n')
    res[[paste(remove_item, sep='-')]] <- suppressMessages({d_irt %>% 
        filter(item!=remove_item) %>%
        compute_split_half_reliability_n_combs(max_combs)})
  }
  
  d_relia_out <- data.frame(item = names(res), relia = unlist(res)) %>%
    tibble()
  
  join <- d_irt %>% group_by(item) %>% summarize(easyness = mean(response, na.rm=TRUE))
  
  ans <- d_relia_out %>% left_join(join, by='item') %>%
    select(item, easyness, relia)
  
  return(ans)
}
