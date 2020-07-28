#' @import ggplot2
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @importFrom tibble rownames_to_column
#' @importFrom glue glue
#' @importFrom rlang sym

NULL


#' h2o.shap.varimp
#'
#' calculate variable importance from shap value matrix
#'
#' @param shap_values shap value matrix from \code{h2o.predict_contribution}
#' @return data.table variable importance
#' @export
#' @examples
#' data(shap_values_mtcars)
#' h2o.shap.varimp(shap_values_mtcars)

h2o.shap.varimp <- function(shap_values){

  # extract the expected value
  expected_value <- shap_values[['BiasTerm']][1]
  # extract shap values
  shap_values <- shap_values %>% select(-'BiasTerm')
  # reshape mean of shap values
  shap_values_imp <- shap_values %>%
    mutate_all(abs) %>%
    summarise_all(mean) %>%
    pivot_longer(cols = everything(), names_to = 'variable', values_to = 'shap.value.mean') %>%
    mutate(top_n = rank(-shap.value.mean)) %>%
    arrange(top_n)
  return(shap_values_imp)
}

#' h2o.shap.varimp_plot
#'
#' h2o shap variable importance plot
#'
#' @inheritParams h2o.shap.varimp
#' @param X original dataset
#' @param max.display number of variables to show in the plot
#' @param plot_type {'dot','bar'}
#' @examples
#' data(shap_values_mtcars)
#' data(mtcars)
#' h2o.shap.varimp_plot(shap_values_mtcars, mtcars, 20, 'bar')
#' h2o.shap.varimp_plot(shap_values_mtcars, mtcars, 20, 'dot')
#' @export

h2o.shap.varimp_plot <- function(shap_values, X, max.display = 20, plot_type = 'bar'){

  rescale.num <- function(x){
    # rescale numeric variable
    if(is.numeric(x)){
      x_min <- min(x, na.rm = T)
      x_max <- max(x, na.rm = T)
      x <- (x - x_min) / (x_max - x_min)
    } else{
      x <- NA_real_
    }
    return(x)
  }

  # rescale feature value
  X_norm <- X %>% mutate_all(rescale.num)
  # extract the expected value
  expected_value <- shap_values[['BiasTerm']][1]
  # extract shap values
  shap_values <- shap_values %>% select(-BiasTerm)

  # reshaping for plots
  # reshape mean of shap values
  shap_values_imp <- shap_values %>%
    mutate_all(abs) %>%
    summarise_all(mean) %>%
    pivot_longer(everything(), names_to = 'variable', values_to = 'shap.value.mean') %>%
    mutate(top_n = rank(-shap.value.mean),
           variable = as.character(variable))
  # reshape shap values
  shap_values_melt <- shap_values %>%
    tibble::rownames_to_column('idx') %>%
    pivot_longer(-idx, names_to = 'variable', values_to = 'shap.value') %>%
    mutate(variable = as.character(variable))
  # reshape scaled feature values
  X_norm_melt <- X_norm %>%
    tibble::rownames_to_column('idx') %>%
    pivot_longer(-idx, names_to = 'variable', values_to = 'X_norm') %>%
    mutate(variable = as.character(variable))

  if (plot_type == 'bar'){
    # bar plot
    out <- shap_values_imp %>%
      mutate(variable = factor(variable, levels = rev(shap_values_imp[['variable']][order(shap_values_imp$top_n)]))) %>%
      filter(top_n <= max.display) %>%
      ggplot(aes(x = variable, y = shap.value.mean)) +
      geom_bar(stat = 'identity', fill = lh2.fill()) +
      labs(x = '', y = 'mean(|SHAP value|) (average impact on mdoel output magnitude)') +
      coord_flip(expand = T)
  } else {
    # jitter plot
    res <- shap_values_melt %>%
      left_join(X_norm_melt, by = c('idx','variable')) %>%
      left_join(shap_values_imp , by = 'variable') %>%
      mutate(variable = factor(variable, levels = rev(shap_values_imp$variable[order(shap_values_imp$top_n)])))
    out <- res %>%
      filter(top_n <= max.display) %>%
      ggplot(aes(x = variable, y = shap.value, color = X_norm)) +
      geom_jitter(width = 0.2, size = 1) +
      geom_hline(yintercept = 0.0) +
      labs(x = '', y = 'Shap value (impact on model output)') +
      coord_flip(expand = T) +
      scale_color_gradient(low="blue",high="red", name = 'feature value')
  }
  out + h2o.shap.theme()
}


#' h2o.shap.waterfall_plot
#'
#' h2o shap waterfall plot
#'
#' @inheritParams h2o.shap.varimp_plot
#' @param idx index of the individual prediction
#' @param link linking function {'', 'logit'}. default NULL: identical, alternative logit: logit transform to probability
#' @param centre whether shap.value centred at 0, probabilty centred at 0.5, default FALSE FALSE: add bias term back
#' @param type {bar, arrow}
#' @return ggplot2 waterfall plot
#' @examples
#' data(mtcars)
#' data(shap_values_mtcars)
#' h2o.shap.waterfall_plot(shap_values_mtcars, X = mtcars, idx = 1)
#' @export

h2o.shap.waterfall_plot <- function(shap_values, X, idx,
                                    link = '', centre = FALSE,
                                    max.display = 20, type = 'bar'){

  max.display <- min(max.display, ncol(shap_values) - 1)
  other.vars <- ncol(shap_values) - max.display - 1
  
  sigmoid <- function(x){
    1 / (1 + exp(-x))
  }
  
  color.pushhigher <- '#036ffc'
  color.pushlower <- '#fc0331'
  color.total <- '#03fc77'
  
  # expected shap mean
  expected_value <- shap_values[['BiasTerm']][1]
  
  # individual shap array
  varimp <- h2o.shap.varimp(shap_values[idx,]) %>%
    mutate(variable = as.character(variable))
  
  if(other.vars > 0){
    fnames.others <- varimp[(max.display + 1):(max.display + other.vars), ] %>%
      pull(variable)
    shap.others <- shap_values %>%
      select(one_of(fnames.others)) %>%
      rowSums()
    shap_values <- shap_values %>%
      select(-one_of(fnames.others)) %>%
      mutate(!!rlang::sym(glue('other {other.vars} variables')) := shap.others)
    
    varimp.others <- h2o.shap.varimp(shap_values[idx,]) %>%
      mutate(variable = as.character(variable))
    
    # arrange others to the last
    
    varimp.others <- varimp.others %>%
      filter(variable != glue('other {other.vars} variables')) %>%
      bind_rows(
        varimp.others %>%
          filter(variable == glue('other {other.vars} variables'))
      ) %>%
      select(-top_n) %>%
      tibble::rownames_to_column('top_n') %>%
      mutate(top_n = as.integer(top_n))
    
    shap_1 <- varimp.others 
    shap_1['feature.value'] <- X[idx, varimp[1:max.display,] %>% 
                                   pull(variable)] %>% 
      t() %>% 
      as.character() %>%
      c('')
    shap_1['shap.value.mean'] <- as.numeric(shap_values[idx,shap_1[['variable']]])
    shap_1['y'] <- shap_1['shap.value.mean']
    shap_1['id'] <- rev(shap_1[['top_n']])
    shap_1['x'] <- shap_1['variable']
  } else {
    shap_1 <- varimp 
    shap_1['feature.value'] <- X[idx, varimp %>% pull(variable)] %>% 
      t() %>% 
      as.character() 
    shap_1['shap.value.mean'] <- as.numeric(shap_values[idx,shap_1[['variable']]])
    shap_1['y'] <- shap_1['shap.value.mean']
    shap_1['id'] <- rev(shap_1[['top_n']])
    shap_1['x'] <- shap_1['variable']
  }
  
  
  # add total row
  total_row <- data.frame(variable = 'total',
                          shap.value.mean = sum(shap_1[['shap.value.mean']]),
                          top_n = max(shap_1[['top_n']]+1),
                          feature.value = "",
                          y = -sum(shap_1[['y']]),
                          id = min(shap_1[['id']] - 1),
                          x = "total")
  shap_1 <- rbind(shap_1, total_row)
  
  # calculate cum sum for waterfall plot
  shap_1 <- shap_1 %>%
    mutate(end = cumsum(y),
           start = lag(end,1, default = 0),
           fill = ifelse(y >0, color.pushhigher, color.pushlower),
           prob_start = sigmoid(start),
           prob_end = sigmoid(end),
           x = as.character(x),
           x = paste(x, feature.value, sep = '='),
           fill = ifelse(variable == 'total', color.total , fill)) %>%
    mutate(x = gsub('=$','', x))
  
  # swap total start and end
  a <- shap_1[shap_1[['variable']] == 'total', 'start']
  shap_1[shap_1[['variable']] == 'total', 'start'] <- shap_1[shap_1[['variable']] == 'total', 'end']
  shap_1[shap_1[['variable']] == 'total', 'end'] <- a
  
  if(centre){
    bias <- 0
    bias.prob <- 0.5
  } else {
    bias <- expected_value
    bias.prob <- sigmoid(expected_value)
    shap_1 <- shap_1 %>%
      mutate(shap.value.mean = shap.value.mean + bias,
             y = y + bias,
             start = start + bias,
             end = end + bias,
             prob_start = sigmoid(start),
             prob_end = sigmoid(end))
  }
  
  if(link == 'logit'){
    preds <- round(sigmoid(sum(shap_values[idx,])),2)
    start <- 'prob_start'
    end <- 'prob_end'
    y.bias <- bias.prob
    breaks <- seq(0,1,0.05)
  } else {
    preds <- round(sum(shap_values[idx,]),2)
    start <- 'start'
    end <- 'end'
    y.bias <- bias
    breaks.seq <- max(floor((max(shap_1$y) - min(shap_1$y)) / 2) / 10, 0.1)
    breaks <- seq(-5,
                  round(max(c(shap_1$start, shap_1$end)), 2),
                  breaks.seq)
  }
  
  
  x.margin <- 0.01
  
  # waterfall plot - base
  if(type == 'arrow'){
    out <- shap_1 %>%
      ggplot() +
      geom_segment(aes(x = id, xend = id,
                       y = !!sym(start), yend = !!sym(end)),
                   arrow = arrow(length = unit(0.25, 'cm'), type = 'closed'),
                   arrow.fill = shap_1[['fill']]) +
      geom_hline(yintercept = y.bias, color = 'black') +
      scale_x_continuous(breaks = shap_1$id, labels = shap_1$x, expand = c(x.margin,x.margin)) +
      scale_y_continuous(breaks = breaks) +
      labs(x = 'variable', y = 'shap.value',
           title = glue('waterfall plot (prediction={preds})'))
    
  } else {
    out <- shap_1 %>%
      ggplot() +
      geom_rect(aes(xmin = (id)-0.45, xmax = (id)+0.45,
                    ymin = !!sym(start), ymax = !!sym(end),
                    fill = fill)) +
      scale_fill_identity() +
      geom_hline(yintercept = y.bias, color = 'black') +
      scale_x_continuous(breaks = shap_1[['id']], labels = shap_1[['x']], expand = c(x.margin,x.margin)) +
      scale_y_continuous(breaks = breaks) +
      labs(x = 'variable', y = 'shap.value',
           title = glue('waterfall plot (prediction={preds})'))
  }
  
  out + 
    coord_flip() + 
    theme(legend.position = 'none') +
    h2o.shap.theme()
}

#' shap.dependency.plot
#'
#' shap dependency plot
#'
#' @importFrom ggpubr ggarrange
#' @inheritParams h2o.shap.varimp_plot
#' @param i variable to be plotted
#' @param log whether log transformation is required
#' @examples
#' plots <- list()
#' for(i in c('cyl','disp','hp','wt')){
#'  log <- ifelse(i == 'disp', TRUE, FALSE)
#'  plots[[i]] <- h2o.shap.dependency_plot(shap_values_mtcars, mtcars, i, log)
#' }
#' ggpubr::ggarrange(plotlist = plots, ncol = 2, nrow = 2)
#' @export
h2o.shap.dependency_plot <- function(shap_values, X, i, log = F){

  x <- X %>% pull(!!rlang::sym(i))
  y <- shap_values %>% pull(!!rlang::sym(i))
  res <- data.frame(x = x, y = y)
  if(class(x) == 'character'){
    # categorical plot
    nlevel <- n_distinct(x)
    p <- res %>%
      ggplot(aes(x = x, y = y)) +
      geom_jitter(color = lh2.fill(), alpha = 1) +
      labs(x = '', y = '', title = i) +
      shap.pdp.theme() +
      theme(axis.text.x = element_text(angle = 90))
    if(nlevel > 20 ) {p <- p + scale_x_discrete(labels = NULL)}
  } else {
    # numerical plot
    if(log) res$x <- log10(x+1)
    p <- res %>%
      ggplot(aes(x = x, y = y)) +
      geom_point(color = lh2.fill(), alpha = 1) +
      labs(x = '', y = '', title = ifelse(log, glue('{i} (log10)'),i)) +
      shap.pdp.theme()
  }
  p +  theme(axis.line = element_line(color = 'black'))
}



