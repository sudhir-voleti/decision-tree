data_frame_str <- function(data){
  df_str <- data.frame(variable = names(data),
                       class = sapply(data, class),
                       first_values = sapply(data, function(x) paste0(head(x),  collapse = ", ")),
                       unique_value_count = sapply(data,function(x) length(unique(x))),
                       row.names = NULL) 
  return(df_str)
}
varImp_plot <- function(res){
  df <- data.frame(imp = res$variable.importance)
  df2 <- df %>% 
    tibble::rownames_to_column() %>% 
    dplyr::rename("variable" = rowname) %>% 
    dplyr::arrange(imp) %>%
    dplyr::mutate(variable = forcats::fct_inorder(variable))
  ggplot2::ggplot(df2) +
    geom_col(aes(x = variable, y = imp),
             col = "black", show.legend = F) +
    coord_flip() +
    scale_fill_grey() +
    theme_bw()
}
