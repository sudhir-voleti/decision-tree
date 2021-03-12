data_frame_str <- function(data){
  df_str <- data.frame(variable = names(data),
                       class = sapply(data, class),
                       first_values = sapply(data, function(x) paste0(head(x),  collapse = ", ")),
                       unique_value_count = sapply(data,function(x) length(unique(x))),
                       row.names = NULL) 
  return(df_str)
}
