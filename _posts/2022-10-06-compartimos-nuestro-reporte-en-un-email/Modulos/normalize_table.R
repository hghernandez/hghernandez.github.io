normalize_table <- function(df,date= NULL,id_costumer=NULL,cantidad= NULL,monto= NULL){
  
  df <- dplyr::rename(df,date = rlang::sym(date))
  df <- dplyr::rename(df,user_id = rlang::sym(id_costumer))
  df <- dplyr::rename(df,cantidad = rlang::sym(cantidad))
  df <- dplyr::rename(df,monto= rlang::sym(monto))
  
}