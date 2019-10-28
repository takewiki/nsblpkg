
#' 获取文件黑名单里的精确匹配
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @import tsda
#' @export
#'
#' @examples
#' file_filter_is();
file_filter_is <- function(brand='JBLH'){
  conn<- conn_nsim();
  sql <-paste("select  FKeyWord from filter_file
where FType ='is' and FBrand='",brand,"'",sep="")
  data <- sql_select(conn,sql);
  res <- data$FKeyWord
  return(res)
}


#' 文件级黑名单模糊匹配
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @import tsda
#' @export
#'
#' @examples
#' file_filter_like();
file_filter_like <- function(brand='JBLH'){
  conn<- conn_nsim();
  sql <-paste("select  FKeyWord from filter_file
where FType ='like' and FBrand='",brand,"'",sep="")
  data <- sql_select(conn,sql);
  res <- data$FKeyWord
  return(res)
}






