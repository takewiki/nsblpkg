

#' 获取答案级黑名单里的精确匹配
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @import tsda
#' @export
#'
#' @examples
#' answer_filter_is();
answer_filter_is <- function(brand='JBLH'){
  conn<- conn_nsim();
  sql <-paste("select  FKeyWord from filter_answer
where FType ='is' and FBrand='",brand,"'",sep="")
  data <- sql_select(conn,sql);
  res <- data$FKeyWord
  return(res)
}


#' 答案级黑名单模糊匹配
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @import tsda
#' @export
#'
#' @examples
#' answer_filter_like();
answer_filter_like  <- function(brand='JBLH'){
  conn<- conn_nsim();
  sql <-paste("select  FKeyWord from filter_answer
where FType ='like' and FBrand='",brand,"'",sep="")
  data <- sql_select(conn,sql);
  res <- data$FKeyWord
  return(res)
}
