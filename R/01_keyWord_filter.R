
#' 针对需要删除的关键词进行定义
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' keyword_filter_is();
keyword_filter_is <- function(brand ='JBLH'){

sql <- paste("select FKeyWord from filter_kw
 where FBrand ='",brand,"'",sep="")
conn <- conn_nsim();
data <- sql_select(conn,sql);
res <- data$FKeyWord


return(res)

}


#' 将问题进行版本化处理
#'
#' @param brand  品牌
#' @param version 版本
#'
#' @return 返回值
#' @import tsda
#' @import tsdo
#' @export
#'
#' @examples
#' nsim_item_ques_version();
nsim_item_ques_version <- function(brand='JBLH',version='V1'){
  data <- nsim_read('item_question');
  key <- keyword_filter_is();
  res <- word_col_del(data,'FQuestion',keyWords = key);
  ncount <-nrow(res);
  res$FVersionTxt <-rep(version,ncount);
  #View(res)
  # str(res);
  nsim_save(res,'item_question_version');

}



#' 处理答案版本化问题
#'
#' @param brand 品牌
#' @param version 版本
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_item_answ_version();
nsim_item_answ_version <- function(brand='JBLH',version='V1'){
  data <- nsim_read('item_answer');
  key <- keyword_filter_is();
  res <- word_col_del(data,'FAnswer',keyWords = key);
  ncount <-nrow(res);
  res$FVersionTxt <-rep(version,ncount);
  #View(res)
  # str(res);
  nsim_save(res,'item_answer_version');
}

#' 通过对语料进行删词操作，产生一个新的版本
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @import tsda
#' @import tsdo
#' @export
#'
#' @examples
#' nsim_nscl_version();
nsim_nscl_version <- function(brand='JBLH') {
  #获取产生一个新的版本
  startTime <- Sys.time();
  print(startTime)
  var_version <- nsim_version_getNextVersion(brand,'nscl');
  #进行更新问题与答案，主要是进行删词处理；
  nsim_item_ques_version(brand,var_version);
  #进行答案的处理，主要进行删词处理
  nsim_item_answ_version(brand,var_version);
  # 更新版本号,保证每调用一次，产生一个新的版本
  nsim_version_setCurrentVersion(brand,'nscl',var_version);
  #获取最新的版本进行返回
  endTime <- Sys.time();
  print(endTime)
  print(as.character(endTime-startTime));
  res <- nsim_version_getCurrentVersion(brand,'nscl');
  return(res);
}
