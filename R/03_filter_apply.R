



#' 将文件黑名单应用于问题
#'
#' @param data 问题版本数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' ques_filtered();
ques_filtered  <-function(data){


  #应用文件级黑名单
  #print('1')
  res <- row_del_byKeys_is(data,'FQuestion',keys = file_filter_is());
  #print('2')
  res <- row_del_byKeys_like(res,'FQuestion',keys = file_filter_like());
  #print('3')
  #应用问题级黑名单
  res <-row_del_byKeys_is(res,'FQuestion',question_filter_is());
  #print('4')
  res <- row_del_byKeys_like(res,'FQuestion',question_filter_like());
  #print('5')
  #删除固定字段
  res <- row_del_byKeys_is(res,'FQuestion',"");
  #print('6')
  #删除订单及电话号码
  res<- del_orderNumber_ques(res);
  #print('7')
  res<-del_mobile_ques(res);
  #print('8')
  #删除车架号
  res <- del_car_VIN_ques(res)
  #print('9')

  return(res);
}

#' 定义答案级黑名单
#'
#' @param data 数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' answ_filtered();
answ_filtered <- function(data){
  #应用文件级黑名单
  res <- row_del_byKeys_is(data,'FAnswer',keys = file_filter_is());
  res <- row_del_byKeys_like(res,'FAnswer',keys = file_filter_like());
  #应用答案级黑名单
  res <- row_del_byKeys_is(res,'FAnswer',keys = answer_filter_is());
  res <- row_del_byKeys_like(res,'FAnswer',keys = answer_filter_like());
  #删除固定字段
  res <- row_del_byKeys_is(res,'FAnswer',keys = "");
  #删除订单及电话号码
  res <- del_mobile_answ(res);
  res <- del_orderNumber_answ(res)
  #删除车架号
  res <-del_car_VIN_answ(res)
  return(res);

}


#' 将版本进行过滤操作
#'
#' @param brand 品牌
#' @param version 版本
#'
#' @return 返回值
#' @export
#'
#' @examples
#' item_ques_version_filtered();
item_ques_version_filtered <- function(brand ='JBLH',version='V1'){
  conn <- conn_nsim();
  #print('1')
  var_nscl_version <-nsim_version_getCurrentVersion(brand,'nscl');
  sql <-paste(" select * from  item_question_version
 where FBrand='",brand,"' and FVersionTxt='",var_nscl_version,"'",sep="")
  res <- sql_select(conn,sql);
  #print('2')
  res <- ques_filtered(res);
  #print('3')
  #进行版本化存储
  FBL_VersionTxt <- version
  res$FBL_VersionTxt <- rep(FBL_VersionTxt,nrow(res))
  #print('4')
  nsim_save(res,'item_question_version_bl');
  #print('5')
  return(res);
}


#' 删除版本化管理
#'
#' @param brand 品牌
#' @param version 版本
#'
#' @return 返回值
#' @export
#'
#' @examples
#' item_answ_version_filtered();
item_answ_version_filtered <- function(brand ='JBLH',version='V1'){

  conn <- conn_nsim();
  var_nscl_version <-nsim_version_getCurrentVersion(brand,'nscl');
  sql <-paste("  select * from item_answer_version
 where FBrand='",brand,"' and FVersionTxt='",var_nscl_version,"'",sep="")
  res <- sql_select(conn,sql);
    res <- answ_filtered(res)
  #进行版本化存储
  FBL_VersionTxt <- version;
  res$FBL_VersionTxt <- rep(FBL_VersionTxt,nrow(res))
  nsim_save(res,'item_answer_version_bl');
  return(res);
}


#' 提供主料的品牌处理
#'
#' @param brand 品牌
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsim_nsbl_version();
nsim_nsbl_version <- function(brand ='JBLH'){
  #获取版本
  #print('1')
  var_version <- nsim_version_getNextVersion(brand,'nsbl')
  startTime <- Sys.time();
  print(startTime);
  #设置
  #print('2')
  item_ques_version_filtered(brand,var_version);
  #print('3')
  item_answ_version_filtered(brand,var_version);
  #print('4')
  #更新一下版本
  nsim_version_setCurrentVersion(brand,'nsbl',var_version);
  #print('5')
  #获取最新版本
  res <- nsim_version_getCurrentVersion(brand,'nsbl');
  #print('6')
  endTime <- Sys.time();
  print(endTime);
  print(endTime-startTime);
  return(res)



}


