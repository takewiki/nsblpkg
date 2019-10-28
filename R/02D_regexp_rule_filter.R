#' 删除问题中的订单信息
#'
#' @param data 数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' del_orderNumber_ques();
del_orderNumber_ques <-function(data){
  res <-row_del_byNum(data,'FQuestion',num_digits = 18)
  return(res)
}

#' 删除答案中的订单豪华版
#'
#' @param data 数据
#'
#' @return 返回值
#' @export
#'
#' @examples del_orderNumber_answ()
del_orderNumber_answ <-function(data){
  res <-row_del_byNum(data,'FAnswer',num_digits = 18)
  return(res)
}


#' 删除问题中的电话号码
#'
#' @param data 数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' del_mobile_ques();
del_mobile_ques <-function(data){
  res <-row_del_byNum(data,'FQuestion',num_digits = 11)
  return(res)
}

#' 删除答案中的电话号码
#'
#' @param data 数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' del_mobile_answ();
del_mobile_answ <-function(data){
  res <-row_del_byNum(data,'FAnswer',num_digits = 11)
  return(res)
}


#' 车架在问题在的应用
#'
#' @param data 数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' del_car_VIN_ques();
del_car_VIN_ques <- function(data){
  res <- row_del_byNumChars(data,'FQuestion',num_digits = 17)
  return(res)
}

#' 删除车架吗
#'
#' @param data  数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' del_car_VIN_answ();
del_car_VIN_answ <- function(data){
  res <- row_del_byNumChars(data,'FAnswer',num_digits = 17)
  return(res)
}




