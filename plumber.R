#0-参数设置----
config_token = "config/conn.R"
config_rdbe = "config/rdbe.R"
config_erp = "config/erp.R"
library(tsda)
library(tsdo)


# 查询刀具-----------
#* @post api/mdTool/read
#' 查询刀具
#'
#' @param FTokenErp KR_DMS数据库Token
#' @param FToolingCode 刀具编码
#'
#' @return 返回刀具信息
#' @export
#'
#' @examples
#' rds_u8api_tooling_code_read()
rds_u8api_tooling_code_read<- function(FTokenErp,FToolingCode='DJ-001')
                                  {
  sql_token <- paste0("SELECT 
  count(1) as FCount
  FROM [t_sec_key]
  where FToken ='",FTokenErp,"'")
  conn_token = tsda::odbc_conn_createFromConfigFile(config_file = config_token)
  data_token = tsda::odbc_select(conn_token,sql_token)
  ncount_token = data_token$FCount
  if(ncount_token){
    #token check pass
    sql <- paste0("
    select FToolingID,
       FToolingCode,
       FToolingName,
       FSpecificationModel,
       FMaterial,
       FBrand,
       FStatus,
       FRemarks,
       FCreateTime,
       FModifyTime
from rds_src_t_mdTool
where FToolingCode='",FToolingCode,"'")
    conn = tsda::odbc_conn_createFromConfigFile(config_file = config_erp)
    data = tsda::odbc_select(conn,sql)
    ncount = nrow(data)
    if(ncount){
      res = list(status=TRUE,data=data,message='查询正常',code=200)
    }else{
      res = list(status=FALSE,data='',message='没有查到数据',code=100)
    }
    
  }else{
    # token check failed
    res = list(status=FALSE,data='',message='没有查到ERP数据库Token,请检查',code=100)
  }
  
  
  return(res)
}

# 查询模具-----------
#* @post api/mdMold/read
#' 查询模具
#'
#' @param FTokenErp KR_DMS数据库Token
#' @param FMoldCode 模具编码
#'
#' @return 返回模具信息
#' @export
#'
#' @examples
#' rds_u8api_mold_code_read()
rds_u8api_mold_code_read<- function(FTokenErp,FMoldCode='MJ-001')
{
  sql_token <- paste0("SELECT 
  count(1) as FCount
  FROM [t_sec_key]
  where FToken ='",FTokenErp,"'")
  conn_token = tsda::odbc_conn_createFromConfigFile(config_file = config_token)
  data_token = tsda::odbc_select(conn_token,sql_token)
  ncount_token = data_token$FCount
  if(ncount_token){
    #token check pass
    sql <- paste0("
    select FMoldID,
       FMoldCode,
       FMoldName,
       FSpecificationModel,
       FMoldMaterial,
       FBrand,
       FMoldOutputGroupCount,
       FEnableStatus,
       FBusinessRemarks,
       FCreateTime,
       FModifyTime
from rds_src_t_mdMold
where FMoldCode='",FMoldCode,"'")
    conn = tsda::odbc_conn_createFromConfigFile(config_file = config_erp)
    data = tsda::odbc_select(conn,sql)
    ncount = nrow(data)
    if(ncount){
      res = list(status=TRUE,data=data,message='查询正常',code=200)
    }else{
      res = list(status=FALSE,data='',message='没有查到数据',code=100)
    }
    
  }else{
    # token check failed
    res = list(status=FALSE,data='',message='没有查到ERP数据库Token,请检查',code=100)
  }
  
  
  return(res)
}



# 查询工单-----------
#* @post api/workOrder/read
#' 查询工单
#'
#' @param FTokenErp KR_DMS数据库Token
#' @param FProductionOrderNo 模具编码
#'
#' @return 返回模具信息
#' @export
#'
#' @examples
#' rds_u8api_workOrder_read()
rds_u8api_workOrder_read<- function(FTokenErp,FProductionOrderNo='MO000006')
{
  sql_token <- paste0("SELECT 
  count(1) as FCount
  FROM [t_sec_key]
  where FToken ='",FTokenErp,"'")
  conn_token = tsda::odbc_conn_createFromConfigFile(config_file = config_token)
  data_token = tsda::odbc_select(conn_token,sql_token)
  ncount_token = data_token$FCount
  if(ncount_token){
    #token check pass
    sql <- paste0("
  select FProductionOrderNo	,
FProcessPlanNo	,
FProcessTaskNo	,
FMergeProcessTaskNo	,
FTaskType	,
FTaskDate	,
FPlannedStartDate	,
FPlannedEndDate	,
FProductCode	,
FProductName	,
FTaskQty	,
FProcessCode	,
FProcessName	,
FProcessType	,
FTeamCode	,
FTeamName	,
FMachineCode	,
FToolCode	,
FMoldCode	,
FFirstInspectionFlag	,
FPatrolInspectionFlag	,
FPatrolInspectionInterval	,
FFinalInspectionFlag	
      from rds_src_t_workOrder
      where FProductionOrderNo='",FProductionOrderNo,"'")
    conn = tsda::odbc_conn_createFromConfigFile(config_file = config_erp)
    data = tsda::odbc_select(conn,sql)
    ncount = nrow(data)
    if(ncount){
      res = list(status=TRUE,data=data,message='查询正常',code=200)
    }else{
      res = list(status=FALSE,data='',message='没有查到数据',code=100)
    }
    
  }else{
    # token check failed
    res = list(status=FALSE,data='',message='没有查到ERP数据库Token,请检查',code=100)
  }
  
  
  return(res)
}

#* @post api/sec/readme
#' API作者说明
#'
#' @param name 作者名称
#'
#' @return 返回值
#' @export
#'
#' @examples
#' readme()
readme <- function(name='hulilei') {
  #print(name)
  data = data.frame(FName = name)
  res = list(status=TRUE,data=data,message='查询成功',code=200)
  return(res)
  
}