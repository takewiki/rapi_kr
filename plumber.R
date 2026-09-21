#0-参数设置----
config_token = "config/conn.R"
config_rdbe = "config/rdbe.R"
config_erp = "config/erp.R"
library(tsda)
library(tsdo)


# 查询刀具-----------
#* @post api/md_tooling/read
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
    	select FToolingID	as	id	,
FToolingCode	as	tooling_code	,
FToolingName	as	tooling_name	,
FSpecificationModel	as	specification	,
FMaterial	as	material	,
FBrand	as	brand	,
FStatus	as	status	,
FRemarks	as	remark	,
FCreateTime	as	created_at	,
FModifyTime	as	updated_at	
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
#* @post api/md_mold/read
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
    	select FMoldID	as	id	,
FMoldCode	as	mold_code	,
FMoldName	as	mold_name	,
FSpecificationModel	as	specification	,
FMoldMaterial	as	material	,
FBrand	as	brand	,
FMoldOutputGroupCount	as	output_group_count	,
FEnableStatus	as	status	,
FBusinessRemarks	as	remark	,
FCreateTime	as	created_at	,
FModifyTime	as	updated_at	
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
    select FProductionOrderNo	as	生产订单号	,
FProcessPlanNo	as	工序计划单号	,
FProcessTaskNo	as	工序任务单号	,
FTaskType	as	任务类型	,
FProcessStatus	as	工序状态	,
FTaskDate	as	任务日期	,
FPlannedStartDate	as	计划开工日期	,
FPlannedEndDate	as	计划完成日期	,
FProcessCode	as	工序编码	,
FProcessName	as	工序名称	,
FProcessType	as	工序类型	,
FOutsourcingSupplierCode	as	委外加工商编码	,
FDepartmentCode	as	部门编码	,
FProductionWorkshop	as	生产车间	,
FTeamCode	as	班组编码	,
FTeamName	as	班组名称	,
FMachineCode	as	加工机台	,
FTeamLeaderEmpNo	as	班组长工号	,
FTeamLeaderName	as	班组长姓名	,
FInspectorEmpNo	as	质检员工号	,
FInspectorName	as	质检员姓名	,
FFirstInspectionFlag	as	首检是否检验	,
FFirstInspectionMethod	as	首检检验方式	,
FFirstInspectionTemplateCode	as	首检模板编码	,
FPatrolInspectionFlag	as	巡检是否检验	,
FPatrolInspectionMethod	as	巡检检验方式	,
FPatrolInspectionTemplateCode	as	巡检模板编码	,
FPatrolInspectionInterval	as	巡检间隔	,
FFinalInspectionFlag	as	末检是否检验	,
FFinalInspectionMethod	as	末检检验方式	,
FFinalInspectionTemplateCode	as	末检模板编码	,
FProductCode	as	产品编码	,
FProductName	as	产品名称	,
FSpecificationModel	as	规格型号	,
FTaskQty	as	任务数量	,
FTaskMaxQty	as	任务数量上限	,
FCreator	as	创建人	,
FCreateDate	as	创建日期	
from rds_src_t_workOrder where FProductionOrderNo='",FProductionOrderNo,"'")
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