## 下载在线的数据库
download_data <- function(filename=filename, dir=dir){

  if(missing(filename)) stop('需要对下载文件进行命名哦！')

  if (!require(curl))  {
                        install.packages('curl')
                        library(curl)
  }

  URL <- 'https://stumail-my.sharepoint.cn/:x:/g/personal/19hychen1_stu_edu_cn/EV4DKqJ_C-tOkwJJzW0t4GIBxD0atOVSSUQ3qrHHWmkktQ'
  URL <- unlist(strsplit(URL,"[?]"))[1]
  URL <- paste0(URL,"?download=1")

  curl_download(
    URL,
    destfile = file.path(tempdir(), filename),
    mode = "wb"
  )


  if(missing(dir)){
    file.copy(
      from = paste0(tempdir(),"\\", filename),
      to = "./",
      overwrite = TRUE)
    sprintf('%s 成功保存在 %s', filename, getwd())
  } else {
    file.copy(
      from = paste0(tempdir(),"\\", filename),
      to = paste0(dir,'/'),
      overwrite = TRUE
    )
  sprintf('%s 成功保存在 %s', filename, paste0(dir,'/'))
  }
}
