## 实时读入数据库

get_data <- function(...,from=FALSE, to=FALSE){
  options(warn = -1)
  if (sum(isFALSE(from), isFALSE(to))==1) stop('开始时间和截止时间都需要填的哦')

  if (!require(curl))  {
                         install.packages('curl')
                         library(curl)
  }

  if (!require(tidyverse))  {
                         install.packages('tidyverse')
                         library(tidyverse)
  }
  if (!require(lubridate))  {
                         install.packages('lubridate')
                         library(lubridate)
  }
  if (!require(readxl))  {
                         install.packages('readxl')
                         library(readxl)
  }

  URL <- 'https://stumail-my.sharepoint.cn/:x:/g/personal/19hychen1_stu_edu_cn/EV4DKqJ_C-tOkwJJzW0t4GIBxD0atOVSSUQ3qrHHWmkktQ'
  URL <- unlist(strsplit(URL,"[?]"))[1]
  URL <- paste0(URL,"?download=1")

  curl_download(
      URL,
      destfile = file.path(tempdir(), 'test.xlsx'),
      mode = "wb")

  data<- read_excel(paste0(tempdir(), "\\" ,'test.xlsx'))

  if (isFALSE(from) & isFALSE(to)) {
        data %>% filter(...) ->data
  } else if (!isFALSE(from) & !isFALSE(to)) {
        data %>% filter(between(ymd(收血日期_baseline),ymd(from),ymd(to)),...) -> data
  }

return(data)
}
