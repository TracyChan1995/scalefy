# 这个函数用于深圳妇幼系统产妇混杂数据的标准化
#
# 例子 scale_baseline('C:\destop\data.xlsx'),读入的数据必须是xlsx为结尾的数据库,且按照规定格式
# 即可显示标准化后的数据库
#
# 作者：陈恒颖
# 时间：2020.12.12
# 联系方式：www.chenhengying@gmail.com

scale_baseline <- function(file= file, sheet= FALSE, include.missing= FALSE, from=FALSE, to=FALSE) {

##  读入数据库
  if (!require(readxl)) {
                         install.packages('readxl')
                         library(readxl)
  }
  if (!require(lubridate)) {
    install.packages('lubridate')
    library(lubridate)
  }
  file<- substitute(file)
  sheet <- substitute(sheet)
  if (isFALSE(sheet)) {
    data_total<-read_excel(file, col_names = FALSE)
  } else {
  data_total<-read_excel(file, sheet=sheet, col_names = FALSE)
  }

  stopifnot(nrow(data_total) %% 24==0)
  stopifnot(ncol(data_total) %% 6==0)
  if (!isFALSE(include.missing) & isFALSE(from) & isFALSE(to)) stop('from的开始日期和to的截止日期需要填写')
  if (!isFALSE(include.missing) & !isFALSE(from) & isFALSE(to))  stop('to的截止日期需要填写')
  if (!isFALSE(include.missing) & isFALSE(from) & !isFALSE(to))  stop('from的开始日期需要填写')
  if (isFALSE(include.missing) & any(!isFALSE(from) & !isFALSE(to)))  stop('inchude.missing填写为TRUE')

  bottle<- data.frame()

  for (i in 1:(nrow(data_total)%/%24)) {
## 对产妇的初检信息进行清洗
  data<- data_total[(1+(i-1)*24):(i*24),]

  if (!require(tidyverse)|!require(reshape2)) {
                     install.packages(c('tidyverse','reshape2'))
                     library(tidyverse, reshape2)
  }

  #选择预产期的数据合并到第五列
   ...6<- substitute('...6')               #要把选择的列先去字符化

data_firstvisit<-   data %>%
                         select(...6) %>%
                         rename(...5=...6) %>%
                         rbind(data[,'...5']) %>%
                         na.omit()

  #转化数据框格式

data_firstvisit <- data_firstvisit %>%
                   mutate(variable=1:nrow(data_firstvisit), ID=i) %>%
                   acast(ID~variable, value.var = '...5') %>%
                   as.data.frame() %>%
                   rename(parity='8') %>%
                   mutate(parity2=parity) %>%
                   select('1','2','4','6','parity','11','12','15','16','18','parity2') %>%
                   relocate('parity2',.after='parity')

  #文本信息提取
  pattern1<- regex('(?<=预产期\\s).+')
  pattern2<- regex('(?<=检查日期\\s).+')
  pattern3<- regex('(?<=末次月经\\s).+')
  pattern4<- regex('(?<=妊娠结局\\s受孕方式:\\s).+')
  pattern5<- regex('(?<=孕产情况\\s孕次:\\s).(?=\\s产次:)')
  pattern6<- regex('(?<=产次:\\s).*(?=\\s现有男女数:\\s)')
  pattern7<- regex('(?<=孕产史\\s).*')
  pattern8<- regex('(?<=个人史\\s).*')
  pattern9<- regex('(?<=血压:\\s).+(?=\\smmHg)')
  pattern10<- regex('(?<=孕前体重:\\s).+(?=\\skg)')
  pattern11<-regex('(?<=身高:\\s).+(?=\\scm)')

  data_firstvisit <-
           mapply(str_extract, data_firstvisit,
           list(pattern1,pattern2,pattern3,pattern4,pattern5, pattern6, pattern7, pattern8,pattern9,pattern10,pattern11)) %>%
           t() %>% as.data.frame() %>%
           rename('预产期'='1', '检查日期'='2','末次月经'='4', '受孕方式'='6', '孕次'='parity', '产次'='parity2','孕产史'='11',
           '个人史'='12','初检血压'='15','孕前体重'='16','身高'='18')

  #更换孕史的标点符号
  displace_dot1<- function(x) {
    x<- gsub('(\U00A0)',',',x)
    x<- gsub('[ ;，]',',',x)
    x
  }

  data_firstvisit[,c('孕产史','个人史')]<-
    data_firstvisit[,c('孕产史','个人史')] %>% map_df(displace_dot1)

## 丈夫的基本信息
  ...3<- substitute(...3)
  ...4<- substitute(...4)

data_husband<-
              data[18:24,] %>%
              select(...3,...4) %>%
              rename(...1=...3,...2=...4) %>%
              bind_rows(data[18:24,c(1,2)]) %>%
              filter(!is.na(...1)) %>%
              mutate(ID=i, prefix='丈夫') %>%
              acast(ID~prefix+...1, value.var='...2') %>%
              as.data.frame() %>%
              select('丈夫_姓名','丈夫_年龄','丈夫_文化程度','丈夫_民族','丈夫_职业','丈夫_血型')

data_work<- bind_cols(data_husband, data_firstvisit)

## 孕妇的基本信息提取
data_characteristic<-
              data[1:16,] %>% select(...3,...4) %>%
              rename(...1=...3,...2=...4) %>%
              bind_rows(data[1:16,c(1,2)])

row_na<-
              data_characteristic %>%
              select(...1) %>%
              is.na()

data_characteristic<-
              data_characteristic[!row_na,] %>%
              mutate(ID=i, prefix='产妇') %>%
              acast(ID~prefix+...1, value.var='...2') %>%
              as.data.frame() %>%
              select(产妇_姓名,产妇_保健号,产妇_身份证件类别,产妇_身份证件号码,产妇_出生日期,
                     产妇_户口所在地,产妇_民族, 产妇_文化程度,产妇_职业,
                     产妇_初潮, 产妇_月经量,产妇_周期,产妇_既往史,产妇_家族遗传史)

  #更换病史史的标点符号
displace_dot2<- function(x) {
  x<- gsub('\\s{3}',',',x)
  x<- gsub(' ','',x)
  x<- gsub('[，;；+-、]',',',x)
  x
}

data_characteristic[,c('产妇_既往史','产妇_家族遗传史')]<-
                   data_characteristic[,c('产妇_既往史','产妇_家族遗传史')] %>% map_df(displace_dot2)

data_work<- bind_cols(data_characteristic, data_work)
bottle<- bind_rows(bottle, data_work)
  }

  if (!isFALSE(include.missing) & !isFALSE(from) & !isFALSE(to) ){
    bottle<-
      scalefy::get_data() %>% filter(between(ymd(收血日期_baseline),ymd(from), ymd(to)))  %>% select(姓名) %>% left_join(bottle,c('姓名'='产妇_姓名'))
  }
  return(bottle)
}
