source('managePackages.R')
source('utils.R')

data <- read.csv("data.csv", fileEncoding = "GBK")
data[, 1] <- ym(gsub("年", "-", gsub("月", "", data[, 1])))
# head(data)
# 这里一共有六列数据
data_time = data[, 1]# 时间 
data_1 = data[, 2]  # 货币和准货币(M2)供应量期末值(亿元)
data_2 = data[, 3]  # 货币和准货币(M2)供应量同比增长(%)
data_3 = data[, 4]  # 货币(M1)供应量期末值(亿元)
data_4 = data[, 5]  # 货币(M1)供应量同比增长(%)
data_5 = data[, 6]  # 流通中现金(M0)供应量期末值(亿元)
data_6 = data[, 7]  # 流通中现金(M0)供应量同比增长(%)



