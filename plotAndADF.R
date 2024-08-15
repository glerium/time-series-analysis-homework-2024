source('managePackages.R')
source('utils.R')
source('read_data.R')


adf_test(data_1)
easy_plot(data_time, data_1, "货币和准货币(M2)供应量期末值(亿元)")  # ggplot绘图函数
adf_test(diff(data_1))
easy_plot(data_time[2:length(data_time)], diff(data_1), 
          title = "货币和准货币(M2)供应量期末值(亿元)")


adf_test(data_2)
easy_plot(data_time, data_2, "货币和准货币(M2)供应量同比增长(%)") 
adf_test(diff(data_2))
easy_plot(data_time[2:length(data_time)], diff(data_2), 
          title = "货币和准货币(M2)供应量同比增长(%)")

adf_test(data_3)
easy_plot(data_time, data_3, "货币(M1)供应量期末值(亿元)") 
adf_test(diff(data_3))
easy_plot(data_time[2:length(data_time)], diff(data_3), 
          title = "货币(M1)供应量期末值(亿元)")


adf_test(data_4)
easy_plot(data_time, data_4, "货币(M1)供应量同比增长(%)") 
adf_test(diff(data_4))
easy_plot(data_time[2:length(data_time)], diff(data_4), 
          title = "货币(M1)供应量同比增长(%)")


adf_test(data_5)
easy_plot(data_time, data_5, "流通中现金(M0)供应量期末值(亿元)") 
adf_test(diff(data_5))
easy_plot(data_time[2:length(data_time)], diff(data_5), 
          title = "流通中现金(M0)供应量期末值(亿元)")

adf_test(data_6)
easy_plot(data_time, data_6, "流通中现金(M0)供应量同比增长(%)") 
adf_test(diff(data_6))
easy_plot(data_time[2:length(data_time)], diff(data_6), 
          title = "流通中现金(M0)供应量同比增长(%)")

