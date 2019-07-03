
# 読み込むライブラリ ---------------------------------------------------------------

library(tidyverse)
library(scales)
library(zoo)
library(hms)

# ディレクトリ ------------------------------------------------------------------

user_dir <- Sys.getenv("USERPROFILE")
data_dir <- paste0(user_dir, "\\Dropbox\\Yamaha-lab\\0_semi\\2019Temp\\5years data\\")

# 外れ値の検出とNAに置き換える関数 ----------------------------------------------------------

RepOutliersNA <- function(column) {
  column <- as_tibble(column)
  xx1 <- as_tibble(column) %>% mutate(
    Norm = (column[[1]]-min(column[[1]], na.rm = T)) / (max(column[[1]], na.rm = T)-min(column[[1]], na.rm = T)) * (1-0) + 0
  )
  qq <- data.frame(quantile(xx1[[2]], c(0.25, 0.75), na.rm = T))
  Q1 <- qq[1, 1]
  Q3 <- qq[2, 1]
  
  outer_l_Q1 <- Q1 - 1.5 * (Q3 - Q1)
  outer_m_Q3 <- Q3 + 3 * (Q3 - Q1)
  outer_ll <- which(xx1$Norm < outer_l_Q1)
  outer_mm <- which(xx1$Norm > outer_m_Q3)
  
  # 重複なく昇順に行番号を抽出
  row_num_out <- unique(c(outer_ll, outer_mm)) %>% sort()
  
  # 外れ値の出力
  outer_outlier <- cbind.data.frame(column[row_num_out,], row_number = row_num_out)
  column_removeOutliers <- column
  column_removeOutliers[outer_outlier$row_number, 1] <- NA
  
  
  return(column_removeOutliers)
}

# データの読み込み ----------------------------------------------------------------

setwd(data_dir)
# 2018年度のデータ
year2018 <- read_csv("year2018.csv")
year2018 <- year2018 %>% select(-ﾌﾛﾝﾃｨｱ研究棟電力量) %>% as_tibble()

# 列番号をコンソールで入力
prompt_message <- paste0("列番号を入力してください(2 ~ ", length(colnames(year2018)), "): ")

# パラメータ1
col_num <- readline(prompt = prompt_message)
col_num <- as.integer(col_num) # 整数型に

# 外れ値なしのデータ
valuesRmOutlier <- RepOutliersNA(year2018[[col_num]]) %>% na.spline() # 外れ値欠損値処理
dataset <- cbind.data.frame(year2018$date_time, valuesRmOutlier)
the_colname <- colnames(year2018)[col_num]
the_colname2 <- paste0("var", col_num)
colnames(dataset) <- c("label", the_colname)


# ヒートマップの作成 ---------------------------------------------------------------
# 1年分のデータ
data_heatmap <- dataset %>% 
  mutate(day = as.Date(substr(label, 1, 100)),
         time = as.hms(substr(label, 12, 19)),
         week = format(as.POSIXct(label), "%W"),
         month = format(as.POSIXct(label), "%m"))

ggsave(filename, plot = last_plot(), device = NULL, path = NULL,yaer2018
the_month <- readline(prompt = "月を入力してください(01 ~ 12): ")


data_heatmap2 <- data_heatmap %>% filter(month == the_month) # 行方向に検索 1か月分のデータ

gp <- ggplot(data_heatmap2, aes(x=day, y=time)) + 
  geom_raster(aes_string(fill=the_colname2)) + scale_fill_gradient(low="blue", high="orange") +
  coord_cartesian(expand = F) + labs(fill=the_colname)
print(gp)


# # ラベルの調整
# br = seq(0, 86400, 7200)
# lab = c("00:00", "02:00", "04:00","06:00","08:00","10:00","12:00","14:00","16:00","18:00","20:00","22:00","24:00")
# 
# gp2 <- gp + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#   scale_y_reverse(breaks = br, labels = lab) + 
#   scale_x_date(breaks = date_breaks("1 day"), date_labels = "%y/%m/%d") +
#   labs(fill="凡例") + ggtitle(paste0(the_colname, "のヒートマップ")) +
#   theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), 
#         axis.title = element_blank(), title = element_text(size = 15, face = "bold"))
# print(gp2) # 保存するサイズ(width=1800, height=750)  
