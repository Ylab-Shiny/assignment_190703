
# 読み込むライブラリ ---------------------------------------------------------------

library(tidyverse)
library(zoo)
library(hms)
library(scales)

# ディレクトリ ------------------------------------------------------------------


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

# 2018年度のデータ
year2018 <- read_csv("year2018.csv")
year2018 <- year2018

# 列番号をコンソールで入力
prompt_message <- paste0("列番号を入力してください(2 ~ ", length(colnames(year2018)), "): ")
col_num <- readline(prompt = prompt_message)
col_num <- as.integer(col_num)

# 外れ値なしのデータ
valuesRmOutlier <- RepOutliersNA(year2018[[col_num]]) %>% na.spline()
dataset <- cbind.data.frame(year2018$date_time, valuesRmOutlier)
the_colname <- colnames(year2018)[col_num]
colnames(dataset) <- c("label", the_colname)


# ヒートマップの作成 ---------------------------------------------------------------

data_heatmap <- dataset %>%
  mutate(day = as.Date(substr(label, 1,10)),
         time = as.hms(substr(label, 12, 19)),
         week = format(as.POSIXct(label),"%W"),
        month  = format(as.POSIXct(label), "%m")) 
         xlim(1, max(PlantGrowth$weight))
         expand_limits(x=0)
         
         
data_heatmap2 <- data_heatmap %>% filter(month == the_month)

gp <- ggplot(data_heatmap2, aes(x=week, y=time)) +
  geom_raster(aes_string(fill=the_colname)) + scale_fill_gradient(low="blue", high="orange") +
  coord_cartesian(expand = F) 
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
