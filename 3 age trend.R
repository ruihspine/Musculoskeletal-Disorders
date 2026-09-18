# ============================================================
# 完整代码：中国、日本、英国、美国 15-49 岁女性
# 骨骼肌肉疾病（Musculoskeletal disorders）
# 发病数量(柱状图)与年龄标化率(折线+阴影)比较图
# ============================================================

# 加载必要的包
library(tidyverse)
library(scales)

# 清空环境（可选）
rm(list = ls())

# 设置工作目录
setwd("D:\\R\\Musculoskeletal Disorders")

# 读取数据
df <- read.csv("data1.csv", header = TRUE)

# ---------- 1. 数据筛选 ----------
df_filtered <- df %>%
  filter(measure_name == "Incidence") %>%
  filter(cause_name == "Musculoskeletal disorders") %>%
  filter(location_id %in% c(6, 67, 95, 102)) %>%
  filter(year == 2023) %>%
  filter(!metric_name == "Percent") %>%
  filter(sex_name == "Female") %>%
  filter(age_id %in% 7:13) %>%
  mutate(age_name = factor(age_name, 
                           levels = c("15-19 years", "20-24 years", "25-29 years",
                                      "30-34 years", "35-39 years", "40-44 years",
                                      "45-49 years")))

# 分离 Number 和 Rate
df_number <- df_filtered %>% filter(metric_name == "Number")
df_rate   <- df_filtered %>% filter(metric_name == "Rate")

# 添加数值型 x 坐标
df_number <- df_number %>% mutate(x_numeric = as.numeric(age_name))
df_rate   <- df_rate   %>% mutate(x_numeric = as.numeric(age_name))

# ---------- 2. 双轴缩放因子 ----------
max_number <- max(df_number$val, na.rm = TRUE)
max_rate   <- max(df_rate$val, na.rm = TRUE)
scaling_factor <- max_number / max_rate

# ---------- 3. 四个地区配色 ----------
location_colors <- c(
  "China"                    = "#0072B2",   # 深蓝
  "Japan"                    = "#D55E00",   # 橙
  "United Kingdom"           = "#56B4E9",   # 天蓝
  "United States of America" = "#009E73"    # 墨绿
)

# ---------- 4. 手动设置左右轴刻度 ----------
# 左轴：Number，单位百万 (M)
max_number_mil <- max_number / 1e6
breaks_number <- seq(0, ceiling(max_number_mil), by = 1) * 1e6
labels_number <- paste0(breaks_number / 1e6, "M")

# 右轴：Rate
max_rate_raw <- max_rate
if (max_rate_raw > 25000) {
  step <- 10000
} else if (max_rate_raw > 15000) {
  step <- 5000
} else {
  step <- 2000
}
breaks_rate_raw <- seq(0, ceiling(max_rate_raw / step) * step, by = step)
if (tail(breaks_rate_raw, 1) > max_rate_raw + step) {
  breaks_rate_raw <- head(breaks_rate_raw, -1)
}

# ---------- 5. 绘图参数 ----------
dodge_width <- 0.7

# ---------- 6. 绘制图形（核心修改：优化置信区间阴影） ----------
p <- ggplot() +
  # 柱状图 (Number)
  geom_col(data = df_number,
           aes(x = x_numeric, y = val, fill = location_name),
           position = position_dodge(width = dodge_width),
           width = 0.6, color = "black", linewidth = 0.2) +
  # 折线图 (Rate)
  geom_line(data = df_rate,
            aes(x = x_numeric, y = val * scaling_factor,
                color = location_name, group = location_name),
            linewidth = 1) +  # 略微加粗主折线
  # Rate 置信区间阴影（优化后）
  geom_ribbon(data = df_rate,
              aes(x = x_numeric,
                  ymin = lower * scaling_factor,
                  ymax = upper * scaling_factor,
                  fill = location_name, 
                  color = location_name,  # 边框颜色跟随地区
                  group = location_name),
              alpha = 0.05,               # 大幅降低透明度
              linetype = "dashed",        # 虚线边框
              linewidth = 0.3,            # 细边框
              show.legend = FALSE) +
  # 双轴设置
  scale_y_continuous(
    name = "Incidence number (in millions)",
    breaks = breaks_number,
    labels = labels_number,
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(
      trans = ~ . / scaling_factor,
      name = "Age-standardized incidence rate per 100,000",
      breaks = breaks_rate_raw,
      labels = label_number(accuracy = 1)
    )
  ) +
  # 横坐标
  scale_x_continuous(
    breaks = 1:7,
    labels = levels(df_filtered$age_name),
    name = "Age Group"
  ) +
  # 颜色映射
  scale_fill_manual(values = location_colors, name = "Location") +
  scale_color_manual(values = location_colors, name = "Location") +
  # 主题美化
  theme_minimal(base_size = 11) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.title.y.right = element_text(color = "black", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9),
    axis.text.y = element_text(size = 8),
    axis.text.y.right = element_text(size = 8),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "grey70", linewidth = 0.3),
    axis.line = element_line(color = "black", linewidth = 0.3)
  )

# 显示图形
print(p)

# ---------- 7. 保存图形 ----------
ggsave("MSK_Four_Countries_2023_15-49_Age_Incidence.pdf", p, width = 11, height = 7)
ggsave("MSK_Four_Countries_2023_15-49_Age_Incidence.tiff", p, width = 11, height = 7,
       dpi = 600, compression = "lzw", bg = "white")

cat("骨骼肌肉疾病双轴图已保存为 PDF 和 TIFF 格式。\n")