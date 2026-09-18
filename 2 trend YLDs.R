# ============================================================
# YLDs 专项分析（顶刊风格细化版）
# 数据源：data1.csv
# 输出：YLDs_Analysis_Advanced.pdf
# ============================================================

library(tidyverse)
library(ggsci)
library(patchwork)
library(scales)
library(cowplot)

setwd("D:\\R\\Musculoskeletal Disorders")
df <- read.csv("data1.csv", header = TRUE)

# ---------- 1. 基础设置 ----------
location_levels <- c("Global", "China", "Japan", 
                     "United Kingdom", "United States of America")

# 提取YLDs数据
df_ylds <- df %>%
  filter(measure_name == "YLDs (Years Lived with Disability)") %>%
  filter(location_id %in% c(1, 6, 67, 95, 102)) %>%
  filter(age_id == 24) %>%
  filter(sex_name == "Female") %>%
  filter(cause_name == "Musculoskeletal disorders") %>%
  mutate(location_name = factor(location_name, levels = location_levels))

df_ylds_num <- df_ylds %>% filter(metric_name == "Number")
df_ylds_rate <- df_ylds %>% filter(metric_name == "Rate")

# ---------- 2. 定义顶刊风格主题 ----------
theme_advanced <- function(base_size = 11) {
  theme_classic(base_size = base_size) %+replace%
    theme(
      # 文本与标题
      plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0, margin = margin(b = 10)),
      axis.title = element_text(size = rel(0.9), face = "bold"),
      axis.text = element_text(size = rel(0.8), color = "grey30"),
      # 坐标轴
      axis.line = element_line(color = "grey40", linewidth = 0.5),
      axis.ticks = element_line(color = "grey40", linewidth = 0.5),
      # 网格线（只保留主要水平线，淡化处理）
      panel.grid.major.y = element_line(color = "grey92", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      # 图例
      legend.position = "bottom",
      legend.title = element_text(size = rel(0.8), face = "bold"),
      legend.text = element_text(size = rel(0.8)),
      legend.key.size = unit(0.6, "cm"),
      # 分面（facet）标签
      strip.background = element_blank(),
      strip.text = element_text(size = rel(0.9), face = "bold", margin = margin(b = 5))
    )
}

# ---------- 3. 图1：YLDs Number 趋势 ----------
p1 <- ggplot(df_ylds_num, aes(x = year, y = val, color = location_name, fill = location_name)) +
  # 置信区间阴影：透明度降低，并添加细边框
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, linetype = "dashed", color = "grey70", size = 0.2) +
  geom_line(size = 0.8) +
  geom_point(size = 1.2, alpha = 0.6) +
  scale_color_npg() +
  scale_fill_npg() +
  labs(x = "Year", y = "YLDs cases", title = "A") +
  theme_advanced()

# ---------- 4. 图2：YLDs Rate 趋势 ----------
p2 <- ggplot(df_ylds_rate, aes(x = year, y = val, color = location_name, fill = location_name)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, linetype = "dashed", color = "grey70", size = 0.2) +
  geom_line(size = 0.8) +
  geom_point(size = 1.2, alpha = 0.6) +
  scale_color_npg() +
  scale_fill_npg() +
  labs(x = "Year", y = "Age-standardized YLDs rate (per 100,000)", title = "B") +
  theme_advanced()

# ---------- 5. 图3：2023年各亚型 YLDs Number 构成 ----------
df_ylds_sub <- df %>%
  filter(measure_name == "YLDs (Years Lived with Disability)") %>%
  filter(location_id %in% c(1, 6, 67, 95, 102)) %>%
  filter(age_id == 24) %>%
  filter(sex_name == "Female") %>%
  filter(metric_name == "Number") %>%
  filter(year == 2023) %>%
  filter(cause_name != "Musculoskeletal disorders") %>%
  filter(cause_name %in% c("Rheumatoid arthritis", "Osteoarthritis", 
                           "Low back pain", "Neck pain", "Gout", 
                           "Other musculoskeletal disorders")) %>%
  mutate(location_name = factor(location_name, levels = location_levels))

df_ylds_sub_prop <- df_ylds_sub %>%
  group_by(location_name) %>%
  mutate(total = sum(val, na.rm = TRUE),
         proportion = val / total * 100) %>%
  ungroup()

# 为堆叠图使用稍微明亮的NPG配色
p3 <- ggplot(df_ylds_sub_prop, aes(x = location_name, y = proportion, fill = cause_name)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  scale_fill_npg(name = "Cause") +
  labs(x = "", y = "Proportion of YLDs (%)", title = "C") +
  theme_advanced() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# ---------- 6. 图4（可选）：YLDs Rate vs SDI ----------
if ("sdi" %in% names(df)) {
  df_sdi <- df %>%
    filter(measure_name == "YLDs (Years Lived with Disability)") %>%
    filter(location_id %in% c(1, 6, 67, 95, 102)) %>%
    filter(age_id == 24) %>%
    filter(sex_name == "Female") %>%
    filter(metric_name == "Rate") %>%
    filter(cause_name == "Musculoskeletal disorders") %>%
    filter(year == 2023) %>%
    select(location_name, sdi, val) %>%
    drop_na()
  
  p4 <- ggplot(df_sdi, aes(x = sdi, y = val, color = location_name)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed", fill = "grey80") +
    scale_color_npg() +
    labs(x = "Socio-demographic Index (SDI)", 
         y = "YLDs rate (per 100,000)", 
         title = "D") +
    theme_advanced() +
    theme(legend.position = "bottom")
} else {
  p4 <- NULL
}

# ---------- 7. 高级组合与保存 ----------
# 使用 plot_annotation 添加总标题，并精细调整布局
combined_top <- (p1 + p2) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

if (!is.null(p4)) {
  combined_bottom <- (p3 + p4) + 
    plot_layout(guides = "collect") & 
    theme(legend.position = "bottom")
  final_plot <- (combined_top / combined_bottom) + 
    plot_layout(heights = c(1, 0.9)) +
    plot_annotation(title = "YLDs Burden of Musculoskeletal Disorders",
                    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)))
} else {
  final_plot <- (combined_top / p3) + 
    plot_layout(heights = c(1, 0.7)) +
    plot_annotation(title = "YLDs Burden of Musculoskeletal Disorders",
                    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)))
}

# 保存为高分辨率PDF
ggsave("YLDs_Analysis_Advanced.pdf", final_plot, 
       width = 14, height = 12, dpi = 300)
cat("YLDs分析（顶刊风格细化版）已保存为 YLDs_Analysis_Advanced.pdf\n")