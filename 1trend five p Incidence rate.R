library(tidyverse)
library(scales)
library(ggsci)
library(patchwork)
library(cowplot)

setwd("D:\\R\\Musculoskeletal Disorders")
df <- read.csv("data1.csv", header = TRUE)

# ----- 基础过滤：15-49岁女性，Incidence Rate -----
df_base <- df %>% 
  filter(measure_name == "Incidence") %>%
  filter(location_id %in% c(1, 6, 67, 95, 102)) %>%
  filter(age_id == 24) %>%
  filter(metric_name == "Rate") %>%
  filter(sex_name == "Female")

# ----- 提取除 Other 外的5种疾病（因为 Other 在 Incidence 中无数据）-----
sub_causes <- c("Rheumatoid arthritis", "Osteoarthritis", "Low back pain", 
                "Neck pain", "Gout")   # 注意：不含 Other

df_sub <- df_base %>% 
  filter(cause_name %in% sub_causes)

# ----- 排序：与 Prevalence 图的前5个顺序完全一致 ----
# 直接从 Prevalence 数据中获取顺序（避免重复计算，但这里重新计算以确保一致）
# 使用与 Prevalence 图完全相同的逻辑：按全球2023年患病数降序
order_no_other <- df %>%
  filter(measure_name == "Prevalence",
         location_id == 1,
         year == 2023,
         age_id == 24,
         metric_name == "Number",
         sex_name == "Female",
         cause_name %in% sub_causes) %>%
  group_by(cause_name) %>%
  summarise(total_global_2023 = sum(val)) %>%
  arrange(desc(total_global_2023)) %>%
  pull(cause_name)

# 将因子水平设为该顺序
df_sub$cause_name <- factor(df_sub$cause_name, levels = order_no_other)

# ----- 设置地区顺序（Global, China, USA, UK, Japan）-----
location_order <- c(1, 6, 67, 95, 102)
loc_names <- df_sub %>%
  distinct(location_id, location_name) %>%
  arrange(match(location_id, location_order)) %>%
  pull(location_name)

df_sub <- df_sub %>%
  mutate(location_name = factor(location_name, levels = loc_names))

# 检查是否有全部为0的疾病（一般不会，但以防万一，不做剔除，仅保留）
# 如果有全零，但因子水平保留，图例仍会显示（但数据为0不会出现条块）

# ----- 定义统一的主题（子图不显示图例，x轴标签旋转45度）-----
my_theme <- theme_classic() +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    legend.position = "none",
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
    plot.margin = margin(5, 5, 5, 5)
  )

# ----- 为每个地区单独绘图（堆叠条形图）-----
plots <- list()
for (i in seq_along(loc_names)) {
  loc <- loc_names[i]
  
  # 纵轴标签精简
  y_label <- "Incidence rate (per 100,000)"
  
  p <- df_sub %>%
    filter(location_name == loc) %>%
    ggplot(aes(x = year, y = val, fill = cause_name)) +
    geom_bar(stat = "identity", position = "stack") +
    my_theme +
    labs(x = "Year", y = y_label) +
    scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
    scale_y_continuous(labels = label_number(accuracy = 1)) +
    scale_fill_npg(name = "Cause") +   # 颜色自动按因子顺序分配，与前5个一致
    ggtitle(loc)
  plots[[loc]] <- p
}

# ----- 提取共享图例 -----
legend_plot <- df_sub %>%
  ggplot(aes(x = year, y = val, fill = cause_name)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_npg(name = "Cause")
legend <- cowplot::get_legend(legend_plot)

# ----- 拼图布局：2行3列，右下角空白占位-----
blank_plot <- plot_spacer()
design <- "
  ABC
  DEF
"
combined <- wrap_plots(A = plots[[1]], B = plots[[2]], C = plots[[3]],
                       D = plots[[4]], E = plots[[5]], F = blank_plot,
                       design = design) +
  plot_annotation(theme = theme(plot.margin = margin())) &
  theme(plot.title = element_text(size = 12, hjust = 0.5))

final_plot <- combined + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")

# ----- 保存为PDF -----
ggsave("Incidence_rate_stack_5causes.pdf", final_plot, 
       width = 14, height = 9, dpi = 300, device = cairo_pdf)