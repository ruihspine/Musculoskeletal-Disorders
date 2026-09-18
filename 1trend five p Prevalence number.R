library(tidyverse)
library(scales)
library(ggsci)
library(patchwork)
library(cowplot)

setwd("D:\\R\\Musculoskeletal Disorders")
df <- read.csv("data1.csv", header = TRUE)

# ----- 基础过滤：15-49岁女性，Prevalence Number -----
df_base <- df %>% 
  filter(measure_name == "Prevalence") %>%
  filter(location_id %in% c(1, 6, 67, 95, 102)) %>%
  filter(age_id == 24) %>%
  filter(metric_name == "Number") %>%
  filter(sex_name == "Female")

# ----- 提取骨骼肌肉疾病的6个子类（排除总类）-----
sub_causes <- c("Rheumatoid arthritis", "Osteoarthritis", "Low back pain", 
                "Neck pain", "Gout", "Other musculoskeletal disorders")

df_sub <- df_base %>% 
  filter(cause_name %in% sub_causes)

# ----- 排序：除 Other 外的5种按全球2023年患病数降序，Other 置底 ----
sub_causes_no_other <- sub_causes[sub_causes != "Other musculoskeletal disorders"]

order_no_other <- df_sub %>%
  filter(location_id == 1, year == 2023, cause_name %in% sub_causes_no_other) %>%
  group_by(cause_name) %>%
  summarise(total_global_2023 = sum(val)) %>%
  arrange(desc(total_global_2023)) %>%
  pull(cause_name)

# 最终因子水平：前5种 + Other（最后）
final_order <- c(order_no_other, "Other musculoskeletal disorders")
df_sub$cause_name <- factor(df_sub$cause_name, levels = final_order)

# ----- 设置地区顺序（Global, China, USA, UK, Japan）-----
location_order <- c(1, 6, 67, 95, 102)
loc_names <- df_sub %>%
  distinct(location_id, location_name) %>%
  arrange(match(location_id, location_order)) %>%
  pull(location_name)

df_sub <- df_sub %>%
  mutate(location_name = factor(location_name, levels = loc_names))

# ----- 定义统一的主题（子图不显示图例，x轴标签旋转45度）-----
my_theme <- theme_classic() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9),
    legend.position = "none",
    plot.title = element_text(size = 12, hjust = 0.5)
  )

# ----- 为每个地区单独绘图（堆叠条形图）-----
plots <- list()
for (loc in loc_names) {
  p <- df_sub %>%
    filter(location_name == loc) %>%
    ggplot(aes(x = year, y = val, fill = cause_name)) +
    geom_bar(stat = "identity", position = "stack") +
    my_theme +
    labs(x = "Year", y = "Number of cases") +
    scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
    scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
    scale_fill_npg(name = "Cause") +
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
ggsave("Prevalence_stack_6causes_OtherBottom.pdf", final_plot, 
       width = 14, height = 9, dpi = 300, device = cairo_pdf)