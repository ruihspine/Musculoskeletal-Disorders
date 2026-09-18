# ============================================================
# 骨骼肌肉疾病（MSK）风险因素归因分析（最终完整版）
# 数据：GBD 2021 Risk Factors (YLDs, Rate, 15-49岁女性)
# 输出：
#   1. MSK_Risk_Trends_Subtypes.pdf - 时间趋势图（所有面板显示横轴，单组图例）
#   2. MSK_Risk_Combined.pdf - 构成图（上）+ 热图（下），热图列宽已调小
#   3. CSV表格文件（用于论文正文/补充材料）
# ============================================================

# 加载包
library(tidyverse)
library(ggsci)
library(patchwork)
library(cowplot)
library(scales)
library(pheatmap)
library(RColorBrewer)
library(ggh4x)

# 设置工作目录
setwd("D:\\R\\Musculoskeletal Disorders")

# 读取数据
df <- read.csv("data2.csv", header = TRUE)

# ============================================================
# 1. 数据清洗
# ============================================================
df_clean <- df %>%
  filter(measure_name == "YLDs (Years Lived with Disability)") %>%
  filter(metric_name == "Rate") %>%
  filter(sex_name == "Female") %>%
  filter(age_id == 24) %>%
  filter(cause_name != "Musculoskeletal disorders")

# 排除父级风险因素
exclude_risks <- c("Environmental/occupational risks", 
                   "Occupational risks", 
                   "Behavioral risks", 
                   "Tobacco", 
                   "Metabolic risks")
df_clean <- df_clean %>%
  filter(!rei_name %in% exclude_risks)

cat("保留的具体风险因素:\n")
print(unique(df_clean$rei_name))

# ============================================================
# 2. 风险因素-疾病清单（输出表格）
# ============================================================
risk_inventory <- df_clean %>%
  filter(val > 0) %>%
  distinct(cause_name, rei_name) %>%
  arrange(cause_name, rei_name)

risk_count <- risk_inventory %>%
  group_by(cause_name) %>%
  summarise(Risk_Factors = n())

write.csv(risk_inventory, "MSK_Risk_Inventory_Subtypes.csv", row.names = FALSE)
write.csv(risk_count, "MSK_Risk_Count_Subtypes.csv", row.names = FALSE)

# ============================================================
# 3. 地区顺序和风险分组
# ============================================================
location_levels <- c("Global", "China", "Japan", 
                     "United Kingdom", "United States of America")
df_clean <- df_clean %>%
  mutate(location_name = factor(location_name, levels = location_levels))

# 选出主要风险因素（前8位，这里只有4个，全部保留）
top_risks <- df_clean %>%
  group_by(rei_name) %>%
  summarise(total_burden = sum(val, na.rm = TRUE)) %>%
  arrange(desc(total_burden)) %>%
  slice_head(n = 8) %>%
  pull(rei_name)

df_clean <- df_clean %>%
  mutate(risk_group = ifelse(rei_name %in% top_risks, rei_name, "Other"))

# ============================================================
# 4. 时间趋势图（所有面板显示横轴，单组图例右侧）
# ============================================================
base_theme <- theme_classic(base_size = 10) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "none"
  )

plot_list <- list()
for (loc in location_levels) {
  p <- df_clean %>%
    filter(location_name == loc) %>%
    ggplot(aes(x = year, y = val, color = risk_group)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 0.8, alpha = 0.5) +
    facet_wrap2(~ cause_name, 
                scales = "free_y", 
                ncol = 2,
                axes = "all") +
    base_theme +
    labs(x = "Year", 
         y = "Age-standardized YLDs rate (per 100,000)") +
    scale_color_npg() +
    ggtitle(loc) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
  plot_list[[loc]] <- p
}

# 组合五个地区（2行3列，右下空白）
blank <- plot_spacer()
design <- "ABC\nDEF"
combined_trend <- wrap_plots(A = plot_list[[1]], B = plot_list[[2]], C = plot_list[[3]],
                             D = plot_list[[4]], E = plot_list[[5]], F = blank,
                             design = design) +
  plot_annotation(theme = theme(plot.margin = margin(5, 5, 5, 5))) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.box = "vertical")

ggsave("MSK_Risk_Trends_Subtypes.pdf", combined_trend, 
       width = 18, height = 12, dpi = 300)
cat("时间趋势图已保存: MSK_Risk_Trends_Subtypes.pdf\n")

# ============================================================
# 5. 2023年风险因素构成图（所有面板显示横轴，图例右侧）
# ============================================================
df_2023 <- df_clean %>% filter(year == 2023)

df_2023_prop <- df_2023 %>%
  group_by(location_name, cause_name) %>%
  mutate(total_rate = sum(val, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(proportion = val / total_rate * 100)

p_stack <- df_2023_prop %>%
  filter(risk_group != "Other") %>%
  ggplot(aes(x = cause_name, y = proportion, fill = risk_group)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap2(~ location_name, 
              ncol = 2,
              axes = "all") +
  labs(x = "", y = "Proportion of attributable YLDs rate (%)", 
       fill = "Risk factor") +
  scale_fill_npg() +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.box = "vertical") +
  guides(fill = guide_legend(ncol = 1))

# ============================================================
# 6. 热图（2023年，列宽调小以显示左侧聚类树）
# ============================================================
df_heat <- df_2023 %>%
  filter(risk_group != "Other") %>%
  group_by(location_name, cause_name, risk_group) %>%
  summarise(mean_rate = mean(val, na.rm = TRUE), .groups = "drop") %>%
  unite("loc_cause", location_name, cause_name, sep = " | ") %>%
  pivot_wider(names_from = loc_cause, values_from = mean_rate, values_fill = 0) %>%
  column_to_rownames(var = "risk_group")

# 生成热图（关键：cellwidth = 28，使热图变窄，左侧留出空间）
heatmap_grob <- pheatmap(log1p(df_heat),
                         main = "Risk factor attribution to MSK subtypes by region (2023)",
                         color = colorRampPalette(rev(brewer.pal(9, "RdBu")))(50),
                         border_color = NA,
                         cellwidth = 28,      # 调小列宽，左侧聚类树完整显示
                         cellheight = 18,
                         fontsize = 8,
                         cluster_rows = TRUE,
                         cluster_cols = TRUE,
                         silent = TRUE)

# 使用 wrap_elements 包装为可组合元素
p_heatmap <- wrap_elements(heatmap_grob$gtable)

# ============================================================
# 7. 组合构成图 + 热图（上下排列，高度比例保持不变）
# ============================================================
combined_bottom <- (p_stack / p_heatmap) +
  plot_layout(heights = c(1.2, 1)) +
  plot_annotation(theme = theme(plot.margin = margin(5, 5, 5, 5)))

ggsave("MSK_Risk_Combined.pdf", combined_bottom, 
       width = 14, height = 12, dpi = 300)
cat("合并图已保存: MSK_Risk_Combined.pdf\n")

# ============================================================
# 8. 表格输出（用于论文正文/补充材料）
# ============================================================
# 2023年各疾病各风险因素归因率（按地区）
table_2023 <- df_2023 %>%
  filter(risk_group != "Other") %>%
  group_by(location_name, cause_name, risk_group) %>%
  summarise(ylds_rate = round(sum(val, na.rm = TRUE), 2), .groups = "drop") %>%
  pivot_wider(names_from = location_name, 
              values_from = ylds_rate, 
              values_fill = 0)

write.csv(table_2023, "MSK_Risk_Attribution_2023_Table.csv", row.names = FALSE)
cat("2023年归因率表格已保存: MSK_Risk_Attribution_2023_Table.csv\n")

# 汇总统计
summary_stats <- df_clean %>%
  group_by(cause_name, location_name) %>%
  summarise(
    max_rate = max(val, na.rm = TRUE),
    mean_rate = mean(val, na.rm = TRUE),
    total_burden = sum(val, na.rm = TRUE),
    .groups = "drop"
  )
write.csv(summary_stats, "MSK_Risk_Summary_Stats.csv", row.names = FALSE)
cat("汇总统计已保存: MSK_Risk_Summary_Stats.csv\n")

# ============================================================
# 9. 完成提示
# ============================================================
cat("\n========== 所有分析完成！==========\n")
cat("生成文件清单：\n")
cat("1. MSK_Risk_Trends_Subtypes.pdf - 时间趋势图（所有面板显示横轴，单组图例右侧）\n")
cat("2. MSK_Risk_Combined.pdf - 构成图（上）+ 热图（下），热图列宽已调小\n")
cat("3. MSK_Risk_Inventory_Subtypes.csv - 各疾病风险因素清单（补充材料）\n")
cat("4. MSK_Risk_Count_Subtypes.csv - 各疾病风险因素个数\n")
cat("5. MSK_Risk_Attribution_2023_Table.csv - 2023年归因率表格（正文表格）\n")
cat("6. MSK_Risk_Summary_Stats.csv - 汇总统计（补充材料）\n")