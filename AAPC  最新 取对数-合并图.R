# ================================================================
# 完整脚本：骨骼肌肉疾病亚型发病率时间趋势分析
# - 出版级图表：子图仅标字母，疾病名在图注中说明
# - 真正的 AAPC（对数转换 + 分段线性回归）
# ================================================================

# ---- 0. 自动安装并加载所需包 ----
packages <- c("tidyverse", "ggsci", "patchwork", "segmented", "cowplot", "ggh4x")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ---- 1. 设置工作目录及读入数据 ----
setwd("D:\\R\\Musculoskeletal Disorders")  # 请修改为你的实际路径
df <- read.csv("data1.csv", header = TRUE)

# ---- 2. 数据筛选 ----
df_inc <- df %>%
  filter(measure_name == "Incidence") %>%
  filter(location_id %in% c(1, 6, 67, 95, 102)) %>%
  filter(age_id == 24) %>%
  filter(metric_name == "Rate") %>%
  filter(sex_name == "Female") %>%
  filter(cause_name != "Musculoskeletal disorders")

sub_causes <- c("Rheumatoid arthritis", "Osteoarthritis", "Low back pain", 
                "Neck pain", "Gout", "Other musculoskeletal disorders")
df_inc <- df_inc %>% filter(cause_name %in% sub_causes)

# ---- 3. 确定疾病显示顺序（按全球患病数降序） ----
order_no_other <- df %>%
  filter(measure_name == "Prevalence",
         location_id == 1,
         year == 2023,
         age_id == 24,
         metric_name == "Number",
         sex_name == "Female",
         cause_name %in% sub_causes,
         cause_name != "Other musculoskeletal disorders") %>%
  group_by(cause_name) %>%
  summarise(total = sum(val)) %>%
  arrange(desc(total)) %>%
  pull(cause_name)
final_order <- c(order_no_other, "Other musculoskeletal disorders")
present_causes <- unique(df_inc$cause_name)
present_causes <- final_order[final_order %in% present_causes]  # 实际存在的疾病

# ---- 4. 地区因子顺序 ----
location_order <- c(1, 6, 67, 95, 102)
loc_names <- df_inc %>%
  distinct(location_id, location_name) %>%
  arrange(match(location_id, location_order)) %>%
  pull(location_name)
df_inc <- df_inc %>%
  mutate(location_name = factor(location_name, levels = loc_names))

# ---- 5. AAPC 计算函数（对数转换 + 分段线性回归） ----
calc_aapc <- function(data, psi = c(1997, 2005, 2012)) {
  if (nrow(data) < 5) return(NA_character_)
  
  lm_fit <- glm(log(val) ~ year, data = data)
  seg_fit <- tryCatch(segmented(lm_fit, seg.Z = ~ year, psi = psi), error = function(e) NULL)
  if (is.null(seg_fit)) return(NA_character_)
  
  bp <- seg_fit$psi[, "Est."]
  if (length(bp) == 0) return(NA_character_)
  
  sl <- slope(seg_fit)
  if (!"year" %in% names(sl)) return(NA_character_)
  sl_mat <- sl$year
  if (!"Est." %in% colnames(sl_mat)) return(NA_character_)
  se_col <- intersect(c("St.Err.", "St.Err", "Std. Error"), colnames(sl_mat))
  if (length(se_col) == 0) return(NA_character_)
  
  slopes <- sl_mat[, "Est."]
  se_slopes <- sl_mat[, se_col[1]]
  years <- sort(unique(data$year))
  min_year <- min(years); max_year <- max(years)
  breakpoints_all <- sort(c(min_year, bp, max_year))
  seg_durations <- diff(breakpoints_all)
  total_years <- max_year - min_year
  
  avg_slope <- sum(slopes * seg_durations) / total_years
  avg_se <- sqrt(sum((se_slopes * seg_durations)^2)) / total_years
  
  aapc <- (exp(avg_slope) - 1) * 100
  aapc_lower <- (exp(avg_slope - 1.96 * avg_se) - 1) * 100
  aapc_upper <- (exp(avg_slope + 1.96 * avg_se) - 1) * 100
  
  sprintf("%.2f (%.2f-%.2f)", aapc, aapc_lower, aapc_upper)
}

# ---- 6. 计算并保存 AAPC 表格 ----
aapc_results <- df_inc %>%
  group_by(location_name, cause_name) %>%
  group_modify(~ tibble(AAPC = calc_aapc(.x))) %>%
  ungroup()
write.csv(aapc_results, "AAPC_Incidence_MSK_subcauses.csv", row.names = FALSE)

# ---- 7. 出版级趋势图（子图仅标字母，疾病名在图注中） ----

# 配色（Lancet 风格）
library(ggsci)
region_colors <- pal_lancet()(5)
names(region_colors) <- loc_names

# 主题（干净，带浅横向网格）
base_theme <- theme_classic(base_size = 12, base_family = "sans") +
  theme(
    plot.tag = element_text(size = 14, face = "bold", hjust = 0, vjust = 0),
    plot.tag.position = "topleft",
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 9, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    axis.line = element_line(linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),          # 不使用分面标题
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.key.size = unit(1.2, "cm"),
    legend.spacing.x = unit(0.2, "cm"),
    plot.margin = margin(5, 5, 5, 5)
  )

# 生成各子图（仅带字母标签）
plot_list <- list()
for (i in seq_along(present_causes)) {
  disease <- present_causes[i]
  p <- df_inc %>%
    filter(cause_name == disease) %>%
    ggplot(aes(x = year, y = val, color = location_name)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2, alpha = 0.5) +
    labs(
      tag = letters[i],          # 只显示字母
      x = "Year",
      y = "ASIR (per 100,000)"
    ) +
    scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
    scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
    scale_color_manual(values = region_colors, name = "Region") +
    base_theme +
    theme(legend.position = "none")
  plot_list[[disease]] <- p
}

# 提取共享图例（加粗图例线条）
legend_plot <- df_inc %>%
  ggplot(aes(x = year, y = val, color = location_name)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = region_colors, name = "Region") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 9),
        legend.key.size = unit(1.2, "cm")) +
  guides(color = guide_legend(override.aes = list(linewidth = 1.5)))
shared_legend <- cowplot::get_legend(legend_plot)

# 组合5个图（2行：上3下2，右下留空）
design <- "
  ABC
  DEF
"
blank <- plot_spacer()
combined <- wrap_plots(
  A = plot_list[[1]], B = plot_list[[2]], C = plot_list[[3]],
  D = plot_list[[4]], E = plot_list[[5]], F = blank,
  design = design
) +
  plot_annotation(
    title = "Trends in age‑standardised incidence rates of musculoskeletal disorder subtypes, 1990–2023",
    subtitle = paste0(
      "a) ", present_causes[1], "; b) ", present_causes[2], "; c) ", present_causes[3],
      "; d) ", present_causes[4], "; e) ", present_causes[5]
    ),
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 4)),
      plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 10))
    )
  )

# 组合图与图例拼接
final_plot <- combined / shared_legend +
  plot_layout(heights = c(9, 1))

# ---- 8. 显示并保存 ----
print(final_plot)
ggsave("Incidence_MSK_by_disease_topstyle.pdf", final_plot,
       width = 15, height = 9, dpi = 300, device = cairo_pdf)