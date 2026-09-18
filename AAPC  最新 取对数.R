# ============================================================
# 骨骼肌肉疾病各子类 Incidence Rate 时间趋势
# 每个地区独立分面，所有面板显示横轴，疾病通过图例区分
# 计算真正的 AAPC（基于对数转换的分段线性回归）
# ============================================================

# 自动安装缺失包
packages <- c("tidyverse", "ggsci", "patchwork", "segmented", "cowplot", "ggh4x")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# 设置工作目录（请改成你自己的路径）
setwd("D:\\R\\Musculoskeletal Disorders")
df <- read.csv("data1.csv", header = TRUE)

# ---------- 1. 数据筛选 ----------
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

# ---------- 2. 疾病顺序（按全球2023 Prevalence Number 降序，Other 置底） ----------
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
present_causes <- unique(df_inc$cause_name)  # 实际存在的疾病
present_causes <- final_order[final_order %in% present_causes]  # 按顺序

# ---------- 3. 颜色映射（与 Prevalence 堆叠图一致） ----------
npg_colors <- c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", 
                "#F39B7FFF", "#8491B4FF")
full_color_map <- setNames(npg_colors[1:length(final_order)], final_order)
color_subset <- full_color_map[present_causes]

# ---------- 4. 地区顺序 ----------
location_order <- c(1, 6, 67, 95, 102)
loc_names <- df_inc %>%
  distinct(location_id, location_name) %>%
  arrange(match(location_id, location_order)) %>%
  pull(location_name)
df_inc <- df_inc %>%
  mutate(location_name = factor(location_name, levels = loc_names))

# ============================================================
# 5. AAPC 计算函数（改为对数转换，计算真正的百分比变化）
# ============================================================
calc_aapc <- function(data, psi = c(1997, 2005, 2012)) {
  if (nrow(data) < 5) return(NA_character_)
  
  # <<< 改动 1：对 val 取自然对数
  lm_fit <- glm(log(val) ~ year, data = data)   # 原为 glm(val ~ year)
  
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
  
  # 加权平均斜率（对数尺度）
  avg_slope <- sum(slopes * seg_durations) / total_years
  avg_se <- sqrt(sum((se_slopes * seg_durations)^2)) / total_years
  
  # <<< 改动 2：转换为年度百分比变化 (%)
  aapc <- (exp(avg_slope) - 1) * 100
  aapc_lower <- (exp(avg_slope - 1.96 * avg_se) - 1) * 100
  aapc_upper <- (exp(avg_slope + 1.96 * avg_se) - 1) * 100
  
  sprintf("%.2f (%.2f-%.2f)", aapc, aapc_lower, aapc_upper)
}

# ---------- 6. 计算并保存 AAPC 表格 ----------
aapc_results <- df_inc %>%
  group_by(location_name, cause_name) %>%
  group_modify(~ tibble(AAPC = calc_aapc(.x))) %>%
  ungroup()
write.csv(aapc_results, "AAPC_Incidence_MSK_subcauses.csv", row.names = FALSE)

# ---------- 7. 绘图（与原代码相同，只展示原始发病率趋势） ----------
base_theme <- theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    strip.background = element_blank(),
    strip.text = element_blank(),
    legend.position = "none"
  )

plots <- list()
for (loc in loc_names) {
  p <- df_inc %>%
    filter(location_name == loc) %>%
    ggplot(aes(x = year, y = val, color = cause_name)) +
    geom_line(size = 0.8) +
    geom_point(size = 1.2, alpha = 0.6) +
    ggh4x::facet_wrap2(~ cause_name, 
                       scales = "free_y", 
                       ncol = 2,
                       axes = "all") +
    base_theme +
    labs(x = "Year", 
         y = "Age-standardized incidence rate (per 100,000)", 
         title = loc) +
    scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
    scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
    scale_color_manual(values = color_subset, name = "Cause")
  plots[[loc]] <- p
}

# 提取共享图例
legend_plot <- df_inc %>%
  ggplot(aes(x = year, y = val, color = cause_name)) +
  geom_line() +
  scale_color_manual(values = color_subset, name = "Cause") +
  theme(legend.position = "right")
legend <- cowplot::get_legend(legend_plot)

# 组合五个地区图（2行3列，右下空白）
design <- "ABC\nDEF"
blank_plot <- plot_spacer()
combined <- wrap_plots(A = plots[[1]], B = plots[[2]], C = plots[[3]],
                       D = plots[[4]], E = plots[[5]], F = blank_plot,
                       design = design) +
  plot_annotation(theme = theme(plot.margin = margin(10, 5, 5, 5)))

final_plot <- combined + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")

# 显示并保存
print(final_plot)
ggsave("Incidence_MSK_facet_all_axes.pdf", final_plot, 
       width = 15, height = 12, dpi = 300, device = cairo_pdf)

# ============================================================
# 温馨提示：
# 现在 AAPC 结果是真正的年度百分比变化（%），叙述时请加 % 符号。
# 例如：“全球腰痛的 AAPC 为 -1.50%（95% CI：-1.82% ~ -1.18%）”
# ============================================================