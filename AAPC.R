# 加载必要的包
library(tidyverse)
library(ggsci)
library(patchwork)
library(segmented)
library(cowplot)

# 设置工作目录（请根据实际路径修改）
setwd("D:\\R\\depress")

# 读取数据
df <- read.csv("data1.csv", header = TRUE)

# ---------- 1. 数据筛选 ----------
df_inc <- df %>%
  filter(measure_name == "Incidence") %>%
  filter(location_id %in% c(1, 6, 67, 95, 102)) %>%
  filter(age_id == 24) %>%
  filter(metric_name == "Rate") %>%
  filter(sex_name == "Female") %>%
  filter(cause_name != "Mental disorders")

# 剔除所有 val 为 0 的疾病（例如 Other mental disorders 和 ADHD）
zero_causes <- df_inc %>%
  group_by(cause_name) %>%
  summarise(all_zero = all(val == 0, na.rm = TRUE)) %>%
  filter(all_zero == TRUE) %>%
  pull(cause_name)
if (length(zero_causes) > 0) {
  message("剔除零值疾病：", paste(zero_causes, collapse = ", "))
  df_inc <- df_inc %>% filter(!cause_name %in% zero_causes)
}

# 确保地区顺序（按 location_id: 1,6,67,95,102）
location_order <- c(1, 6, 67, 95, 102)
loc_names <- df_inc %>%
  distinct(location_id, location_name) %>%
  arrange(match(location_id, location_order)) %>%
  pull(location_name)

df_inc <- df_inc %>%
  mutate(location_name = factor(location_name, levels = loc_names))

# ---------- 2. 颜色映射（与 prevalence 图一致，使用 NPG 10 色）----------
all_causes_prevalence <- c("Anxiety disorders",
                           "Attention-deficit/hyperactivity disorder",
                           "Autism spectrum disorders",
                           "Bipolar disorder",
                           "Conduct disorder",
                           "Depressive disorders",
                           "Eating disorders",
                           "Idiopathic developmental intellectual disability",
                           "Other mental disorders",
                           "Schizophrenia")
npg_10 <- c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF",
            "#8491B4FF", "#91D1C2FF", "#DC0000FF", "#7E6148FF", "#B09C85FF")
full_color_map <- setNames(npg_10, all_causes_prevalence)

# 只保留 incidence 中实际存在的疾病
present_causes <- unique(df_inc$cause_name)
color_subset <- full_color_map[present_causes]

# ---------- 3. 分段回归及 AAPC 计算函数（修正列名）----------
calc_aapc <- function(data, psi = c(1997, 2005, 2012)) {
  if (nrow(data) < 5) return(NA_character_)   # 数据点太少，跳过
  lm_fit <- glm(val ~ year, data = data)
  seg_fit <- tryCatch({
    segmented(lm_fit, seg.Z = ~ year, psi = psi)
  }, error = function(e) NULL)
  if (is.null(seg_fit)) return(NA_character_)
  
  # 提取断点
  bp <- seg_fit$psi[, "Est."]
  if (length(bp) == 0) return(NA_character_)
  
  # 获取各段斜率及标准误
  sl <- slope(seg_fit)
  if (!"year" %in% names(sl)) return(NA_character_)
  sl_mat <- sl$year
  
  # 检查必需列
  if (!"Est." %in% colnames(sl_mat)) return(NA_character_)
  # 标准误列名可能是 "St.Err."（诊断输出显示带点）
  se_col <- intersect(c("St.Err.", "St.Err", "Std. Error"), colnames(sl_mat))
  if (length(se_col) == 0) return(NA_character_)
  
  slopes <- sl_mat[, "Est."]
  se_slopes <- sl_mat[, se_col[1]]
  
  # 计算加权平均斜率
  years <- sort(unique(data$year))
  min_year <- min(years)
  max_year <- max(years)
  breakpoints_all <- sort(c(min_year, bp, max_year))
  seg_durations <- diff(breakpoints_all)
  total_years <- max_year - min_year
  avg_slope <- sum(slopes * seg_durations) / total_years
  avg_se <- sqrt(sum((se_slopes * seg_durations)^2)) / total_years
  
  aapc <- 100 * avg_slope
  aapc_lower <- 100 * (avg_slope - 1.96 * avg_se)
  aapc_upper <- 100 * (avg_slope + 1.96 * avg_se)
  sprintf("%.3f(%.3f-%.3f)", aapc, aapc_lower, aapc_upper)
}

# ---------- 4. 计算 AAPC 表格 ----------
aapc_results <- df_inc %>%
  group_by(location_name, cause_name) %>%
  group_modify(~ tibble(AAPC = calc_aapc(.x))) %>%
  ungroup()

print(aapc_results)
write.csv(aapc_results, "AAPC_incidence_by_disease.csv", row.names = FALSE)

# ---------- 5. 绘制每个地区的多疾病折线图（顶刊风格）----------
# 主题设置
base_theme <- theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 5)),
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

# 生成每个地区的图
plots <- list()
for (loc in loc_names) {
  p <- df_inc %>%
    filter(location_name == loc) %>%
    ggplot(aes(x = year, y = val, color = cause_name)) +
    geom_line(size = 0.8) +
    geom_point(size = 1.2, alpha = 0.6) +
    base_theme +
    labs(x = "Year", 
         y = str_wrap("Age-standardized incidence rate (per 100,000)", width = 20),
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

# 布局：2行3列，右下角空白
design <- "ABC\nDEF"
blank_plot <- plot_spacer()
combined <- wrap_plots(A = plots[[1]], B = plots[[2]], C = plots[[3]],
                       D = plots[[4]], E = plots[[5]], F = blank_plot,
                       design = design) +
  plot_annotation(theme = theme(plot.margin = margin(10, 5, 5, 5))) &
  theme(plot.title = element_text(size = 12, hjust = 0.5))

final_plot <- combined + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")

# 显示并保存
print(final_plot)
ggsave("Incidence_trends_by_disease_NPG.pdf", final_plot, width = 14, height = 9, dpi = 300, device = cairo_pdf)