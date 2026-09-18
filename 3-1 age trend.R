# ============================================================
# 完整代码（调整全球图单位为 Millions，刻度更密集）
# ============================================================

library(tidyverse)
library(scales)
library(patchwork)

rm(list = ls())
setwd("D:\\R\\Musculoskeletal Disorders")
df <- read.csv("data1.csv", header = TRUE)

# 配色
location_colors_high <- c(
  "Global"                   = "#1F77B4",
  "China"                    = "#D62728",
  "Japan"                    = "#FF7F0E",
  "United Kingdom"           = "#2CA02C",
  "United States of America" = "#9467BD"
)
four_country_ids <- c(6, 67, 95, 102)
four_country_names <- c("China", "Japan", "United Kingdom", "United States of America")
colors_four <- location_colors_high[four_country_names]

# 绘图函数（增加参数 step_left 用于自定义刻度步长）
plot_msk <- function(location_ids, title, y_left_unit, y_left_div, 
                     show_legend = TRUE, colors = NULL, 
                     global_mode = FALSE, step_left = NULL) {
  # 筛选数据同前...
  df_filtered <- df %>%
    filter(measure_name == "Incidence") %>%
    filter(cause_name == "Musculoskeletal disorders") %>%
    filter(location_id %in% location_ids) %>%
    filter(year == 2023) %>%
    filter(!metric_name == "Percent") %>%
    filter(sex_name == "Female") %>%
    filter(age_id %in% 7:14) %>%
    mutate(age_name = factor(age_name, 
                             levels = c("15-19 years", "20-24 years", "25-29 years",
                                        "30-34 years", "35-39 years", "40-44 years",
                                        "45-49 years")))
  df_number <- df_filtered %>% filter(metric_name == "Number")
  df_rate   <- df_filtered %>% filter(metric_name == "Rate")
  df_number <- df_number %>% mutate(x_numeric = as.numeric(age_name))
  df_rate   <- df_rate   %>% mutate(x_numeric = as.numeric(age_name))
  
  max_number <- max(df_number$val, na.rm = TRUE)
  max_rate   <- max(df_rate$val, na.rm = TRUE)
  scaling_factor <- max_number / max_rate
  
  # 左轴刻度（若提供了 step_left 则用固定步长，否则用 pretty_breaks）
  max_scaled <- max_number / y_left_div
  if (!is.null(step_left)) {
    breaks_number <- seq(0, ceiling(max_scaled / step_left) * step_left, by = step_left) * y_left_div
  } else {
    breaks_number <- scales::pretty_breaks(n = 5)(c(0, max_scaled)) * y_left_div
  }
  if (length(breaks_number) < 2) {
    breaks_number <- seq(0, ceiling(max_scaled), by = 1) * y_left_div
  }
  labels_number <- paste0(breaks_number / y_left_div, 
                          ifelse(y_left_div == 1e6, "M", "B"))
  
  # 右轴刻度
  max_rate_raw <- max_rate
  if (max_rate_raw > 25000) step <- 10000
  else if (max_rate_raw > 15000) step <- 5000
  else step <- 2000
  breaks_rate_raw <- seq(0, ceiling(max_rate_raw / step) * step, by = step)
  if (tail(breaks_rate_raw, 1) > max_rate_raw + step) {
    breaks_rate_raw <- head(breaks_rate_raw, -1)
  }
  
  # 绘图
  p <- ggplot() +
    geom_col(data = df_number,
             aes(x = x_numeric, y = val, fill = location_name),
             position = position_dodge(width = 0.7),
             width = 0.6, color = "black", linewidth = 0.2) +
    geom_line(data = df_rate,
              aes(x = x_numeric, y = val * scaling_factor,
                  color = location_name, group = location_name),
              linewidth = if (global_mode) 1.8 else 1.2) +
    geom_ribbon(data = df_rate,
                aes(x = x_numeric,
                    ymin = lower * scaling_factor,
                    ymax = upper * scaling_factor,
                    fill = location_name, 
                    color = location_name,
                    group = location_name),
                alpha = 0.08,
                linetype = "dashed",
                linewidth = 0.3,
                show.legend = FALSE) +
    scale_y_continuous(
      name = paste("Incidence number (in", y_left_unit, ")"),
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
    scale_x_continuous(
      breaks = 1:7,
      labels = levels(df_filtered$age_name),
      name = "Age Group"
    ) +
    scale_fill_manual(values = colors, name = "Location") +
    scale_color_manual(values = colors, name = "Location") +
    labs(title = title) +
    theme_minimal(base_size = 11) +
    theme(
      axis.title = element_text(face = "bold"),
      axis.title.y.right = element_text(color = "black", size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9),
      axis.text.y = element_text(size = 8),
      axis.text.y.right = element_text(size = 8),
      legend.position = if (show_legend) "right" else "none",
      legend.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA, color = "grey70", linewidth = 0.3),
      axis.line = element_line(color = "black", linewidth = 0.3),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
    )
  return(p)
}

# 绘制：全球图使用 Millions 单位，并设定步长为 5M（可根据最大值调整）
p_global <- plot_msk(
  location_ids = 1,
  title = "Global",
  y_left_unit = "Millions",
  y_left_div = 1e6,
  show_legend = FALSE,
  colors = location_colors_high["Global"],
  global_mode = TRUE,
  step_left = 5   # 每 5M 一个刻度，如果最大值 20M，则会有 0,5,10,15,20
)

p_four <- plot_msk(
  location_ids = four_country_ids,
  title = "China, Japan, United Kingdom, United States",
  y_left_unit = "Millions",
  y_left_div = 1e6,
  show_legend = TRUE,
  colors = colors_four,
  global_mode = FALSE
)

# 拼图
combined <- (p_global + theme(plot.margin = margin(b = 5))) / 
  (p_four + theme(plot.margin = margin(t = 5))) +
  plot_layout(heights = c(1, 2)) +
  plot_annotation(
    title = "Musculoskeletal disorders in females aged 15–49, 2023",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16,
                                margin = margin(b = 10))
    )
  )

print(combined)
ggsave("MSK_Global_and_Four_Countries_2023_15-49.pdf", combined, width = 11, height = 9)
ggsave("MSK_Global_and_Four_Countries_2023_15-49.tiff", combined, width = 11, height = 9, dpi = 600, compression = "lzw", bg = "white")
