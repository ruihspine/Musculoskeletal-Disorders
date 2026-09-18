setwd("D:\\R\\Musculoskeletal Disorders")
load("BAPC_All_Regions.RData")
# 然后直接运行上面的绘图代码

# ================================================================
#  最终精修版：多地区分解分析（GBD Figure 3 标准风格）
#  水平堆叠条形图，地区在 Y 轴，X 轴为百分比变化
#  已去掉顶部标题
# ================================================================

# 加载包
library(ggplot2)
library(BAPC)

# 1. 定义绝对分解函数（同前）
das_gupta_decomp <- function(pop0, pop1, rate0, rate1){
  cases0 <- sum(pop0 * rate0)
  cases1 <- sum(pop1 * rate1)
  total_change <- cases1 - cases0
  total_pop0 <- sum(pop0)
  total_pop1 <- sum(pop1)
  age_struct0 <- pop0 / total_pop0
  pop_growth <- total_pop1 * age_struct0
  cases_growth <- sum(pop_growth * rate0)
  growth_effect <- cases_growth - cases0
  age_struct1 <- pop1 / total_pop1
  pop_aging <- total_pop0 * age_struct1
  cases_aging <- sum(pop_aging * rate0)
  aging_effect <- cases_aging - cases0
  cases_epi <- sum(pop0 * rate1)
  epi_effect <- cases_epi - cases0
  sum_effects <- growth_effect + aging_effect + epi_effect
  interaction <- total_change - sum_effects
  growth_effect_adj <- growth_effect + interaction/3
  aging_effect_adj   <- aging_effect   + interaction/3
  epi_effect_adj     <- epi_effect     + interaction/3
  return(c(growth = growth_effect_adj,
           aging  = aging_effect_adj,
           epi    = epi_effect_adj,
           total  = total_change))
}

# 2. 单个地区分解函数
decomp_one_region <- function(bapc_result, pop_df, region_name){
  age_spec_list <- agespec.proj(bapc_result)
  age_groups <- names(age_spec_list)
  rate_mat <- sapply(age_spec_list, function(x) x[, "mean"])
  rownames(rate_mat) <- rownames(age_spec_list[[1]])
  cases_pred_2023 <- rate_mat["2023", ] / 1e8
  cases_pred_2040 <- rate_mat["2040", ] / 1e8
  pop_2023 <- pop_df["2023", age_groups]
  pop_2040 <- pop_df["2040", age_groups]
  rate_2023 <- cases_pred_2023 / pop_2023
  rate_2040 <- cases_pred_2040 / pop_2040
  res <- das_gupta_decomp(pop_2023, pop_2040, rate_2023, rate_2040)
  return(res)
}

# 3. 五个地区的结果（确保环境中有这些对象）
regions <- list(
  list(name = "Global",   res = IBD_bapc_result_global, pop = pop_global),
  list(name = "China",    res = IBD_bapc_result_china,  pop = pop_china),
  list(name = "Japan",    res = IBD_bapc_result_japan,  pop = pop_japan),
  list(name = "UK",       res = IBD_bapc_result_uk,     pop = pop_uk),
  list(name = "USA",      res = IBD_bapc_result_usa,    pop = pop_usa)
)

# 4. 计算相对贡献
all_results_rel <- data.frame()
for (r in regions) {
  abs_vals <- decomp_one_region(r$res, r$pop, r$name)
  sum_abs <- abs(abs_vals["growth"]) + abs(abs_vals["aging"]) + abs(abs_vals["epi"])
  if (sum_abs == 0) {
    rel_vals <- c(growth = 0, aging = 0, epi = 0, total = 0)
  } else {
    rel_vals <- c(
      growth = abs_vals["growth"] / sum_abs * 100,
      aging  = abs_vals["aging"]  / sum_abs * 100,
      epi    = abs_vals["epi"]    / sum_abs * 100,
      total  = abs_vals["total"]  / sum_abs * 100
    )
  }
  df <- data.frame(
    Region = r$name,
    Factor = c("Population growth", "Population aging", "Epidemiological change", "Total"),
    Contribution = rel_vals
  )
  all_results_rel <- rbind(all_results_rel, df)
}

# ================================================================
# 5. 最终绘图（已去掉顶部标题）
# ================================================================
plot_data <- subset(all_results_rel, Factor != "Total")
plot_data$Factor <- factor(plot_data$Factor,
                           levels = c("Population growth", "Population aging", "Epidemiological change"))
plot_data$Region <- factor(plot_data$Region,
                           levels = rev(c("Global", "China", "Japan", "UK", "USA")))

total_data <- subset(all_results_rel, Factor == "Total")
total_data <- total_data[!duplicated(total_data), ]
total_data$Region <- factor(total_data$Region,
                            levels = rev(c("Global", "China", "Japan", "UK", "USA")))

max_val <- 110
hjust_vec <- ifelse(total_data$Contribution >= 0, -0.3, 1.3)

p <- ggplot(plot_data, aes(x = Contribution, y = Region, fill = Factor)) +
  geom_col(position = "stack", width = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  scale_x_continuous(
    limits = c(-max_val, max_val),
    expand = c(0, 0),
    breaks = seq(-100, 100, by = 50),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    x = "Percentage change (%)",
    y = ""                     # 这里删除了 title 参数
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.text.y = element_text(size = 11, face = "bold", color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.line.x = element_line(color = "black", linewidth = 0.8),
    axis.line.y = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.x = element_blank()
  ) +
  geom_text(data = total_data,
            aes(x = Contribution, y = Region, label = paste0(round(Contribution, 1), "%")),
            inherit.aes = FALSE,
            hjust = hjust_vec,
            size = 3.5, fontface = "bold", color = "black")

# 显示
print(p)

# 保存（可选）
ggsave("Decomposition_Figure3_NoTitle11.pdf", p, width = 7, height = 4, dpi = 600)
