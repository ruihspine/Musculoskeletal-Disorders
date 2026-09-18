# 加载包
library(ggplot2)
library(patchwork)
library(BAPC)

# 1. 绝对分解函数（保持不变）
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

# 2. 提取绝对贡献函数（同前）
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

# 3. 依次计算五个地区的相对贡献（绝对值之和为分母）
regions <- list(
  list(name = "Global",   res = IBD_bapc_result_global, pop = pop_global),
  list(name = "China",    res = IBD_bapc_result_china,  pop = pop_china),
  list(name = "Japan",    res = IBD_bapc_result_japan,  pop = pop_japan),
  list(name = "UK",       res = IBD_bapc_result_uk,     pop = pop_uk),
  list(name = "USA",      res = IBD_bapc_result_usa,    pop = pop_usa)
)

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
# 4. 绘图（标准水平堆叠条形图，地区在 Y 轴）
# ================================================================
plot_data <- subset(all_results_rel, Factor != "Total")
plot_data$Factor <- factor(plot_data$Factor,
                           levels = c("Population growth", "Population aging", "Epidemiological change"))
# 设置地区顺序：Y轴从下到上，这里从 USA 到 Global 会更自然，或者按您需要的顺序
plot_data$Region <- factor(plot_data$Region,
                           levels = c("USA", "UK", "Japan", "China", "Global"))  # 下方是最小的，上方是最大的

total_data <- subset(all_results_rel, Factor == "Total")
total_data <- total_data[!duplicated(total_data), ]
total_data$Region <- factor(total_data$Region,
                            levels = c("USA", "UK", "Japan", "China", "Global"))

# 统一 X 轴范围
max_val <- 110

# 计算标签的 hjust（水平条形图用 hjust 控制左右位置）
hjust_vec <- ifelse(total_data$Contribution >= 0, -0.3, 1.3)

# 绘制
p <- ggplot(plot_data, aes(x = Contribution, y = Region, fill = Factor)) +
  geom_col(position = "stack", width = 0.6) +                # 水平堆叠条形图
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  scale_x_continuous(limits = c(-max_val, max_val), expand = c(0, 0)) +
  labs(title = "Relative contribution of drivers to projected change in cases (2023–2040)",
       x = "Relative contribution (%)",
       y = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom",          # 图例在底部，只一份
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  # 添加总变化百分比标签
  geom_text(data = total_data,
            aes(x = Contribution, y = Region, label = paste0(round(Contribution, 1), "%")),
            inherit.aes = FALSE,
            hjust = hjust_vec,
            size = 3.5, fontface = "bold")

# 显示图形
print(p)

# 保存（可选）
# ggsave("Decomposition_GBD_Figure3_Style.pdf", p, width = 10, height = 4.5)