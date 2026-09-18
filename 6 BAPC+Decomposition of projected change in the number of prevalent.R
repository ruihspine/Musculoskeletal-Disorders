# ================================================================
#  完整脚本：多地区分解分析（标准 GBD Figure 3 风格）
#  水平堆叠条形图，地区在 Y 轴，相对贡献在 X 轴
# ================================================================
# 设置工作目录
setwd("D:\\R\\Musculoskeletal Disorders")

# 批量运行五个 BAPC 脚本
source("BAPC Global.R")
source("BAPC china.R")
source("BAPC Japan.R")
source("BAPC United Kingdom.R")
source("BAPC United States of America.R")

# 运行完成后，立即保存工作空间，避免下次重复劳动
save.image("BAPC_All_Regions.RData")

print("所有地区的 BAPC 模型已运行完毕并保存！")


# ================================================================
#  最终精修版：多地区分解分析（GBD Figure 3 标准风格）
#  水平堆叠条形图，地区在 Y 轴，X 轴为百分比变化
# ================================================================

# 0. 加载包
library(ggplot2)
library(BAPC)

# ================================================================
# 1. 定义绝对分解函数
# ================================================================
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

# ================================================================
# 2. 单个地区分解函数
# ================================================================
decomp_one_region <- function(bapc_result, pop_df, region_name){
  age_spec_list <- agespec.proj(bapc_result)
  age_groups <- names(age_spec_list)
  rate_mat <- sapply(age_spec_list, function(x) x[, "mean"])
  rownames(rate_mat) <- rownames(age_spec_list[[1]])
  
  # 2023和2040年预测发病人数（缩放1e8）
  cases_pred_2023 <- rate_mat["2023", ] / 1e8
  cases_pred_2040 <- rate_mat["2040", ] / 1e8
  
  # 人口数据
  pop_2023 <- pop_df["2023", age_groups]
  pop_2040 <- pop_df["2040", age_groups]
  
  # 计算发病率（每1人口）
  rate_2023 <- cases_pred_2023 / pop_2023
  rate_2040 <- cases_pred_2040 / pop_2040
  
  # 执行分解
  res <- das_gupta_decomp(pop_2023, pop_2040, rate_2023, rate_2040)
  return(res)
}

# ================================================================
# 3. 加载五个地区的 BAPC 结果（您已通过 source() 生成）
# ================================================================
regions <- list(
  list(name = "Global",   res = IBD_bapc_result_global, pop = pop_global),
  list(name = "China",    res = IBD_bapc_result_china,  pop = pop_china),
  list(name = "Japan",    res = IBD_bapc_result_japan,  pop = pop_japan),
  list(name = "UK",       res = IBD_bapc_result_uk,     pop = pop_uk),
  list(name = "USA",      res = IBD_bapc_result_usa,    pop = pop_usa)
)

# ================================================================
# 4. 计算相对贡献（分母为绝对值之和）
# ================================================================
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
# 5. 最终精修绘图（完美 GBD Figure 3 风格）
# ================================================================
plot_data <- subset(all_results_rel, Factor != "Total")
plot_data$Factor <- factor(plot_data$Factor,
                           levels = c("Population growth", "Population aging", "Epidemiological change"))
# 地区顺序（从上到下）：Global, China, Japan, UK, USA
plot_data$Region <- factor(plot_data$Region,
                           levels = rev(c("Global", "China", "Japan", "UK", "USA")))

total_data <- subset(all_results_rel, Factor == "Total")
total_data <- total_data[!duplicated(total_data), ]
total_data$Region <- factor(total_data$Region,
                            levels = rev(c("Global", "China", "Japan", "UK", "USA")))

# 统一 X 轴范围 -110 ~ 110（留10%边距）
max_val <- 110
hjust_vec <- ifelse(total_data$Contribution >= 0, -0.3, 1.3)

# 绘制
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
    title = "Decomposition of projected change in cases (2023–2040)",
    x = "Percentage change (%)",
    y = ""
  ) +
  theme_minimal() +
  theme(
    # 图例
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    # 坐标轴
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.text.y = element_text(size = 11, face = "bold", color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.line.x = element_line(color = "black", linewidth = 0.8),
    axis.line.y = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length = unit(0.2, "cm"),
    # 网格线（保留x轴浅灰网格，去掉y轴网格）
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.x = element_blank(),
    # 标题
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 15))
  ) +
  # 添加总变化百分比标签
  geom_text(data = total_data,
            aes(x = Contribution, y = Region, label = paste0(round(Contribution, 1), "%")),
            inherit.aes = FALSE,
            hjust = hjust_vec,
            size = 3.5, fontface = "bold", color = "black")

# 显示并保存
print(p)
 ggsave("Decomposition_Figure3_Final.pdf", p, width = 7, height = 5, dpi = 300)
 