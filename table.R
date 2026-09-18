library(dplyr)
library(tidyr)

df <- read.csv("data1.csv", header = TRUE)

latest_year <- max(df$year, na.rm = TRUE)   # 你的数据中最大年份为 2006

# ----- 1. 定义目标地区（可自行增删）-----
# 这里使用与趋势图一致的 5 个地区（location_id 1,6,67,95,102）
target_locations <- c("Global", "China", "Japan", "United Kingdom", "United States of America")
# 如果你想包含更多地区，可以这样：
# target_locations <- unique(df$location_name)   # 所有地区
# 或筛选特定地区，例如中国各省：
# target_locations <- df %>% filter(location_id >= 10000) %>% pull(location_name) %>% unique()

# ----- 2. 定义骨骼肌肉疾病及其亚类（根据 GBD 分类）-----
# 注意：cause_name 必须与数据中的名称完全一致
cause_list <- c(
  "Musculoskeletal disorders",               # 大分类（总计）
  "Low back pain",                          # 下背痛
  "Neck pain",                              # 颈痛
  "Osteoarthritis",                         # 骨关节炎
  "Rheumatoid arthritis",                   # 类风湿关节炎
  "Gout",                                   # 痛风
  "Other musculoskeletal disorders"         # 其他骨骼肌肉疾病
)
# 如果你的数据中还有 "Osteoporosis" 等，可以加入
# 先查看数据中有哪些骨骼肌肉相关疾病：
# unique(df$cause_name[grepl("Musculoskeletal|back|neck|arthritis|gout|osteoporosis", df$cause_name, ignore.case = TRUE)])

# ----- 3. 过滤数据 -----
df_filtered <- df %>%
  filter(measure_name %in% c("Prevalence", 
                             "DALYs (Disability-Adjusted Life Years)", 
                             "Incidence",
                             "YLDs (Years Lived with Disability)"),
         metric_name %in% c("Number", "Rate"),
         age_id == 24,                        # 15-49 岁
         sex_name == "Female",                # 女性
         year %in% c(1990, latest_year),
         location_name %in% target_locations,
         cause_name %in% cause_list,
         cause_name != "",
         !is.na(cause_name)) %>%
  select(location_name, cause_name, year, measure_name, metric_name, val, lower, upper)

# ----- 4. 格式化数值（保留整数，显示 95% UI）-----
df_format <- df_filtered %>%
  mutate(value_display = paste0(round(val), "\n(", round(lower), " to ", round(upper), ")")) %>%
  select(location_name, cause_name, year, measure_name, metric_name, value_display)

# ----- 5. 转换为宽表（每个指标一列）-----
df_wide <- df_format %>%
  unite(col, year, measure_name, metric_name, sep = "_") %>%
  pivot_wider(names_from = col, values_from = value_display)

# ----- 6. 排序（按地区和疾病顺序）-----
df_final <- df_wide %>%
  mutate(cause_name = factor(cause_name, levels = cause_list),
         location_name = factor(location_name, levels = target_locations)) %>%
  arrange(location_name, cause_name)

# 查看结果
print(df_final)

# 导出 CSV
write.csv(df_final, "table_musculoskeletal_disorders.csv", row.names = FALSE, na = "")