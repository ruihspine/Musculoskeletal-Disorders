library(tidyverse)
library(scales)
library(ggsci)

setwd("D:\\R\\Musculoskeletal Disorders")
df <- read.csv("data1.csv", header = TRUE)

# ----- 定义地区顺序（按 location_id 1,6,67,95,102）-----
location_levels <- df %>%
  filter(location_id %in% c(1, 6, 67, 95, 102)) %>%
  distinct(location_id, location_name) %>%
  arrange(match(location_id, c(1, 6, 67, 95, 102))) %>%
  pull(location_name)

# -------------------- 图1：Prevalence Number --------------------
df1 <- df %>% 
  filter(measure_name == "Prevalence") %>% 
  filter(location_id %in% c(1, 6, 67, 95, 102)) %>% 
  filter(cause_name == "Musculoskeletal disorders") %>%   # 主疾病
  filter(age_id == 24) %>%   # 15-49岁
  filter(metric_name == "Number") %>% 
  filter(sex_name == "Female") %>%
  mutate(location_name = factor(location_name, levels = location_levels))

ggplot(df1, aes(x = year, y = val, color = location_name, linetype = location_name)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  theme_classic(base_size = 12) +
  labs(
    x = "Year",
    y = "Prevalent cases"   # 精简标签
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1, big.mark = ",")
  ) +
  scale_color_npg() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

# -------------------- 图2：Prevalence Rate --------------------
df2 <- df %>% 
  filter(measure_name == "Prevalence") %>% 
  filter(location_id %in% c(1, 6, 67, 95, 102)) %>% 
  filter(cause_name == "Musculoskeletal disorders") %>% 
  filter(age_id == 24) %>% 
  filter(metric_name == "Rate") %>% 
  filter(sex_name == "Female") %>%
  mutate(location_name = factor(location_name, levels = location_levels))

ggplot(df2, aes(x = year, y = val, color = location_name, linetype = location_name)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  theme_classic(base_size = 12) +
  labs(
    x = "Year",
    y = "Age-standardized prevalence rate (per 100,000)"   # 精简标签
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1, big.mark = ",")
  ) +
  scale_color_npg() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

# -------------------- 图3：Incidence Number --------------------
df3 <- df %>% 
  filter(measure_name == "Incidence") %>% 
  filter(location_id %in% c(1, 6, 67, 95, 102)) %>% 
  filter(cause_name == "Musculoskeletal disorders") %>% 
  filter(age_id == 24) %>% 
  filter(metric_name == "Number") %>% 
  filter(sex_name == "Female") %>%
  mutate(location_name = factor(location_name, levels = location_levels))

ggplot(df3, aes(x = year, y = val, color = location_name, linetype = location_name)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  theme_classic(base_size = 12) +
  labs(
    x = "Year",
    y = "Incident cases"   # 精简标签
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1, big.mark = ",")
  ) +
  scale_color_npg() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

# -------------------- 图4：Incidence Rate --------------------
df4 <- df %>% 
  filter(measure_name == "Incidence") %>% 
  filter(location_id %in% c(1, 6, 67, 95, 102)) %>% 
  filter(cause_name == "Musculoskeletal disorders") %>% 
  filter(age_id == 24) %>% 
  filter(metric_name == "Rate") %>% 
  filter(sex_name == "Female") %>%
  mutate(location_name = factor(location_name, levels = location_levels))

ggplot(df4, aes(x = year, y = val, color = location_name, linetype = location_name)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  theme_classic(base_size = 12) +
  labs(
    x = "Year",
    y = "Age-standardized incidence rate (per 100,000)"   # 精简标签
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1, big.mark = ",")
  ) +
  scale_color_npg() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

# -------------------- 图5：YLDs Number --------------------
df5 <- df %>% 
  filter(measure_name == "YLDs (Years Lived with Disability)") %>% 
  filter(location_id %in% c(1, 6, 67, 95, 102)) %>% 
  filter(cause_name == "Musculoskeletal disorders") %>% 
  filter(age_id == 24) %>% 
  filter(metric_name == "Number") %>% 
  filter(sex_name == "Female") %>%
  mutate(location_name = factor(location_name, levels = location_levels))

ggplot(df5, aes(x = year, y = val, color = location_name, linetype = location_name)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  theme_classic(base_size = 12) +
  labs(
    x = "Year",
    y = "YLDs cases"   # 精简标签（也可用 "YLDs (cases)"）
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1, big.mark = ",")
  ) +
  scale_color_npg() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )

# -------------------- 图6：YLDs Rate --------------------
df6 <- df %>% 
  filter(measure_name == "YLDs (Years Lived with Disability)") %>% 
  filter(location_id %in% c(1, 6, 67, 95, 102)) %>% 
  filter(cause_name == "Musculoskeletal disorders") %>% 
  filter(age_id == 24) %>% 
  filter(metric_name == "Rate") %>% 
  filter(sex_name == "Female") %>%
  mutate(location_name = factor(location_name, levels = location_levels))

ggplot(df6, aes(x = year, y = val, color = location_name, linetype = location_name)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  theme_classic(base_size = 12) +
  labs(
    x = "Year",
    y = "Age-standardized YLDs rate (per 100,000)"   # 精简标签
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1, big.mark = ",")
  ) +
  scale_color_npg() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3)
  )