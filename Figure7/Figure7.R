library(reshape2);library(ggplot2);library(dbplyr);library(dplyr);library(ggsignif);library(ggrepel);library(ggpubr)
setwd("C:/Users/11915/Desktop/PRDX6-laste/Code/Figure7/")
#################Figure 7A####################
setwd("Fig 7a/")
data<-read.csv("PRDX6-肠宽2.csv",header = T)
data$Group <- factor(data$Group, levels = c("PRDX6-WT", "PRDX6-K63Q", "PRDX6-K63R"))
table(data$Group)
group_colors <- c("PRDX6-WT" = "#7A93BC", "PRDX6-K63Q" = "#CC6063", "PRDX6-K63R" = "#EAAC68")

my_comparisons <- list(c("PRDX6-WT", "PRDX6-K63Q"),
                       c("PRDX6-K63Q","PRDX6-K63R"),
                       c("PRDX6-WT", "PRDX6-K63R"))
# 计算四分位数
data2 <-data%>%
  group_by(Group) %>%
  filter({
    q1 <- quantile(Value, 0.25, na.rm = TRUE)
    q3 <- quantile(Value, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    Value >= (q1 - 1.5 * iqr) & Value <= (q3 + 1.5 * iqr)
  }) %>%
  ungroup()

# 查看结果
length(data2) 



ggplot(data, aes(x = Group, y = Value)) +
  # 箱型图：不显示离群值（outlier.shape = NA），边框黑色
  geom_boxplot(aes(color=Group),width = 0.55, outlier.shape = NA, fill = "white", alpha = 0.9) +
  
  # 添加每个点：透明度稍低，带黑边
  geom_jitter(aes(color=Group,fill=Group),position = position_jitter(width = 0.15), 
              size = 2.5,stroke = 0.6) +
  
  # 显著性比较
  stat_compare_means(comparisons = my_comparisons, method = "t.test", 
                     label = "p.format", size = 4,step.increase = 0.3,
                     tip.length = 0.03, bracket.size = 0.75) +
  
  # 设置颜色
  scale_fill_manual(values = group_colors,guide= FALSE) +
  scale_color_manual(values = group_colors,guide= FALSE) +
  # 坐标轴和图例美化
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
  labs(x = NULL, y = "Relative intestinal width") +
  
  # 主题风格美化
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 13, angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(size = 13, color = "black"),
    axis.line = element_line(size = 0.75, color = "black"),
    axis.ticks = element_line(size = 0.75, color = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.text = element_text(size = 13),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 13),
    plot.title = element_text(size = 13, hjust = 0.5, color = "black")
  ) +
  guides(fill = guide_legend(title = NULL))
ggsave("肠宽.pdf", width =5, height =4)

#################Figure 7B####################
setwd("Fig 7b/")
data<-read.csv("PRDX6-肠漏4.csv",header = T)
data$Group <- factor(data$Group, levels = c("PRDX6-WT", "PRDX6-K63Q", "PRDX6-K63R"))
table(data$Group)
group_colors <- c("PRDX6-WT" = "#7A93BC", "PRDX6-K63Q" = "#CC6063", "PRDX6-K63R" = "#EAAC68")

my_comparisons <- list(c("PRDX6-WT", "PRDX6-K63Q"),
                       c("PRDX6-K63Q","PRDX6-K63R"),
                       c("PRDX6-WT", "PRDX6-K63R"))
# 计算四分位数
data2 <-data%>%
  group_by(Group) %>%
  filter({
    q1 <- quantile(Value, 0.25, na.rm = TRUE)
    q3 <- quantile(Value, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    Value >= (q1 - 1.5 * iqr) & Value <= (q3 + 1.5 * iqr)
  }) %>%
  ungroup()

# 查看结果
length(data2) 



ggplot(data, aes(x = Group, y = Value)) +
  # 箱型图：不显示离群值（outlier.shape = NA），边框黑色
  geom_boxplot(aes(color=Group),width = 0.55, outlier.shape = NA, fill = "white", alpha = 0.9) +
  
  # 添加每个点：透明度稍低，带黑边
  geom_jitter(aes(color=Group,fill=Group),position = position_jitter(width = 0.15), 
              size = 2.5,stroke = 0.6) +
  
  # 显著性比较
  stat_compare_means(comparisons = my_comparisons, method = "t.test", 
                     label = "p.format", size = 4,step.increase = 0.1,
                     tip.length = 0.03, bracket.size = 0.75) +
  
  # 设置颜色
  scale_fill_manual(values = group_colors,guide= FALSE) +
  scale_color_manual(values = group_colors,guide= FALSE) +
  # 坐标轴和图例美化
  scale_y_continuous(breaks = seq(0, 120, 30), limits = c(0, 120)) +
  labs(x = NULL, y = "Mean intensity") +
  
  # 主题风格美化
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 13, angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(size = 13, color = "black"),
    axis.line = element_line(size = 0.75, color = "black"),
    axis.ticks = element_line(size = 0.75, color = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.text = element_text(size = 13),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 13),
    plot.title = element_text(size = 13, hjust = 0.5, color = "black")
  ) +
  guides(fill = guide_legend(title = NULL))
ggsave("肠漏.pdf", width =, height =4.3)

#################Figure 7C####################
setwd("Fig 7c/")
dat <- read.csv("寿命3.csv")    
# 2. 宽表转成长表：每一行 = 一条线虫
long <- pivot_longer(dat,
                     cols = -Day,
                     names_to  = "Group",
                     values_to = "status")

# 只要 status = 1 的行（NaN 去掉），这些都是“死亡事件”
long <- long[!is.na(long$status), ]
long$status <- 1   # 都是死亡，没有删失


fit <- survfit(Surv(Day, status) ~ Group, data =long)

ggsurvplot(fit,
           data     = long,
           fun      = "pct",        # y 轴显示百分比
           conf.int = FALSE,
           censor   = FALSE,
           palette  = c(  "#CC6063", "#EAAC68"),
           pval     = TRUE,         # 显示 log-rank p 值
           xlab     = "Days",
           ylab     = "Survival (%)",
           xlim     = c(0, 27),
           ylim     = c(0, 100))
ggsave("lifespanall.pdf", width =3.4, height =3.6)

#################Figure 7D####################
setwd("Fig 7d/")
data<-read.csv("代谢物处理的PRDX6-肠宽.csv")
data$Group <- factor(data$Group, levels = c("Control_WT", "Control_R", "5mM NaBu_WT","5mM NaBu_R"))
data$Group3 <- factor(data$Group, levels = c("Control", "5mM NaBu"))
table(data$Group)
group_colors <- c("Control_WT" = "#7A93BC", "Control_R" = "#CC6063"
                  , "5mM NaBu_WT" = "#7A93BC", "5mM NaBu_R" = "#CC6063")

my_comparisons <- list(c("Control_WT", "Control_R"),
                       c("5mM NaBu_WT","5mM NaBu_R"),
                       c("Control_WT", "5mM NaBu_WT"),
                       c("Control_R", "5mM NaBu_R"))
data2 <-data%>%
  group_by(Group) %>%
  filter({
    q1 <- quantile(Value, 0.25, na.rm = TRUE)
    q3 <- quantile(Value, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    Value >= (q1 - 1.5 * iqr) & Value <= (q3 + 1.5 * iqr)
  }) %>%
  ungroup()

# 查看结果
length(data2) 



ggplot(data, aes(x = Group, y = Value)) +
  # 箱型图：不显示离群值（outlier.shape = NA），边框黑色
  geom_boxplot(aes(color=Group),width = 0.55, outlier.shape = NA, fill = "white", alpha = 0.9) +
  
  # 添加每个点：透明度稍低，带黑边
  geom_jitter(aes(color=Group,fill=Group),position = position_jitter(width = 0.15), 
              size = 2.5,stroke = 0.6) +
  
  # 显著性比较
  stat_compare_means(comparisons = my_comparisons, method = "t.test", 
                     label = "p.format", size = 4,step.increase = 0.05,
                     tip.length = 0.03, bracket.size = 0.75) +
  
  # 设置颜色
  scale_fill_manual(values = group_colors,guide= FALSE) +
  scale_color_manual(values = group_colors,guide= FALSE) +
  # 坐标轴和图例美化
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  labs(x = NULL, y = "Relative intestinal width") +
  
  # 主题风格美化
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 13, angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(size = 13, color = "black"),
    axis.line = element_line(size = 0.75, color = "black"),
    axis.ticks = element_line(size = 0.75, color = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.text = element_text(size = 13),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 13),
    plot.title = element_text(size = 13, hjust = 0.5, color = "black")
  ) +
  guides(fill = guide_legend(title = NULL))
ggsave("代谢肠宽.pdf", width =6, height =4)
#################Figure 7E####################
setwd("Fig 7e/")
data<-read.csv("代谢物处理的PRDX6-肠漏2.csv")
data<-data[which(data$Group3=="Control"|data$Group3=="20mM NaBu"),]
data$Group <- factor(data$Group, levels = c("Control_WT", "Control_R", "20mM NaBu_WT","20mM NaBu_R"))
data$Group3 <- factor(data$Group, levels = c("Control", "20mM NaBu"))
table(data$Group)
group_colors <- c("Control_WT" = "#7A93BC", "Control_R" = "#CC6063", "20mM NaBu_WT" = "#7A93BC", "20mM NaBu_R" = "#CC6063")

my_comparisons <- list(c("Control_WT", "Control_R"),
                       c("20mM NaBu_WT","20mM NaBu_R"),
                       c("Control_WT", "20mM NaBu_WT"),
                       c("Control_R", "20mM NaBu_R"))
data2 <-data%>%
  group_by(Group) %>%
  filter({
    q1 <- quantile(Value, 0.25, na.rm = TRUE)
    q3 <- quantile(Value, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    Value >= (q1 - 1.5 * iqr) & Value <= (q3 + 1.5 * iqr)
  }) %>%
  ungroup()

# 查看结果
length(data2) 



ggplot(data, aes(x = Group, y = Value)) +
  # 箱型图：不显示离群值（outlier.shape = NA），边框黑色
  geom_boxplot(aes(color=Group),width = 0.55, outlier.shape = NA, fill = "white", alpha = 0.9) +
  
  # 添加每个点：透明度稍低，带黑边
  geom_jitter(aes(color=Group,fill=Group),position = position_jitter(width = 0.15), 
              size = 4,stroke = 0.6) +
  
  # 显著性比较
  stat_compare_means(comparisons = my_comparisons, method = "t.test", 
                     label = "p.format", size = 4,step.increase = 0.05,
                     tip.length = 0.03, bracket.size = 0.75) +
  
  # 设置颜色
  scale_fill_manual(values = group_colors,guide= FALSE) +
  scale_color_manual(values = group_colors,guide= FALSE) +
  # 坐标轴和图例美化
  #scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  labs(x = NULL, y = "Relative intestinal width") +
  
  # 主题风格美化
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 13, angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(size = 13, color = "black"),
    axis.line = element_line(size = 0.75, color = "black"),
    axis.ticks = element_line(size = 0.75, color = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.text = element_text(size = 13),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 13),
    plot.title = element_text(size = 13, hjust = 0.5, color = "black")
  ) +
  guides(fill = guide_legend(title = NULL))
ggsave("代谢肠漏.pdf", width =6.2, height =4.5)
#################Figure 7F####################
setwd("Fig 7f/")
# 1. 读寿命表
dat <- read.csv("寿命.csv")   
# 2. 宽表转成长表：每一行 = 一条线虫
long <- pivot_longer(dat,
                     cols = -Day,
                     names_to  = "Group",
                     values_to = "status")

# 只要 status = 1 的行（NaN 去掉），这些都是“死亡事件”
long <- long[!is.na(long$status), ]
long$status <- 1   # 都是死亡，没有删失

fit_wt <- survfit(Surv(Day, status) ~ Group, data = wt)

ggsurvplot(fit_wt,
           data     = wt,
           fun      = "pct",        # y 轴显示百分比
           conf.int = FALSE,
           censor   = FALSE,
           palette  = c("black", "red"),
           pval     = TRUE,         # 显示 log-rank p 值
           xlab     = "Days",
           ylab     = "Survival (%)",
           xlim     = c(0, 27),
           ylim     = c(0, 100))

