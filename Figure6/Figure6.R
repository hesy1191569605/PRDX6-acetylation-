library(vegan);library(ape);library(ggplot2);library(ggforce);library(readxl);library(dplyr)
library(tidyr);library(ggpubr);library(ggbeeswarm) 
setwd("C:/Users/11915/Desktop/PRDX6-laste/Code/Figure6/")
#################Figure 6A####################
setwd("Fig 6a/")
data1<-read.csv("MAFCA-HSY.csv",row.names = 1)
otu_table <-data1[,-c(1:7)]

metadata <- data.frame(
  SampleID = rownames(otu_table),
  data1[,c(1:7)]
)
bray_dist <- vegdist(otu_table, method = "bray")
# 主坐标分析
pcoa_res <- pcoa(bray_dist)
# 提取前两个主成分
pcoa_df <- data.frame(
  SampleID = rownames(otu_table),
  PC1 = pcoa_res$vectors[, 1],
  PC2 = pcoa_res$vectors[, 2]
)
pcoa_res <- cmdscale(bray_dist, k = 2, eig = TRUE)
pcoa_df <- as.data.frame(pcoa_res$points)
colnames(pcoa_df) <- c("PC1", "PC2")
pcoa_df$SampleID<-rownames(pcoa_df)
percent_var <- round(100 * pcoa_res$values$Relative_eig, 2)
# 合并分组信息
pcoa_df <- merge(pcoa_df, metadata, by = "SampleID")
ggplot(pcoa_df, aes(x = PC1, y = PC2, color = AGE)) +
  geom_point(size = 4, alpha = 0.8) +
  theme_classic() +
  labs(title = "PCoA based on Bray-Curtis",
       x = paste0("PC1 (", round(pcoa_res$values$Relative_eig[1] * 100, 1), "%)"),
       y = paste0("PC2 (", round(pcoa_res$values$Relative_eig[2] * 100, 1), "%)"))


library(ape);pcoa_res <- pcoa(bray_dist);percent_var <- round(100 * pcoa_res$values$Relative_eig, 2)
percent_var[1:5]
# 为不同分组设置点的形状和颜色
pcoa_df$Shape <- ifelse(pcoa_df$AGE == "Young", 16,ifelse(pcoa_df$AGE == "Middle age",17,18))  # 圆 vs 三角
pcoa_df$Color <- ifelse(pcoa_df$AGE == "Young", "#406B97",ifelse(pcoa_df$AGE == "Middle age","#D38559","#A02D2B"))  

# 计算每组质心
centroids <- aggregate(cbind(PC1, PC2) ~ AGE, data = pcoa_df, mean)

# 绘图
ggplot(pcoa_df, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = AGE, shape = AGE), size = 3) +  # 样本点
  #geom_path(data = rbind(pcoa_df, pcoa_df[1, ]), aes(group = AGE), arrow = arrow(length = unit(0, "cm")), linetype = "dashed", color = "black")
  geom_segment(data = merge(pcoa_df, centroids, by = "AGE"),aes(x = PC1.y, y = PC2.y, xend = PC1.x, yend = PC2.x),
               linetype = "dashed",color = "black",arrow = arrow(length = unit(0.3, "cm"), type = "closed"))+        # 指向质心线
  #geom_mark_ellipse(aes(group = AGE, fill = AGE), alpha = 0.3, show.legend =T)
  #stat_ellipse(aes(fill = AGE), geom = "polygon", alpha = 0.3, level = 0.5, color = NA, type = "euclid") +  # 椭圆
  
  scale_shape_manual(values = c("Young"=16,"Old"=18,"Middle age"=17),guide=FALSE) +
  scale_color_manual(values = c("Young"="#406B97","Old"="#A02D2B","Middle age"="#D38559"),guide=FALSE)+
  scale_fill_manual(values = c("Young"="#406B97","Old"="#A02D2B","Middle age"="#D38559"),guide=FALSE)+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) +
  labs(title = "PCoA at ASV level",x = "PC1 (39.57%)",y = "PC2 (14.35%)")
ggsave("PcoA.pdf", width = 3, height = 3)


#################Figure 6B####################
setwd("Fig 6b/")

DATA<-read.csv("test_MAFCA_ASV.csv")
DATA[DATA=="0"]<-NA
DATA[DATA=="Inf"]<-NA
DATA$color_pre[(DATA$P_PRDX6>0.05)]<-"no"
DATA$color_pre[(DATA$P_PRDX6<0.05)&(DATA$RHO_PRDX6>0)] <- "up"
DATA$color_pre[(DATA$P_PRDX6<0.05)&(DATA$RHO_PRDX6<0)]  <- "down"
ggplot(DATA,aes(x=DATA$RHO_PRDX6,y=-log10(DATA$P_PRDX6),fill=-log10(P_PRDX6)*sign(RHO_PRDX6)))+
  geom_point(aes(fill=-log10(P_PRDX6)*sign(RHO_PRDX6),size=abs(-log10(P_PRDX6))),shape=21,color='black',)+
  scale_fill_gradient2(low ="#406B97",mid ="white",high ="#A02D2B",guide = FALSE)+
  geom_hline(yintercept =-log10(0.05),linetype=2,color="grey",size=1)+
  scale_size_continuous(range=c(0,10),guide = FALSE)+
  labs(x="",y="")+
  xlim(-1,1)+
  theme(axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) 

#################Figure 6C####################
setwd("Fig 6c/")
otu_table <- read.csv("MAFCA-HSY.csv")
taxonomy_table <- read.csv("taxonomy_MAFCA.csv")
butyrate_genera <- c("Faecalibacterium", "Roseburia", "Eubacterium",
                     "Anaerostipes", "Butyricicoccus", "Clostridium",
                     "Coprococcus", "Ruminococcus")
butyrate_asv <- taxonomy_table %>%
  filter(Genus %in% butyrate_genera) %>%
  pull(ASV)
butyrate_df <- otu_table %>%
  select(ID = `ID.1`, AGE, all_of(butyrate_asv))
butyrate_z <- butyrate_df %>%
  select(all_of(butyrate_asv)) %>%
  mutate(across(everything(), ~ (.-mean(., na.rm=TRUE))/sd(., na.rm=TRUE)))

butyrate_z <- bind_cols(butyrate_df %>% select(ID, AGE), butyrate_z)

butyrate_long <- butyrate_z %>%
  pivot_longer(cols = -c(ID, AGE), names_to = "ASV", values_to = "Zscore")

butyrate_long <- left_join(butyrate_long, taxonomy_table %>% select(ASV, Genus), by = "ASV")

butyrate_long <- butyrate_long %>%
  mutate(Genus = factor(Genus, levels = c("Ruminococcus", "Eubacterium",
                                          "Faecalibacterium", "Roseburia",
                                          "Coprococcus", "Butyricicoccus",
                                          "Anaerostipes")))
ggplot(butyrate_long[which(butyrate_long$Genus=="Faecalibacterium"),], aes(x=AGE, y=Zscore, fill=AGE)) +
  geom_violin(trim=FALSE, alpha=0.6, width=0.9) +
  geom_boxplot(aes(color=AGE),fill="white",width=0.4) +
  stat_summary(aes(group=1), fun=median, geom="line", color="black", size=1) +
  scale_x_discrete(limits=c("Young","Middle age","Old"))+
  scale_fill_manual(values = c("Young"="#406B97","Old"="#A02D2B","Middle age"="#D38559")) +
  scale_color_manual(values = c("Young"="#406B97","Old"="#A02D2B","Middle age"="#D38559")) +
  labs(y="Z-score (within ASV)", x="Age Group") +
  geom_signif(comparisons =  list(c('Young','Middle age'),c('Old','Young'),c('Middle age','Old')),
              test = "t.test",step_increase = 0.5,map_signif_level =F,color="black")+
  facet_wrap(~ Genus, ncol =4,scales = "free_y") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(size=12),
    strip.placement = "outside",
    axis.title = element_text(color="black"),
    axis.text = element_text(size = 15, color = "black", vjust = 0.5, hjust = 0.5),
    axis.line = element_line(linetype=1, color="black", size=0.75),
    axis.ticks = element_line(color="black", size=0.75, lineend=1)
  )
ggsave("提琴图.pdf", width =8.6, height =5)

#################Figure 6D####################
setwd("Fig 6d/")
df<- read.csv("f.p.csv")
df$F..prausnitzii
ggplot(df, aes(x = Age, y = F..prausnitzii)) +
  geom_point(color = "#e69f00", alpha = 0.3, size = 4) +
  stat_cor(method = "spearman")+
  theme_minimal(base_size = 14) +
  theme(
    strip.background.x = element_blank(),
    strip.text.x       = element_text(size = 13),
    axis.line          = element_line(color = "black", size = 0.75),
    axis.ticks         = element_line(color = "black", size = 0.75),
    panel.background   = element_rect(fill = "white"),
    panel.grid         = element_blank(),
    legend.position    = "none",
    axis.text.y        = element_text(size = 13, color = "black",
                                      vjust = 0.5, hjust = 1),
    axis.text.x        = element_text(size = 13, color = "black",
                                      hjust = 0.5),
    plot.title         = element_text(size = 13, color = "black",
                                      vjust = 0.5, hjust = 0.5)
  )


#################Figure 6E####################
setwd("Fig 6e/")
data<- read.csv("buty.csv")
ggplot(data=data,aes(x=scale(data$Butyrate),y=data$F..prausnitzii..ASV214.))+
  geom_point(color="#E69191",size=4.5,shape=16) +
  geom_smooth(method="lm",size=1.3, se =T,color="grey30")+
  stat_cor(method = "spearman")+
  labs(y="ASV-0214",x="Butyric acid")+
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13),
        axis.line=element_line(color="black",size = 0.75),
        axis.ticks=element_line(color="black",size = 0.75),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        legend.text = element_text(size = 13, color = "black"),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))
#################Figure 6F####################
setwd("Fig 6f/")
df <- read_excel("POINT.xlsx", sheet = 1)
my_comparisons <- list(c("Control", "DSS"),
                       c("DSS", "DSS+F. p"),
                       c("Control", "DSS+F. p"))
group_colors <- c("Control" = "#E69F00", "DSS" = "#009E73", "DSS+F. p" = "#56B4E9")
medians <- df %>%
  group_by(Group) %>%
  summarise(median = mean(Con)) %>%
  mutate(xmin = as.numeric(factor(Group)) - 0.25,
         xmax = as.numeric(factor(Group)) + 0.25,
         x = as.numeric(factor(Group)))
ggplot(df, aes(x = Group, y = Con, fill = Group)) +
  geom_point(position = position_jitter(width = 0.15), size = 5, shape = 21, stroke = 0.8, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, color = "black", size = 1) +
  geom_segment(data = medians,aes(x = xmin, xend = xmax, y = median, yend = median),
               inherit.aes = FALSE,size = 1,color = "black")+
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label = "p.format",
                     tip.length = 0.01, bracket.size = 0.6, size = 4) +
  scale_fill_manual(values = group_colors) +
  theme_classic(base_size = 14) +
  labs(y = "FITC-dextran (ug/mL)", x = NULL) +
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13),
        axis.line=element_line(color="black",size = 0.75),
        axis.ticks=element_line(color="black",size = 0.75),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        legend.text = element_text(size = 13, color = "black"),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+
  guides(fill = guide_legend(title = NULL))
ggsave("FITC-dextean.pdf", width = 4.36, height =3.23)

# Colon length
df <- read_excel("POINT.xlsx", sheet = 2)
my_comparisons <- list(c("Control", "DSS"),
                       c("DSS", "DSS+F. p"),
                       c("Control", "DSS+F. p"))
group_colors <- c("Control" = "#E69F00", "DSS" = "#009E73", "DSS+F. p" = "#56B4E9")
medians <- df %>%
  group_by(Group) %>%
  summarise(median = mean(Length)) %>%
  mutate(xmin = as.numeric(factor(Group)) - 0.25,
         xmax = as.numeric(factor(Group)) + 0.25,
         x = as.numeric(factor(Group)))
df2 <- df%>%
  group_by(Group) %>%
  mutate(
    Q1 = quantile(Length, 0.25, na.rm = TRUE),
    Q3 = quantile(Length, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1
  ) %>%
  filter(Length >= (Q1 - 1.5*IQR),
         Length <= (Q3 + 1.5*IQR)) %>%
  select(-Q1, -Q3, -IQR)



ggplot(df, aes(x = Group, y = Length, fill = Group)) +
  geom_point(position = position_jitter(width = 0.15), size = 5, shape = 21, stroke = 0.8, color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, color = "black", size = 1) +
  geom_segment(data = medians,aes(x = xmin, xend = xmax, y = median, yend = median),
               inherit.aes = FALSE,size = 1,color = "black")+
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label = "p.format",
                     tip.length = 0.01, bracket.size = 0.6, size = 4) +
  scale_fill_manual(values = group_colors) +
  theme_classic(base_size = 14) +
  labs(y = "Colon length (cm)", x = NULL) +
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13),
        axis.line=element_line(color="black",size = 0.75),
        axis.ticks=element_line(color="black",size = 0.75),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        legend.text = element_text(size = 13, color = "black"),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+
  guides(fill = guide_legend(title = NULL))
ggsave("Colon length.pdf", width = 4.36, height =3.23)


#################Figure 6G####################
setwd("Fig 6g/")
df <- read_excel("DSS+F.xlsx", sheet = 1)
df<-na.omit(df)
# 将数据转成长格式
df_long <- df %>%
  pivot_longer(cols = c(`0`, `1`, `2`, `3`,`4`,`5`,`6`,`7`), 
               names_to = "Day", 
               values_to = "Weight") %>%
  mutate(Day = as.numeric(Day)) %>%
  group_by(Group2, group) %>%
  mutate(
    weight_change = Weight / Weight[Day == 0] * 100  # 相对于 Day 0 的变化
  ) %>%
  ungroup()

# 汇总均值和标准误差
df_summary <- df_long %>%
  group_by(group, Day) %>%
  summarise(
    mean_weight = mean(weight_change, na.rm = TRUE),
    se_weight = sd(weight_change, na.rm = TRUE) / sqrt(n())*0.8,
    .groups = "drop"
  )

# 改名方便绘图
names(df_summary)[1] <- "Group"


ggplot() +
  geom_line(data =df_summary, aes(x = as.numeric(Day), y = mean_weight, color = Group, group = Group),size = 1) +
  geom_point(data =df_summary, aes(x = as.numeric(Day), y = mean_weight, color = Group, group = Group),size = 3) +
  geom_errorbar(data =df_summary, aes(x = as.numeric(Day), y = mean_weight,group = Group,ymin = mean_weight - se_weight, ymax = mean_weight + se_weight), width = 0.2,color="black") +
  labs(x = "Days", y = "Mouse weight (%)")+
  scale_color_manual(values = c("Control" = "#E69F00", "DSS" = "#009E73", "DSS+F. p" = "#56B4E9"))+
  scale_x_continuous(expand = c(0,0),breaks = c(0,1,2,3,4,5,6,7),limits = c(0,8))+
  scale_y_continuous(expand = c(0,0),breaks = c(85,90,95,100,105),limits = c(85,105))+
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        strip.text = element_text(size = 13),
        axis.text.y = element_text(size = 15, color = "black",  vjust = 0.5, hjust =1),
        axis.text.x= element_text(size = 15, color = "black",  vjust = 0.5, hjust =0.5),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))
ggsave("weight-FP.pdf", width =5.7, height =3.2)

df <- read_excel("DSS+F.xlsx", sheet = 2)
df<-na.omit(df)
# 将数据转成长格式
df_long <- df %>%
  pivot_longer(cols = c(`0`, `1`, `2`, `3`,`4`,`5`,`6`,`7`), 
               names_to = "Day", 
               values_to = "DAI") %>%
  mutate(Day = as.numeric(Day)) %>%
  ungroup()
df_long$DAI<-df_long$DAI
# 汇总均值和标准误差
df_summary <- df_long %>%
  group_by(group, Day) %>%
  summarise(
    mean_weight = mean(DAI, na.rm = TRUE),
    se_weight = sd(DAI, na.rm = TRUE) / sqrt(n())*0.8,
    .groups = "drop"
  )

# 改名方便绘图
names(df_summary)[1] <- "Group"


ggplot() +
  geom_line(data =df_summary, aes(x = as.numeric(Day), y = mean_weight, color = Group, group = Group),size = 1) +
  geom_point(data =df_summary, aes(x = as.numeric(Day), y = mean_weight, color = Group, group = Group),size = 3) +
  geom_errorbar(data =df_summary, aes(x = as.numeric(Day), y = mean_weight,group = Group,ymin = mean_weight - se_weight, ymax = mean_weight + se_weight), width = 0.2,color="black") +
  labs(x = "Days", y = "DAI")+
  scale_color_manual(values = c("Control" = "#E69F00", "DSS" = "#009E73", "DSS+F. p" = "#56B4E9"))+
  scale_x_continuous(expand = c(0,0),breaks = c(0,1,2,3,4,5,6,7),limits = c(0,8))+
  scale_y_continuous(expand = c(0,0),breaks = c(0,1,2,3,4),limits = c(0,4))+
  theme_classic()+
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        strip.text = element_text(size = 13),
        axis.text.y = element_text(size = 15, color = "black",  vjust = 0.5, hjust =1),
        axis.text.x= element_text(size = 15, color = "black",  vjust = 0.5, hjust =0.5),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))
ggsave("DAI-FP.pdf", width =5.7, height =3.2)


#################Figure 6H####################
setwd("Fig 6h/")
df <- read.csv("HE绘图.csv")
my_comparisons <- list(c("Control", "DSS"),
                       c("DSS", "DSS+F. p"),
                       c("Control", "DSS+F. p"))
group_colors <- c("Control" = "#DB8734", "DSS" = "#589A7E", "DSS+F. p" = "#5083A9")
df_A<-df[which(df$Group2=="A"),]
df_D<-df[which(df$Group2=="D"),]
medians <- df_A %>%
  group_by(Group1) %>%
  summarise(median = mean(Score)) %>%
  mutate(xmin = as.numeric(factor(Group1)) - 0.25,
         xmax = as.numeric(factor(Group1)) + 0.25,
         x = as.numeric(factor(Group1)))


ggplot(df_A, aes(x = Group1, y = Score, fill = Group1)) +
  geom_bar( aes(color = Group1),stat = "summary", fun = "mean", width = 0.6,fill="white", size = 1) +
  geom_quasirandom(aes(color = Group1, fill = Group1),position = position_jitter(width = 0.15), size = 3, shape = 21, stroke = 0.8,, groupOnX = TRUE) +
  stat_summary(aes(color = Group1),fun.data = mean_se, geom = "errorbar", width = 0.1, size = 1) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label = "p.format",
                     tip.length = 0.01, bracket.size = 0.6, size = 4) +
  scale_fill_manual(values = group_colors) +
  scale_color_manual(values = group_colors) +
  theme_classic(base_size = 14) +
  scale_y_continuous(expand = c(0,0),limits = c(0,4))+
  labs(y = "Histopathology score", x = NULL) +
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13),
        axis.line=element_line(color="black",size = 0.75),
        axis.ticks=element_line(color="black",size = 0.75),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        legend.text = element_text(size = 13, color = "black"),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+
  guides(fill = guide_legend(title = NULL))
ggsave("HE-A.pdf", width = 4.23, height =2.77)


medians <- df_D %>%
  group_by(Group1) %>%
  summarise(median = mean(Score)) %>%
  mutate(xmin = as.numeric(factor(Group1)) - 0.25,
         xmax = as.numeric(factor(Group1)) + 0.25,
         x = as.numeric(factor(Group1)))


ggplot(df_D, aes(x = Group1, y = Score, fill = Group1)) +
  geom_bar( aes(color = Group1),stat = "summary", fun = "mean", width = 0.6,fill="white", size = 1) +
  geom_quasirandom(aes(color = Group1, fill = Group1),position = position_jitter(width = 0.15), size = 3, shape = 21, stroke = 0.8,, groupOnX = TRUE) +
  stat_summary(aes(color = Group1),fun.data = mean_se, geom = "errorbar", width = 0.1, size = 1) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label = "p.format",
                     tip.length = 0.01, bracket.size = 0.6, size = 4) +
  scale_fill_manual(values = group_colors) +
  scale_color_manual(values = group_colors) +
  theme_classic(base_size = 14) +
  scale_y_continuous(expand = c(0,0),limits = c(0,5))+
  labs(y = "Histopathology score", x = NULL) +
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13),
        axis.line=element_line(color="black",size = 0.75),
        axis.ticks=element_line(color="black",size = 0.75),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        legend.text = element_text(size = 13, color = "black"),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+
  guides(fill = guide_legend(title = NULL))
ggsave("HE-D.pdf", width = 4.23, height =2.77)
#################Figure 6I####################
setwd("Fig 6i/")

df <- read.csv("β-半乳糖苷.csv")
my_comparisons <- list(c("Control", "DSS"),
                       c("DSS", "DSS+F. p"),
                       c("Control", "DSS+F. p"))
group_colors <- c("Control" = "#DB8734", "DSS" = "#589A7E", "DSS+F. p" = "#5083A9")
df_A<-df[which(df$Group2=="A"),]
df_D<-df[which(df$Group2=="D"),]
medians <- df_A %>%
  group_by(Group1) %>%
  summarise(median = mean(Value)) %>%
  mutate(xmin = as.numeric(factor(Group1)) - 0.25,
         xmax = as.numeric(factor(Group1)) + 0.25,
         x = as.numeric(factor(Group1)))


ggplot(df_A, aes(x = Group1, y = Value, fill = Group1)) +
  geom_bar( aes(color = Group1),stat = "summary", fun = "mean", width = 0.6,fill="white", size = 1) +
  geom_quasirandom(aes(color = Group1, fill = Group1),position = position_jitter(width = 0.15), size = 3, shape = 21, stroke = 0.8) +
  stat_summary(aes(color = Group1),fun.data = mean_se, geom = "errorbar", width = 0.1, size = 1) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label = "p.format",
                     tip.length = 0.01, bracket.size = 0.6, size = 4) +
  scale_fill_manual(values = group_colors) +
  scale_color_manual(values = group_colors) +
  theme_classic(base_size = 14) +
  scale_y_continuous(expand = c(0,0),limits = c(0,3.5))+
  labs(y = "SA-β-gal positive area(%)", x = NULL) +
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13),
        axis.line=element_line(color="black",size = 0.75),
        axis.ticks=element_line(color="black",size = 0.75),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        legend.text = element_text(size = 13, color = "black"),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+
  guides(fill = guide_legend(title = NULL))
ggsave("β-A.pdf", width = 4.23, height =2.77)


medians <- df_D %>%
  group_by(Group1) %>%
  summarise(median = mean(Value)) %>%
  mutate(xmin = as.numeric(factor(Group1)) - 0.25,
         xmax = as.numeric(factor(Group1)) + 0.25,
         x = as.numeric(factor(Group1)))


ggplot(df_D, aes(x = Group1, y = Value, fill = Group1)) +
  geom_bar( aes(color = Group1),stat = "summary", fun = "mean", width = 0.6,fill="white", size = 1) +
  geom_quasirandom(aes(color = Group1, fill = Group1),position = position_jitter(width = 0.15), size = 3, shape = 21, stroke = 0.8,, groupOnX = TRUE) +
  stat_summary(aes(color = Group1),fun.data = mean_se, geom = "errorbar", width = 0.1, size = 1) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label = "p.format",
                     tip.length = 0.01, bracket.size = 0.6, size = 4) +
  scale_fill_manual(values = group_colors) +
  scale_color_manual(values = group_colors) +
  theme_classic(base_size = 14) +
  scale_y_continuous(expand = c(0,0),limits = c(0,4.5))+
  labs(y = "Histopathology score", x = NULL) +
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13),
        axis.line=element_line(color="black",size = 0.75),
        axis.ticks=element_line(color="black",size = 0.75),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        legend.text = element_text(size = 13, color = "black"),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+
  guides(fill = guide_legend(title = NULL))
ggsave("β-D.pdf", width = 4.23, height =2.77)


#################Figure 6J####################
setwd("Fig 6j/")
df <- read.csv("DHE PLOT.csv")
my_comparisons <- list(c("Control", "DSS"),
                       c("DSS", "DSS+F. p"),
                       c("Control", "DSS+F. p"))
group_colors <- c("Control" = "#DB8734", "DSS" = "#589A7E", "DSS+F. p" = "#5083A9")
df_A<-df[which(df$Group2=="A"),]
df_D<-df[which(df$Group2=="D"),]
medians <- df_A %>%
  group_by(Group1) %>%
  summarise(median = mean(Score)) %>%
  mutate(xmin = as.numeric(factor(Group1)) - 0.25,
         xmax = as.numeric(factor(Group1)) + 0.25,
         x = as.numeric(factor(Group1)))


ggplot(df_A, aes(x = Group1, y = Score, fill = Group1)) +
  geom_bar( aes(color = Group1),stat = "summary", fun = "mean", width = 0.6,fill="white", size = 1) +
  geom_quasirandom(aes(color = Group1, fill = Group1),position = position_jitter(width = 0.15), size = 3, shape = 21, stroke = 0.8,, groupOnX = TRUE) +
  stat_summary(aes(color = Group1),fun.data = mean_se, geom = "errorbar", width = 0.1, size = 1) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label = "p.format",
                     tip.length = 0.01, bracket.size = 0.6, size = 4) +
  scale_fill_manual(values = group_colors) +
  scale_color_manual(values = group_colors) +
  theme_classic(base_size = 14) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1))+
  labs(y = "DHE/DAPI(a.u.)", x = NULL) +
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13),
        axis.line=element_line(color="black",size = 0.75),
        axis.ticks=element_line(color="black",size = 0.75),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        legend.text = element_text(size = 13, color = "black"),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+
  guides(fill = guide_legend(title = NULL))
ggsave("DHE-A.pdf", width = 4.23, height =2.77)


medians <- df_D %>%
  group_by(Group1) %>%
  summarise(median = mean(Score)) %>%
  mutate(xmin = as.numeric(factor(Group1)) - 0.25,
         xmax = as.numeric(factor(Group1)) + 0.25,
         x = as.numeric(factor(Group1)))


ggplot(df_D, aes(x = Group1, y = Score, fill = Group1)) +
  geom_bar( aes(color = Group1),stat = "summary", fun = "mean", width = 0.6,fill="white", size = 1) +
  geom_quasirandom(aes(color = Group1, fill = Group1),position = position_jitter(width = 0.15), size = 3, shape = 21, stroke = 0.8,, groupOnX = TRUE) +
  stat_summary(aes(color = Group1),fun.data = mean_se, geom = "errorbar", width = 0.1, size = 1) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label = "p.format",
                     tip.length = 0.01, bracket.size = 0.6, size = 4) +
  scale_fill_manual(values = group_colors) +
  scale_color_manual(values = group_colors) +
  theme_classic(base_size = 14) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1))+
  labs(y = "DHE/DAPI(a.u.)", x = NULL) +
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13),
        axis.line=element_line(color="black",size = 0.75),
        axis.ticks=element_line(color="black",size = 0.75),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        legend.text = element_text(size = 13, color = "black"),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+
  guides(fill = guide_legend(title = NULL))
ggsave("DHE-D.pdf", width = 4.23, height =2.77)

#################Figure 6K####################
setwd("Fig 6k/")
df <- read.csv("WB.csv")
my_comparisons <- list(c("Control", "DSS"),
                       c("DSS", "DSS+F. p"),
                       c("Control", "DSS+F. p"))
group_colors <- c("Control" = "#DB8734", "DSS" = "#589A7E", "DSS+F. p" = "#5083A9")
df_A<-df[which(df$Group2=="A"),]
df_D<-df[which(df$Group2=="D"),]
medians <- df_A %>%
  group_by(Group1) %>%
  summarise(median = mean(Ratio)) %>%
  mutate(xmin = as.numeric(factor(Group1)) - 0.25,
         xmax = as.numeric(factor(Group1)) + 0.25,
         x = as.numeric(factor(Group1)))


ggplot(df_A, aes(x = Group1, y = Ratio, fill = Group1)) +
  geom_bar( aes(color = Group1),stat = "summary", fun = "mean", width = 0.6,fill="white", size = 1) +
  geom_quasirandom(aes(color = Group1, fill = Group1),position = position_jitter(width = 0.15), size = 3, shape = 21, stroke = 0.8) +
  stat_summary(aes(color = Group1),fun.data = mean_se, geom = "errorbar", width = 0.1, size = 1) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label = "p.format",
                     tip.length = 0.01, bracket.size = 0.6, size = 4) +
  scale_fill_manual(values = group_colors) +
  scale_color_manual(values = group_colors) +
  theme_classic(base_size = 14) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.2))+
  labs(y = "Histopathology score", x = NULL) +
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13),
        axis.line=element_line(color="black",size = 0.75),
        axis.ticks=element_line(color="black",size = 0.75),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        legend.text = element_text(size = 13, color = "black"),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+
  guides(fill = guide_legend(title = NULL))
ggsave("WB-A.pdf", width = 4.5, height =2.5)


medians <- df_D %>%
  group_by(Group1) %>%
  summarise(median = mean(Ratio)) %>%
  mutate(xmin = as.numeric(factor(Group1)) - 0.25,
         xmax = as.numeric(factor(Group1)) + 0.25,
         x = as.numeric(factor(Group1)))


ggplot(df_D, aes(x = Group1, y = Ratio, fill = Group1)) +
  geom_bar( aes(color = Group1),stat = "summary", fun = "mean", width = 0.6,fill="white", size = 1) +
  geom_quasirandom(aes(color = Group1, fill = Group1),position = position_jitter(width = 0.15), size = 3, shape = 21, stroke = 0.8,, groupOnX = TRUE) +
  stat_summary(aes(color = Group1),fun.data = mean_se, geom = "errorbar", width = 0.1, size = 1) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test", label = "p.format",
                     tip.length = 0.01, bracket.size = 0.6, size = 4) +
  scale_fill_manual(values = group_colors) +
  scale_color_manual(values = group_colors) +
  theme_classic(base_size = 14) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1.2))+
  labs(y = "Histopathology score", x = NULL) +
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13),
        axis.line=element_line(color="black",size = 0.75),
        axis.ticks=element_line(color="black",size = 0.75),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        legend.text = element_text(size = 13, color = "black"),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+
  guides(fill = guide_legend(title = NULL))
ggsave("WB-D.pdf", width = 4.5, height =2.5)