library(GSVA);library(ggplot2);library(ggsignif);library(ggpubr);library(reshape);library(dplyr)
setwd("C:/Users/11915/Desktop/PRDX6-laste/Code/Figure2/")

#################Figure 2A####################
setwd("Fig 2a/")
temp = list.files(pattern="*csv")
i=1
data<-read.csv(temp[i])
ALL<-data
for(i in 2:length(temp)) {
  data<-read.csv(temp[i])
  ALL<-rbind(ALL,data)
}
ALL<-ALL[which(ALL$gene=="VPS13A"|ALL$gene=="LEMD2"|
                 ALL$gene=="NEK9"|ALL$gene=="PGM2"|
                 ALL$gene=="CDKN2AIP"|ALL$gene=="VPS51"|
                 ALL$gene=="HK3"|ALL$gene=="STK38"|
                 ALL$gene=="NIPSNAP3A"|ALL$gene=="hpx"),]

ALL %>%
  mutate(gene = factor(gene, levels = c( "VPS13A","LEMD2","NEK9","PGM2","CDKN2AIP","VPS51","STK38","NIPSNAP3A","HK3","HPX"))) %>%
  ggplot(aes(y=D,x=group))+
  geom_boxplot(aes(color=group),outlier.colour = NA,width=0.65,size=0.75,fill="white") +
  geom_jitter(aes(color=group),stroke =1.5,size=1,position = position_jitter(0.2),fill="white")+
  scale_color_manual(values =c('ctrl'='#7171B8','gene'='#D85656'),guide=FALSE)+
  geom_signif(comparisons =  list(c("ctrl", "gene")),test = "t.test",map_signif_level =T, step_increase = 0.5,na.rm =T, show.legend = NA)+
  scale_y_continuous(limits =  c(0,1.00)) + 
  labs(x="",y="Relative intestinal width")+
  theme(strip.background.x = element_rect(color="black"),
        strip.text.x=element_text(size = 13, color = "black"),
        axis.line=element_line(color="black"),
        axis.ticks=element_line(color="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        panel.border = element_rect(color="black",fill=NA),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_blank(),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5)) +
  facet_wrap( ~gene, ncol =4,scales="free")
ggsave("线虫实验.pdf", width = 8, height =6)
#################Figure 2B####################
setwd("Fig 2b/")
data=read.csv("pro.csv",row.names = 1) #因为下载的是count矩阵，如果是tpm这一步可以省略
#data=read.csv("../第六版/FIG5/PRO/PRDX6_PROTEIN.csv",row.names = 3)
dat=data.frame(t(data[,-c(1:3)]))
#dat=data.frame(data[,-c(1:2)])
uni_matrix <- dat
gene_set<- read.csv("list-10.csv",header = T)
bg_genes <- split(as.matrix(gene_set)[,1], gene_set[,2])
gsva_matrix <- gsva(as.matrix(uni_matrix), bg_genes, method='ssgsea', kcdf='Gaussian', 
                    mx.diff = F,verbose = T,ssgsea.norm=TRUE,min.sz=2)
score<-data.frame(t(gsva_matrix))
all_scores<-cbind(data[1:3],score)
all_scores$Aging<-all_scores$pos-all_scores$neg
write.csv(all_scores,"AGE Score-PRO.csv")

data2=read.csv("AGE Score-PRO10.csv")
data %>%
  mutate(AGE = factor(AGE , levels = c('Young','Middle age','Old'))) %>%
  #ggplot(aes(x=AGE,y = scale(value),color=AGE))+
  ggplot(aes(x=AGE,y = Aging,color=AGE))+
  geom_boxplot(aes(fill=AGE),outlier.colour = NA,width = 0.65,position = position_dodge(0.6),size=0.75,color="black") +
  geom_jitter(aes(group=AGE,fill=AGE),size=3,stroke =0.75,position = position_jitterdodge(jitter.width = 0.85,dodge.width = 0.7),color="black",shape=21)+
  stat_compare_means(method = "anova",paired = F,label = "p.format")+
  #geom_signif(comparisons =  list(c('Young','Middle age'),c('Young', "Old"),c('Middle age', "Old")),test = "t.test",step_increase = 0.1,map_signif_level =T)+
  scale_fill_manual(values =c('Young'='#92A7C8','Middle age'='#EEBA83','Old'='#CC6063'))+
  labs(x="",y="Age score")+
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13, color = "white"),
        axis.line=element_line(color="black",size = 0.75),
        axis.ticks=element_line(color="black",size = 0.75),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        legend.text = element_text(size = 13, color = "black"),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))

data %>%
  mutate(AGE = factor(AGE , levels = c('Young','Middle age','Old'))) %>%
  #ggplot(aes(x=AGE,y = scale(value),color=AGE))+
  ggplot(aes(x=GENDER,y = Aging,color=AGE))+
  geom_boxplot(aes(fill=AGE),outlier.colour = NA,width = 0.65,position = position_dodge(0.6),size=0.75,color="black") +
  geom_jitter(aes(group=AGE,fill=AGE),size=3,stroke =0.75,position = position_jitterdodge(jitter.width = 0.85,dodge.width = 0.7),color="black",shape=21)+
  stat_compare_means(method = "anova",paired = F,label = "p.format")+
  #geom_signif(comparisons =  list(c('Young','Middle age'),c('Young', "Old"),c('Middle age', "Old")),test = "t.test",step_increase = 0.1,map_signif_level =T)+
  scale_fill_manual(values =c('Young'='#92A7C8','Middle age'='#EEBA83','Old'='#CC6063'))+
  labs(x="",y="Age score")+
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13, color = "white"),
        axis.line=element_line(color="black",size = 0.75),
        axis.ticks=element_line(color="black",size = 0.75),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        legend.text = element_text(size = 13, color = "black"),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))
#################Figure 2C####################
setwd("Fig 2c/")
{anova_df <- read.csv("acAOV_test.csv", check.names = FALSE)
cor_df <- read.csv("accor_test.csv", check.names = FALSE)

# 共用的P值列（以P_开头）
common_cols <- intersect(colnames(anova_df), colnames(cor_df))
p_cols <- common_cols[grepl("^P_", common_cols)]

# 初始化结果数据框
plot_df <- data.frame()
# 遍历每个公共P值列
col="P_ALL"
for (col in p_cols) {
  anova_sig <- anova_df %>% filter(!!sym(col) < 0.05)
  cor_sig <- cor_df %>% filter(!!sym(col) < 0.05)
  
  # 取交集基因
  common_genes <- intersect(anova_sig$name, cor_sig$name)  # 第1列是名字列
  
  if (length(common_genes) > 0) {
    for (gene in common_genes) {
      # 提取对应行
      
      anova_row <- anova_sig %>% filter(name == gene)
      cor_row <- cor_sig %>% filter( name== gene)
      
      # 形状由RHO正负决定
      
      p_aov<-anova_row %>% pull(gsub("^P_", "P_", col))
      p_cor<-cor_row %>% pull(gsub("^P_", "P_", col))
      rho <- cor_row %>% pull(gsub("^P_", "RHO_", col))
      shape <- ifelse(rho >= 0, "triangle", "circle")
      
      # 颜色由Y/O中FC决定
      # 提取该列的后缀（如 F、RF、AM 等）
      region_suffix <- gsub("^P_", "", col)
      
      # 构造对应的中位数列名
      y_col <- paste0("Y", region_suffix, "_median")
      m_col <- paste0("M", region_suffix, "_median")
      o_col <- paste0("O", region_suffix, "_median")
      
      # 提取中位数值（如果列不存在就用 NA）
      y <- if (y_col %in% colnames(anova_row)) anova_row[[y_col]] else NA
      m <- if (m_col %in% colnames(anova_row)) anova_row[[m_col]] else NA
      o <- if (o_col %in% colnames(anova_row)) anova_row[[o_col]] else NA
      
      # 判断颜色：O > Y 为红色，Y > O 为蓝色
      color <- if (!is.na(y) && !is.na(o)) {
        color <- if (o > y) {
          "#B51700"
        } else if (y > o) {
          "#004B7F"
        } else {
          "gray"
        }
      } else {
        "gray"
      }
      
      # 收集信息
      plot_df <- rbind(plot_df, data.frame(
        Gene = gene,
        Region =region_suffix,
        RHO = rho,
        P_A=p_aov,
        P_C=p_cor,
        Shape = shape,
        Color = color
      ))
    }
  }
}
table(plot_df$Region)
# 可视化预览
plot_df$Region <- factor(plot_df$Region,
                         levels = c("ALL", "AF", "TF", "DF", "RF", "AM", "TM", "DM", "RM"),
                         ordered = TRUE)
head(plot_df)
plot_df$Color
# 示例绘图（你可以自行调整）
ggplot(plot_df, aes(x = -log10(P_C), y = -log10(P_A), shape = Shape, color = Color)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_shape_manual(values = c(triangle = 17, circle = 16)) +
  #scale_x_continuous(limits = c(1,6),breaks=c(2,6),expand = c(0,0))+
  scale_color_manual(values = c("#004B7F"= "#004B7F", "#B51700" = "#B51700")) +
  theme(strip.background.x = element_rect(color="white", fill="white"),
        strip.text.x=element_text(size = 13),
        axis.line=element_line(color="black",size = 0.75),
        axis.ticks=element_line(color="black",size = 0.75),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        legend.text = element_text(size = 13, color = "black"),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust =0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+facet_wrap(~Region,ncol=9, drop = FALSE)
ggsave("sandian plot ac.pdf", width =12, height = 2.4)}
{anova_df <- read.csv("glyAOV_test.csv", check.names = FALSE)
  cor_df <- read.csv("glycor_test.csv", check.names = FALSE)
  
  # 共用的P值列（以P_开头）
  common_cols <- intersect(colnames(anova_df), colnames(cor_df))
  p_cols <- common_cols[grepl("^P_", common_cols)]
  
  # 初始化结果数据框
  plot_df <- data.frame()
  # 遍历每个公共P值列
  for (col in p_cols) {
    anova_sig <- anova_df %>% filter(!!sym(col) < 0.05)
    cor_sig <- cor_df %>% filter(!!sym(col) < 0.05)
    
    # 取交集基因
    common_genes <- intersect(anova_sig$name, cor_sig$name)  # 第1列是名字列
    
    if (length(common_genes) > 0) {
      for (gene in common_genes) {
        # 提取对应行
        
        anova_row <- anova_sig %>% filter(name == gene)
        cor_row <- cor_sig %>% filter( name== gene)
        
        # 形状由RHO正负决定
        
        p_aov<-anova_row %>% pull(gsub("^P_", "P_", col))
        p_cor<-cor_row %>% pull(gsub("^P_", "P_", col))
        rho <- cor_row %>% pull(gsub("^P_", "RHO_", col))
        shape <- ifelse(rho >= 0, "triangle", "circle")
        
        # 颜色由Y/O中FC决定
        # 提取该列的后缀（如 F、RF、AM 等）
        region_suffix <- gsub("^P_", "", col)
        
        # 构造对应的中位数列名
        y_col <- paste0("Y", region_suffix, "_median")
        m_col <- paste0("M", region_suffix, "_median")
        o_col <- paste0("O", region_suffix, "_median")
        
        # 提取中位数值（如果列不存在就用 NA）
        y <- if (y_col %in% colnames(anova_row)) anova_row[[y_col]] else NA
        m <- if (m_col %in% colnames(anova_row)) anova_row[[m_col]] else NA
        o <- if (o_col %in% colnames(anova_row)) anova_row[[o_col]] else NA
        
        # 判断颜色：O > Y 为红色，Y > O 为蓝色
        color <- if (!is.na(y) && !is.na(o)) {
          color <- if (o > y) {
            "#B51700"
          } else if (y > o) {
            "#004B7F"
          } else {
            "gray"
          }
        } else {
          "gray"
        }
        
        # 收集信息
        plot_df <- rbind(plot_df, data.frame(
          Gene = gene,
          Region =region_suffix,
          RHO = rho,
          P_A=p_aov,
          P_C=p_cor,
          Shape = shape,
          Color = color
        ))
      }
    }
  }
  table(plot_df$Region)
  # 可视化预览
  plot_df$Region <- factor(plot_df$Region,
                           levels = c("ALL", "AF", "TF", "DF", "RF", "AM", "TM", "DM", "RM"),
                           ordered = TRUE)
  head(plot_df)
  plot_df$Color
  # 示例绘图（你可以自行调整）
  ggplot(plot_df, aes(x = -log10(P_C), y = -log10(P_A), shape = Shape, color = Color)) +
    geom_point(size = 2, alpha = 0.8) +
    scale_shape_manual(values = c(triangle = 17, circle = 16)) +
    scale_x_continuous(breaks=c(2,4,6,8))+
    scale_color_manual(values = c("#004B7F"= "#004B7F", "#B51700" = "#B51700")) +
    theme(strip.background.x = element_rect(color="white", fill="white"),
          strip.text.x=element_text(size = 13),
          axis.line=element_line(color="black",size = 0.75),
          axis.ticks=element_line(color="black",size = 0.75),
          panel.background = element_rect(fill = "white"),
          panel.grid=element_blank(),
          legend.text = element_text(size = 13, color = "black"),
          axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
          axis.text.x= element_text(size = 13, color = "black",   hjust =0.5),
          title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+facet_wrap(~Region,ncol=9, drop = FALSE)
  ggsave("sandian plot gly.pdf", width =12, height = 2.4)}
{anova_df <- read.csv("phosAOV_test.csv", check.names = FALSE)
  cor_df <- read.csv("phoscor_test.csv", check.names = FALSE)
  
  # 共用的P值列（以P_开头）
  common_cols <- intersect(colnames(anova_df), colnames(cor_df))
  p_cols <- common_cols[grepl("^P_", common_cols)]
  
  # 初始化结果数据框
  plot_df <- data.frame()
  # 遍历每个公共P值列
  for (col in p_cols) {
    anova_sig <- anova_df %>% filter(!!sym(col) < 0.05)
    cor_sig <- cor_df %>% filter(!!sym(col) < 0.05)
    
    # 取交集基因
    common_genes <- intersect(anova_sig$name, cor_sig$name)  # 第1列是名字列
    
    if (length(common_genes) > 0) {
      for (gene in common_genes) {
        # 提取对应行
        
        anova_row <- anova_sig %>% filter(name == gene)
        cor_row <- cor_sig %>% filter( name== gene)
        
        # 形状由RHO正负决定
        
        p_aov<-anova_row %>% pull(gsub("^P_", "P_", col))
        p_cor<-cor_row %>% pull(gsub("^P_", "P_", col))
        rho <- cor_row %>% pull(gsub("^P_", "RHO_", col))
        shape <- ifelse(rho >= 0, "triangle", "circle")
        
        # 颜色由Y/O中FC决定
        # 提取该列的后缀（如 F、RF、AM 等）
        region_suffix <- gsub("^P_", "", col)
        
        # 构造对应的中位数列名
        y_col <- paste0("Y", region_suffix, "_median")
        m_col <- paste0("M", region_suffix, "_median")
        o_col <- paste0("O", region_suffix, "_median")
        
        # 提取中位数值（如果列不存在就用 NA）
        y <- if (y_col %in% colnames(anova_row)) anova_row[[y_col]] else NA
        m <- if (m_col %in% colnames(anova_row)) anova_row[[m_col]] else NA
        o <- if (o_col %in% colnames(anova_row)) anova_row[[o_col]] else NA
        
        # 判断颜色：O > Y 为红色，Y > O 为蓝色
        color <- if (!is.na(y) && !is.na(o)) {
          color <- if (o > y) {
            "#B51700"
          } else if (y > o) {
            "#004B7F"
          } else {
            "gray"
          }
        } else {
          "gray"
        }
        
        # 收集信息
        plot_df <- rbind(plot_df, data.frame(
          Gene = gene,
          Region =region_suffix,
          RHO = rho,
          P_A=p_aov,
          P_C=p_cor,
          Shape = shape,
          Color = color
        ))
      }
    }
  }
  table(plot_df$Region)
  # 可视化预览
  plot_df$Region <- factor(plot_df$Region,
                           levels = c("ALL", "AF", "TF", "DF", "RF", "AM", "TM", "DM", "RM"),
                           ordered = TRUE)
  head(plot_df)
  plot_df$Color
  # 示例绘图（你可以自行调整）
  ggplot(plot_df, aes(x = -log10(P_C), y = -log10(P_A), shape = Shape, color = Color)) +
    geom_point(size = 2, alpha = 0.8) +
    scale_shape_manual(values = c(triangle = 17, circle = 16)) +
    scale_x_continuous(breaks=c(2,4,6,8))+
    scale_color_manual(values = c("#004B7F"= "#004B7F", "#B51700" = "#B51700")) +
    theme(strip.background.x = element_rect(color="white", fill="white"),
          strip.text.x=element_text(size = 13),
          axis.line=element_line(color="black",size = 0.75),
          axis.ticks=element_line(color="black",size = 0.75),
          panel.background = element_rect(fill = "white"),
          panel.grid=element_blank(),
          legend.text = element_text(size = 13, color = "black"),
          axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
          axis.text.x= element_text(size = 13, color = "black",   hjust =0.5),
          title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+facet_wrap(~Region,ncol=9, drop = FALSE)
  ggsave("sandian plot phos.pdf", width =12, height = 2.4)}
#################Figure 2D####################
setwd("Fig 2d/")
data<-read.csv("pathway-2.csv")
ggplot(data, aes( y =data$Term,x=data$Group)) +
  geom_point(aes(fill =data$Log10p,size=data$Count),shape=21) + # 注意将width宽度设小      # 将点的size设置大一些比较好看
  scale_y_discrete(limits=rev(unique(c(data$Term))))+
  scale_x_discrete(limits=c("ac_UP","ac_DOWN","Gly_UP","Gly_DOWN","phos_UP","phos_DOWN"))+
  scale_fill_gradient2(low="#737AAC",mid="white",high = "#AF322F")+
  #scale_color_gradient2(low="#737AAC",mid="white",high = "#AF322F")+
  scale_size_continuous(range = c(3,8))+
  theme(strip.background.x = element_rect(color="white", fill="white"),
        axis.line=element_line(linetype=1,color="black",size=0.75),
        panel.border = element_rect(fill=NA,color="black", size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_line(color="grey",linetype=2,size=0.75),
        axis.text.y= element_blank(),
        axis.text.x= element_blank(),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5),legend.position = "bottom")
ggsave("pathway-all.pdf", width =2.5, height =9.8)