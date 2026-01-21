library(reshape2);library(ggplot2);library(dbplyr);library(dplyr);library(ggsignif);library(ggrepel);library(ggpubr)
library(visNetwork);library(igraph)
setwd("C:/Users/11915/Desktop/PRDX6-laste/Code/Figure3/")
#################Figure 3B,C####################
setwd("Fig 3b,c/")
DATA<-read.csv("gly-rank.csv")
table(DATA$COLOR)
ggplot(DATA,aes(x=rank,y =-log10(P)))+
  #ggplot(aes(x=GROUP,y = value,color=AGE))+
  geom_line(group=1,color="grey60",size=1)+
  #geom_point(aes(color=DATA$COLOR),size=3) +
  geom_point(data=DATA[which(DATA$COLOR=="no"),],aes(x=rank,y =-log10(P)),color='grey',size=3) +
  geom_point(data=DATA[which(DATA$COLOR=="adhesion"),],aes(x=rank,y =-log10(P)),color='#AFA1C8',size=3) +
  geom_point(data=DATA[which(DATA$COLOR=="Immune"),],aes(x=rank,y =-log10(P)),color='#88A1C6',size=3) +
  geom_point(data=DATA[which(DATA$COLOR=="transcript"),],aes(x=rank,y =-log10(P)),color='#9FB489',size=3) +
  geom_point(data=DATA[which(DATA$COLOR=="metabolism"),],aes(x=rank,y =-log10(P)),color='#C67C74',size=3) +
  scale_color_manual(values =c('metabolism'='#C67C74','transcript'='#9FB489','Immune'='#88A1C6','adhesion'='#AFA1C8','no'='grey90'))+
  labs(x="Rank",y="-Log10(P Vaule)")+
  #geom_text_repel(aes(label =DATA$NAMES), size = 4,show.legend = F)+
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


ggsave("functional score2.pdf", width =3.5, height =3.2)

df_bar <- DATA %>%
  count(COLOR) %>%
  mutate(Proportion = n / sum(n)) %>%
  arrange(desc(Proportion)) %>%
  mutate(COLOR = factor(COLOR, levels = COLOR))
df_bar$Category <- factor(df_bar$COLOR, levels = df_bar$COLOR)  # 保持排序

ggplot(df_bar[which(df_bar$Category!="no"),], aes(x = "All", y = Proportion, fill = Category)) +
  geom_bar(stat = "identity", width = 0.4) +
  theme_minimal(base_size = 13) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values =c('metabolism'='#C67C74','transcript'='#9FB489','Immune'='#88A1C6','adhesion'='#AFA1C8'))+
  theme(#axis.text = element_line(),
    axis.ticks = element_line(),
    panel.grid = element_blank(),
    legend.position = "right") +
  scale_y_continuous(labels = scales::percent_format())

ggsave("functional score.pdf", width =4.8, height =3.2)

#################Figure 3D####################
setwd("Fig 3d/")
data<-read.csv("21pathway.csv")
MF<-data[which(data$Category=="GO Molecular Functions"),]
KEGG<-data[which(data$Category=="KEGG Pathway"),]
library(ggplot2)
ggplot(MF,aes(x =Term,y =-LogP,fill=-LogP))+
  geom_bar(stat="identity",width = 0.55)+
  coord_flip()+
  #scale_fill_gradient(high = "#58539f",low= "#BBBBD6")+
  scale_fill_gradient(high = "#C16E71",low = "#F1D0C6")+
  scale_x_discrete(limits=rev(MF$Term))+
  labs(y="Count",x="")+
  scale_y_continuous(expand = c(0,0),breaks = c(0,5,10,15)) + 
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5),legend.position = "bottom")  

ggsave("pathway-MF.pdf", width = 3, height = 4)


ggplot(KEGG,aes(x =Term,y =-LogP,fill=-LogP))+
  geom_bar(stat="identity",width = 0.55)+
  coord_flip()+
  scale_fill_gradient(high = "#6E8FB2",low= "#B5C3D7")+
  #scale_fill_gradient(high = "#d86967",low = "#EEBABB")+
  scale_x_discrete(limits=rev(KEGG$Term))+
  labs(y="Count",x="")+
  scale_y_continuous(expand = c(0,0),breaks = c(0,5,10,15,20)) + 
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_blank(),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5),legend.position = "bottom")  
ggsave("pathway-KEGG.pdf", width = 3, height = 4)
#################Figure 3E####################
setwd("Fig 3e/")
ppi <- read.csv("ppi1.csv",                 # 改成你的路径
                check.names = FALSE,
                stringsAsFactors = FALSE)

# ppi 现在应该有两列：#node1, node2
colnames(ppi)
# [1] "#node1" "node2"

edges <- ppi %>%
  transmute(from = node1,
            to   = node2)


g <- graph_from_data_frame(edges, directed = FALSE)
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

deg <- degree(g)  
nodes <- data.frame(
  id    = names(deg),
  label = names(deg),
  value = as.numeric(deg),   
  stringsAsFactors = FALSE
)

ppi2 <- read.csv("ppi2.csv",
                 check.names = FALSE,
                 stringsAsFactors = FALSE)
colnames(ppi2)
nodes <- nodes %>%
  left_join(ppi2, by = c("id" = "gene"))

if (!"group" %in% colnames(nodes)) {

  stop("ppi2.csv 里需要一列名为 'group' 的分组信息列")
}

nodes$group[is.na(nodes$group)] <- "Others"
group <- sort(unique(nodes$group))

nice_pal <- c(
  "#AFA1C8", # 1
  "#D4D2D4", # 2
  "#E19D27",
  "#88A1C6")
nice_pal <- nice_pal[seq_along(group)]
col_map <- setNames(nice_pal, group)

nodes$color.background           <- col_map[nodes$group]  # 点颜色
nodes$color.border               <- "#333333"             # 点的线（边框）颜色
nodes$color.highlight.background <- nodes$color.background
nodes$color.highlight.border     <- "#000000"


edges$color <- "#DBDBD0"  
edges$width <- 3      

nodes$label <- ""
p<-visNetwork(nodes, edges, height = "700px", width = "100%") %>%
  # 节点：value 映射到大小，颜色按 group
  visNodes(
    shape   = "dot",
    scaling = list(
      min = 10,  # 最小圈大小
      max = 30   # 最大圈大小
    ),
    shadow  = TRUE
  ) %>%
  # 边：设为曲线 + 颜色
  visEdges(
    smooth = list(
      enabled   = TRUE,
      type      = "curvedCW",  # 曲线类型，可以试 "curvedCCW" 等
      roundness = 0.2
    ),
    color = list(
      color     = "grey100",
      highlight = "#ff6b6b"
    )
  ) %>%
  # 交互：高亮邻居 + 节点选择
  visOptions(
    highlightNearest = list(enabled = TRUE, degree = 1),
    nodesIdSelection = TRUE
  ) %>%
  # 物理引擎关闭：点不会自己乱动，但你拖它会动
  visPhysics(enabled = FALSE) %>%
  visInteraction(
    dragNodes = TRUE,  # 允许你用鼠标拖节点
    dragView  = TRUE,
    zoomView  = TRUE
  )   %>%
  visExport(type = "pdf", name = "network")
p

#################Figure 3G,H####################
setwd("Fig 3g,h/")
data<-read.csv("prdx6-boxplot.csv")

data %>%
  mutate(AGE = factor(AGE , levels = c('Young','Middle age','Old'))) %>%
  #ggplot(aes(x=AGE,y = scale(value),color=AGE))+
  ggplot(aes(x=AGE,y = PRDX6_K63 ,color=AGE))+
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
  ggplot(aes(x=Aging.score,y = PRDX6_K63 ,fill=AGE))+
  geom_point(aes(fill=AGE),size=4,color="black",shape=21) +
  scale_fill_manual(values =c('Young'='#92A7C8','Middle age'='#EEBA83','Old'='#CC6063'))+
  labs(x="PRDX6_K63",y="Age score")+
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
