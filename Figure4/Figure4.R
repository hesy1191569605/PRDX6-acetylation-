library(reshape2);library(ggplot2);library(dbplyr);library(dplyr);library(ggsignif);library(ggrepel);library(ggpubr)
library(visNetwork);library(igraph)
setwd("C:/Users/11915/Desktop/PRDX6-laste/Code/Figure4/")


#################Figure 4A####################
setwd("Fig 4a/")
DATA<-read.csv("Gal3.csv")
data %>%  
  group_by(Group) %>% 
  summarise(mean_value=mean(Value),sd_value=sd(Value)) -> df1
df1 %>% 
  group_by(Group) %>% 
  mutate(new_col=cumsum(mean_value)) -> df2


ggplot(data=df2,aes(x=Group,y=mean_value,fill=Group))+
  geom_errorbar(aes(ymin=new_col-sd_value,ymax=new_col+sd_value,x=Group),position =position_dodge(0.7),stat="identity",width=0.35,size=1.2)+
  geom_bar(aes(y=mean_value,fill=Group),position = "dodge",stat="identity",width = 0.7)+
  geom_jitter(data=data,aes(x=Group,y=Value,colors=Group,color=Group),shape=21,stroke =1.5,size=3.5,position = position_jitterdodge(0.2),fill="white")+
  scale_fill_manual(values = c("1-sh"="#D6E5ED","2-WT"="#8CA6C1","3-Q"="#C68180","4-R"="#E5B69B"),guide=FALSE)+
  scale_color_manual(values = c("1-sh"="#8CA6C1","2-WT"="#406B97","3-Q"="#A02D2B","4-R"="#D38559"),guide=FALSE)+
  geom_signif(comparisons =  list(c('3-Q','4-R'),c('1-sh', "2-WT"),c('2-WT', '3-Q'),c('2-WT', '4-R')),test = "t.test",step_increase = 0.02,map_signif_level =T,color="black")+
  scale_y_continuous(limits = c(0,2),breaks=seq(0,2,0.5),expand = c(0,0))+
  labs(x="",y="% ")+
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 0.5),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))
ggsave("GAL.pdf", width =4.65, height =4)

#################Figure 4B####################
setwd("Fig 4b/")
data<-read.csv("ROS.csv")

data %>%  
  group_by(Group) %>% 
  summarise(mean_value=mean(Value),sd_value=sd(Value)) -> df1
df1 %>% 
  group_by(Group) %>% 
  mutate(new_col=cumsum(mean_value)) -> df2


ggplot(data=df2,aes(x=Group,y=mean_value,fill=Group))+
  geom_errorbar(aes(ymin=new_col-sd_value,ymax=new_col+sd_value,x=Group),position =position_dodge(0.7),stat="identity",width=0.35,size=1.2)+
  geom_bar(aes(y=mean_value,fill=Group),position = "dodge",stat="identity",width = 0.7)+
  geom_jitter(data=data,aes(x=Group,y=Value,colors=Group,color=Group),shape=21,stroke =1.5,size=3.5,position = position_jitterdodge(0.2),fill="white")+
  scale_fill_manual(values = c("1-sh"="#D6E5ED","2-WT"="#8CA6C1","3-Q"="#C68180","4-R"="#E5B69B"),guide=FALSE)+
  scale_color_manual(values = c("1-sh"="#8CA6C1","2-WT"="#406B97","3-Q"="#A02D2B","4-R"="#D38559"),guide=FALSE)+
  geom_signif(comparisons =  list(c('3-Q','4-R'),c('1-sh', "2-WT"),c('2-WT', '3-Q'),c('2-WT', '4-R')),test = "t.test",step_increase = 0.02,map_signif_level =T,color="black")+
  scale_y_continuous(limits = c(0,40),breaks=seq(0,40,10),expand = c(0,0))+
  labs(x="",y="% ")+
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 0.5),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))
ggsave("ROS.pdf", width =4.65, height =4)


data<-read.csv("GPx.csv")

data %>%  
  group_by(Group) %>% 
  summarise(mean_value=mean(Value),sd_value=sd(Value)) -> df1
df1 %>% 
  group_by(Group) %>% 
  mutate(new_col=cumsum(mean_value)) -> df2


ggplot(data=df2,aes(x=Group,y=mean_value,fill=Group))+
  geom_errorbar(aes(ymin=new_col-sd_value,ymax=new_col+sd_value,x=Group),position =position_dodge(0.7),stat="identity",width=0.35,size=1.2)+
  geom_bar(aes(y=mean_value,fill=Group),position = "dodge",stat="identity",width = 0.7)+
  geom_jitter(data=data,aes(x=Group,y=Value,colors=Group,color=Group),shape=21,stroke =1.5,size=3.5,position = position_jitterdodge(0.2),fill="white")+
  scale_fill_manual(values = c("1-sh"="#D6E5ED","2-WT"="#8CA6C1","3-Q"="#C68180","4-R"="#E5B69B"),guide=FALSE)+
  scale_color_manual(values = c("1-sh"="#8CA6C1","2-WT"="#406B97","3-Q"="#A02D2B","4-R"="#D38559"),guide=FALSE)+
  geom_signif(comparisons =  list(c('3-Q','4-R'),c('1-sh', "2-WT"),c('2-WT', '3-Q'),c('2-WT', '4-R')),test = "t.test",step_increase = 0.02,map_signif_level =T,color="black")+
  scale_y_continuous(limits = c(0,80),breaks=seq(0,80,20),expand = c(0,0))+
  labs(x="",y="% ")+
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 0.5),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))
ggsave("GPx.pdf",width =4.65, height =4)


data<-read.csv("GSSG.csv")
data %>%  
  group_by(Group) %>% 
  summarise(mean_value=mean(Value),sd_value=sd(Value)) -> df1
df1 %>% 
  group_by(Group) %>% 
  mutate(new_col=cumsum(mean_value)) -> df2


ggplot(data=df2,aes(x=Group,y=mean_value,fill=Group))+
  geom_errorbar(aes(ymin=new_col-sd_value,ymax=new_col+sd_value,x=Group),position =position_dodge(0.7),stat="identity",width=0.35,size=1.2)+
  geom_bar(aes(y=mean_value,fill=Group),position = "dodge",stat="identity",width = 0.7)+
  geom_jitter(data=data,aes(x=Group,y=Value,colors=Group,color=Group),shape=21,stroke =1.5,size=3.5,position = position_jitterdodge(0.2),fill="white")+
  scale_fill_manual(values = c("1-sh"="#D6E5ED","2-WT"="#8CA6C1","3-Q"="#C68180","4-R"="#E5B69B"),guide=FALSE)+
  scale_color_manual(values = c("1-sh"="#8CA6C1","2-WT"="#406B97","3-Q"="#A02D2B","4-R"="#D38559"),guide=FALSE)+
  geom_signif(comparisons =  list(c('3-Q','4-R'),c('1-sh', "2-WT"),c('2-WT', '3-Q'),c('2-WT', '4-R')),test = "t.test",step_increase = 0.02,map_signif_level =T,color="black")+
  scale_y_continuous(limits = c(0,2.5),breaks=seq(0,2.5,0.5),expand = c(0,0))+
  labs(x="",y="% ")+
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 0.5),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))
ggsave("GSSG.pdf",width =4.65, height =4)

data<-read.csv("GSH_GSSG.csv")
data %>%  
  group_by(Group) %>% 
  summarise(mean_value=mean(Value),sd_value=sd(Value)) -> df1
df1 %>% 
  group_by(Group) %>% 
  mutate(new_col=cumsum(mean_value)) -> df2


ggplot(data=df2,aes(x=Group,y=mean_value,fill=Group))+
  geom_errorbar(aes(ymin=new_col-sd_value,ymax=new_col+sd_value,x=Group),position =position_dodge(0.7),stat="identity",width=0.35,size=1.2)+
  geom_bar(aes(y=mean_value,fill=Group),position = "dodge",stat="identity",width = 0.7)+
  geom_jitter(data=data,aes(x=Group,y=Value,colors=Group,color=Group),shape=21,stroke =1.5,size=3.5,position = position_jitterdodge(0.2),fill="white")+
  scale_fill_manual(values = c("1-sh"="#D6E5ED","2-WT"="#8CA6C1","3-Q"="#C68180","4-R"="#E5B69B"),guide=FALSE)+
  scale_color_manual(values = c("1-sh"="#8CA6C1","2-WT"="#406B97","3-Q"="#A02D2B","4-R"="#D38559"),guide=FALSE)+
  geom_signif(comparisons =  list(c('3-Q','4-R'),c('1-sh', "2-WT"),c('2-WT', '3-Q'),c('2-WT', '4-R')),test = "t.test",step_increase = 0.02,map_signif_level =T,color="black")+
  scale_y_continuous(limits = c(0,15),breaks=seq(0,15,5),expand = c(0,0))+
  labs(x="",y="% ")+
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 0.5),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))
ggsave("GSH_GSSG.pdf", width =4.65, height =4)


#################Figure 4C####################
setwd("Fig 4a/")
data <- read.csv("cell-pro.csv", stringsAsFactors = FALSE)

df <-  data%>%
  pivot_longer(
    cols = names(data)[-c(1:2)],   # 除去元数据列，其他都当成指标
    names_to  = "variable",
    values_to = "value"
  ) %>%
  mutate(Gene = factor(Gene, levels = c("WT1","WT2","WT3",
                                        "KQ1","KQ2","KQ3",
                                        "KR1","KR2","KR3")))

df <- df %>%
  mutate(
    Group = factor(Group, levels = c("WT", "KQ", "KR")),
    # 面板顺序（按你图里从左到右、从上到下的顺序排）
    variable = factor(
      variable,
      levels = c("STK38", "LEMD2","PGM2", "CDKN2AIP", "VPS13A", "NIPSNAP3A", "VPS51", "NEK9")))


df %>%
  #mutate(AGE = factor(AGE , levels = c('Young','Middle age','Old'))) %>%
  mutate(Group = factor(Group, levels = c('WT','KQ',"KR"))) %>%
  #ggplot(aes(x=AGE,y = scale(value),color=AGE))+
  ggplot(aes(x=Group,y = value,color=Group))+
  geom_boxplot(aes(fill=Group),outlier.colour = NA,width = 0.55,position = position_dodge(0.6),size=0.75,color="black") +
  geom_jitter(aes(group=Group,fill=Group),size=3,stroke =0.75,position = position_jitterdodge(jitter.width = 0.85,dodge.width = 0.7),color="black",shape=21)+
  #stat_compare_means(method = "t.tes",paired = F,label = "p.format")+
  geom_signif(comparisons =  list(c('WT','KQ'),c('WT', "KR"),c('KQ', "KR")),test = "t.test",step_increase = 0.1,map_signif_level =F,color="black")+
  scale_fill_manual(values =c('WT'='#92A7C8','KQ'='#EEBA83','KR'='#CC6063'))+
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
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+
  facet_wrap(~ variable, nrow = 2, scales = "free_y") 
ggsave("Age score genes-cell.pdf", width = 9, height =4.5)

#################Figure 4D####################
setwd("Fig 4d/")
data<-read.csv("pro-test.csv",header = T)
df1<- data

df1$color_pre[(df1$P_K63Q_K63R >= 0.05)]<-"no"
df1$color_pre[(df1$P_K63Q_K63R< 0.05)&(data$FC_K63Q_K63R>2)] <- "up"
df1$color_pre[(df1$P_K63Q_K63R< 0.05)&(data$FC_K63Q_K63R < 0.5)] <- "down"
df1$color_pre[is.na(df1$color_pre)]<-"no"

table(df1$color_pre)

ggplot(df1,aes(x=log2(FC_K63Q_K63R),y=-log10(P_K63Q_K63R),color=color_pre))+
  geom_point(aes(color=color_pre),size=2)+
  scale_color_manual(values =c('up'='#C96F6D','down'='#92A7C8','no'='grey'),guide=FALSE)+ 
  #geom_text_repel(aes(label =data$X), size = 3,show.legend = F)+
  geom_hline(yintercept =-log10(0.05),linetype=2,color="grey",size=1)+
  xlim(-3,3)+
  labs(x="Log2(after/before)",y="-log10(P_Value)")+
  #theme(panel.background = element_rect(fill='transparent',color = 'black'),text =element_blank(),legend.position = "none")
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 15, color = "black",   hjust = 0.5),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))  
ggsave("pro_DIFF.pdf", width = 4.5, height =4)




#################Figure 4E####################
setwd("Fig 4e/")

data<-read.csv("pro-pathway.csv")

ggplot(data, aes( x =data$Term,y=data$Group)) +
  geom_point(aes(fill =data$P),shape=21,color="grey20",size=8) + # 注意将width宽度设小      # 将点的size设置大一些比较好看
  scale_x_discrete(limits=c(data$Term))+
  scale_fill_gradient2(low="#737AAC",mid="white",high = "#AF322F")+
  theme(strip.background.x = element_rect(color="white", fill="white"),
        axis.line=element_line(linetype=1,color="black",size=0.75),
        panel.border = element_rect(fill=NA,color="black", size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_line(color="grey",linetype=2,size=0.75),
        axis.text.y= element_blank(),
        axis.text.x= element_blank(),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5),legend.position = "bottom")
ggsave("pathway.pdf", width = 6, height = 2)
#################Figure 4F####################
setwd("Fig 4f/")

data<-read.csv("JAM1.csv")

data %>%  
  group_by(Group) %>% 
  summarise(mean_value=mean(Value),sd_value=sd(Value)) -> df1
df1 %>% 
  group_by(Group) %>% 
  mutate(new_col=cumsum(mean_value)) -> df2


ggplot(data=df2,aes(x=Group,y=mean_value,fill=Group))+
  geom_errorbar(aes(ymin=new_col-sd_value,ymax=new_col+sd_value,x=Group),position =position_dodge(0.7),stat="identity",width=0.35,size=1.2)+
  geom_bar(aes(y=mean_value,fill=Group),position = "dodge",stat="identity",width = 0.7)+
  geom_jitter(data=data,aes(x=Group,y=Value,colors=Group,color=Group),shape=21,stroke =1.5,size=3.5,position = position_jitterdodge(0.2),fill="white")+
  scale_fill_manual(values = c("1-sh"="#D6E5ED","2-WT"="#8CA6C1","3-Q"="#C68180","4-R"="#E5B69B"),guide=FALSE)+
  scale_color_manual(values = c("1-sh"="#8CA6C1","2-WT"="#406B97","3-Q"="#A02D2B","4-R"="#D38559"),guide=FALSE)+
  geom_signif(comparisons =  list(c('3-Q','4-R'),c('1-sh', "2-WT"),c('2-WT', '3-Q'),c('2-WT', '4-R')),test = "t.test",step_increase = 0.02,map_signif_level =T,color="black")+
  scale_y_continuous(limits = c(0,2.0),breaks=seq(0,2.0,0.5),expand = c(0,0))+
  labs(x="",y="% ")+
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 0.5),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))
ggsave("JAM1.pdf", width = 5, height =4)




data<-read.csv("JUP.csv")

data %>%  
  group_by(Group) %>% 
  summarise(mean_value=mean(Value),sd_value=sd(Value)) -> df1
df1 %>% 
  group_by(Group) %>% 
  mutate(new_col=cumsum(mean_value)) -> df2

ggplot(data=df2,aes(x=Group,y=mean_value,fill=Group))+
  geom_errorbar(aes(ymin=new_col-sd_value,ymax=new_col+sd_value,x=Group),position =position_dodge(0.7),stat="identity",width=0.35,size=1.2)+
  geom_bar(aes(y=mean_value,fill=Group),position = "dodge",stat="identity",width = 0.7)+
  geom_jitter(data=data,aes(x=Group,y=Value,colors=Group,color=Group),shape=21,stroke =1.5,size=3.5,position = position_jitterdodge(0.2),fill="white")+
  scale_fill_manual(values = c("1-sh"="#D6E5ED","2-WT"="#8CA6C1","3-Q"="#C68180","4-R"="#E5B69B"),guide=FALSE)+
  scale_color_manual(values = c("1-sh"="#8CA6C1","2-WT"="#406B97","3-Q"="#A02D2B","4-R"="#D38559"),guide=FALSE)+
  geom_signif(comparisons =  list(c('3-Q','4-R'),c('1-sh', "2-WT"),c('2-WT', '3-Q'),c('2-WT', '4-R')),test = "t.test",step_increase = 0.02,map_signif_level =T,color="black")+
  scale_y_continuous(limits = c(0,2.0),breaks=seq(0,2.0,0.5),expand = c(0,0))+
  labs(x="",y="% ")+
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 0.5),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))
ggsave("JUP.pdf", width = 5, height =4)



data<-read.csv("CLDN1.csv")

data %>%  
  group_by(Group) %>% 
  summarise(mean_value=mean(Value),sd_value=sd(Value)) -> df1
df1 %>% 
  group_by(Group) %>% 
  mutate(new_col=cumsum(mean_value)) -> df2

ggplot(data=df2,aes(x=Group,y=mean_value,fill=Group))+
  geom_errorbar(aes(ymin=new_col-sd_value,ymax=new_col+sd_value,x=Group),position =position_dodge(0.7),stat="identity",width=0.35,size=1.2)+
  geom_bar(aes(y=mean_value,fill=Group),position = "dodge",stat="identity",width = 0.7)+
  geom_jitter(data=data,aes(x=Group,y=Value,colors=Group,color=Group),shape=21,stroke =1.5,size=3.5,position = position_jitterdodge(0.2),fill="white")+
  scale_fill_manual(values = c("1-sh"="#D6E5ED","2-WT"="#8CA6C1","3-Q"="#C68180","4-R"="#E5B69B"),guide=FALSE)+
  scale_color_manual(values = c("1-sh"="#8CA6C1","2-WT"="#406B97","3-Q"="#A02D2B","4-R"="#D38559"),guide=FALSE)+
  geom_signif(comparisons =  list(c('3-Q','4-R'),c('1-sh', "2-WT"),c('2-WT', '3-Q'),c('2-WT', '4-R')),test = "t.test",step_increase = 0.02,map_signif_level =T,color="black")+
  scale_y_continuous(limits = c(0,2.0),breaks=seq(0,2.0,0.5),expand = c(0,0))+
  labs(x="",y="% ")+
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 0.5),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))
ggsave("CLDN1.pdf", width = 5, height =4)

#################Figure 4G####################
setwd("Fig 4g/")
data <- read.csv("immune.csv")

df <-  data%>%
  pivot_longer(
    cols = names(data)[-c(1:2)],   # 除去元数据列，其他都当成指标
    names_to  = "variable",
    values_to = "value"
  ) %>%
  mutate(Gene = factor(Gene, levels = c("WT1","WT2","WT3",
                                        "KQ1","KQ2","KQ3",
                                        "KR1","KR2","KR3")))
colnames(data)
df <- df %>%
  mutate(
    Group = factor(Group, levels = c("WT", "KQ", "KR")),
    # 面板顺序（按你图里从左到右、从上到下的顺序排）
    variable = factor(
      variable,
      levels = c("IL.1A", "NFKB","STAT3", "STAT3_S727p")))
df %>%
  #mutate(AGE = factor(AGE , levels = c('Young','Middle age','Old'))) %>%
  mutate(Group = factor(Group, levels = c('WT','KQ',"KR"))) %>%
  #ggplot(aes(x=AGE,y = scale(value),color=AGE))+
  ggplot(aes(x=Group,y = value,color=Group))+
  geom_boxplot(aes(fill=Group),outlier.colour = NA,width = 0.55,position = position_dodge(0.6),size=0.75,color="black") +
  geom_jitter(aes(group=Group,fill=Group),size=3,stroke =0.75,position = position_jitterdodge(jitter.width = 0.85,dodge.width = 0.7),color="black",shape=21)+
  #stat_compare_means(method = "t.tes",paired = F,label = "p.format")+
  geom_signif(comparisons =  list(c('WT','KQ'),c('WT', "KR"),c('KQ', "KR")),test = "t.test",step_increase = 0.1,map_signif_level =F,color="black")+
  scale_fill_manual(values =c('WT'='#92A7C8','KQ'='#EEBA83','KR'='#CC6063'))+
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
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))+
  facet_wrap(~ variable, nrow = 2, scales = "free_y") 
