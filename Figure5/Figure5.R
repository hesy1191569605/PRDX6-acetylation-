library(reshape2);library(ggplot2);library(dbplyr);library(dplyr);library(ggsignif);library(ggrepel);library(ggpubr)
setwd("C:/Users/11915/Desktop/PRDX6-laste/Code/Figure5/")

#################Figure 5C####################
setwd("Fig 5c/")
data<-read.csv("meta.csv")
ggplot(data,aes(x=Acetate,y = PRDX6.K63ac))+
  geom_point(size=2,color="#C68180") +
  geom_smooth(method="lm",size=1.3, se =T,color="#C68180",alpha=0.5)+
  stat_cor(method = "spearman")+
  labs(x="Acetate",y="PRDX6.K63ac")+
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
ggplot(data,aes(x=Butyrate,y = PRDX6.K63ac))+
  geom_point(size=2,color="#C68180") +
  geom_smooth(method="lm",size=1.3, se =T,color="#C68180",alpha=0.5)+
  stat_cor(method = "spearman")+
  labs(x="Butyrate",y="PRDX6.K63ac")+
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



#################Figure 5D####################
setwd("Fig 5d/")
data<-read.csv("all.csv")
data %>%  
  group_by(Group) %>% 
  summarise(mean_value=mean(Value),sd_value=sd(Value)) -> df1
df1 %>% 
  group_by(Group) %>% 
  mutate(new_col=cumsum(mean_value))-> df2

table(data$Group)
ggplot(data=df2,aes(x=Group,y=mean_value,fill=Group))+
  geom_errorbar(aes(ymin=new_col-sd_value,ymax=new_col+sd_value,x=Group),position =position_dodge(0.7),stat="identity",width=0.35,size=1.2)+
  geom_bar(aes(y=mean_value,fill=Group),position = "dodge",stat="identity",width = 0.7)+
  geom_jitter(data=data,aes(x=Group,y=Value,colors=Group,color=Group),shape=21,stroke =1.5,size=3.5,position = position_jitterdodge(0.2),fill="white")+
  scale_fill_manual(values = c("Control_WT"="#8CA6C1","5mM NaBu_WT"="#8CA6C1","Control_R"="#C68180","5mM NaBu_R"="#C68180","10mM NaAc_WT"="#8CA6C1","10mM NaAc_R"="#C68180"),guide=FALSE)+
  scale_color_manual(values = c("Control_WT"="#406B97","5mM NaBu_WT"="#406B97","Control_R"="#A02D2B","5mM NaBu_R"="#A02D2B","10mM NaAc_WT"="#406B97","10mM NaAc_R"="#A02D2B"),guide=FALSE)+
  scale_x_discrete(limit=c("Control_WT","Control_R","5mM NaBu_WT","5mM NaBu_R","10mM NaAc_WT","10mM NaAc_R"))+
  geom_signif(data=data,aes(x=Group,y=Value),comparisons =  list(c('Control_WT','5mM NaBu_WT'),c('5mM NaBu_R', "Control_R"),
                                                                 c('Control_WT','10mM NaAc_WT'),c('5mM NaBu_R', "10mM NaAc_R"),
                                                                 c('Control_WT','Control_R'),c('5mM NaBu_R', "5mM NaBu_WT"),c('10mM NaAc_WT', "10mM NaAc_R")
  ),test = "t.test",step_increase = 0.02,map_signif_level =F,color="black")+
  scale_y_continuous(limits = c(0,0.4),breaks=seq(0,0.2,0.1),expand = c(0,0))+
  labs(x="",y="SA-β-gal positive cells (%)")+
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text= element_text(size = 15, color = "black",  vjust = 0.5, hjust = 0.5),
        title=element_text(size = 15,  color = "black",  vjust = 0.5, hjust = 0.5))
ggsave("all.pdf", width =6, height =4)