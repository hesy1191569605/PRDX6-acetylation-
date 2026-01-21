library(ggplot2);library(ggpubr);library(dplyr);library(stringr);library(tidyr);library(data.table);library(RColorBrewer)
setwd("C:/Users/11915/Desktop/PRDX6-laste/Code/Figure1/")

#################Figure 1C####################
setwd("Fig 1c/")
ratio4 <- read.csv("IS.csv", check.names = FALSE, stringsAsFactors = FALSE)
ggbarplot(ratio4, x = "x", y = "prob", width=.65,
          add = c("mean_se"), add.params = list(width=0.2),fill="#92A7C8")+
  #geom_text(aes(label=prob), size=3)+
  geom_jitter(width = .3, shape=21, fill="#5D82A7",color="black")+
  scale_y_continuous(expand = c(0,0), limits = c(0,110),breaks = c(0,25,50,75,100))+
  theme(axis.line = element_line(color="black", linewidth = .7),
        axis.ticks = element_line(color="black", linewidth = .7),
        axis.ticks.length =  unit(1.25,"mm"))
ggsave("ratio-is.pdf", width = 3.4, height = 3)


#################Figure 1D####################
setwd("Fig 1d/")
ratio4 <- read.csv("mix.csv", check.names = FALSE, stringsAsFactors = FALSE)
ggbarplot(ratio4, x = "x", y = "prob", width=.65,
          add = c("mean_se"), add.params = list(width=0.2),fill="#92A7C8")+
  #geom_text(aes(label=prob), size=3)+
  geom_jitter(width = .3, shape=21, fill="#5D82A7",color="black")+
  scale_y_continuous(expand = c(0,0), limits = c(0,110),breaks = c(0,25,50,75,100))+
  theme(axis.line = element_line(color="black", linewidth = .7),
        axis.ticks = element_line(color="black", linewidth = .7),
        axis.ticks.length =  unit(1.25,"mm"))
ggsave("ratio-mix.pdf", width = 3.4, height = 3)

#################Figure 1G####################
a<-c("11953","4428")
b<-c("Phosphosites","Phosphoproteins")
data<-data.frame(a,b)
names(data)<-c("Count","X")
library(ggplot2);library(dplyr);library(reshape2);library(ggplot2);library(ggpubr)
data$X <- factor(data$X,
                 levels = rev(c("Phosphosites","Phosphoproteins")),
                 ordered = TRUE)
ggplot(data,aes(x =X, y =as.numeric(Count), fill =X)) + 
  geom_bar(stat = 'identity', width = 0.65) + 
  scale_fill_manual(values = c("#E0C8E2","#8E6084"))+
  labs(x = '', y = 'Numbers') +
  #coord_polar(theta = "y", start = 0) +
  scale_y_continuous(expand = c(0,0),limits = c(0,16000))+
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))  
#################Figure 1I####################
a<-c("16706","5586","4128")
b<-c("N-glycopeptides","N-glycosites","N-glycoproteins")
data<-data.frame(a,b)
names(data)<-c("Count","X")
library(ggplot2);library(dplyr);library(reshape2);library(ggplot2);library(ggpubr)
data$X <- factor(data$X,
                 levels = rev(c("N-glycopeptides","N-glycosites","N-glycoproteins")),
                 ordered = TRUE)
ggplot(data,aes(x =X, y =as.numeric(Count), fill =X)) + 
  geom_bar(stat = 'identity', width = 0.65) + 
  scale_fill_manual(values = c("#AACFE5",'#94BBD8',"#4A8CBD"))+
  labs(x = '', y = 'Numbers') +
  #coord_polar(theta = "y", start = 0) +
  scale_y_continuous(expand = c(0,0),breaks = c(0,5000,10000,15000,20000),limits = c(0,21000))+
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))  

#################Figure 1J####################
setwd("Fig 1j/")
data1 <- read.csv("sites count in proteins.csv")

data1 <- data1 %>%
  group_by(group) %>%
  mutate(
    prop = Value / sum(Value),
    label = paste0(Value, "\n", scales::percent(prop, accuracy = 0.1))  # 两行文字，换行符\n分隔
  ) %>%
  ungroup()

data1 %>%
  mutate(COUNT = factor(COUNT, levels = c("1","2","3","4","5"))) %>%
  ggplot(aes(x = '', y = prop, fill = COUNT)) + 
  geom_bar(stat = 'identity', width = 1) + 
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2, color = "black") +  
  theme_bw() + 
  coord_polar(theta = 'y', start = 0, direction = -1) + 
  scale_fill_manual(values = rev(c('#9DD0AE','#D3D3D3',"#FEFDA1","#41ADB6","#B1D0D5"))) +
  labs(x = '', y = '', title = '') +
  theme(axis.line = element_line(linetype=1,color="white",size=0.75),
        axis.ticks = element_line(color="white",size=0.75,lineend = 1),
        panel.grid = element_line(linetype = 2,colour = "white"),
        panel.background = element_rect(fill = "white"),
        axis.text.y = element_text(size = 3, color = "white", vjust = 0.5, hjust = 1),
        axis.text.x = element_text(size = 3, color = "white", hjust = 0.5),
        title = element_text(size = 3, color = "white", vjust = 0.5, hjust = 1)) +
  facet_grid(~ group)


#################Figure 1K####################
data1<-data.frame(c(0,10,20,30),
                  c(1168,3487,905,26),
                  c(0.2091,0.6242,0.1620,0.0047))
names(data1)<-c("group","count","freq")

data1 %>%
  mutate(group = factor(group, levels = c("0","10","20","30"))) %>%
  ggplot(aes(x =group, y = freq, fill = group)) + 
  geom_bar(stat = 'identity', width = 0.6) + 
  geom_smooth(aes(y=freq, group=1), method="loess", se=FALSE, color="black")
theme_bw() + 
  scale_fill_manual(values = rev(c('#9DD0AE','#D3D3D3',"#FEFDA1","#41ADB6"))) +
  labs(x = '', y = '', title = '') +
  theme(axis.line=element_line(linetype=1,color="black",size=0.75),
        axis.ticks=element_line(color="black",size=0.75,lineend = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid=element_blank(),
        axis.text.y= element_text(size = 13, color = "black",  vjust = 0.5, hjust = 1),
        axis.text.x= element_text(size = 13, color = "black",   hjust = 0.5),
        title=element_text(size = 13,  color = "black",  vjust = 0.5, hjust = 0.5))