###  Box plot_luncheng_20220413 （https://mp.weixin.qq.com/s/b3zxlhB4a94P256doaRXKA）


library(tidyverse)
library(ggpubr)
library(data.table)
######################################## ROM Method #############################################

# read data
d1 <- readxl::read_xlsx('E:/phD/Papers/paper2/You_et_al_2022/public/meta_regression_luncheng.xlsx',sheet = 1)
d1 <- as.data.table(d1)

#X轴自定义排序
#d1$Moderator1 <- factor(d1$Moderator1, levels = c("Fertilizer type: organic or combined",
                                                # "Fertilizer type: enhanced",
                                                # "Fertilizer type: mineral",
                                                # "Fertilizer strategy: placement",
                                                # "Fertilizer strategy: rate",
                                                # "Fertilizer strategy: timing",
                                                # "Biochar",
                                                # "Crop residue",
                                                # "Cover crop or crop rotation",
                                                # "Tillage: no or reduced",
                                                # "Crop type: wheat or maize",
                                                # "Crop type: rice",
                                                # "Crop type: other",
                                                # "N dose scaled",
                                                # "Clay scaled",
                                                # "pH scaled",
                                                # "MAP scaled",
                                                # "MAT scaled",
                                                # "N dose scaled ˟ SOC scaled"))

d1$Moderator1 <- factor(d1$Moderator1, levels = c("OF/CF",
                                                 "EE",
                                                 "MF",
                                                 "RFP",
                                                 "RFR",
                                                 "RFT",
                                                 "BC",
                                                 "RES",
                                                 "CC/ROT",
                                                 "NT/RT",
                                                 "Cr:W/M",
                                                 "Cr:R",
                                                 "Cr:O",
                                                 "N dose",
                                                 "Clay",
                                                 "pH",
                                                 "MAP",
                                                 "MAT",
                                                 "N dose˟SOC"))

p1 <- ggplot(d1,aes(Moderator1,Parameter_estimate))+
  geom_col()+
  theme_bw()+#去掉阴影
  theme(panel.grid=element_blank())+#去掉网格线
  geom_col(fill = c("#66c2a5", "#66c2a5", "#66c2a5",
                     "#fb8072", "#fb8072","#fb8072",
                    "#8da0cb","#e78ac3", 
                    "#a6d854","#e78ac3", "#fdbf6f",
                    "#fdbf6f", "#fdbf6f", "#fdbf6f",
                    "#bebada", "#bebada","#bebada",
                    "#bebada", "#bebada"))+   #自定义颜色
  #geom_text(aes(label=Parameter_estimate),size=3.5, vjust=-0.5, colour="black")+ #为柱形图添加数字标签，并且通过vjust参数来调整数字的位置
  # geom_errorbar(aes(Moderator,#添加误差线
  #                   ymin=Parameter_estimate-Standard_error,
  #                   ymax=Parameter_estimate+Standard_error,
  #                   color=Parameter_estimate),width=0.3,size=1)+
  xlab("")+
  ylab("Parameter estimate")+
  ylim(-0.15,0.5)+
  theme(legend.position ="none", #去除标签
        axis.title = element_text(size=22, colour="black", face = "bold"),#对x和y的标题名称进行更改
        axis.text.x = element_text(size=22,angle = 45, hjust = 1, vjust = 1, colour="black"),#对y的轴的字体和角度进行调整
        axis.text.y = element_text(size=22, colour="black"))+ #对y的轴的字体进行调整
  #使用annotate()函数，对柱子增加标签，然后需要增加几个就复制几个，更改一下x和y的位置就行
  annotate("text",x=1,y=0.165,label="***",size=12)+
  annotate("text",x=2,y=0.305,label="***",size=12)+
  annotate("text",x=3,y=0.045,label="*",size=12)+
  annotate("text",x=4,y=-0.11,label="***",size=12)+
  annotate("text",x=5,y=0.195,label="***",size=12)+
  annotate("text",x=6,y=0.315,label="***",size=12)+
  annotate("text",x=7,y=0.215,label="***",size=12)+
  annotate("text",x=8,y=0.085,label="***",size=12)+
  annotate("text",x=9,y=0.155,label="***",size=12)+
  annotate("text",x=10,y=-0.12,label="***",size=12)+
  annotate("text",x=11,y=-0.07,label="***",size=12)+
  annotate("text",x=12,y=-0.09,label="***",size=12)+
  annotate("text",x=13,y=0.305,label="***",size=12)+
  annotate("text",x=14,y=-0.16,label="***",size=12)+
  annotate("text",x=15,y=-0.09,label="***",size=12)+
  annotate("text",x=16,y=0.075,label="***",size=12)+
  annotate("text",x=17,y=0.085,label="***",size=12)+
# annotate("text",x=18,y=-0.1,label="***",size=4)+
  annotate("text",x=19,y=-0.06,label="***",size=12)+
  annotate("text",x=1,y=0.5,label="ROM",size=8, face = "bold", colour="black")

  # theme(panel.border = element_rect(fill=NA,color = "black",size = 1),#给外围增加一个线框
  #       panel.background = element_rect(fill = "lightblue"))#填充图里面的颜色
p1  

######################################## MD Method #############################################

# read data
d2 <- readxl::read_xlsx('E:/phD/Papers/paper2/You_et_al_2022/public/meta_regression_luncheng.xlsx',sheet = 2)
d2 <- as.data.table(d2)

#X轴自定义排序
#d2$Moderator1 <- factor(d2$Moderator1, levels = c("Fertilizer type: combined",
                                                # "Fertilizer type: enhanced",
                                                # "Fertilizer type: mineral",
                                                # "Fertilizer type: organic",
                                                # "Fertilizer strategy: placement",
                                                # "Fertilizer strategy: rate",
                                                # "Fertilizer strategy: timing",
                                                # "Biochar",
                                                # "Crop residue",
                                                # "Cover crop or crop rotation",
                                                # "Tillage: no or reduced",
                                                # "Crop type: wheat or maize",
                                                # "Crop type: rice",
                                                # "Crop type: other",
                                                # "N dose scaled",
                                                # "SOC scaled",
                                                # "Clay scaled",
                                                # "pH scaled",
                                                # "MAP scaled",
                                                # "MAT scaled"))

d2$Moderator1 <- factor(d2$Moderator1, levels = c("CF",
                                                 "EE",
                                                 "MF",
                                                 "OF",
                                                 "RFP",
                                                 "RFR",
                                                 "RFT",
                                                 "BC",
                                                 "RES",
                                                 "CC/ROT",
                                                 "NT/RT",
                                                 "Cr:W/M",
                                                 "Cr:R",
                                                 "Cr:O",
                                                 "N dose",
                                                 "SOC",
                                                 "Clay",
                                                 "pH",
                                                 "MAP",
                                                 "MAT"))

p2 <- ggplot(d2,aes(Moderator1, Parameter_estimate))+
  geom_col()+
  theme_bw()+#去掉阴影
  theme(panel.grid=element_blank())+#去掉网格线
  geom_col(fill = c("#66c2a5", "#66c2a5", "#66c2a5",
                                     "#66c2a5", "#fb8072", "#fb8072",
                                     "#fb8072", "#8da0cb","#e78ac3", 
                                     "#a6d854","#e78ac3", "#fdbf6f",
                                     "#fdbf6f", "#fdbf6f", "#fdbf6f",
                                     "#bebada", "#bebada","#bebada",
                                     "#bebada", "#bebada"))+   #自定义颜色
  #geom_text(aes(label=Parameter_estimate),size=3.5, vjust=-0.5, colour="black")+ #为柱形图添加数字标签，并且通过vjust参数来调整数字的位置
  # geom_errorbar(aes(Moderator,#添加误差线
  #                   ymin=Parameter_estimate-Standard_error,
  #                   ymax=Parameter_estimate+Standard_error,
  #                   color=Parameter_estimate),width=0.3,size=1)+
  xlab("")+
  ylab("Parameter estimate")+
  ylim(-4,12)+
  theme(legend.position ="none", #去除标签
        axis.title = element_text(size=22, colour="black", face = "bold"),#对x和y的标题名称进行更改
        axis.text.x = element_text(size=22,angle = 45, hjust = 1, colour="black", vjust = 1),#对y的轴的字体和角度进行调整
        axis.text.y = element_text(size=22, colour="black"))+ #对y的轴的字体进行调整
  #使用annotate()函数，对柱子增加标签，然后需要增加几个就复制几个，更改一下x和y的位置就行
  annotate("text",x=1,y=5.84,label="***",size=12)+
  annotate("text",x=2,y=6.97,label="***",size=12)+
  annotate("text",x=3,y=3.67,label="***",size=12)+
  annotate("text",x=4,y=2.84,label="***",size=12)+
  annotate("text",x=5,y=-1.51,label="*",size=12)+
  annotate("text",x=6,y=4.13,label="***",size=12)+
  annotate("text",x=7,y=3.96,label="***",size=12)+
  annotate("text",x=8,y=4.88,label="***",size=12)+
  annotate("text",x=9,y=1.29,label="***",size=12)+
  annotate("text",x=10,y=1.87,label="***",size=12)+
  annotate("text",x=11,y=-3.75,label="***",size=12)+
  annotate("text",x=12,y=1.21,label="**",size=12)+
  annotate("text",x=13,y=-1.75,label="***",size=12)+
  annotate("text",x=14,y=9.09,label="***",size=12)+
  annotate("text",x=15,y=-3.91,label="***",size=12)+
  annotate("text",x=16,y=1.09,label="***",size=12)+
  annotate("text",x=17,y=-2.78,label="***",size=12)+
  annotate("text",x=18,y=1.12,label="***",size=12)+
  annotate("text",x=19,y=1.22,label="**",size=12)+
# annotate("text",x=20,y=0.73,label="***",size=4)
  annotate("text",x=0.8,y=12,label="MD",size=8, face = "bold", colour="black")

p2

######################################## SMD Method ############################################


# read data
d3 <- readxl::read_xlsx('E:/phD/Papers/paper2/You_et_al_2022/public/meta_regression_luncheng.xlsx',sheet = 3)
d3 <- as.data.table(d3)

#X轴自定义排序
# d3$Moderator1 <- factor(d3$Moderator1, levels = c("Fertilizer type: combined",
#                                                 "Fertilizer type: enhanced",
#                                                 "Fertilizer type: mineral",
#                                                 "Fertilizer type: organic",
#                                                 "Fertilizer strategy: placement",
#                                                 "Fertilizer strategy: rate",
#                                                 "Fertilizer strategy: timing",
#                                                 "Biochar",
#                                                 "Crop residue",
#                                                 "Cover crop or crop rotation",
#                                                 "Tillage: no or reduced",
#                                                 "Crop type: wheat or maize",
#                                                 "Crop type: rice",
#                                                 "Crop type: other",
#                                                 "N dose scaled",
#                                                 "SOC scaled",
#                                                 "Clay scaled",
#                                                 "pH scaled",
#                                                 "MAP scaled",
#                                                 "MAT scaled"))

d3$Moderator1 <- factor(d2$Moderator1, levels = c("CF",
                                                  "EE",
                                                  "MF",
                                                  "OF",
                                                  "RFP",
                                                  "RFR",
                                                  "RFT",
                                                  "BC",
                                                  "RES",
                                                  "CC/ROT",
                                                  "NT/RT",
                                                  "Cr:W/M",
                                                  "Cr:R",
                                                  "Cr:O",
                                                  "N dose",
                                                  "SOC",
                                                  "Clay",
                                                  "pH",
                                                  "MAP",
                                                  "MAT"))

p3 <- ggplot(d3,aes(Moderator1,Parameter_estimate))+
  geom_col()+
  theme_bw()+#去掉阴影
  theme(panel.grid=element_blank())+#去掉网格线
  geom_col(fill = c("#66c2a5", "#66c2a5", "#66c2a5",
                    "#66c2a5", "#fb8072", "#fb8072",
                    "#fb8072", "#8da0cb","#e78ac3", 
                    "#a6d854","#e78ac3", "#fdbf6f",
                    "#fdbf6f", "#fdbf6f", "#fdbf6f",
                    "#bebada", "#bebada","#bebada",
                    "#bebada", "#bebada"))+   #自定义颜色
  #geom_text(aes(label=Parameter_estimate),size=3.5, vjust=-0.5, colour="black")+ #为柱形图添加数字标签，并且通过vjust参数来调整数字的位置
  # geom_errorbar(aes(Moderator,#添加误差线
  #                   ymin=Parameter_estimate-Standard_error,
  #                   ymax=Parameter_estimate+Standard_error,
  #                   color=Parameter_estimate),width=0.3,size=1)+
  xlab("Management practices & Site conditions")+
  ylab("Parameter estimate")+
  ylim(-0.7,1.5)+
  theme(legend.position ="none", #去除标签
        axis.title = element_text(size=22, face = "bold", colour="black"),#对x和y的标题名称进行更改
        axis.text.x = element_text(size=22,angle = 45, hjust = 1, vjust = 1, colour="black"),#对x的轴的字体和角度进行调整
        axis.text.y = element_text(size=22, colour="black"))+ #对y的轴的字体进行调整
  #使用annotate()函数，对柱子增加标签，然后需要增加几个就复制几个，更改一下x和y的位置就行
  annotate("text",x=1,y=0.87,label="***",size=12)+
  annotate("text",x=2,y=1.23,label="***",size=12)+
  annotate("text",x=3,y=0.43,label="***",size=12)+
# annotate("text",x=4,y=0.72,label="**",size=4)+
  annotate("text",x=5,y=0.79,label="***",size=12)+
  annotate("text",x=6,y=0.96,label="***",size=12)+
  annotate("text",x=7,y=0.64,label="***",size=12)+
  annotate("text",x=8,y=1.03,label="***",size=12)+
  annotate("text",x=9,y=0.29,label="*",size=12)+
  annotate("text",x=10,y=0.45,label="***",size=12)+
  annotate("text",x=11,y=-0.57,label="*",size=12)+
  annotate("text",x=12,y=0.05,label="*",size=12)+
#  annotate("text",x=13,y=0.7,label="**",size=4)+
# annotate("text",x=14,y=-0.68,label="***",size=4)+
#  annotate("text",x=15,y=-0.07,label="*",size=4)+
#  annotate("text",x=16,y=-0.34,label="***",size=4)+
  annotate("text",x=17,y=-0.22,label="*",size=12)+
  annotate("text",x=18,y=0.16,label="*",size=12)+
  annotate("text",x=19,y=0.22,label="*",size=12)+
    #  annotate("text",x=20,y=-3.25,label="***",size=4)+
    annotate("text",x=1,y=1.48,label="SMD",size=8, face = "bold", colour="black")
    
p3

p<-ggarrange(p1, p2, p3, ncol = 1, nrow = 3, #common.legend = TRUE,legend = "bottom",
             labels = c("a", "b","c"), font.label=list(size=28),hjust = -0.2, vjust = 1)
p

ggsave(file = "meta_regression.png",width = 410,height = 410, units = "mm")

