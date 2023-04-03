library(dplyr)
#install.packages("ggpubr")
library(ggpubr)
library(ggplot2)
#install.packages("cowplot")
library(cowplot)
#install.packages("gridExtra")
library(gridExtra)
#--------------------------------------------------------------------
# create database
#--------------------------------------------------------------------
malit <- read.csv("E:/phD/Papers/paper2/You_et_al_2022/public/covariates3.csv")

# create subset of columns 
df <- data.frame(reference=malit$reference, man_group=malit$man_group, man_name=malit$man_name,
                 ind_code=malit$ind_code, type=malit$type,
                 moderator.factor=malit$moderator.factor, co.variate.label=malit$co.variate.label, 
                 dyr.1=malit$dyr.1, SEyr.1=as.numeric(malit$SDyr.1), n.O=malit$n)

# weighted mean of each covariate group, higher weight where more observations
df.wm <- df %>% group_by(ind_code,man_group,man_name,co.variate.label,type=malit$type) %>% 
  summarise(dyr.1 = signif(weighted.mean(dyr.1, n.O), digits=2),
            reference = paste0(reference, collapse = ", "), n.S = n(), n.O = sum(n.O))

df.wm$co.variate.label <- factor(as.factor(df.wm$co.variate.label), levels=c(
  "other crop",
  "rice crop",
  "maize crop",
  "wheat crop",
  "high N rate",
  "medium N rate",
  "low N rate",
  "high SOC",
  "medium SOC",
  "low SOC",
  "high soil pH",
  "neutral soil pH",
  "low soil pH",
  "high clay",
  "medium clay",
  "low clay",
  "high MAT",
  "medium MAT",
  "low MAT",
  "high MAP",
  "medium MAP",
  "low MAP"))

# overall weighted mean of all covariate groups for each measure-impact
range <- df.wm %>% group_by(ind_code,man_group,man_name) %>% 
  summarise(min=signif(min(dyr.1), digits=2), max=signif(max(dyr.1), digits=2), dyr.1 = signif(weighted.mean(dyr.1, n.O), digits=2), min.perc.diff = signif((((min-dyr.1)/dyr.1)*100),digits=2), max.perc.diff = signif((((max-dyr.1)/dyr.1)*100),digits=2))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                     NUE and Crop rotation plot
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#subset of data man/ind pair and columns needed
s <- subset(df.wm, ind_code == "NUE" & man_group=="Crop rotation")
t <-data.frame(ind=s$ind_code,man=s$man_name,type=as.factor(s$type),group=as.factor(s$co.variate.label),group_mean=as.numeric(s$dyr.1))

#extract overall mean man/ind pair
i<-which(range$ind_code == "NUE" & range$man_group=="Crop rotation")
grand_mean <- rep(range$dyr.1[i],length(t$group))
t.nrot <- as.data.frame(cbind(grand_mean, t))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                     NUE and Cover cropping plot
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#subset of data man/ind pair and columns needed
s <- subset(df.wm, ind_code == "NUE" & man_group=="Cover cropping")
t <-data.frame(ind=s$ind_code,man=s$man_name,type=as.factor(s$type),group=as.factor(s$co.variate.label),group_mean=as.numeric(s$dyr.1))

#extract overall mean man/ind pair
i<-which(range$ind_code == "NUE" & range$man_group=="Cover cropping")
grand_mean <- rep(range$dyr.1[i],length(t$group))
t.ncc <- as.data.frame(cbind(grand_mean, t))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                      NUE and Residue retention plot
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#subset of data man/ind pair and columns needed
s <- subset(df.wm, ind_code == "NUE" & man_group=="Residue retention")
t <-data.frame(ind=s$ind_code,man=s$man_name,type=as.factor(s$type),group=as.factor(s$co.variate.label),group_mean=as.numeric(s$dyr.1))

#extract overall mean man/ind pair
i<-which(range$ind_code == "NUE" & range$man_group=="Residue retention")
grand_mean <- rep(range$dyr.1[i],length(t$group))
t.nres <- as.data.frame(cbind(grand_mean, t))


#================ combine data for each indicator =============================
t1 <- rbind(t.nres,t.ncc,t.nrot)
#==============================================================================

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                     NUE and Reduced tillage plot
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#subset of data man/ind pair and columns needed
s <- subset(df.wm, ind_code == "NUE" & man_group=="Reduced tillage")
t <-data.frame(ind=s$ind_code,man=s$man_name,type=as.factor(s$type),group=as.factor(s$co.variate.label),group_mean=as.numeric(s$dyr.1))

#extract overall mean man/ind pair
i<-which(range$ind_code == "NUE" & range$man_group=="Reduced tillage")
grand_mean <- rep(range$dyr.1[i],length(t$group))
t.nrt <- as.data.frame(cbind(grand_mean, t))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                     NUE and No tillage plot
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#subset of data man/ind pair and columns needed
s <- subset(df.wm, ind_code == "NUE" & man_group=="Zero tillage")
t <-data.frame(ind=s$ind_code,man=s$man_name,type=as.factor(s$type),group=as.factor(s$co.variate.label),group_mean=as.numeric(s$dyr.1))

#extract overall mean man/ind pair
i<-which(range$ind_code == "NUE" & range$man_group=="Zero tillage")
grand_mean <- rep(range$dyr.1[i],length(t$group))
t.nnt <- as.data.frame(cbind(grand_mean, t))


#================ combine data for each indicator =============================
t2 <- rbind(t.nnt,t.nrt)
#==============================================================================


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                      NUE and Organic fertilizer plot
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#subset of data man/ind pair and columns needed
s <- subset(df.wm, ind_code == "NUE" & man_group=="Organic fertilizer")
t <-data.frame(ind=s$ind_code,man=s$man_name,type=as.factor(s$type),group=as.factor(s$co.variate.label),group_mean=as.numeric(s$dyr.1))

#extract overall mean man/ind pair
i<-which(range$ind_code == "NUE" & range$man_group=="Organic fertilizer")
grand_mean <- rep(range$dyr.1[i],length(t$group))
t.nof <- as.data.frame(cbind(grand_mean, t))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                     NUE and Combined fertilizer plot
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#subset of data man/ind pair and columns needed
s <- subset(df.wm, ind_code == "NUE" & man_group=="Combined fertilizer")
t <-data.frame(ind=s$ind_code,man=s$man_name,type=as.factor(s$type),group=as.factor(s$co.variate.label),group_mean=as.numeric(s$dyr.1))

#extract overall mean man/ind pair
i<-which(range$ind_code == "NUE" & range$man_group=="Combined fertilizer")
grand_mean <- rep(range$dyr.1[i],length(t$group))
t.ncf <- as.data.frame(cbind(grand_mean, t))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                      NUE and Right fertilizer rate plot
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#subset of data man/ind pair and columns needed
s <- subset(df.wm, ind_code == "NUE" & man_group=="Fertilizer rate")
t <-data.frame(ind=s$ind_code,man=s$man_name,type=as.factor(s$type),group=as.factor(s$co.variate.label),group_mean=as.numeric(s$dyr.1))

#extract overall mean man/ind pair
i<-which(range$ind_code == "NUE" & range$man_group=="Fertilizer rate")
grand_mean <- rep(range$dyr.1[i],length(t$group))
t.nrfr <- as.data.frame(cbind(grand_mean, t))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                     NUE and Right fertilizer timing plot
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#subset of data man/ind pair and columns needed
s <- subset(df.wm, ind_code == "NUE" & man_group=="Fertilizer timing")
t <-data.frame(ind=s$ind_code,man=s$man_name,type=as.factor(s$type),group=as.factor(s$co.variate.label),group_mean=as.numeric(s$dyr.1))

#extract overall mean man/ind pair
i<-which(range$ind_code == "NUE" & range$man_group=="Fertilizer timing")
grand_mean <- rep(range$dyr.1[i],length(t$group))
t.nrft <- as.data.frame(cbind(grand_mean, t))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                     NUE and Right fertilizer placement plot
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#subset of data man/ind pair and columns needed
s <- subset(df.wm, ind_code == "NUE" & man_group=="Fertilizer placement")
t <-data.frame(ind=s$ind_code,man=s$man_name,type=as.factor(s$type),group=as.factor(s$co.variate.label),group_mean=as.numeric(s$dyr.1))

#extract overall mean man/ind pair
i<-which(range$ind_code == "NUE" & range$man_group=="Fertilizer placement")
grand_mean <- rep(range$dyr.1[i],length(t$group))
t.nrfp <- as.data.frame(cbind(grand_mean, t))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                      NUE and Enhanced efficiency plot
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#subset of data man/ind pair and columns needed
s <- subset(df.wm, ind_code == "NUE" & man_group=="Enhanced efficiency")
t <-data.frame(ind=s$ind_code,man=s$man_name,type=as.factor(s$type),group=as.factor(s$co.variate.label),group_mean=as.numeric(s$dyr.1))

#extract overall mean man/ind pair
i<-which(range$ind_code == "NUE" & range$man_group=="Enhanced efficiency")
grand_mean <- rep(range$dyr.1[i],length(t$group))
t.nee <- as.data.frame(cbind(grand_mean, t))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                      NUE and Biochar plot
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#subset of data man/ind pair and columns needed
s <- subset(df.wm, ind_code == "NUE" & man_group=="Biochar")
t <-data.frame(ind=s$ind_code,man=s$man_name,type=as.factor(s$type),group=as.factor(s$co.variate.label),group_mean=as.numeric(s$dyr.1))

#extract overall mean man/ind pair
i<-which(range$ind_code == "NUE" & range$man_group=="Biochar")
grand_mean <- rep(range$dyr.1[i],length(t$group))
t.nbc <- as.data.frame(cbind(grand_mean, t))


#================ combine data for each indicator =============================
t3 <- rbind(t.nee,t.ncf,t.nof,t.nrfp,t.nrfr,t.nrft,t.nbc)
#==============================================================================

t1$man <- factor(as.factor(t1$man), levels=c(
  "Residue retention",
  "Cover cropping",
  "Crop rotation"))
t2$man <- factor(as.factor(t2$man), levels=c(
  "Zero tillage",
  "Reduced tillage"))
t3$man <- factor(as.factor(t3$man), levels=c(
  "Enhanced efficiency",
  "Combined fertilizer",
  "Organic fertilizer",
  "Fertilizer placement",
  "Fertilizer rate",
  "Fertilizer timing",
  "Biochar"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
p1 <- ggplot(data = t1, aes(x = group, y = group_mean, color = type)) + 
  # add vertical line with grand mean
  geom_hline(data = t1, aes(yintercept = grand_mean), linetype= 1, size=1, color="grey18", group= "man") +
  # add vertical line for zero
  geom_hline(yintercept=0, linetype = 2, size=1, color="grey50")+
  # add points, flip coordinates and add own color scheme
  geom_point(size=3) + coord_flip() + 
  scale_color_manual(values=c("#1b9e77", "#d95f02","#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d"))+
  # add manual ylims
  #scale_y_continuous(breaks = c(-10,0,10,20,30), limits = c(-15,35))+
  #scale_x_discrete(breaks=sc.x)+
  # facet grids
  facet_grid(cols = vars(man)) + 
  # add labels and general theme
  xlab('Crop management') + ylab('Mean relative change (%)') + theme_bw()+
  theme(axis.text.x = element_text(color="black", size = 18),
        axis.text.y = element_text(color="black", size = 18), 
        axis.title = element_text(color="black", size = 18, face="bold"),
        strip.text = element_text(color="black", size = 15),
        plot.tag = element_text(color="black", size=30),
        legend.position = "none")
plot(p1)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
p2 <-  ggplot(data = t2, aes(x = group, y = group_mean, color = type)) + 
  # add vertical line with grand mean
  geom_hline(data = t2, aes(yintercept = grand_mean), linetype= 1, size=1, color="grey18", group= "man") +
  # add vertical line for zero
  geom_hline(yintercept=0, linetype = 2, size=1, color="grey50")+
  # add points, flip coordinates and add own color scheme
  geom_point(size=3) + coord_flip() + 
  scale_color_manual(values=c("#1b9e77", "#d95f02","#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d"))+
  # add manual ylims
  #scale_y_continuous(breaks = c(-10,0,10,20,30), limits = c(-15,35))+
  #scale_x_discrete(breaks=sc.x)+
  # facet grids
  facet_grid(cols = vars(man)) + 
  # add labels and general theme
  xlab('Soil management') + ylab('Mean relative change (%)')  + theme_bw()+
  theme(axis.text.x = element_text(color="black", size = 18),
        axis.text.y = element_text(color="black", size = 18), 
        axis.title = element_text(color="black", size = 18, face="bold"),
        strip.text = element_text(color="black", size = 15),
        plot.tag = element_text(color="black", size=30),
        legend.position = "none")
plot(p2)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
p3 <- ggplot(data = t3, aes(x = group, y = group_mean, color = type)) + 
  # add vertical line with grand mean
  geom_hline(data = t3, aes(yintercept = grand_mean), linetype= 1, size=1, color="grey18", group= "man") +
  # add vertical line for zero
  geom_hline(yintercept=0, linetype = 2, size=1, color="grey50")+
  # add points, flip coordinates and add own color scheme
  geom_point(size=3) + coord_flip() + 
  scale_color_manual(values=c("#1b9e77", "#d95f02","#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d"))+
  # add manual ylims
  scale_y_continuous(limits = c(-50,50), breaks = c(-30,0,30))+
  #scale_y_continuous(breaks = c(-10,0,10,20,30), limits = c(-15,35))+
  #scale_x_discrete(breaks=sc.x, labels=sc.x)+
  # facet grids
  facet_grid(cols = vars(man)) + 
  # add labels and general theme
  xlab('Nutrient management') + ylab('Mean relative change (%)') + theme_bw() +
  theme(axis.text.x = element_text(color="black", size = 18),
        axis.text.y = element_text(color="black", size = 18), 
        axis.title = element_text(color="black", size = 18, face="bold"),
        strip.text = element_text(color="black", size = 15),
        plot.tag = element_text(color="black", size=30),
        legend.position = "none")


plot(p3) 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(showtext)
gg <- ggdraw() +     
  draw_plot(p1, 0, 0, 0.6, 0.5) + # 在母图左下角，占母图比例2/6  
  draw_plot(p2, 0.6, 0, 0.4, 0.5) + # 在母图右下角，占母图比例1/6  
  draw_plot(p3, 0, 0.5, 1, 0.5)+  # 在母图上半部，占母图比例1/2 
  draw_plot_label(c("a", "b", "c"), c(0, 0, 0.6), c(1, 0.5, 0.5), size = 28, colour = "black", family = "Dancing") # 加上标签，
showtext_begin()
print(gg)
showtext_end()

ggsave(file = "covariates_revision.png",width = 410,height = 310, units = "mm")






#================ combine data for each indicator =============================
# t1 <- rbind(t.nrot, t.ncc, t.nres, t.nrt, t.nnt, t.nof, t.ncf, t.nrfr, t.nrft, t.nrfp, t.nee, t.nbc)
# #==============================================================================
# t1$man <- factor(as.factor(t1$man), levels=c(
#   "Crop rotation",
#   "Cover cropping",
#   "Residue retention",
#   "Reduced tillage",
#   "No tillage",
#   "Organic fertilizer",
#   "Combined fertilizer",
#   "Fertilizer rate",
#   "Fertilizer timing",
#   "Fertilizer placement",
#   "Enhanced efficiency",
#   "Biochar"))
# 
# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# p1 <- ggplot(data = t1, aes(x = group, y = group_mean, color = type)) + 
#   # add vertical line with grand mean
#   geom_hline(data = t1, aes(yintercept = grand_mean), linetype= 1, size=0.6, color="cyan4", group= "man") +
#   # add vertical line for zero
#   geom_hline(yintercept=0, linetype = 2, size=0.5, color="deeppink")+
#   # add points, flip coordinates and add own color scheme
#   geom_point(size=2) + coord_flip() + 
#   scale_color_manual(values=c("firebrick1","green3","maroon1", "blue"))+
#   # add  X axis range
#   scale_y_continuous(limits = c(-40,40), breaks = c(-30,0,30))+
#   #scale_x_discrete(breaks=sc.x)+
#   # facet grids
#   facet_grid(cols = vars(man)) + 
#   # add labels and general theme
#   xlab('') + ylab('') +
#   theme(axis.text.x = element_text(color="black", size = 12),
#         axis.text.y = element_text(color="black", size = 12), 
#         axis.title = element_text(color="black", size = 25),
#         strip.text = element_text(size = 12, color="black"),
#         plot.tag = element_text(size=30),
#         legend.position = "none")+ theme(panel.grid = element_blank())+
#   theme(panel.background = element_rect(fill = "gray97"))
# plot(p1)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
