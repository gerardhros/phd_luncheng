# Figure 2

# require packages
require(readxl);require(data.table); require(ggplot2);library(ggpubr)

# read in the excel sheet for the data
metaresult_group<- readxl::read_xlsx('articles/ncoms23/Source Data.xlsx',sheet = "Figure2")
metaresult_group <- as.data.table(metaresult_group)

# subset the dataset
mydata <- metaresult_group[Vari=='NUE' & `Group type` == 'Data']

# make the plot (a)
p1 <- ggplot(data = metaresult_group[Vari=='NUE' & `Group type` == 'Data' & grepl('Prima',Group)],
             aes(x=Management,y=mean)) +
      geom_hline(yintercept=0,linetype = "dashed",linewidth=0.3)+
      geom_errorbar(position=position_dodge(0.7),aes(ymin = ci.lb, ymax = ci.ub), width=0.3,size=0.8)+
      geom_point(position=position_dodge(0.7), size=4, stroke = 0.5,shape = 21) +
      #scale_shape_manual(values=c("Meta-analytical data (ROM)"=21,"Primary data (ROM)"=24))+
      geom_text(aes(x = Management, y = ci.ub +6, label = n),
                position = position_dodge(width = 0.7),vjust = 0, hjust=0.5,
                size = 4.5, check_overlap = FALSE)+
      scale_x_discrete(limits=rev(c("Reduced tillage","No tillage","Crop rotation",
                                    "Cover cropping","Residue retention","Fertilizer timing",
                                    "Fertilizer rate","Fertilizer placement","Organic fertilizer",
                                    "Combined fertilizer","Enhanced efficiency")),
                       labels = rev(c("RT","ZT","ROT","CC","RES","RFT","RFR","RFP","OF","CF","EE")))+

      scale_y_continuous(limits=c(-70,120), breaks = c(-50,0,50,100))+
      labs(x = "Management practice", y = "Relative change of NUEr (%)",colour = 'black')+
      theme_bw()+
      theme(legend.title = element_blank(),
            legend.direction = "horizontal",
            legend.position=c(0.3,0.1),
            legend.key = element_rect(fill = "white",size = 1.5),
            legend.key.width = unit(0.4,"lines"),
            legend.key.height = unit(0.5,"lines"),
            legend.background = element_blank(),
            legend.text=element_text(colour = 'black', size=18),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title=element_text(size=20, colour = 'black', face='bold'),
            axis.title.x=element_blank(),
            axis.text.y = element_text(colour = 'black', size = 22),
            axis.text.x = element_text(colour = 'black', size = 22, hjust = 0.5, vjust = 0.5))






mydata <- metaresult_group[Vari=='NUE' & `Group type` == 'Method']

# make the plot (b)
p2 <- ggplot(data = metaresult_group[Vari=='NUE' & `Group type` == 'Method' & grepl('Method2',Group)],
             aes(y=Management,x=mean)) +
      geom_bar(aes(x = mean, y = Management), stat = "identity", color='lightblue', fill = 'lightblue') +
      geom_errorbar(aes(y = Management, xmin = ci.lb, xmax = ci.ub), width = 0.4) +

      geom_text(aes(y = Management, x = ci.ub + 3, label = n),
                position = position_dodge(width = 0.7),vjust = 0.5,
                hjust=0, size = 4.5, check_overlap = FALSE)+
      scale_y_discrete(limits=rev(c("Reduced tillage","No tillage","Crop rotation",
                                    "Cover cropping","Residue retention","Fertilizer timing",
                                    "Fertilizer rate","Fertilizer placement","Organic fertilizer",
                                    "Combined fertilizer","Enhanced efficiency")),
                       labels = rev(c("RT","ZT","CR","CC","RR","RFT","RFR","RFP","OF","CF","EE")))+
      scale_x_continuous(limits=c(-15,20), breaks = c(-10,0,10,20))+
      labs(y = "\nManagement practices", x = "Absolute change of NUE (%)   ",colour = 'black')+
      theme_bw()+
      ggtitle('Effect of management practices on NUE')

# save the plot
ggsave(plot = p2, file = "products/240704_figure_1_new.png",width = 180,height = 0.5*270, units = "mm")


# -----


# load in the data
d2 <- readxl::read_xlsx('articles/ncoms23/Source Data.xlsx',sheet ="Figure3b")
d2 <- as.data.table(d2)
d2[grepl('RES',Moderator1),Moderator1 := 'RR']
d2[grepl('CC/ROT',Moderator1),Moderator1 := 'CC/CR']

# set moderater levels
d2$Moderator1 <- factor(d2$Moderator1,
                        levels = c("EE","CF","OF", "MF","RFP",
                                  "RFR","RFT", "RR","CC/CR","ZT/RT",
                                   "Cr_w","Cr_m","Cr_r","N_sc","Clay_sc",
                                  "SOC_sc","pH_sc","MAP_sc","MAT_sc",
                                  "N_sq_sc","RFP˟Cr_m","MAT_sc˟Cr_m","N_sc˟SOC_sc"))
d2$colorfactor <- c("#66c2a5", "#66c2a5", "#66c2a5",
                    "#66c2a5", "#66c2a5", "#66c2a5",
                    "#66c2a5", "#e78ac3",
                    "#e78ac3","#fb8072", "#fdbf6f",
                    "#fdbf6f", "#fdbf6f", "#fdbf6f","#bebada",
                    "#bebada","#bebada","#bebada","#bebada","#bebada","#a6d854",
                    "#a6d854", "#a6d854")

# plot the figure (b)
p2 <- ggplot(d2,aes(x = Moderator1, y = Parameter_estimate))+
      geom_bar(stat = "identity", aes(fill = colorfactor), alpha = 0.7,show.legend = F)+
      xlab("Management practices and site properties")+  ylab("Parameter estimate")+
      annotate("text",x = d2$Moderator1[c(1:8,10,14,15,20:23)],y = d2$Parameter_estimate[c(1:8,10,14,15,20:23)]+0.5,label='***',size=5)+
      annotate("text",x = d2$Moderator1[c(9,16)],y = d2$Parameter_estimate[c(9,16)]+0.5,label='**',size=5)+
      annotate("text",x = d2$Moderator1[c(11,13,18)],y = d2$Parameter_estimate[c(11,13,18)]+0.5,label='*',size=5)+
      annotate("text",x = d2$Moderator1[c(12,17,19)],y = d2$Parameter_estimate[c(12,17,19)]+0.5,label='',size=5)+
      ggtitle('meta-regression model on NUE')+ theme_bw()+
      annotate("text",x=21,y=-28,label= expression(paste("Pseudo-",italic(R^2),"= 0.65")), size=4, colour="deeppink4")+
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
            panel.grid = element_blank(),
            legend.position = "none") + ylim(-40,40)


ggsave(plot = p2, file = "products/240704_figure_2_new.png", width = 180, height = 210/3, units = "mm")

