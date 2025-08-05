# own function to plot multiple histograms
# all these plot functions have been defined for a single paper and axis titles or plot titles are specific for this purpose.
ggplot_hist <- function(...) {

  obj <- list(...)

  # make empty list
  out <- list()

  # convert all residual objects into one data.table
  for(i in 1:length(obj)){

    out[[i]] <- data.table(label = levels(obj[[i]]$`_label_`),
                           residual = obj[[i]]$`_residuals_`)

  }
  out <- rbindlist(out)
  out <- out[order(label)]

  # make a residual plot
  if(length(obj) > 1){

    p <- ggplot(out,aes(x=residual,y=..density..,fill=label)) +
      geom_histogram(position='identity',fill = '#482173')  +
      geom_density(aes(x=residual,y=..density..),fill = '#482173')+
      facet_wrap(~label,scales = "free_y")

  } else {

    p <- ggplot(out,aes(x=residual,y=..density..,fill=label)) +
      geom_histogram(position='identity',fill = '#482173')  +
      geom_density(aes(x=residual,y=..density..),fill = '#482173')

  }

  p <- p + theme_bw() + theme(legend.position = 'none',
                              axis.text = element_text(size = 20),
                              axis.title = element_text(size=20),
                              plot.title = element_text(size=22))


  return(p)
}

# plot 1 to 1 plots

# plot multiple histograms
ggplot_onetoone <- function(...) {

  obj <- list(...)

  # make empty list
  out <- list()

  # convert all residual objects into one data.bele
  for(i in 1:length(obj)){

    out[[i]] <- data.table(label = levels(obj[[i]]$`_label_`),
                           obs = obj[[i]]$`_y_`,
                           pred = obj[[i]]$`_y_hat_`)

  }
  out <- rbindlist(out)
  out <- out[order(label)]
  out[,label := gsub('_lm','_glm',label)]

  # make a residual plot
  if(length(obj) > 1){

    p <- ggplot(out,aes(x=obs,y=pred,col='#482173')) +
      geom_point(fill = '#482173',col='#482173')  + geom_abline(intercept = 0,slope = 1,lty=2)+
      facet_wrap(~label)

  } else {

    p <- ggplot(out,aes(x=obs,y=pred,col='#482173')) +
      geom_point(fill = '#482173',col='#482173')  + geom_abline(intercept = 0,slope = 1,lty=2)

  }

  p <- p + theme_bw() + theme(legend.position = 'none',
                              axis.text = element_text(size = 20),
                              axis.title = element_text(size=20),
                              plot.title = element_text(size=22)) +
    ylab('Predicted yield change') +
    xlab('Observed yield change')

  return(p)
}

ggplot_ale <- function(...,ftitle = NULL,pncol = NULL,tsc = 1){

  obj <- list(...)

  # make empty list
  out = out.cat = out.num = pout.cat = pout.num = list()
  count.cat = count.num = 0

  # convert all residual objects into one data.table
  for(i in 1:length(obj)){

    # plot title
    pt = list()

    # overwrite object name
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='fert_cat'] <- 'fertilizer type'
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='crop_type'] <- 'crop type'
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='nsplitc'] <- 'number of doses'
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='appc'] <- 'application mode'
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='mat'] <- 'temperature'
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='map'] <- 'precipitation'
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='tn'] <- 'total soil N'
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='ph'] <- 'soil pH'
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='bd'] <- 'bulk density'
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='n_dose'] <- 'N dose'
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='p_dose'] <- 'P dose'
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='k_dose'] <- 'K dose'
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='clay'] <- 'clay content'
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='cec'] <- 'CEC'
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='soc'] <- 'SOC'
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='crop_cat'] <- 'crop type'
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='GEnZ'] <- 'climate zone'
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='phtype'] <- 'pH type'
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='cmp_croprotation'] <- 'crop rotation'
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='cmp_rfp'] <- 'fertilizer placement'
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='cmp_rft'] <- 'fertilizer timing'
    obj[[i]]$`_vname_`[obj[[i]]$`_vname_`=='cmp_rfr'] <- 'fertilizer rate'

    # set fixed title
    #pt[[i]] <- paste0('Impact of ',unique(obj[[i]]$`_vname_`)," on change yield")
    pt[[i]] <- paste0('Impact of site properties on change in yield')

    if(class(obj[[i]]$`_x_`)=='factor'){

      count.cat <- count.cat + 1
      out <- data.table(parm = unique(obj[[i]]$`_vname_`),
                        label = unique(obj[[i]]$`_label_`),
                        y = obj[[i]]$`_yhat_`,
                        x = obj[[i]]$`_x_`)

      p <- ggplot(out,aes(x = x,y = y,color=parm,fill=parm)) +
        geom_bar(stat='identity',show.legend = FALSE)  +
        facet_wrap(~label,scales='free')

      p <- p + theme_bw() +
        scale_color_viridis_d(name ='driver')+
        scale_fill_viridis_d(name ='driver')+
        ylab(expression(delta*"yield")) +
        xlab('') +
        theme(axis.text.x = element_text(size = 20*tsc),
              axis.text.y = element_text(size = 20*tsc),
              axis.title = element_text(size = 20*tsc),
              plot.title = element_text(size = 22*tsc),
              strip.background = element_blank(),
              strip.text = element_blank()) +
        ggtitle(label=pt[[i]])

      pout.cat[[count.cat]] <- p

    } else {

      out.num[[i]] <- data.table(parm = unique(obj[[i]]$`_vname_`),
                                 label = unique(obj[[i]]$`_label_`),
                                 y = obj[[i]]$`_yhat_`,
                                 x = obj[[i]]$`_x_`)

    }
  }

  if(length(out.num)>0){

    # combine numeric ones per pname
    out <- rbindlist(out.num)

    # set factor to ensure order of plots
    #glevel <- c('nh3','n2o','no','no3')
    #glevel <- glevel[glevel %in% unique(out$pname)]
    #out[,pname := factor(pname,levels = glevel)]

    # split over the emission route
    # out2 <- split(out,out$pname)

    pt <- paste0('Impact of site properties on change in yield')
    count.num <- 0

    xaxmax <- min(8,max(out$x))
    xaxmin <- max(-5,min(out$x))

    for(j in 1:1){

      count.num <- count.num + 1

      out3 <- out# out2[[j]]

      if(j==1){
        p <- ggplot(out3,aes(x=x,y=y,color=parm,group=parm)) +
          geom_point(show.legend = TRUE)+ geom_smooth(show.legend = FALSE)
      } else {
        p <- ggplot(out3,aes(x=x,y=y,color=parm,group=parm)) +
          geom_point(show.legend = FALSE)  + geom_smooth(show.legend = FALSE)
      }


      p <- p + theme_bw() +
        theme(legend.position ='inside',
              legend.position.inside = c(0.8,0.5)) +
        scale_color_viridis_d(name ='driver')+
        #scale_color_manual(name = 'driver',
        #                   values = c('soil pH'='red3','N dose'='black','clay content'='green3',
        #                              'precipitation'='skyblue','SOC'='gray75','temperature'='orange'))+
        ylab(expression(delta*"yield change")) +
        xlab('Change in variable') +
        xlim(xaxmin,xaxmax) +
        theme(axis.text.x = element_text(size = 20*tsc),
              axis.text.y = element_text(size = 20*tsc),
              axis.title = element_text(size=20*tsc),
              plot.title = element_text(size=22*tsc),
              legend.text = element_text(size=20*tsc),
              legend.title = element_text(size=20*tsc),
              strip.background = element_blank(),
              strip.text = element_blank()
        ) +
        ggtitle(pt)

      if (j==1 & length(unique(out3$parm))>4){p <- p + guides(colour=guide_legend(ncol=2))}

      pout.num[[count.num]] <- p
      rm(p)
    }
  }


  ptout <- as.list(c(pout.cat,pout.num))

  if(is.null(pncol)){pncol =  min(length(ptout),4)} else {pncol = pncol}
  p <- patchwork::wrap_plots(ptout, ncol=pncol,axis_titles = 'collect')


  return(p)
}


ggplot_pdp <- function(obj, x) {


  out <- as.data.table(obj$agr_profiles)
  out[,label]
  p <-
    as_tibble() %>%
    mutate(`_label_` = stringr::str_remove(`_label_`, "^[^_]*_")) %>%
    ggplot(aes(`_x_`, `_yhat_`)) +
    geom_line(data = as_tibble(obj$cp_profiles),
              aes(x = {{ x }}, group = `_ids_`),
              size = 0.5, alpha = 0.05, color = "gray50")

  num_colors <- n_distinct(obj$agr_profiles$`_label_`)

  if (num_colors > 1) {
    p <- p + geom_line(aes(color = `_label_`), size = 1.2, alpha = 0.8)
  } else {
    p <- p + geom_line(color = "midnightblue", size = 1.2, alpha = 0.8)
  }

  p
}

ggplot_vip <- function(...,vexclude = NULL,ftitle = NULL,plotrow = 1){

  obj <- list(...)

  out <- list()

  # convert all residual objects into one data.bele
  for(i in 1:length(obj)){

    obj2 <- obj[[i]]

    if(!is.null(vexclude)){
      obj2 <- obj2[!obj2$variable %in% c(vexclude),]
    }

    # plot
    if(is.null(ftitle)){
      pt = pst = list()
      pst[[i]] <- 'created for meta-regression model\npredicting yield change'
      pt[[i]] <- 'Feature importance plot'
    } else {

      pt = pst = list()
      print("this option is not implemented yet. Replace by default")
      pt = pst = list()
      pst[[i]] <- 'created for meta-regression model\npredicting yield change'
      pt[[i]] <- 'Feature importance plot'

      }

    # adjust the names of the variables
    obj2[obj2$variable=='man','variable'] <- 'management'
    obj2[obj2$variable=='mat','variable'] <- 'temperature'
    obj2[obj2$variable=='map','variable'] <- 'precipitation'
    obj2[obj2$variable=='ph','variable'] <- 'soil pH'
    obj2[obj2$variable=='bd','variable'] <- 'bulk density'
    obj2[obj2$variable=='tn','variable'] <- 'total N'
    obj2[obj2$variable=='n_dose','variable'] <- 'N dose'
    obj2[obj2$variable=='p_dose','variable'] <- 'P dose'
    obj2[obj2$variable=='k_dose','variable'] <- 'K dose'
    obj2[obj2$variable=='clay','variable'] <- 'clay content'
    obj2[obj2$variable=='cec','variable'] <- 'CEC'
    obj2[obj2$variable=='soc','variable'] <- 'SOC'
    obj2[obj2$variable=='crop_type','variable'] <- 'crop type'
    obj2[obj2$variable=='cmp_croprotation','variable'] <- 'crop rotation'
    obj2[obj2$variable=='cmp_rfp','variable'] <- 'fertilizer placement'
    obj2[obj2$variable=='cmp_cropresidue','variable'] <- 'crop residue'
    obj2[obj2$variable=='cmp_covercrop','variable'] <- 'cover crop'
    obj2[obj2$variable=='cmp_ferttype','variable'] <- 'fertilizer type'
    obj2[obj2$variable=='cmp_tillage','variable'] <- 'tillage'
    obj2[obj2$variable=='cmp_rft','variable'] <- 'fertilizer timing'
    obj2[obj2$variable=='cmp_rfr','variable'] <- 'fertilizer rate'
    obj2[obj2$variable=='GEnZ','variable'] <- 'climate zone'


    out[[i]] <- plot(obj2) + theme_bw() + theme(legend.position = 'none') +
      ggtitle(label=pt[[i]],
              subtitle=pst[[i]])+
      ylab('') +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size=12),
            plot.title = element_text(size=14),
            plot.subtitle = element_text(size=12),
            strip.background = element_blank(),
            strip.text = element_blank())

  }
  names(out) <- paste0('p',1:length(obj))
  print(names(out))
  pout <- patchwork::wrap_plots(out, nrow=plotrow)

  return(pout)}




