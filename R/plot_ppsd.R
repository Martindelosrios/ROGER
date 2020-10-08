#' A function to plot the projection phase space distribution.
#'
#' This function allows you to plot the projection phase space distribution of a
#'  dataset.
#' @param cl, bs, ing, rin, int: Data frames with the cluster galaxies (cl),
#'             backsplash galaxies (bs), infalling galaxies (ing),
#'             recent infalling galaxies (rin) and interlooper galaxies (int)
#'             of the catalog that will be plotted.
#'             It should have as columns the
#'             distance to the cluster center normalized to the virial radius of each
#'             cluster ('r') and the line-of-sight velocity relative to the cluster
#'             normalized to the line-of-sight velocity of the cluster ('v').
#' @param cl0, bs0, ing0, rin0, int0: Data frames with the cluster galaxies (cl),
#'             backsplash galaxies (bs), infalling galaxies (ing),
#'             recent infalling galaxies (rin) and interlooper galaxies (int)
#'             of the reference catalog that will be plotted.
#'             It should have as columns the
#'             distance to the cluster center normalized to the virial radius of each
#'             cluster ('r') and the line-of-sight velocity relative to the cluster
#'             normalized to the line-of-sight velocity of the cluster ('v').
#' @param dens: Flag indicating if the density should be plotted. Default = TRUE.
#' @param reference: Flag indicating is the properties of the reference catalog should be plotted. 
#'                  Default = FALSE.
#' @export
#' @examples
#' plot_ppsd(cl, bs, ing, rin, int)

plot_ppsd <- function(cl, bs, ing, rin, int, dens = TRUE, cl0 = cl_gal0, bs0 = bs_gal0, ing0 = in_gal0, rin0 = rin_gal0, int0 = int_gal0, reference = FALSE, hist_bins_x = 25, hist_bins_y = 20, geom = 'line'){

  cl   <- espejo(cl)
  bs   <- espejo(bs)
  ing  <- espejo(ing)
  rin  <- espejo(rin)
  int  <- espejo(int)
  cl0  <- espejo(cl0)
  bs0  <- espejo(bs0)
  ing0 <- espejo(ing0)
  rin0 <- espejo(rin0)
  int0 <- espejo(int0)


  p0 <- ggplot(data = cl, aes(r, v)) +
        theme_bw() +
        theme(text = element_text(family = "serif"),
              axis.text.x = element_text(size=10),
              axis.text.y = element_text(size=10))
        
  if(dens == TRUE){
    p <- p0+
      #scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.5)) +
      #scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.5)) +
      labs(x = TeX('R_{proj}/R_{200}'),
           y = TeX('|$\\Delta$V_{los}|/$\\sigma$')) +

      geom_point(data = int,aes(r,v),shape=19,size=0.2,color=c5, alpha = 0.5) +
  
      geom_density_2d(data = ing, aes(x = r, y = v, colour = ..level..), lwd = 1, bins = 6) + 
      scale_colour_gradient(low = rgb(1,1,1,0.0), high = c3)+
      new_scale_fill()+
      geom_density_2d_filled(data = ing, aes(x = r, y = v, alpha = ..level.., fill = ..level..), lwd = 1, bins = 6) + 
      scale_fill_manual(values = c('#ffffff','#dfdbe7','#bfb7d0','#9f93b9','#7f6fa2','#5f4b8b'))+
  
      new_scale_colour()+
      geom_density_2d(data = bs, aes(x = r, y = v, colour = ..level..), lwd = 1, bins = 6) + 
      scale_colour_gradient(low = rgb(1,1,1,0.0), high = c2)+
      new_scale_fill()+
      geom_density_2d_filled(data = bs, aes(x = r , y = v, alpha = ..level.., fill = ..level..), lwd = 1, bins = 6) + 
      scale_fill_manual(values = c('#ffffff','#ffe6d1','#ffcea3','#ffb576','#ff9d48','#ff851b'))+
  
      new_scale_colour()+
      geom_density_2d(data = rin, aes(x = r, y = v, colour = ..level..), lwd = 1, bins = 6) + 
      scale_colour_gradient(low =  rgb(1,1,1,0.0), high = c4)+
      new_scale_fill()+
      geom_density_2d_filled(data = rin, aes(x = r, y = v, alpha = ..level.., fill = ..level..), lwd = 1, bins = 6)+ 
      scale_fill_manual(values = c('#ffffff','#d8eae3','#b1d6c8','#8ac1ac','#63ad91','#3d9976'))+
  
      new_scale_colour()+
      geom_density_2d(data = cl, aes(x = r, y = v, colour = ..level..), lwd = 1, bins = 6) + 
      scale_colour_gradient(low =  rgb(1,1,1,0.0), high = c1)+
      new_scale_fill()+
      geom_density_2d_filled(data = cl, aes(x = r , y = v, alpha = ..level.., fill = ..level..), lwd = 1, bins = 6) + 
      scale_fill_manual(values = c('#ffffff','#f1cfd4','#e3a0a9','#d5707e','#c74153','#b91228'))+
 
      theme(text = element_text(family = "serif"),
            axis.text.x      = element_text(size=15),
            axis.text.y      = element_text(size=15),
            axis.title       = element_text(size=18),
            legend.position  = 'none') +
      coord_cartesian(ylim= c(0, 3), xlim= c(0,3), expand = FALSE)

  } else {
  
    p <- p0+
         #scale_x_continuous(limits=c(0,3),breaks=seq(0,3,0.5))+
         #scale_y_continuous(limits=c(0,3),breaks=seq(0,3,0.5))+
         labs(x=expression(paste(R/R[200])),
              y=expression(paste('|',Delta,V,'|/',sigma)))+
         geom_point(data=cl ,aes(r,v),shape=19,size=0.5,color=c1)+
         geom_point(data=bs ,aes(r,v),shape=19,size=0.5,color=c2)+
         geom_point(data=ing,aes(r,v),shape=19,size=0.5,color=c3)+
         geom_point(data=rin,aes(r,v),shape=19,size=0.5,color=c4)+
         geom_point(data=int,aes(r,v),shape=19,size=0.2,color=c5) + 
         coord_cartesian(ylim= c(0, 3), xlim= c(0,3), expand = FALSE)

  }
  
  if(reference == FALSE){
    hx <- ggplot(data=cl,aes(r))+ 
          labs(x = element_blank(),
               y = 'Norm. Counts')+
          #scale_x_continuous(limits=c(0,3),breaks=seq(0,3,0.5))+
          theme_minimal()+
          theme(text = element_text(family = "serif"),
                axis.title  = element_text(size=15), 
                axis.text.x = element_text(size=10),
                axis.text.y = element_text(size=10))+
          stat_bin(color=c1,lwd=1.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_x)+
          stat_bin(data = bs, color=c2,lwd=1.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_x)+
          stat_bin(data = ing, color=c3,lwd=1.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_x)+
          stat_bin(data = rin, color=c4,lwd=1.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_x)+
          stat_bin(data = int, color=c5,lwd=1.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_x)+
          coord_cartesian(xlim= c(0,3), expand = FALSE)

          #geom_density(color=c1,lwd=1.5)+
          #geom_density(data=bs, aes(r),color=c2,lwd=1.5)+
          #geom_density(data=ing,aes(r),color=c3,lwd=1.5)+
          #geom_density(data=rin,aes(r),color=c4,lwd=1.5)+
          #geom_density(data=int,aes(r),color=c5,lwd=1.5)
    
    hy <- ggplot(data=cl,aes(v))+
          labs(x = element_blank(),
               y = 'Norm. Counts')+
          scale_y_continuous(breaks=seq(0,1,0.07))+
          theme_minimal()+
          theme(text = element_text(family = "serif"),
                axis.title  = element_text(size=15), 
                axis.text.x = element_text(size=10),
                axis.text.y = element_text(size=10))+
          stat_bin(color=c1,lwd=1.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_y)+
          stat_bin(data = bs, color=c2,lwd=1.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_y)+
          stat_bin(data = ing, color=c3,lwd=1.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_y)+
          stat_bin(data = rin, color=c4,lwd=1.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_y)+
          stat_bin(data = int, color=c5,lwd=1.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_y)+
          coord_cartesian(xlim= c(0,3), ylim= c(0,3))+
          coord_flip(xlim = c(0,3), expand = FALSE)
          #geom_density(color=c1,lwd=1.5)+ 
          #geom_density(data=bs,aes(v),color=c2,lwd=1.5)+ 
          #geom_density(data=ing,aes(v),color=c3,lwd=1.5)+ 
          #geom_density(data=rin,aes(v),color=c4,lwd=1.5)+ 
          #geom_density(data=int,aes(v),color=c5,lwd=1.5)+ 

  } else {
    hx <- ggplot(data=cl,aes(r))+ 
          labs(x = element_blank(),
               y = 'Norm. Counts')+
          #scale_x_continuous(limits=c(0,3),breaks=seq(0,3,0.5))+
          theme_minimal()+
          theme(text = element_text(family = "serif"),
                axis.title  = element_text(size=15), 
                axis.text.x = element_text(size=13),
                axis.text.y = element_text(size=13))+
          stat_bin(color=c1,lwd=1.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_x)+
          stat_bin(data = bs, color=c2,lwd=1.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_x)+
          stat_bin(data = ing, color=c3,lwd=1.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_x)+
          stat_bin(data = rin, color=c4,lwd=1.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_x)+
          stat_bin(data = int, color=c5,lwd=1.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_x)+

          #geom_density(color=c1,lwd=1.5)+
          #geom_density(data=bs, aes(r),color=c2,lwd=1.)+
          #geom_density(data=ing,aes(r),color=c3,lwd=1.)+
          #geom_density(data=rin,aes(r),color=c4,lwd=1.)+
          #geom_density(data=int,aes(r),color=c5,lwd=1.)+
          stat_bin(data = cl0, lty = 2, color=c1,lwd=0.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_x)+
          stat_bin(data = bs0, lty = 2, color=c2,lwd=0.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_x)+
          stat_bin(data = ing0, lty = 2, color=c3,lwd=0.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_x)+
          stat_bin(data = rin0, lty = 2, color=c4,lwd=0.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_x)+
          stat_bin(data = int0, lty = 2, color=c5,lwd=0.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_x)+
          coord_cartesian(xlim= c(0,3), expand = FALSE)
          #geom_density(data=cl0, aes(r), color = c1, lwd = 0.5, lty = 2)+
          #geom_density(data=bs0, aes(r), color = c2, lwd = 0.5, lty = 2)+
          #geom_density(data=ing0, aes(r), color = c3, lwd = 0.5, lty = 2)+
          #geom_density(data=rin0, aes(r), color = c4, lwd = 0.5, lty = 2)+
          #geom_density(data=int0, aes(r), color = c5, lwd = 0.5, lty = 2)
    
    hy <- ggplot(data=cl,aes(v))+
          labs(x = element_blank(),
               y = 'Norm. Counts')+
          scale_y_continuous(breaks=seq(0,1,0.07))+
          theme_minimal()+
          theme(text = element_text(family = "serif"),
                axis.title  = element_text(size=15), 
                axis.text.x = element_text(size=13),
                axis.text.y = element_text(size=13))+
          stat_bin(color=c1,lwd=1.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_y)+
          stat_bin(data = bs, color=c2,lwd=1.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_y)+
          stat_bin(data = ing, color=c3,lwd=1.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_y)+
          stat_bin(data = rin, color=c4,lwd=1.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_y)+
          stat_bin(data = int, color=c5,lwd=1.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_y)+
          #geom_density(color=c1,lwd=1.5)+ 
          #geom_density(data=bs,aes(v),color=c2,lwd=1.5)+ 
          #geom_density(data=ing,aes(v),color=c3,lwd=1.5)+ 
          #geom_density(data=rin,aes(v),color=c4,lwd=1.5)+ 
          #geom_density(data=int,aes(v),color=c5,lwd=1.5)+ 
          stat_bin(data = cl0, lty = 2, color=c1,lwd=0.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_y)+
          stat_bin(data = bs0, lty = 2, color=c2,lwd=0.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_y)+
          stat_bin(data = ing0, lty = 2, color=c3,lwd=0.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_y)+
          stat_bin(data = rin0, lty = 2, color=c4,lwd=0.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_y)+
          stat_bin(data = int0, lty = 2, color=c5,lwd=0.5,aes(y=..count../sum(..count..)), geom = geom, bins = hist_bins_y)+
          #geom_density(data=cl0, aes(v), color = c1, lwd = 0.5, lty = 2)+
          #geom_density(data=bs0, aes(v), color = c2, lwd = 0.5, lty = 2)+
          #geom_density(data=ing0, aes(v), color = c3, lwd = 0.5, lty = 2)+
          #geom_density(data=rin0, aes(v), color = c4, lwd = 0.5, lty = 2)+
          #geom_density(data=int0, aes(v), color = c5, lwd = 0.5, lty = 2)+
          coord_cartesian(xlim= c(0,3), ylim= c(0,3))+
          coord_flip(xlim = c(0,3), expand = FALSE)
  }
   
  blankPlot <- ggplot()+geom_blank(aes(1,1))+
    theme(
     plot.background = element_blank(), 
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(), 
     panel.border = element_blank(),
     panel.background = element_blank(),
     axis.title.x = element_blank(),
     axis.title.y = element_blank(),
     axis.text.x = element_blank(), 
     axis.text.y = element_blank(),
     axis.ticks = element_blank(),
     axis.line = element_blank()
       )
  
  plot <- grid.arrange(hx, blankPlot, p, hy, 
          ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))
  
  return(plot)
}

espejo <- function(dat){
  dat0 <- dat
  dat1 <- dat
  dat1$v <- -dat1$v
  dat <- rbind(dat0, dat1)
  return(dat)
} 

