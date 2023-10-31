library(reshape2)
library(ggplot2)
library(svglite)
library(ggpubr)
library(Rmisc)
library(stringr)
library(ggthemes)
library(scales)
library(RColorBrewer)
pub_theme <- function() {
  theme_bw() + 
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.border = element_blank(), 
      axis.line = element_line(colour = "black"), 
      text=element_text(family = 'sans', size=8),  
      plot.margin= margin(0,2,0,0,'cm'), 
      panel.background = element_blank(),
      plot.background = element_blank())
}
get_wraper <- function(width) {
  function(x) {
    lapply(strwrap(x, width = width, simplify = FALSE), paste, collapse="\n")
  }
}

fluor <- read.csv('./data-4D.txt', header=T, sep='\t')
colnames(fluor) <- c('strain', 'replicate', 'tech_avg', 'contr_average', 'ratio', 'name')
fluor$strain <- factor(fluor$strain, levels=c('VC', 'gna_bilr', 'gna_bilrAA'), 
                       labels= c('Vector Control', 'pET_gna_bilR', 'pET_gna_bilR(DR166AA)'))
data_summary <- summarySE(fluor, measurevar='ratio', groupvars=c('strain'))

t_pcw_gna_bilR <- t.test(log2(fluor$ratio[fluor$strain == 'pET_gna_bilR']), log2(fluor$ratio[fluor$strain == 'Vector Control']), alternative = "greater")
t_pcw_gna_bilRmut <- t.test(log2(fluor$ratio[fluor$strain == 'pET_gna_bilR(DR166AA)']), log2(fluor$ratio[fluor$strain == 'Vector Control']), alternative = "greater")
t_pcw_gna_bilRcomp <- t.test(log2(fluor$ratio[fluor$strain == 'pET_gna_bilR']), log2(fluor$ratio[fluor$strain == 'pET_gna_bilR(DR166AA)']), alternative = "greater")

ggplot(data=data_summary, aes(x=strain, y=ratio, label=ratio)) + geom_hline(yintercept = 1, color='darkgray') + geom_bar(stat='identity', fill='#4E79A7') + 
  geom_errorbar(aes(ymin=ratio-se, ymax=ratio+se), width=0.5) +
  ylab('Ratio (strain / vector control)') + xlab(element_blank()) +
  scale_x_discrete(labels=get_wraper(10)) +
  pub_theme() + theme(axis.text.x=element_text(size=4), axis.text.y=element_text(size=6), axis.title.y = element_text(size=6)) + 
  geom_bracket(xmin=c('Vector Control', 'Vector Control', 'pET_gna_bilR'), xmax=c('pET_gna_bilR', 'pET_gna_bilR(DR166AA)', 'pET_gna_bilR(DR166AA)'), 
               label=c('3.48e-07', '3.92e-03', '3.27e-06'), y.position = c(90, 100, 90), size=0.5, label.size=2, vjust=0.5) +
  geom_jitter(data=fluor, aes(x=strain, y=ratio), size=0.5, height=0, width=0.2)
ggsave('Figure4D.png', height=1.7, width=2.5, bg='transparent')

