library(reshape2)
library(ggplot2)
library(svglite)
library(ggpubr)
library(Rmisc)
library(stringr)
library(ggthemes)
library(scales)
library(RColorBrewer)
library(patchwork)
pub_theme <- function() {
  theme_bw() + 
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.border = element_blank(), 
      text=element_text(family = 'Arial', size=5), 
      panel.background = element_blank(),
      plot.background = element_blank(), axis.line = element_line(size=0.25))
}
get_wraper <- function(width) {
  function(x) {
    lapply(strwrap(x, width = width, simplify = FALSE), paste, collapse="\n")
  }
}

fluor <- read.csv('./native_mesobilirubin.txt', sep='\t', header=T)
fluor$strain <- factor(fluor$strain, levels=c('BHI blank', 'Dif02', 'Sym01', 'Gna01'), labels=c('BHI Blank', 'C. difficile', 'C. symbiosum', 'R. gnavus'))
data_summary <- summarySE(fluor, measurevar='norm', groupvars=c('strain'))

t_sym3 <- t.test(log2(fluor$norm[fluor$strain == 'C. symbiosum']), log2(fluor$norm[fluor$strain == 'BHI Blank']), alternative = "greater", var.equal = FALSE)
t_gna3 <- t.test(log2(fluor$norm[fluor$strain == 'R. gnavus']), log2(fluor$norm[fluor$strain == 'BHI Blank']), alternative = "greater", var.equal = FALSE)
t_dif <- t.test(log2(fluor$norm[fluor$strain == 'C. difficile']), log2(fluor$norm[fluor$strain == 'BHI Blank']), alternative = "greater", var.equal = FALSE)


native_meso <- ggplot(data=data_summary, aes(x=strain, y=norm, label=norm)) + geom_bar(stat='identity', fill='#4E79A7') + geom_errorbar(aes(ymin=norm-se, ymax=norm+se), width=0.5, linewidth=0.25) +
  ylab('Ratio (Sample / Blank)') + xlab(element_blank()) + geom_hline(yintercept = 1, color='darkgray') + ylim(0,130) +
  geom_bracket(xmin=c('BHI Blank', 'BHI Blank', 'BHI Blank'), xmax=c('C. difficile', 'C. symbiosum', 'R. gnavus'), 
               label=c('7.1e-04', '2.9e-03', '1.9e-03'), y.position = c(90, 100, 110), size=0.25, label.size=2, vjust=-0.1) + 
  pub_theme() + theme(axis.text.x=element_text(angle=-45, vjust=1, hjust=0, size=5))+
  geom_jitter(data=fluor, aes(x=strain, y=norm), size=0.5, height=0, width=0.2) + labs(tag='a') + theme(plot.tag = element_text(face='bold'))


fluor <- read.csv('./vector_mesobilirubin.txt', sep='\t', header=T)
fluor$strain <- factor(fluor$strain, levels=c('VC', 'pCW_sym_bilRS', 'pCW_gna_bilR'), labels=c('Vector Control', 'pCW_sym_bilRS', 'pCW_gna_bilR'))
data_summary <- summarySE(fluor, measurevar='norm', groupvars=c('strain'))

t_sym <- t.test(log2(fluor$norm[fluor$strain == 'pCW_sym_bilRS']), log2(fluor$norm[fluor$strain == 'Vector Control']), alternative = "greater", var.equal = FALSE)
t_gna <- t.test(log2(fluor$norm[fluor$strain == 'pCW_gna_bilR']), log2(fluor$norm[fluor$strain == 'Vector Control']), alternative = "greater", var.equal = FALSE)


vector_meso <- ggplot(data=data_summary, aes(x=strain, y=norm, label=norm)) + geom_bar(stat='identity', fill='#4E79A7') + geom_errorbar(aes(ymin=norm-se, ymax=norm+se), width=0.5, linewidth=0.25) +
  ylab('Ratio (Sample / Blank)') + xlab(element_blank()) + geom_hline(yintercept = 1, color='darkgray') + ylim(0,65) +
  geom_bracket(xmin=c('Vector Control', 'Vector Control'), xmax=c('pCW_sym_bilRS', 'pCW_gna_bilR'), 
               label=c('6.7e-03', '1.5e-04'), y.position = c(52, 60), size=0.25, label.size=2, vjust=-0.1) + 
  pub_theme() + theme(axis.text.x=element_text(angle=-45, vjust=1, hjust=0, size=5))+
  geom_jitter(data=fluor, aes(x=strain, y=norm), size=0.5, height=0, width=0.2)  + labs(tag='b') + theme(plot.tag = element_text(face='bold'))


design <- "
  334
"

native_meso + vector_meso + plot_layout(design = design) 
ggsave(file='Extende_dat_Fig6.png', bg='transparent', width=3.5, height=3, units='in', dpi=330)

