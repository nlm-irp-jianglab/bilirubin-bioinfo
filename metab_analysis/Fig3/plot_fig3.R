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
# Set up generic functions for plotting and label formatting. 
pub_theme <- function() {
  theme_bw() + 
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.border = element_blank(), 
      axis.line = element_line(colour = "black"), 
      text=element_text(family = 'Arial', size=5), 
      panel.background = element_blank(),
      plot.background = element_blank())
}
get_wraper <- function(width) {
  function(x) {
    lapply(strwrap(x, width = width, simplify = FALSE), paste, collapse="\n")
  }
}

# Read in fluorescence data for C. diff and C. sym transformants. 
flour <- read.csv('./05_duke_III_flour.csv')
colnames(flour) <- c('sample', 'replicate', 'ratio')
flour$sample <- factor(flour$sample, levels=c('VC', 'DifYZ', 'SymYZ'), labels=c('Vector Control', 'pCW_dif_bilRS', 'pCW_sym_bilRS'))
# Summarize mean and standard error per sample
data_summary <- summarySE(flour, measurevar='ratio', groupvars=c('sample'))

# Test for significant difference in bilirubin reduction based on log transformed data
t_difYZ <- t.test(log2(flour$ratio[flour$sample == 'pCW_dif_bilRS']), log2(flour$ratio[flour$sample == 'Vector Control']), alternative = "greater", var.equal = FALSE)
t_symYZ <- t.test(log2(flour$ratio[flour$sample == 'pCW_sym_bilRS']), log2(flour$ratio[flour$sample == 'Vector Control']), alternative = "greater", var.equal = FALSE)

# Plot C. diff and C. sym data
flour_dukeIII <- ggplot(data=data_summary, aes(x=sample, y=ratio, label=ratio)) + geom_bar(stat='identity', fill='#4E79A7') + geom_errorbar(aes(ymin=ratio-se, ymax=ratio+se), width=0.5, linewidth=0.25) +
  ylab('Ratio (Sample / Vector Control)') + xlab(element_blank()) + geom_hline(yintercept = 1, color='darkgray') + ylim(0,37)+
  geom_bracket(xmin=c('Vector Control', 'Vector Control'), xmax=c('pCW_dif_bilRS', 'pCW_sym_bilRS'), label=c('9.99e-06', '2.29e-05'), y.position = c(34, 37), size=0.35, label.size=1.6, vjust=0) + 
  pub_theme() + theme(axis.text.x=element_text(angle=-45, vjust=1, hjust=0, size=6), axis.line = element_line(linewidth=0.25), axis.text.y = element_text(size=5), axis.title.y=element_text(size=6)) + 
  geom_jitter(data=flour, aes(x=sample, y=ratio), size=0.5, height=0, width=0.2)


# Read in C. diff and C. sym metabolomics data
metab <- read.csv('./05_duke_III_metab.csv')
colnames(metab) <- c('sample', 'replicate', 'signal')
metab$sample <- factor(metab$sample, levels=c( 'VC', 'DifYZ', 'SymYZ'), labels=c( 'Vector Control', 'pCW_dif_bilRS', 'pCW_sym_bilRS'))
# Remove media blank from the data
metab <- metab[!is.na(metab$sample),]
# Summarize mean and standard error per sample
data_summary <- summarySE(metab, measurevar='signal', groupvars=c('sample'))

# Test ofr significant difference in bilirubin reduction based on log transformed data
t.test(log2(metab$signal[metab$sample == 'pCW_sym_bilRS']), log2(metab$signal[metab$sample == 'Vector Control']), alternative='greater', var.equal=FALSE)
t.test(log2(metab$signal[metab$sample == 'pCW_dif_bilRS']), log2(metab$signal[metab$sample == 'Vector Control']), alternative='greater', var.equal=FALSE)

# Plot C. sym and C. diff log transformed data. 
metab_dukeIII <- ggplot(data=data_summary, aes(x=sample, y=signal, label=signal)) + geom_bar(stat='identity', fill='#4E79A7') + geom_errorbar(aes(ymin=signal-se, ymax=signal+se), width=0.5, size=0.25) +
  ylab('Urobilin Signal') +
  geom_bracket(xmin=c('Vector Control', 'Vector Control'), xmax=c('pCW_dif_bilRS', 'pCW_sym_bilRS'), label=c('7.59e-05', '2.78e-04'), y.position = c(1.2e7, 1.35e7), size=0.35, label.size=1.6, vjust=-0.15) +
  xlab(element_blank()) + pub_theme() + theme(axis.text.x=element_text(angle=-45, vjust=1, hjust=0, size=6), axis.line = element_line(size=0.25), axis.text.y = element_text(size=5), axis.title.y=element_text(size=6)) + scale_y_continuous(labels=scientific) + 
  geom_jitter(data=metab, aes(x=sample, y=signal), size=0.5, height=0, width=0.2)


# Read in fluorescence data for R. gnavus bilR transformant. 
flour <- read.csv('./07_bilR_mut_flour.csv', header=F, sep=',')
colnames(flour) <- c('sample', 'replicate', 'ratio')
flour$sample <- factor(flour$sample, levels=c('Vector Control', 'pCW-gna-bilR', 'pCW-gna-bilR(DR166GG)'), 
                       labels= c('Vector Control', 'pCW_gna_bilR', 'pCW_gna_bilR(DR166GG)'))
# Summarize mean and standard error per sample
data_summary <- summarySE(flour, measurevar='ratio', groupvars=c('sample'))

# test if R. gnavus bilR tranformant has higher bilirubin reduction activity. 
t_pcw_gna_bilR <- t.test(log2(flour$ratio[flour$sample == 'pCW_gna_bilR']), log2(flour$ratio[flour$sample == 'Vector Control']), alternative = "greater")




fluor <- read.csv('./data-4D.txt', header=T, sep='\t')
colnames(fluor) <- c('strain', 'replicate', 'tech_avg', 'contr_average', 'ratio', 'name')
fluor$strain <- factor(fluor$strain, levels=c('VC', 'gna_bilr', 'gna_bilrAA'), 
                       labels= c('Vector Control', 'pET_gna_bilR', 'pET_gna_bilR(DR166AA)'))
data_summary <- summarySE(fluor, measurevar='ratio', groupvars=c('strain'))
t_pcw_gna_bilR <- t.test(log2(fluor$ratio[fluor$strain == 'pET_gna_bilR']), log2(fluor$ratio[fluor$strain == 'Vector Control']), alternative = "greater")

flour_gnavus <- ggplot(data=data_summary[data_summary$strain != 'pET_gna_bilR(DR166AA)',], aes(x=strain, y=ratio, label=ratio)) + geom_hline(yintercept = 1, color='darkgray') + geom_bar(stat='identity', fill='#4E79A7') + 
  geom_errorbar(aes(ymin=ratio-se, ymax=ratio+se), width=0.5) +
  ylab('Ratio (strain / vector control)') + xlab(element_blank()) +
  scale_x_discrete(labels=get_wraper(20)) +
  pub_theme() + theme(axis.text.x=element_text(angle=-45, vjust=1, hjust=0, size=6), axis.line = element_line(size=0.25), axis.text.y = element_text(size=5), axis.title.y=element_text(size=6)) +
  geom_bracket(xmin=c('Vector Control'), xmax=c('pET_gna_bilR'), 
               label=c('3.48e-07'), y.position = c(90), size=0.5, label.size=2, vjust=0.5) +
  geom_jitter(data=fluor[fluor$strain != 'pET_gna_bilR(DR166AA)',], aes(x=strain, y=ratio), size=0.5, height=0, width=0.2)

# Combine plots and make figure file
flour_dukeIII + metab_dukeIII + flour_gnavus + plot_layout(widths=c(1,1,1), ncol=3)
ggsave(file='Figure3CDE.png', bg='transparent', width=3.5, height=3, units='in')


