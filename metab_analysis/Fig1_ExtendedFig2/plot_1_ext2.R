library(ggplot2)
library(patchwork)
library(ggbreak)
library(cowplot)
library(Rmisc)

pub_theme <- function() {
  theme_bw() + 
    theme(
      panel.grid.major = element_blank(), legend.position = "none",
      panel.grid.minor = element_blank(), 
      panel.border = element_blank(), 
      axis.line = element_line(colour = "black"), 
      text=element_text(family = 'sans', size=14),  
      plot.margin= margin(0,2,0,0,'cm'), 
      panel.background = element_blank(),
      plot.background = element_blank())
}
get_wraper <- function(width) {
  function(x) {
    lapply(strwrap(x, width = width, simplify = FALSE), paste, collapse="\n")
  }
}

dat <- read.csv('./Overall-data-org.txt', sep='\t', header=T)
ggplot(data=dat, aes(x=strain, y=ratio.to.blank)) + 
  geom_bar(stat='identity', position='dodge', aes(fill=bili)) + 
  pub_theme() + geom_hline(yintercept = 5, col='gray') + scale_fill_manual(values=c('lightgray', 'darkgray')) + 
  pub_theme()


dat_summary <- summarySE(dat, measurevar='ratio.to.blank', groupvars=c('strain', 'bili'))
dat_summary <- dat_summary[dat_summary$strain != 'BHI',]
dat_summary$strain <- factor(dat_summary$strain, 
                             levels=c('M62' , 'Dif02' , 'Dif22' , 'Dif23' , 'Dif25' , 'Dif38' , 'sym_02' , 'sym01' , 'gna-01' , 'Bol01' , 'Cit01','Clo01' , 'Col02' , 'Inn01' , 'Reu01' , '5_alpha' , 'BL21' , 'JM109' , 'Cell01' , 'Fin01' , 'Fra02' , 'Fra04' , 'Ado01'),
                             labels=c('Clostridium sp. M62/1' , 'C. difficile CD3' , 'C. difficile P7' , 'C. difficile P8' , 'C. difficile P11' , 'C. difficile Isolate 7' , 'C. symbiosum WAL-14163' , 'C. symbiosum WAL-14673' , 'R. gnavus CC55_001C' , 'C. boltae CC43_001B' ,  'C. citroniae WAL-17109', 'C. clostridioforme 2_1_49FAA' , 'C. clostridioforme Strain WAL-7855' , 'C. innoculum 6_1_30' , 'L. reuteri CF48-3A' , 'E. coli 5-alpha' , 'E. coli BL21' , 'E. coli JM109' , 'B. cellulosilytious CL02T12C19' , 'B. finegoldi - CL09T03C10' , 'B. fragilis Strain 3_1_12' , 'B. fragilis Strain CL07T00C01' , 'B. adolescentis - L2-32'))
dat <- dat[dat$strain != 'BHI',]
dat$strain <- factor(dat$strain, 
                             levels=c('M62' , 'Dif02' , 'Dif22' , 'Dif23' , 'Dif25' , 'Dif38' , 'sym_02' , 'sym01' , 'gna-01' , 'Bol01' , 'Cit01', 'Clo01' , 'Col02' , 'Inn01' , 'Reu01' , '5_alpha' , 'BL21' , 'JM109' , 'Cell01' , 'Fin01' , 'Fra02' , 'Fra04' , 'Ado01'),
                             labels=c('Clostridium sp. M62/1' , 'C. difficile CD3' , 'C. difficile P7' , 'C. difficile P8' , 'C. difficile P11' , 'C. difficile Isolate 7' , 'C. symbiosum WAL-14163' , 'C. symbiosum WAL-14673' , 'R. gnavus CC55_001C' , 'C. boltae CC43_001B' , 'C. citroniae WAL-17109', 'C. clostridioforme 2_1_49FAA' , 'C. clostridioforme Strain WAL-7855' , 'C. innoculum 6_1_30' , 'L. reuteri CF48-3A' , 'E. coli 5-alpha' , 'E. coli BL21' , 'E. coli JM109' , 'B. cellulosilytious CL02T12C19' , 'B. finegoldi - CL09T03C10' , 'B. fragilis Strain 3_1_12' , 'B. fragilis Strain CL07T00C01' , 'B. adolescentis - L2-32'))

# ggplot(data=dat_summary, aes(x=strain, y=ratio.to.blank, fill=bili)) + 
#   geom_bar(stat='identity', position='dodge') + 
#   pub_theme() + geom_hline(yintercept = 5, col='gray') + scale_fill_manual(values=c('lightgray', '#4E79A7')) + 
#   geom_errorbar(aes(ymin=ratio.to.blank-se, ymax=ratio.to.blank+se), width=0.5, position=position_dodge(0.9)) +
#   theme(axis.text.x=element_text(angle=-45, vjust=1, hjust=0, size=10, face='italic')) + ylab('Fluorescence ratio to blank') + xlab('Strain') +
#   geom_point(data=dat, aes(x=strain, y=ratio.to.blank, fill=bili), size=0.5, position=position_jitterdodge(dodge.width=1, jitter.width=0.1))
# 
# ggsave('Figure-1C-Screening.svg', bg='transparent', width=8, height=6)


ggplot(data=dat_summary[dat_summary$bili == 'TRUE',], aes(x=strain, y=ratio.to.blank, fill=bili)) + 
  geom_bar(stat='identity', position='dodge') + 
  pub_theme() + geom_hline(yintercept = 5, col='gray') + scale_fill_manual(values=c('#4E79A7', 'lightgray')) + 
  geom_errorbar(aes(ymin=ratio.to.blank-se, ymax=ratio.to.blank+se), width=0.5, position=position_dodge(0.9), linewidth=0.15) +
  theme(axis.line=element_line(linewidth=0.15), axis.text.x=element_text(angle=-45, vjust=1, hjust=0, size=5, face='italic'), axis.title.y=element_text(size=6), axis.text.y=element_text(size=5)) + ylab('Ratio (sample / blank)') + xlab(NULL) +
  geom_point(data=dat[dat$bili == 'TRUE',], aes(x=strain, y=ratio.to.blank, fill=bili), size=0.25, position=position_jitterdodge(dodge.width=1, jitter.width=0.1))
ggsave('Figure1C.png', bg='transparent', width=3.25, height=2)

ggplot(data=dat_summary[dat_summary$bili == 'FALSE',], aes(x=strain, y=ratio.to.blank, fill=bili)) + 
  geom_bar(stat='identity', position='dodge') + 
  pub_theme() + geom_hline(yintercept = 5, col='gray') + scale_fill_manual(values=c( 'lightgray', '#4E79A7')) + 
  geom_errorbar(aes(ymin=ratio.to.blank-se, ymax=ratio.to.blank+se), width=0.5, position=position_dodge(0.9)) +
  theme(axis.text.x=element_text(angle=-45, vjust=1, hjust=0, size=10, face='italic')) + ylab('Fluorescence ratio to blank') + xlab('Strain') +
  geom_point(data=dat[dat$bili == 'FALSE',], aes(x=strain, y=ratio.to.blank, fill=bili), size=0.5, position=position_jitterdodge(dodge.width=1, jitter.width=0.1))
ggsave('Extended_dat_Fig2.png', bg='transparent', width=8, height=6)
