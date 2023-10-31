library(ggplot2)
library(patchwork)
library(ggbreak)
library(cowplot)

pub_theme <- function() {
  theme_bw() + 
    theme(
      panel.grid.major = element_blank(), legend.position = "none",
      panel.grid.minor = element_blank(), 
      panel.border = element_blank(), 
      axis.line = element_line(colour = "black"), 
      text=element_text(family = 'sans', size=8),
      plot.title=element_text(family='sans', size=8, face='bold'),
      axis.text = element_text(family='sans', size=8),
      plot.margin= margin(0,0.25,0,0,'cm'), 
      panel.background = element_blank(),
      plot.background = element_blank())
}
get_wraper <- function(width) {
  function(x) {
    lapply(strwrap(x, width = width, simplify = FALSE), paste, collapse="\n")
  }
}

process_input <- function(f, lab) {
  d <- read.csv(file=f, sep='\t', header=TRUE)
  colnames(d) <- c('Retention', 'Mesobilirubin', 'Stercobilin', 'Urobilin')
  uro <- d[c('Retention', 'Urobilin')]
  uro$label <- 'Urobilin'
  colnames(uro) <- c('Retention', 'Intensity', 'Label')
  ste <- d[c('Retention', 'Stercobilin')]
  ste$label <- 'Stercobilin'
  colnames(ste) <- c('Retention', 'Intensity', 'Label')
  mes <- d[c('Retention', 'Mesobilirubin')]
  mes$label <- 'Mesobilirubin'
  colnames(mes) <- c('Retention', 'Intensity', 'Label')
  d2 <- rbind(uro, ste, mes)
  d2$Label <- factor(d2$Label, levels=c('Urobilin', 'Stercobilin', 'Mesobilirubin'))
  return(d2)
}

df_vc1 <- process_input('./data-export/vc_1.tsv', 'vc1')
df_vc2 <- process_input('./data-export/vc_2.tsv', 'vc2')
df_vc3 <- process_input('./data-export/vc_3.tsv', 'vc3')
df_vc4 <- process_input('./data-export/vc_4.tsv', 'vc4')

df_dif1 <- process_input('./data-export/dif_1.tsv', 'dif1')
df_dif2 <- process_input('./data-export/dif_2.tsv', 'dif2')
df_dif3 <- process_input('./data-export/dif_3.tsv', 'dif3')
df_dif4 <- process_input('./data-export/dif_4.tsv', 'dif4')

df_sym1 <- process_input('./data-export/sym_1.tsv', 'sym1')
df_sym2 <- process_input('./data-export/sym_2.tsv', 'sym2')
df_sym3 <- process_input('./data-export/sym_3.tsv', 'sym3')
df_sym4 <- process_input('./data-export/sym_4.tsv', 'sym4')

df_bhi1 <- process_input('./data-export/bhi_1.tsv')
df_bhi2 <- process_input('./data-export/bhi_2.tsv')
df_bhi3 <- process_input('./data-export/bhi_3.tsv')
df_bhi4 <- process_input('./data-export/bhi_4.tsv')


# The standards have different channel orders for the data export than the samples
d <- read.csv(file='./data-export/std_bil.tsv', sep='\t', header=TRUE)
colnames(d) <- c('Retention', 'Urobilin', 'Stercobilin', 'Mesobilirubin')
uro <- d[c('Retention', 'Urobilin')]
uro$label <- 'Urobilin'
colnames(uro) <- c('Retention', 'Intensity', 'Label')
ste <- d[c('Retention', 'Stercobilin')]
ste$label <- 'Stercobilin'
colnames(ste) <- c('Retention', 'Intensity', 'Label')
mes <- d[c('Retention', 'Mesobilirubin')]
mes$label <- 'Mesobilirubin'
colnames(mes) <- c('Retention', 'Intensity', 'Label')
df_bil1 <- rbind(uro, ste, mes)
df_bil1$Label <- factor(df_bil1$Label, levels=c('Urobilin', 'Stercobilin', 'Mesobilirubin'))

d <- read.csv(file='./data-export/std_uro.tsv', sep='\t', header=TRUE)
colnames(d) <- c('Retention', 'Stercobilin', 'Mesobilirubin', 'Urobilin')
uro <- d[c('Retention', 'Urobilin')]
uro$label <- 'Urobilin'
colnames(uro) <- c('Retention', 'Intensity', 'Label')
ste <- d[c('Retention', 'Stercobilin')]
ste$label <- 'Stercobilin'
colnames(ste) <- c('Retention', 'Intensity', 'Label')
mes <- d[c('Retention', 'Mesobilirubin')]
mes$label <- 'Mesobilirubin'
colnames(mes) <- c('Retention', 'Intensity', 'Label')
df_uro1 <- rbind(uro, ste, mes)
df_uro1$Label <- factor(df_uro1$Label, levels=c('Urobilin', 'Stercobilin', 'Mesobilirubin'))

d <- read.csv(file='./data-export/std_ste.tsv', sep='\t', header=TRUE)
colnames(d) <- c('Retention', 'Mesobilirubin', 'Urobilin', 'Stercobilin')
uro <- d[c('Retention', 'Urobilin')]
uro$label <- 'Urobilin'
colnames(uro) <- c('Retention', 'Intensity', 'Label')
ste <- d[c('Retention', 'Stercobilin')]
ste$label <- 'Stercobilin'
colnames(ste) <- c('Retention', 'Intensity', 'Label')
mes <- d[c('Retention', 'Mesobilirubin')]
mes$label <- 'Mesobilirubin'
colnames(mes) <- c('Retention', 'Intensity', 'Label')
df_ste1 <- rbind(uro, ste, mes)
df_ste1$Label <- factor(df_ste1$Label, levels=c('Urobilin', 'Stercobilin', 'Mesobilirubin'))



p_bhi1 <- ggplot(data=df_bhi1, aes(x=Retention, y=Intensity, group=Label, color=Label)) + 
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,6000000) + xlim(2,6) + ggtitle('BHI 1') + labs(tag='p') + theme(plot.tag = element_text(face='bold', size=8))
p_bhi2 <- ggplot(data=df_bhi2, aes(x=Retention, y=Intensity, group=Label, color=Label)) + 
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,6000000) + xlim(2,6) + ggtitle('BHI 2') + labs(tag='q') + theme(plot.tag = element_text(face='bold', size=8))
p_bhi3 <- ggplot(data=df_bhi3, aes(x=Retention, y=Intensity, group=Label, color=Label)) + 
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,6000000) + xlim(2,6) + ggtitle('BHI 3') + labs(tag='r') + theme(plot.tag = element_text(face='bold', size=8))
p_bhi4 <- ggplot(data=df_bhi4, aes(x=Retention, y=Intensity, group=Label, color=Label)) + 
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,6000000) + xlim(2,6) + ggtitle('BHI 4') + labs(tag='s') + theme(plot.tag = element_text(face='bold', size=8))


p_bil1 <- ggplot(data=df_bil1, aes(x=Retention, y=Intensity, group=Label, color=Label)) + 
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,25000000) + xlim(2,6.5) + ggtitle('Mesobilirubin Standard') + labs(tag='a') + theme(plot.tag = element_text(face='bold', size=8))
p_uro1 <- ggplot(data=df_uro1, aes(x=Retention, y=Intensity, group=Label, color=Label)) + 
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,6500000) + xlim(2,6) + ggtitle('Urobilin Standard') + labs(tag='b') + theme(plot.tag = element_text(face='bold', size=8))
p_ste1 <- ggplot(data=df_ste1, aes(x=Retention, y=Intensity, group=Label, color=Label)) + 
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,25000000) + xlim(2,6) + ggtitle('Stercobilin Standard') + labs(tag='c') + theme(plot.tag = element_text(face='bold', size=8))


p_vc1 <- ggplot(data=df_vc1, aes(x=Retention, y=Intensity, group=Label, color=Label)) + 
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,6000000) + xlim(2,6)  + ggtitle('E. coli + pCW-lic 1') + labs(tag='l')  + theme(plot.tag = element_text(face='bold', size=8))# +ylab(NULL) +xlab(NULL) +ggtitle('Vector Control') +
  #annotate('text', x=c(2.65, 2.42), y=c(650000, 757000), label=c('Urobilin', 'Mesobilirubin'), angle=45)

p_vc2 <- ggplot(data=df_vc2, aes(x=Retention, y=Intensity, group=Label, color=Label)) + 
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,6000000) + xlim(2,6)  + ggtitle('E. coli + pCW-lic 2') + labs(tag='m') + theme(plot.tag = element_text(face='bold', size=8))
p_vc3 <- ggplot(data=df_vc3, aes(x=Retention, y=Intensity, group=Label, color=Label)) + 
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,6000000) + xlim(2,6)  + ggtitle('E. coli + pCW-lic 3') + labs(tag='n') + theme(plot.tag = element_text(face='bold', size=8))
p_vc4 <- ggplot(data=df_vc4, aes(x=Retention, y=Intensity, group=Label, color=Label)) + 
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,6000000) + xlim(2,6)  + ggtitle('E. coli + pCW-lic 4') + labs(tag='o') + theme(plot.tag = element_text(face='bold', size=8))

p_dif1 <- ggplot(data=df_dif1, aes(x=Retention, y=Intensity, group=Label, color=Label)) + 
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,6000000) + xlim(2,6)  + ggtitle('E. coli + \npCW-dif-bilRS 1') + labs(tag='d') + theme(plot.tag = element_text(face='bold', size=8))
p_dif2 <- ggplot(data=df_dif2, aes(x=Retention, y=Intensity, group=Label, color=Label)) + 
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,6000000) + xlim(2,6)  + ggtitle('E. coli + \npCW-dif-bilRS 2') + labs(tag='e') + theme(plot.tag = element_text(face='bold', size=8))
p_dif3 <- ggplot(data=df_dif3, aes(x=Retention, y=Intensity, group=Label, color=Label)) + 
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,6000000) + xlim(2,6)  + ggtitle('E. coli + \npCW-dif-bilRS 3') + labs(tag='f') + theme(plot.tag = element_text(face='bold', size=8))
p_dif4 <- ggplot(data=df_dif4, aes(x=Retention, y=Intensity, group=Label, color=Label)) + 
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,6000000) + xlim(2,6)  + ggtitle('E. coli + \npCW-dif-bilRS 4') + labs(tag='g') + theme(plot.tag = element_text(face='bold', size=8))# + xlab(NULL) +ggtitle('E. coli + C. difficile bilR') + 
  #annotate('text', x=c(2.65, 2.42), y=c(3200000, 757000), label=c('Urobilin', 'Mesobilirubin'), angle=45) + theme(plot.title = element_text(face = "italic"))


p_sym1 <- ggplot(data=df_sym1, aes(x=Retention, y=Intensity, group=Label, color=Label)) + 
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,6000000) + xlim(2,6)  + ggtitle('E. coli + \npCW-sym-bilRS 1') + labs(tag='h') + theme(plot.tag = element_text(face='bold', size=8))
p_sym2 <- ggplot(data=df_sym2, aes(x=Retention, y=Intensity, group=Label, color=Label)) + 
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,6000000) + xlim(2,6)  + ggtitle('E. coli + \npCW-sym-bilRS 2') + labs(tag='i') + theme(plot.tag = element_text(face='bold', size=8))
p_sym3 <- ggplot(data=df_sym3, aes(x=Retention, y=Intensity, group=Label, color=Label)) + 
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,6000000) + xlim(2,6)  + ggtitle('E. coli + \npCW-sym-bilRS 3') + labs(tag='j') + theme(plot.tag = element_text(face='bold', size=8))# + ylab(NULL) + xlab('Retention Time (min)') + ggtitle('E. coli + C. symbiosum bilR') +
  #annotate('text', x=c(2.65, 2.42), y=c(2100000, 757000), label=c('Urobilin', 'Mesobilirubin'), angle=45) + theme(plot.title = element_text(face = "italic"))
p_sym4 <- ggplot(data=df_sym4, aes(x=Retention, y=Intensity, group=Label, color=Label)) + 
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,6000000) + xlim(2,6)   + ggtitle('E. coli + \npCW-sym-bilRS 4') + labs(tag='k') + theme(plot.tag = element_text(face='bold', size=8))

p_legend <- ggplot(data=df_sym4, aes(x=Retention, y=Intensity, group=Label, color=Label)) + 
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,6000000) + xlim(2,6) + labs(color = 'Metabolite') + theme(legend.position='right')

legend <- cowplot::get_legend(p_legend)

design <- "
  ABCD
  EFGH
  IJKL
  MNOP
  QRST
"

p_bil1 + p_uro1 + p_ste1 + legend + p_dif1 + p_dif2 + p_dif3 + p_dif4 + p_sym1 + p_sym2 + p_sym3 + p_sym4 + p_vc1 + p_vc2 + p_vc3 + p_vc4 + p_bhi1 + p_bhi2 + p_bhi3 + p_bhi4 + plot_layout(design=design)

#(p_bil1 | p_uro1 | p_ste1 | plot_spacer()) / (p_dif1 | p_dif2 | p_dif3 | p_dif4) / (p_sym1 | p_sym2 | p_sym3 | p_sym4) / (p_vc1 | p_vc2 | p_vc3 | p_vc4) / (p_bhi1 | p_bhi2 | p_bhi3 | p_bhi4 )
ggsave('./Extended_data_Fig5.png', heigh=290, width=200, units='mm', bg='transparent', dpi=330)  
  