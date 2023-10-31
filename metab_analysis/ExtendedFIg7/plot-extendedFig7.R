library(ggplot2)
library(patchwork)
library(ggbreak)
library(cowplot)
library(scales)

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

process_input <- function(f, lab, cols) {
  d <- read.csv(file=f, sep='\t', header=TRUE)
  colnames(d) <- cols
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










# The standards have different channel orders for the data export than the samples
d <- read.csv(file='./data_export/std-mesobil.tsv', sep='\t', header=TRUE)
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

d <- read.csv(file='./data_export/std-urobil.tsv', sep='\t', header=TRUE)
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

d <- read.csv(file='./data_export/std-sterco.tsv', sep='\t', header=TRUE)
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


p_legend <- ggplot(data=df_ste1, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,6000000) + xlim(2,6) + labs(color = 'Metabolite') + theme(legend.position='right')

legend <- cowplot::get_legend(p_legend)



p_std_bil <- ggplot(data=df_bil1, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,7000000) + xlim(2,6)   + ggtitle('Mesobilirubin Standard') + labs(tag='a') + theme(plot.tag = element_text(face='bold', size=8))
p_std_uro <- ggplot(data=df_uro1, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,40000000) + xlim(2,6)   + ggtitle('Urobilin Standard') + labs(tag='b') + theme(plot.tag = element_text(face='bold', size=8))
p_std_ste <- ggplot(data=df_ste1, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,500000000) + xlim(2,6)   + ggtitle('Stercobilin Standard') + labs(tag='c') + theme(plot.tag = element_text(face='bold', size=8))

p_std_bil + p_std_uro + p_std_ste + legend


df_pcw_vc_bil <- process_input('./data_export/vec-VC-bil.tsv', 'vc_bil', c('Retention', 'Stercobilin', 'Mesobilirubin', 'Urobilin'))
df_pcw_vc_bilv <- process_input('./data_export/vec-VC-bilv.tsv', 'vc_bilV', c('Retention', 'Urobilin',  'Mesobilirubin', 'Stercobilin'))
df_pcw_vc_meso <- process_input('./data_export/vec-VC-meso.tsv', 'vc_meso', c('Retention', 'Urobilin',  'Mesobilirubin', 'Stercobilin'))

p_pcw_vc_bil <- ggplot(data=df_pcw_vc_bil, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + scale_y_continuous(limits=c(0,1000000), labels=scientific) + xlim(2,6) + ggtitle('E. coli + pCW-lic + bilirubin') + labs(tag='o') + theme(plot.tag = element_text(face='bold', size=8))

p_pcw_vc_bilv <- ggplot(data=df_pcw_vc_bilv, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,1000000) + xlim(2,6)
p_pcw_vc_meso <- ggplot(data=df_pcw_vc_meso, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + scale_y_continuous(limits=c(0,1000000), labels=scientific) + xlim(2,6)+ ggtitle('E. coli + pCW-lic + mesobilirubin') + labs(tag='p') + theme(plot.tag = element_text(face='bold', size=8))

p_pcw_vc_bil + p_pcw_vc_bilv + p_pcw_vc_meso + legend



df_pcw_gnabilr_bil <- process_input('./data_export/vec-pCWgnabilR-bil.tsv', 'pcwgna_bil', c('Retention', 'Mesobilirubin', 'Stercobilin', 'Urobilin'))
df_pcw_gnabilr_bilv <- process_input('./data_export/vec-pCWgnabilR-bilV.tsv', 'pcwgna_bilv', c('Retention', 'Mesobilirubin', 'Urobilin', 'Stercobilin' ))
df_pcw_gnabilr_meso <- process_input('./data_export/vec-pCWgnabilR-meso.tsv', 'pcwgna_meso', c('Retention', 'Mesobilirubin', 'Stercobilin', 'Urobilin'))
df_pcw_gnabilr_nobil <- process_input('./data_export/vec-pCWgnabilR-nobil.tsv', 'pcwgna_nobil', c('Retention', 'Stercobilin', 'Mesobilirubin', 'Urobilin'))

p_pcw_gnabilr_bil <- ggplot(data=df_pcw_gnabilr_bil, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,500000) + xlim(2,6)   + ggtitle('E. coli + pCW-gna-bilR\n + bilirubin') + labs(tag='d') + theme(plot.tag = element_text(face='bold', size=8))
p_pcw_gnabilr_bilv <- ggplot(data=df_pcw_gnabilr_bilv, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,500000) + xlim(2,6)
p_pcw_gnabilr_meso <- ggplot(data=df_pcw_gnabilr_meso, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,4000000) + xlim(2,6)   + ggtitle('E. coli + pCW-gna-bilR\n + mesobilirubin') + labs(tag='e') + theme(plot.tag = element_text(face='bold', size=8))
p_pcw_gnabilr_nobil <- ggplot(data=df_pcw_gnabilr_nobil, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,600000) + xlim(2,6)   + ggtitle('E. coli + pCW-gna-bilR\n (nothing added)') + labs(tag='f') + theme(plot.tag = element_text(face='bold', size=8))


p_pcw_gnabilr_bil + p_pcw_gnabilr_bilv + p_pcw_gnabilr_meso + p_pcw_gnabilr_nobil + legend


df_pcw_sym_bilv <- process_input('./data_export/vec-pCWsymbilRS-bilV.tsv', 'pcwsym_bilv', c('Retention', 'Mesobilirubin', 'Urobilin', 'Stercobilin'))
df_pcw_sym_meso <- process_input('./data_export/vec-pCWsymbilRS-meso.tsv', 'pcwsym_meso', c('Retention',  'Stercobilin', 'Mesobilirubin', 'Urobilin'))
df_pcw_sym_nobil <- process_input('./data_export/vec-pCWsymbilRS-nobil.tsv', 'pcwsym_nobil', c('Retention', 'Urobilin' , 'Mesobilirubin', 'Stercobilin'))

p_pcw_sym_bilv <- ggplot(data=df_pcw_sym_bilv, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,500000) + xlim(2,6)
p_pcw_sym_meso <- ggplot(data=df_pcw_sym_meso, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + scale_y_continuous(limits=c(0,12000000), labels=scientific) + xlim(2,6)  + ggtitle('E. coli + pCW-sym-bilRS \n + mesobilirubin') + labs(tag='k') + theme(plot.tag = element_text(face='bold', size=8))
p_pcw_sym_nobil <- ggplot(data=df_pcw_sym_nobil, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,500000) + xlim(2,6)  + ggtitle('E. coli + pCW-sym-bilRS \n (nothing added)') + labs(tag='l') + theme(plot.tag = element_text(face='bold', size=8))

p_pcw_sym_bilv + p_pcw_sym_meso + p_pcw_sym_nobil + legend 


df_strain_dif02_bilv <- process_input('./data_export/strain-dif02-bilV.tsv', 'dif02_bilv', c('Retention', 'Mesobilirubin', 'Stercobilin', 'Urobilin'))
df_strain_dif02_meso <- process_input('./data_export/strain-dif02-meso.tsv', 'dif02_meso', c('Retention', 'Mesobilirubin', 'Stercobilin', 'Urobilin'))
df_strain_dif02_nobil <- process_input('./data_export/strain-dif02-nobil.tsv', 'dif02_nobil', c('Retention', 'Mesobilirubin', 'Stercobilin', 'Urobilin'))


p_strain_dif02_bilv <- ggplot(data=df_strain_dif02_bilv, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,500000) + xlim(2,6) 
p_strain_dif02_meso <- ggplot(data=df_strain_dif02_meso, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,300000000) + xlim(2,6) + ggtitle('C. difficile + mesobilirubin') + labs(tag='g') + theme(plot.tag = element_text(face='bold', size=8))
p_strain_dif02_nobil <- ggplot(data=df_strain_dif02_nobil, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,500000) + xlim(2,6)  + ggtitle('C. difficile (nothing added)') + labs(tag='h') + theme(plot.tag = element_text(face='bold', size=8))


p_strain_dif02_bilv + p_strain_dif02_meso + p_strain_dif02_nobil + legend 




df_strain_sym01_meso <- process_input('./data_export/strain-sym01-meso.tsv', 'sym01_meso', c('Retention', 'Mesobilirubin', 'Stercobilin', 'Urobilin'))
df_strain_sym01_nobil <- process_input('./data_export/strain-sym01-nobil.tsv', 'sym01_nobil', c('Retention', 'Stercobilin', 'Mesobilirubin', 'Urobilin'))

p_strain_sym01_meso <- ggplot(data=df_strain_sym01_meso, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,300000000) + xlim(2,6)  + ggtitle('C. symbiosum + mesobilirubin') + labs(tag='i') + theme(plot.tag = element_text(face='bold', size=8))
p_strain_sym01_nobil <- ggplot(data=df_strain_sym01_nobil, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,500000) + xlim(2,6)  + ggtitle('C. symbiosum (nothing added)') + labs(tag='j') + theme(plot.tag = element_text(face='bold', size=8))

p_strain_sym01_meso + p_strain_sym01_nobil + legend 


df_coin_sym01_without <- process_input('./data_export/coin-sym01-withoutcoin.tsv', 'coin_sym01_without', c('Retention', 'Mesobilirubin', 'Stercobilin', 'Urobilin'))
df_coin_sym01_coinject <- process_input('./data_export/coin-sym01-coinject.tsv', 'coin_sym01_with', c('Retention', 'Mesobilirubin', 'Stercobilin', 'Urobilin'))

p_coin_sym01_without <- ggplot(data=df_coin_sym01_without, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,10000000) + xlim(2,6) + ggtitle('C. symbiosum + bilirubin') + labs(tag='m') + theme(plot.tag = element_text(face='bold', size=8))
p_coin_sym01_coinject <- ggplot(data=df_coin_sym01_coinject, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,10000000) + xlim(2,6) + ggtitle('C. symbiosum + bilirubin \n (urobilin coinjection)') + labs(tag='n') + theme(plot.tag = element_text(face='bold', size=8))

p_coin_sym01_without + p_coin_sym01_coinject + legend 


df_exp1_bhi_bil <- process_input('./data_export/exp1_bhi_2.tsv', 'coin_sym01_without', c('Retention', 'Mesobilirubin', 'Stercobilin', 'Urobilin'))
df_exp1_pcwdif_bilRS_bil <- process_input('./data_export/exp1_pcw_difbilRS_1.tsv', 'coin_sym01_with', c('Retention', 'Mesobilirubin', 'Stercobilin', 'Urobilin'))
df_exp1_pcwsym_bilRS_bil <- process_input('./data_export/exp1_pcw_symbilRS_1.tsv', 'coin_sym01_with', c('Retention', 'Mesobilirubin', 'Stercobilin', 'Urobilin'))

p_exp1_bhi_bil <- ggplot(data=df_exp1_bhi_bil, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,500000) + xlim(2,6)
p_exp1_pcwdif_bilRS_bil <- ggplot(data=df_exp1_pcwdif_bilRS_bil, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,10000000) + xlim(2,6)
p_exp1_pcwsym_bilRS_bil <- ggplot(data=df_exp1_pcwsym_bilRS_bil, aes(x=Retention, y=Intensity, group=Label, color=Label)) +
  pub_theme() + scale_color_manual(values=c('#4E79A7', '#bf6750', '#4fb090')) +
  geom_line(size=0.5) + ylim(0,500000) + xlim(2,6)




design <- "
  ABCD
  EFG#
  HIJK
  LMNO
  QR##
"

p_std_bil + p_std_uro + p_std_ste + legend + 
  p_pcw_gnabilr_bil + p_pcw_gnabilr_meso + p_pcw_gnabilr_nobil +
  p_strain_dif02_meso + p_strain_dif02_nobil +
  p_strain_sym01_meso + p_strain_sym01_nobil +
  p_pcw_sym_meso + p_pcw_sym_nobil +
  p_coin_sym01_without + p_coin_sym01_coinject + 
  p_pcw_vc_bil + p_pcw_vc_meso + plot_layout(design=design)

ggsave('Extended-data-Fig7.png', bg='transparent', heigh=290, width=200, units='mm', dpi=330)
  
