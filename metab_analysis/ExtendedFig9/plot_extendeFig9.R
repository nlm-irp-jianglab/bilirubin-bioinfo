library(ggplot2)
#70393.6250 molecular weight, / 640 = MRW of 109.99
mol_gr_bilR_conv <- 70393.6250 / 1000
mol_gr_bilRmut_conv <- 72428.04 / 1000
# Following conversion formulas here: https://www.chem.uci.edu/~dmitryf/manuals/Fundamentals/CD%20practical%20guide.pdf

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



aos_bilr_1mm <- read.csv('./dat-3/07202023 Aoshu WT BilR 2.009uM smoothed and baseline corrected .txt', header=F, sep='\t', skip=3)
colnames(aos_bilr_1mm) <- c('WAVELENGTH',	'CD',	'HT VOLTAGE',	'ABSORBANCE')
aos_bilr_1mm$samp <- 'aos_bilr_1mm'
aos_bilr_1mm$normalized_cd <- (aos_bilr_1mm$CD * 109.99 ) / (1 * 10 * 2.009 * mol_gr_bilR_conv)



aos_mut_1mm <- read.csv('./dat-3/07202023 Aoshu Mutant BilR 2.037uM smoothed and baseline corrected .txt', header=F, sep='\t', skip=3)
colnames(aos_mut_1mm) <- c('WAVELENGTH',	'CD',	'HT VOLTAGE',	'ABSORBANCE')
aos_mut_1mm$samp <- 'aos_mut_1mm'
aos_mut_1mm$normalized_cd <- (aos_mut_1mm$CD * 109.99 ) / (1 * 10 * 2.037 * mol_gr_bilRmut_conv)



dat <- rbind(aos_bilr_1mm, aos_mut_1mm)


ggplot(dat[dat$samp %in% c('aos_bilr_1mm', 'aos_mut_1mm'),], aes(x=WAVELENGTH, y=normalized_cd, col=samp)) + 
  pub_theme() + geom_line(linewidth=1) + ylab(bquote('Molar ellipticity (deg'~cm^2~dmol^-1~')')) + 
  theme(legend.key=element_blank(), legend.justification = c("right", "top"), legend.position=c(0.95, 0.95), legend.background=element_blank()) + 
  scale_color_manual('', values = c('#4E79A7', '#bf6750'), labels=c('BilR', 'BilR(DR166AA)')) + xlab('Wavelength (nm)')

ggsave('Extended_data_fig9.png', width=5, height=3)

