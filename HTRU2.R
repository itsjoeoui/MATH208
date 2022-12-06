## Data from https://archive.ics.uci.edu/ml/datasets/HTRU2#

## @knitr libraries_chk
# Load packages
library(here)
library(tidyverse)
library(knitr)
library(gridExtra)

## @knitr read_files_chk
# Read in the CSV
HTRU2 <- 
  read_csv(
    here("Data_Analyses_MATH_208_2020/Datasets/HTRU2/HTRU_2.csv"),
           col_names=FALSE)
# Name the variables
names(HTRU2) = c("Mean_IP", "SD_IP", "EK_IP", "SKW_IP",
                 "Mean_DMSNR", "SD_DMSNR", "EK_DMSNR", "SKW_DMSNR", 
                 "Class")



## @knitr what_is_htru2
class(HTRU2)

## @knitr recode_class_chk
HTRU2 <- HTRU2 %>% 
  mutate(Class=factor(ifelse(Class==0,"Negative","Positive")))


## @knitr univariate_hist_meanip_chk
ggplot(HTRU2, aes(x=Mean_IP)) + 
  geom_histogram(bins=25,col="black",fill="lightblue")

## @knitr univariate_hist_sdip_chk
ggplot(HTRU2, aes(x=SD_IP)) + 
  geom_histogram(bins=25,col="black",fill="lightblue")


## @knitr univariate_bins_meanip_chk
p1 = ggplot(HTRU2, aes(x=Mean_IP)) + 
  geom_histogram(bins=10,col="black",fill="lightblue") + 
  ggtitle("Mean IP: 10 bins")
p2 = ggplot(HTRU2, aes(x=Mean_IP)) + 
  geom_histogram(bins=40,col="black",fill="lightblue") + 
  ggtitle("Mean IP: 40 bins")
grid.arrange(p1,p2)

## @knitr byclass_hist_meanip_chk
ggplot(HTRU2, aes(x=Mean_IP,group=Class,fill=Class)) + 
  geom_histogram(bins=25,col="black") + 
    facet_wrap(~Class)

## @knitr byclass2_hist_meanip_chk
ggplot(HTRU2, aes(x=Mean_IP,group=Class,fill=Class)) + 
  geom_histogram(bins=25,col="black") + 
  facet_grid(rows=vars(Class))


## @knitr byclass3_hist_meanip_chk
ggplot(HTRU2, aes(x=Mean_IP,group=Class,fill=Class)) + 
  geom_histogram(bins=25,col="black") 

## @knitr byclass4_hist_meanip_chk
ggplot(HTRU2, aes(x=Mean_IP,group=Class,fill=Class)) + 
  geom_histogram(bins=25,col="black",position="dodge") 

## @knitr probhist_meanip_chk
ggplot(HTRU2,aes(x=Mean_IP,group=Class,fill=Class)) + 
  geom_histogram(aes(y=..density..),bins=25,col="black")

## @knitr probhist_meanip_dodge_chk
ggplot(HTRU2,aes(x=Mean_IP,group=Class,fill=Class)) + 
  geom_histogram(aes(y=..density..),bins=25,col="black",
                 position="dodge")

## @knitr boxplot_meanip_chk
ggplot(HTRU2,aes(x=NULL,y=Mean_IP)) + geom_boxplot() + 
  xlab("") + ylab("Mean IP")


## @knitr boxplot_meanip_whisk_chk
ggplot(HTRU2,aes(x=NULL,y=Mean_IP)) + 
  stat_boxplot(geom="errorbar",width=0.25) + geom_boxplot() + 
  xlab("") + ylab("Mean IP")


## @knitr boxplot_meanip_class_chk
ggplot(HTRU2,aes(x=Class,y=Mean_IP)) + 
  stat_boxplot(geom="errorbar",width=0.25) + geom_boxplot() +
  ylab("Mean IP")

## @knitr boxplot_meanip_class_fill_chk
ggplot(HTRU2,aes(x=Class,y=Mean_IP,fill=Class)) + 
  stat_boxplot(geom="errorbar",width=0.25) + geom_boxplot() +
  ylab("Mean IP")

## @knitr density_meanip_chk
ggplot(HTRU2,aes(x=Mean_IP,fill=Class)) + 
  geom_density()+ xlab("Mean IP")



## @knitr density_meanip_lines_chk
ggplot(HTRU2,aes(x=Mean_IP)) + 
  geom_density(size=1.5) + xlab("Mean IP")

## @knitr density_meanip_lines_class_chk
ggplot(HTRU2,aes(x=Mean_IP,group=Class)) + 
  geom_density(size=1.5) + xlab("Mean IP")

## @knitr density_meanip_lines_class2_chk
ggplot(HTRU2,aes(x=Mean_IP,col=Class)) + 
  geom_density(size=1.5) + xlab("Mean IP")


## @knitr density_meanip_lines_class3_chk
ggplot(HTRU2,aes(x=Mean_IP,group=Class)) + 
  geom_histogram(aes(y=..density..,fill=Class),
                 col="black",alpha=0.4,bins=60)+
  geom_density(size=1.5,adjust=0.25) + xlab("Mean IP")


## @knitr raincloud_meanip_chk
library(cowplot)
source(here("Documents/rain_cloud.R"))
ggplot(HTRU2,aes(x=Class,y=Mean_IP,fill=Class,col=Class)) + 
  geom_flat_violin(position=position_nudge(x=0.2,y=0),adjust=1) + 
  #note that here we need to set the x-variable to a numeric variable 
  #and  bump it to get the boxplots to line up with the rainclouds. 
  geom_boxplot(aes(x = as.numeric(Class)+0.25, y = Mean_IP),
               outlier.shape = NA, alpha = 0.3, width = .1, 
               colour = "BLACK") +
  geom_point(position=position_jitter(width=0.15),size=0.25) + 
  xlab("Class") + ylab("Mean IP") + 
  guides(fill = FALSE)+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")


## @knitr bad_attempt_chk
ggplot(HTRU2,aes(x=Class,y=Mean_IP)) + geom_point() + 
  xlab("Mean IP")


## @knitr scatterplot_first_chk
ggplot(HTRU2,aes(x=Mean_IP,y=Mean_DMSNR)) + geom_point() + 
  xlab("Mean IP") + ylab("Mean DMSNR")


## @knitr scatterplot_first_class_chk
ggplot(HTRU2,aes(x=Mean_IP,y=Mean_DMSNR,col=Class)) + 
  geom_point() + 
  labs(x="Mean IP", y="Mean DMSNR", 
       title="Mean IP vs. Mean DMSNR")


## @knitr scatterplot_first_class_flip_chk
ggplot(HTRU2,aes(x=Mean_IP,y=Mean_DMSNR,col=Class)) + 
  geom_point() + labs(x="Mean IP",y="Mean DMSNR")+   coord_flip()




## @knitr scatterplot_first_sd_chk
ggplot(HTRU2,aes(x=Mean_IP,y=SD_IP)) + geom_point() + 
  labs(x="Mean IP",y="SD IP")

## @knitr scatterplot_first_sd_skew_chk
ggplot(HTRU2,aes(x=Mean_IP,y=SD_IP,col=SKW_IP)) + geom_point() + 
  labs(x="Mean IP",y="SD IP")

## @knitr scatterplot_class_sd_skew_chk
ggplot(HTRU2,aes(x=Mean_IP,y=SD_IP,col=SKW_IP, shape=Class)) +
  geom_point() + labs(x="Mean IP",y="SD IP")

## @knitr scatterplot_class_mean_skew_chk
ggplot(HTRU2,aes(x=Mean_IP,y=SKW_IP, col=Class)) +
  geom_point() + labs(x="Mean IP",y="Skew IP")

## @knitr 2d_dens_mean_sd_chk
ggplot(HTRU2,aes(x=Mean_IP,y=SKW_IP)) + geom_bin2d(bins=100) + 
         labs(x="Mean IP",y="Skew IP")

## @knitr 2d_dens_mean_sd_2_chk
ggplot(HTRU2,aes(x=Mean_IP,y=SD_IP)) + geom_bin2d(bins=100) +
  scale_fill_continuous(type = "viridis") +
  labs(x="Mean IP",y="SD IP")

## @knitr 2d_dens_mean_sd_3_chk
ggplot(HTRU2,aes(x=Mean_IP,y=SD_IP)) + 
  geom_density_2d(col="red") + labs(x="Mean IP",y="SD IP") + 
  ylim(c(0,100)) + 
  xlim(c(0,200)) 


## @knitr 2d_dens_mean_sd_4_chk
ggplot(HTRU2,aes(x=Mean_IP,y=SD_IP)) + geom_point(size=0.1) + 
  geom_density_2d(col="red") + labs(x="Mean IP",y="SD IP") + 
  ylim(c(0,100)) + 
  xlim(c(0,200)) 

## @knitr 2d_dens_mean_sd_5_chk
ggplot(HTRU2,aes(x=Mean_IP,y=SD_IP)) +  
  geom_density_2d(col="red",bins=30) +
  labs(x="Mean IP",y="SD IP")+ 
  ylim(c(0,100)) + 
  xlim(c(0,200)) 



## @knitr 2d_dens_mean_sd_6_chk
ggplot(HTRU2,aes(x=Mean_IP,y=SD_IP)) +
  stat_density_2d(bins=50,aes(fill = ..level..),
                  geom = "polygon") + 
  labs(x="Mean IP",y="SD IP")+ 
  ylim(c(0,100)) + 
  xlim(c(0,200)) 


## @knitr 2d_dens_mean_sd_7_chk
ggplot(HTRU2,aes(x=Mean_IP,y=SD_IP)) + geom_point(col="white")+
  stat_density_2d(bins=50,alpha=0.7,aes(fill = ..density..), 
                  geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))+ 
  theme(legend.position='none')+ 
  labs(x="Mean IP",y="SD IP")+ ylim(c(0,100)) +  xlim(c(0,200)) 



## @knitr 2d_dens_mean_sd_8_chk
ggplot(HTRU2,aes(x=Mean_IP,y=SD_IP)) +
  stat_density_2d(bins=5000,alpha=1,
                  aes(fill = ..density..), 
                  geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))+ 
  theme(legend.position='none')+ 
  labs(x="Mean IP",y="SD IP")


## @knitr summaries_mean_ip 
HTRU2 %>% summarise(Avg = mean(Mean_IP), 
                    Med = median(Mean_IP),
                    '25%ile' = quantile(Mean_IP,0.25),
                    '75%ile' = quantile(Mean_IP,0.75),
                    StD = sd(Mean_IP),
                    IQR = IQR(Mean_IP)
                    )


## @knitr summaries_mean_ip_class
HTRU2 %>% group_by(Class) %>% 
  summarise(Avg = mean(Mean_IP),
            Med = median(Mean_IP),
            Q25 = quantile(Mean_IP,0.25),
            Q75 = quantile(Mean_IP,0.75),
            StD = sd(Mean_IP),
            IQR = IQR(Mean_IP))

## @knitr summaries_each_class
HTRU2 %>% group_by(Class) %>% select(Class,Mean_IP,Mean_DMSNR) %>%
  summarise_all(list(Avg=mean,Med=median))

## @knitr summaries_each_class_longer
HTRU2 %>% group_by(Class) %>% select(Class,Mean_IP,Mean_DMSNR) %>%
  summarise_all(list(Avg=mean,Med=median)) %>% 
  pivot_longer(cols=starts_with("Mean"),names_to = "Measure") %>% 
  arrange(desc(Measure)) 


## @knitr summaries_each_class_wider
HTRU2 %>% group_by(Class) %>% select(Class,Mean_IP,Mean_DMSNR) %>%
  summarise_all(list(Avg=mean,Med=median,
                     Q25 = ~quantile(.,probs=c(0.25)),
                     Q75 =~quantile(.,probs=c(0.75)))) %>% 
  pivot_longer(cols=starts_with("Mean"),names_to = "Measure") %>%
  pivot_wider(id_cols=Measure,names_from=Class) %>% 
  arrange(desc(Measure)) 


## @knitr cor_dmnsr
HTRU2 %>% group_by(Class) %>% 
  summarise(Cor_MeanIP_Mean_DMSNR = 
                      cor(Mean_IP,Mean_DMSNR))

## @knitr scatterplot_facet_chk
ggplot(HTRU2,aes(x=Mean_IP,y=Mean_DMSNR,col=Class)) + 
  geom_point() + facet_wrap(~Class) + 
  labs(x="Mean IP", y="Mean DMSNR", 
       title="Mean IP vs. Mean DMSNR") + 
  theme(legend.position = "none") + 
  geom_smooth(method="lm",col="black")


## @knitr scatterplot_facet2_chk
HTRU2 <- HTRU2 %>% mutate(Neg_MDMSNR=-Mean_DMSNR)
HTRU2 %>% group_by(Class) %>% summarise(Cor2 = 
                                          cor(Mean_IP,Neg_MDMSNR))

## @knitr scatterplot_facet3_chk
ggplot(HTRU2,aes(x=Mean_IP,y=Neg_MDMSNR,col=Class)) + 
  geom_point() + facet_wrap(~Class) + 
  labs(x="Mean IP", y="Mean DMSNR", 
       title="Mean IP vs. Mean DMSNR") + 
  theme(legend.position = "none") + 
  geom_smooth(method="lm",col="black")


