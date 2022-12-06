## @knitr read_kid_data_chk
Lengths_A <- c(53,NA,NA,51,60,64,69,74,78,84,86,96,104,112,118,125,132,135,143)
Lengths_B <- c(51,53.5,56,66,68,72.5,79,80,81,91,96.8,101,110,117)
Lengths_C <- c(52.5,49.5,60,65,67,74,75, 79,83,90,94)
Weights_A <- c(3.520, 3.260,3.290, 3.40,5.12,6.49,8.24,9.71,10.62,12.72,12.87,
               16.2, 19.7, 23.4, 28, 32.7, 39, 45.4, 54)
Weights_B <- c(3.130, 3.19, 4.64,5.73,6.53,7.12,8, 9.18,10.4,12.7,14.3,
               16.1,18.1, 20.7)
Weights_C <- c(3.67, 3.43, 5.78, 6.95,7.83,9.01,10,11.3,12.6,14.3,15.6)

Dates_A <- as.Date(c("2009-05-02","2009-05-05","2009-05-07","2009-05-11","2009-07-07","2009-09-09",
             "2009-11-16","2010-02-16","2010-05-03","2010-11-09",
             "2011-05-04","2012-05-03","2013-05-18","2014-07-30",
             "2015-09-30","2016-06-08","2017-08-21","2018-09-19",
             "2020-01-29"))
Dates_B <- as.Date(c("2012-08-02","2012-08-15","2012-10-31","2012-12-06",
                     "2013-02-18","2013-05-13","2013-09-05","2014-02-17",
                     "2014-07-30","2015-09-30","2016-08-04","2017-08-21",
                     "2018-09-19",
                     "2020-01-29"))

Dates_C <- as.Date(c("2016-02-04","2016-02-18","2016-04-11","2016-06-08",
                     "2016-08-04","2016-11-17",
                     "2017-03-02","2017-08-21","2018-02-20","2019-02-13",
                     "2020-01-29"))
kids_heights_tbl <- tibble(Kid = c(rep("A",length(Lengths_A)), 
                                      rep("B",length(Lengths_B)),
                                      rep("C",length(Lengths_C))), 
                              Heights = c(Lengths_A,Lengths_B,Lengths_C),
                              Weights =c(Weights_A,Weights_B,Weights_C),
                              Dates = c(Dates_A,Dates_B,Dates_C))
kids_heights_tbl_grp <- kids_heights_tbl %>% group_by(Kid) %>% 
  mutate(Birth = min(Dates),Age = Dates - min(Dates))

## @knitr kid_plot_sep
ggplot(kids_heights_tbl_grp,aes(x=Age,y=Heights,group=Kid,col=Kid)) + 
  geom_line(size=1.5) + 
  facet_wrap(~Kid)

## @knitr kid_plot
ggplot(kids_heights_tbl_grp,aes(x=Age,Heights,group=Kid,col=Kid)) + 
  geom_line() 

## @knitr kid_plot_points
ggplot(kids_heights_tbl_grp,aes(x=Age,Heights,group=Kid,col=Kid)) + 
  geom_line() + geom_point()

## @knitr kid_plot_points_wt
ggplot(kids_heights_tbl_grp,aes(x=Age,y=Weights,group=Kid,col=Kid)) + 
  geom_line() + geom_point()

## @knitr kid_tstibble
library(tsbox) # Need to install, allows conversion
kids_heights_ts_tbl = kids_heights_tbl_grp %>% ts_tbl()
ggplot(kids_heights_ts_tbl,aes(x=Age,y=Heights,col=Kid)) + geom_line()

