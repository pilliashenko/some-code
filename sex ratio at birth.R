# this is a code for sex ratio at birth

# we need two packagesm, lets check if you have them, if not they will be installed
packages <- c("tidyverse", "gridExtra")

# the function will install missing packages if there are some
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}


# uploading all libraries at once
lapply(packages,library,character.only=TRUE)


# we do not need this object anymore
rm(packages)




# uploading and wrangling the data (sorry for messy approach)
# the corresponding file "mf_0_4.csv" should be your working derictory
# subsets and variable have prefixs fm (female male), sorry for confusion but this is just a choise of letters, in all cases the ratios are males to females  

fmb <- read.csv("mf.csv", sep=";", stringsAsFactors = FALSE)
fmb2 <- gather(fmb, year, fmratio, X1962:X2017)
fmb2$year <- substr(fmb2$year, 2,5)
fmb2$year <- as.numeric(fmb2$year)
fmb2$fmratio <- as.numeric(fmb2$fmratio)



# now we start to make graphs
# for simplicity, I create a subset of the data for each graph



########################### 
###########################


fmb_world <- fmb2 %>% filter(ctry== "1W"|
                             ctry== "XD"|
                             ctry== "XT"|
                             ctry== "XP"|
                             ctry== "XN"|
                             ctry== "XM")


fmb_regions <- fmb2 %>% filter(
  ctry == "ZJ"|
    ctry == "XU"|
    ctry == "EU"|
    ctry == "ZQ"|
    ctry == "ZG"|
    ctry == "8S"|
    ctry == "Z4")



pb1 <- ggplot(fmb_world, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Income regions") +
  labs(y = "Male births per female births", x = "", colour = "") +
  theme(legend.position="bottom")


pb2 <- ggplot(fmb_regions, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Geographical regions") +
  labs(y = "Male births per female births", x = "", colour = "") +
  theme(legend.position="bottom")


grid.arrange(pb1, pb2, nrow = 1)



########################### 
###########################



fmb_Anglo <- fmb2 %>% filter(
  ctry == "AU"|
    ctry == "CA"|
    ctry == "GB"|
    ctry == "US")


fmb_GermAndNordic <- fmb2 %>% filter(
  ctry == "DE"|
    ctry == "NL"|
    ctry == "SE")



pb3 <- ggplot(fmb_Anglo, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Anglo") +
  labs(y = "Male births per female births", x = "", colour = "") +
  theme(legend.position="bottom")


pb4 <- ggplot(fmb_GermAndNordic, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Germanic and Nordic") +
  labs(y = "Male births per female births", x = "", colour = "") +
  theme(legend.position="bottom")


grid.arrange(pb3, pb4, nrow = 1)


########################### 
###########################



fmb_South_Asia_1 <- fmb2 %>% filter(
  ctry == "BD"|
    ctry == "IN"|
    ctry == "ID"|
    ctry == "MY"|
    ctry == "NP")
    


fmb_South_Asia_2 <- fmb2 %>% filter(
  ctry == "PK"|
    ctry == "PH"|
    ctry == "LK"|
    ctry == "TH")
    



pb5 <- ggplot(fmb_South_Asia_1, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Southern Asia") +
  labs(y = "Male births per female births", x = "", colour = "") +
  theme(legend.position="bottom")


pb6 <- ggplot(fmb_South_Asia_2, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Southern Asia") +
  labs(y = "Male births per female births", x = "", colour = "") +
  theme(legend.position="bottom")


grid.arrange(pb5, pb6, nrow = 1)



########################### 
###########################



fmb_Conf_Asia <- fmb2 %>% filter(
  ctry == "CN"|
    ctry == "JP"|
    ctry == "KR"|
    ctry == "VN")
    

fmb_Lat_Eur <- fmb2 %>% filter(
  ctry == "BE"|
    ctry == "FR"|
    ctry == "IT"|
    ctry == "PT"|
    ctry == "ES")
    

pb7 <- ggplot(fmb_Conf_Asia, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Confucian Asia") +
  labs(y = "Male births per female births", x = "", colour = "") +
  theme(legend.position="bottom")


pb8 <- ggplot(fmb_Lat_Eur, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Latin Europe") +
  labs(y = "Male births per female births", x = "", colour = "") +
  theme(legend.position="bottom")


grid.arrange(pb7, pb8, nrow = 1)



########################### 
###########################



fmb_East_Eur_1 <- fmb2 %>% filter(
  ctry == "CZ"|
    ctry == "GR"|
    ctry == "KZ"|
    ctry == "PL")
    


fmb_East_Eur_2 <- fmb2 %>% filter(
  ctry == "RO"|
    ctry == "RU"|
    ctry == "UA")
    


pb9 <- ggplot(fmb_East_Eur_1, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Eastern Europe") +
  labs(y = "Male births per female births", x = "", colour = "") +
  theme(legend.position="bottom")


pb10 <- ggplot(fmb_East_Eur_2, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Eastern Europe") +
  labs(y = "Male births per female births", x = "", colour = "") +
  theme(legend.position="bottom")


grid.arrange(pb9, pb10, nrow = 1)





########################### 
###########################



fmb_Middle_East_1 <- fmb2 %>% filter(
  ctry == "EG"|
    ctry == "IR"|
    ctry == "IQ"|
    ctry == "MA")
    



fmb_Middle_East_2 <- fmb2 %>% filter(
  ctry == "SA"|
    ctry == "SN"|
    ctry == "TR")



pb11 <- ggplot(fmb_Middle_East_1, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Middle East") +
  labs(y = "Male births per female births", x = "", colour = "") +
  theme(legend.position="bottom")


pb12 <- ggplot(fmb_Middle_East_2, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Middle East") +
  labs(y = "Male births per female births", x = "", colour = "") +
  theme(legend.position="bottom")


grid.arrange(pb11, pb12, nrow = 1)





########################### 
###########################



fmb_Africa_1 <- fmb2 %>% filter(
  ctry == "AO"|
    ctry == "BF"|
    ctry == "CM"|
    ctry == "GH")
    


fmb_Africa_2 <- fmb2 %>% filter(
  ctry == "KE"|
    ctry == "MW"|
    ctry == "MZ"|
    ctry == "NG")
    


fmb_Africa_3 <- fmb2 %>% filter(
  ctry == "ZA"|
    ctry == "TZ"|
    ctry == "UG"|
    ctry == "ZM"|
    ctry == "ZW")
    



pb13 <- ggplot(fmb_Africa_1, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Sub-Sahara Africa") +
  labs(y = "Male births per female births", x = "", colour = "") +
  theme(legend.position="bottom")


pb14 <- ggplot(fmb_Africa_2, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Sub-Sahara Africa") +
  labs(y = "Male births per female births", x = "", colour = "") +
  theme(legend.position="bottom")



pb15 <- ggplot(fmb_Africa_3, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Sub-Sahara Africa") +
  labs(y = "Male births per female births", x = "", colour = "") +
  theme(legend.position="bottom")


grid.arrange(pb13, pb14, pb15, nrow = 1)




########################### 
###########################



fmb_Lat_Am_1 <- fmb2 %>% filter(
  ctry == "AR"|
    ctry == "BO"|
    ctry == "BR"|
    ctry == "CO")
    



fmb_Lat_Am_2 <- fmb2 %>% filter(
  ctry == "EC"|
    ctry == "ET"|
    ctry == "GT"|
    ctry == "MX")
    



fmb_Lat_Am_3 <- fmb2 %>% filter(
  ctry == "VE"|
    ctry == "CL"|
    ctry == "DO"|
    ctry == "PE")
    




pb16 <- ggplot(fmb_Lat_Am_1, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Latin America") +
  labs(y = "Male births per female births", x = "", colour = "") +
  theme(legend.position="bottom")


pb17 <- ggplot(fmb_Lat_Am_2, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Latin America") +
  labs(y = "Male births per female births", x = "", colour = "") +
  theme(legend.position="bottom")



pb18 <- ggplot(fmb_Lat_Am_3, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Latin America") +
  labs(y = "Male births per female births", x = "", colour = "") +
  theme(legend.position="bottom")


grid.arrange(pb16, pb17, pb18, nrow = 1)
