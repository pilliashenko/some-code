# this is a code for sex ratio ages 0-4

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

fm <- read.csv("mf_0_4.csv", sep=";", stringsAsFactors = FALSE)
fm2 <- gather(fm, year, fmratio, X1960:X2017)
fm2$year <- substr(fm2$year, 2,5)
fm2$year <- as.numeric(fm2$year)
fm2$fmratio <- as.numeric(fm2$fmratio)



# now we start to make graphs
# for simplicity, I create a subset of the data for each graph



########################### 
###########################


fm_world <- fm2 %>% filter(ctry== "1W"|
                             ctry== "XD"|
                             ctry== "XT"|
                             ctry== "XP"|
                             ctry== "XN"|
                             ctry== "XM")


fm_regions <- fm2 %>% filter(
  ctry == "ZJ"|
    ctry == "XU"|
    ctry == "EU"|
    ctry == "ZQ"|
    ctry == "ZG"|
    ctry == "8S"|
    ctry == "Z4")



p1 <- ggplot(fm_world, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Income regions") +
  labs(y = "Male to female, 0-4 years", x = "", colour = "") +
  theme(legend.position="bottom")


p2 <- ggplot(fm_regions, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Geographical regions") +
  labs(y = "Male to female, 0-4 years", x = "", colour = "") +
  theme(legend.position="bottom")


grid.arrange(p1, p2, nrow = 1)



########################### 
###########################



fm_Anglo <- fm2 %>% filter(
  ctry == "AU"|
    ctry == "CA"|
    ctry == "GB"|
    ctry == "US")


fm_GermAndNordic <- fm2 %>% filter(
  ctry == "DE"|
    ctry == "NL"|
    ctry == "SE")



p3 <- ggplot(fm_Anglo, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Anglo") +
  labs(y = "Male to female, 0-4 years", x = "", colour = "") +
  theme(legend.position="bottom")


p4 <- ggplot(fm_GermAndNordic, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Germanic and Nordic") +
  labs(y = "Male to female, 0-4 years", x = "", colour = "") +
  theme(legend.position="bottom")


grid.arrange(p3, p4, nrow = 1)


########################### 
###########################



fm_South_Asia_1 <- fm2 %>% filter(
  ctry == "BD"|
    ctry == "IN"|
    ctry == "ID"|
    ctry == "MY"|
    ctry == "NP")



fm_South_Asia_2 <- fm2 %>% filter(
  ctry == "PK"|
    ctry == "PH"|
    ctry == "LK"|
    ctry == "TH")




p5 <- ggplot(fm_South_Asia_1, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Southern Asia") +
  labs(y = "Male to female, 0-4 years", x = "", colour = "") +
  theme(legend.position="bottom")


p6 <- ggplot(fm_South_Asia_2, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Southern Asia") +
  labs(y = "Male to female, 0-4 years", x = "", colour = "") +
  theme(legend.position="bottom")


grid.arrange(p5, p6, nrow = 1)



########################### 
###########################



fm_Conf_Asia <- fm2 %>% filter(
  ctry == "CN"|
    ctry == "JP"|
    ctry == "KR"|
    ctry == "VN")


fm_Lat_Eur <- fm2 %>% filter(
  ctry == "BE"|
    ctry == "FR"|
    ctry == "IT"|
    ctry == "PT"|
    ctry == "ES")


p7 <- ggplot(fm_Conf_Asia, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Confucian Asia") +
  labs(y = "Male to female, 0-4 years", x = "", colour = "") +
  theme(legend.position="bottom")


p8 <- ggplot(fm_Lat_Eur, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Latin Europe") +
  labs(y = "Male to female, 0-4 years", x = "", colour = "") +
  theme(legend.position="bottom")


grid.arrange(p7, p8, nrow = 1)



########################### 
###########################



fm_East_Eur_1 <- fm2 %>% filter(
  ctry == "CZ"|
    ctry == "GR"|
    ctry == "KZ"|
    ctry == "PL")



fm_East_Eur_2 <- fm2 %>% filter(
  ctry == "RO"|
    ctry == "RU"|
    ctry == "UA")



p9 <- ggplot(fm_East_Eur_1, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Eastern Europe") +
  labs(y = "Male to female, 0-4 years", x = "", colour = "") +
  theme(legend.position="bottom")


p10 <- ggplot(fm_East_Eur_2, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Eastern Europe") +
  labs(y = "Male to female, 0-4 years", x = "", colour = "") +
  theme(legend.position="bottom")


grid.arrange(p9, p10, nrow = 1)





########################### 
###########################



fm_Middle_East_1 <- fm2 %>% filter(
  ctry == "EG"|
    ctry == "IR"|
    ctry == "IQ"|
    ctry == "MA")




fm_Middle_East_2 <- fm2 %>% filter(
  ctry == "SA"|
    ctry == "SN"|
    ctry == "TR")



p11 <- ggplot(fm_Middle_East_1, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Middle East") +
  labs(y = "Male to female, 0-4 years", x = "", colour = "") +
  theme(legend.position="bottom")


p12 <- ggplot(fm_Middle_East_2, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Middle East") +
  labs(y = "Male to female, 0-4 years", x = "", colour = "") +
  theme(legend.position="bottom")


grid.arrange(p11, p12, nrow = 1)





########################### 
###########################



fm_Africa_1 <- fm2 %>% filter(
  ctry == "AO"|
    ctry == "BF"|
    ctry == "CM"|
    ctry == "GH")



fm_Africa_2 <- fm2 %>% filter(
  ctry == "KE"|
    ctry == "MW"|
    ctry == "MZ"|
    ctry == "NG")



fm_Africa_3 <- fm2 %>% filter(
  ctry == "ZA"|
    ctry == "TZ"|
    ctry == "UG"|
    ctry == "ZM"|
    ctry == "ZW")




p13 <- ggplot(fm_Africa_1, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Sub-Sahara Africa") +
  labs(y = "Male to female, 0-4 years", x = "", colour = "") +
  theme(legend.position="bottom")


p14 <- ggplot(fm_Africa_2, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Sub-Sahara Africa") +
  labs(y = "Male to female, 0-4 years", x = "", colour = "") +
  theme(legend.position="bottom")



p15 <- ggplot(fm_Africa_3, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Sub-Sahara Africa") +
  labs(y = "Male to female, 0-4 years", x = "", colour = "") +
  theme(legend.position="bottom")


grid.arrange(p13, p14, p15, nrow = 1)




########################### 
###########################



fm_Lat_Am_1 <- fm2 %>% filter(
  ctry == "AR"|
    ctry == "BO"|
    ctry == "BR"|
    ctry == "CO")




fm_Lat_Am_2 <- fm2 %>% filter(
  ctry == "EC"|
    ctry == "ET"|
    ctry == "GT"|
    ctry == "MX")




fm_Lat_Am_3 <- fm2 %>% filter(
  ctry == "VE"|
    ctry == "CL"|
    ctry == "DO"|
    ctry == "PE")





p16 <- ggplot(fm_Lat_Am_1, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Latin America") +
  labs(y = "Male to female, 0-4 years", x = "", colour = "") +
  theme(legend.position="bottom")


p17 <- ggplot(fm_Lat_Am_2, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Latin America") +
  labs(y = "Male to female, 0-4 years", x = "", colour = "") +
  theme(legend.position="bottom")



p18 <- ggplot(fm_Lat_Am_3, aes(x=year, y=fmratio, colour = factor(country))) +
  geom_line() +
  ggtitle("Latin America") +
  labs(y = "Male to female, 0-4 years", x = "", colour = "") +
  theme(legend.position="bottom")


grid.arrange(p16, p17, p18, nrow = 1)
