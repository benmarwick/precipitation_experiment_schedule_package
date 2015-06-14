library(dplyr)
library(tidyr)
library(ggplot2)

sitenames <- c("Argentina","Cardoso","Colombia",
               "FrenchGuiana","Macae","PuertoRico", "CostaRica")

sitedata <- data_frame(sites = sitenames ) %>%
  mutate(path = paste0("../Experimental.Schedules/",sites),
         file = list.files(path, pattern = "Ppt*", full.names = TRUE)) %>% 
  group_by(sites) %>% 
  do(read.csv(.$file) %>% 
       mutate(day = 1:60) %>% 
       gather(year, ppt, -day))


pdf("sitehists.pdf")
for (i in sitenames) {
  p <- sitedata %>% 
    filter(sites == i) %>% 
    ggplot(aes(x = ppt)) + 
    geom_histogram(binwidth = 3) + 
    facet_wrap( ~ year) +
    ggtitle(i)
  print(p) 
}
dev.off()

selected <- data_frame(sites = c("Argentina","Cardoso","Colombia",
                         "FrenchGuiana","Macae","PuertoRico", "CostaRica"),
                       year = c("Year24", "year2008", "X2004", "yr2006","X2006","X2008","X2001"))

sitedata %>% 
  semi_join(selected) %>% 
  ggplot(aes(x = ppt)) + 
  geom_histogram(binwidth = 3) + 
  facet_wrap( ~ sites)

ggsave("allsites.png")

for (i in sitenames) {
  sitedata %>% 
    semi_join(selected) %>% 
    filter(sites == i) %>% 
    ggplot(aes(x = ppt)) + 
    geom_histogram(binwidth = 3) + 
    ylim(0,50) + xlim(0,100) +
    ggtitle(i)
  ggsave(paste0(i, ".png"))
}

