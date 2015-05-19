library(dplyr)
library(tidyr)

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
    ggplot(aes(x = ppt)) + geom_histogram(binwidth = 3) + facet_wrap( ~ year)
  print(p) 
}
dev.off()
