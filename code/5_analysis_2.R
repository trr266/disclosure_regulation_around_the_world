
###############################################
# This file investigates the development of balance sheet and income statement disclosures over time and across countries 
###############################################

library(dplyr)
library(ggplot2)
library(ggpubr)
library(grDevices)

affected_firms <- read.csv("~/TRR/A01/Repositories/disclosure_regulation_around_the_world/data/disclosure_regulation/affected_firms.csv")


affected_firms$legal_form[affected_firms$legal_form %in% "Limited liability enterprise"]   <- "Limited liability enterprises"
affected_firms$legal_form[affected_firms$legal_form %in% "Partnership, co-operatives, associations, etc."]    <- "Partnerships, co-operatives, associations, etc."
affected_firms$legal_form[affected_firms$legal_form %in% "Sole proprietorship"]                <- "Sole proprietorships"






######## Austria

df <- affected_firms %>% 
  filter(iso2 %in% "AT") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1



df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

at_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/at_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")




######## Belgium

df <- affected_firms %>% 
  filter(iso2 %in% "BE") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1



df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

be_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/be_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")


######## Czech Republic

df <- affected_firms %>% 
  filter(iso2 %in% "CZ") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1



df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

cz_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/cz_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")


######## Denmark

df <- affected_firms %>% 
  filter(iso2 %in% "DK") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1



df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

dk_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/dk_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")


######## Estonia

df <- affected_firms %>% 
  filter(iso2 %in% "EE") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1



df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

ee_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/ee_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")


######## France

df <- affected_firms %>% 
  filter(iso2 %in% "FR") %>%
  filter(year < 2021)
df <- data.frame(df)


df$bs_share[df$bs_share > 1] <- 1



df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

fr_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/fr_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")


######## Germany

df <- affected_firms %>% 
  filter(iso2 %in% "DE") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1



df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

de_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/de_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")


######## Greece

df <- affected_firms %>% 
  filter(iso2 %in% "GR") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1



df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

gr_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/gr_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")

######## Italy

df <- affected_firms %>% 
  filter(iso2 %in% "IT") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1



df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

it_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/it_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")

######## Lithuania

df <- affected_firms %>% 
  filter(iso2 %in% "LT") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1



df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

lt_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/lt_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")

######## Netherlands

df <- affected_firms %>% 
  filter(iso2 %in% "NL") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1



df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

nl_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/nl_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")

######## Poland

df <- affected_firms %>% 
  filter(iso2 %in% "PL") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1



df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

pl_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/pl_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")

######## Portugal

df <- affected_firms %>% 
  filter(iso2 %in% "PT") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1
df$is_share[df$is_share > 1] <- 1


df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

pt_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/pt_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")

######## Romania

df <- affected_firms %>% 
  filter(iso2 %in% "RO") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1



df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

ro_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/ro_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")

######## Slovakia

df <- affected_firms %>% 
  filter(iso2 %in% "SK") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1



df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

sk_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/sk_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")

######## Slovenia

df <- affected_firms %>% 
  filter(iso2 %in% "SI") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1



df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

si_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/si_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")

######## Spain

df <- affected_firms %>% 
  filter(iso2 %in% "ES") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1
df$bs_share[is.na(df$bs_share)] <- 0
df$is_share[is.na(df$is_share)] <- 0

df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

es_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/es_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")





######## Sweden

df <- affected_firms %>% 
  filter(iso2 %in% "SE") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1



df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

se_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/se_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")





######## Slovakia

df <- affected_firms %>% 
  filter(iso2 %in% "SK") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1



df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

sk_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/sk_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")





######## Slovenia

df <- affected_firms %>% 
  filter(iso2 %in% "SI") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1



df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

si_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/si_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")









######## Cyprus

df <- affected_firms %>% 
  filter(iso2 %in% "CY") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1



df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

cy_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/cy_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")





######## Hungary

df <- affected_firms %>% 
  filter(iso2 %in% "HU") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1



df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

hu_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/hu_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")






######## Finland

df <- affected_firms %>% 
  filter(iso2 %in% "FI") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1



df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

fi_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/fi_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")






######## Ireland

df <- affected_firms %>% 
  filter(iso2 %in% "IE") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1


df$bs_share[df$bs_share > 1] <- 1
df$bs_share[is.na(df$bs_share)] <- 0
df$is_share[is.na(df$is_share)] <- 0


df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

ie_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/ie_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")








######## Croatia

df <- affected_firms %>% 
  filter(iso2 %in% "HR") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1


df$bs_share[df$bs_share > 1] <- 1
df$bs_share[is.na(df$bs_share)] <- 0
df$is_share[is.na(df$is_share)] <- 0


df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

hr_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/hr_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")







######## Latvia

df <- affected_firms %>% 
  filter(iso2 %in% "LV") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1


df$bs_share[df$bs_share > 1] <- 1
df$bs_share[is.na(df$bs_share)] <- 0
df$is_share[is.na(df$is_share)] <- 0


df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

lv_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/lv_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")







######## Malta

df <- affected_firms %>% 
  filter(iso2 %in% "MT") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1


df$bs_share[df$bs_share > 1] <- 1
df$bs_share[is.na(df$bs_share)] <- 0
df$is_share[is.na(df$is_share)] <- 0


df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

mt_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/mt_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")


######## Luxemburg

df <- affected_firms %>% 
  filter(iso2 %in% "LU") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1


df$bs_share[df$bs_share > 1] <- 1
df$bs_share[is.na(df$bs_share)] <- 0
df$is_share[is.na(df$is_share)] <- 0


df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

lu_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/lu_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")





######## Bulgaria

df <- affected_firms %>% 
  filter(iso2 %in% "BG") %>%
  filter(year < 2021)
df <- data.frame(df)
df$bs_share[df$bs_share > 1] <- 1


df$bs_share[df$bs_share > 1] <- 1
df$bs_share[is.na(df$bs_share)] <- 0
df$is_share[is.na(df$is_share)] <- 0


df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Limited liability enterprises"]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Partnerships, co-operatives, associations, etc."]  / sum(df$number_firms_eurostat[df$year == 2020])
df$number_firms_eurostat[df$year == 2020 & df$legal_form %in% "Sole proprietorships"]  / sum(df$number_firms_eurostat[df$year == 2020])




df$year <- as.character(df$year)  
names(df)[names(df) == 'legal_form'] <- 'Legal Form'
bs_plot <- ggplot(df, aes(x=year, y=bs_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing a balance sheet") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))


is_plot <- ggplot(df, aes(x=year, y=is_share, group=`Legal Form`)) +
  geom_point(aes(color = `Legal Form`, shape = `Legal Form`)) + 
  ylim(0, 1) +
  xlab("Year") +
  ylab("Share of fimrs disclosing an income statement") +
  scale_x_discrete(breaks = c("2010", "2012", "2014", "2016", "2018", "2020"))+ 
  theme(axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7))

bg_plot <- ggarrange(bs_plot, 
                     is_plot,
                     nrow = 2,
                     legend = "right",
                     common.legend = TRUE) 


ggsave("output/bg_plot.jpeg",
       width = 16,
       height = 11,
       unit = "cm")

