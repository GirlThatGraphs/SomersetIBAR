
# Run Setup script ----------------------------------------------------------
source("SetupNew.R")

# GraphicExport ----------------------------------------------------------
# PNG
PNGWidth <- 8
PNGHeight <- 5
PNGUnits <- "cm"
PNGDPI <- 300


# Data ------------------------------------------------------

IPC <- read_excel("Data/IPC HCAI Monthly Rolling Rates 2019-21x.xlsx") %>% 
  pivot_longer(-c("Organism", "Provider"), names_to = "Date", values_to = "Count") %>% 
  mutate(Date = rep(seq(as.Date("2019-04-01"), as.Date("2023-03-01"), by="month"),42)) %>% 
  filter(Provider %in% c("Primary Care", "Hospital Onset", "Community Onset"),
         Date >= "2021-04-01") %>% 
  drop_na()


# 	CDiffs ------------------------------------------------------

  CDiffsPlot <-
  ggplot(data = IPC %>% filter(Organism == "C Diffs"),
         aes(x = `Date`,
             y = `Count`,
             colour = factor(`Provider`,
                             level = c('Primary Care',
                                       'Hospital Onset',
                                       'Community Onset')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("Primary Care" = NHSAquaGreen,
                                 "Hospital Onset" = NHSPurple,
                                 "Community Onset" = NHSPink)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(expand = c(0,
                                0),
                     labels = comma) +
  scale_x_date(breaks= seq(min(IPC$Date), max(IPC$Date), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Count of C Diffs"),
       subtitle = "<span style='color:#00A499;'>Primary Care, </span><span style='color:#330072;'>Hospital Onset, </span><span style='color:#AE2573;'>Community Onset</span>",
       caption = paste0("Data source: IPC Team, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " ")) 

# Save Output
ggsave(file = paste0("Outputs/IPC/CDiffsPlot.png"),
       plot = CDiffsPlot,
       width = PNGWidth,
       units = PNGUnits,
       height = PNGHeight)



# 	EColi ------------------------------------------------------
  
  EColiPlot <-
  ggplot(data = IPC %>% filter(Organism == "E Coli"),
         aes(x = `Date`,
             y = `Count`,
             colour = factor(`Provider`,
                             level = c('Primary Care',
                                       'Hospital Onset',
                                       'Community Onset')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("Primary Care" = NHSAquaGreen,
                                 "Hospital Onset" = NHSPurple,
                                 "Community Onset" = NHSPink)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(expand = c(0,
                                0),
                     labels = comma) +
  scale_x_date(breaks= seq(min(IPC$Date), max(IPC$Date), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Count of E Coli"),
       subtitle = "<span style='color:#00A499;'>Primary Care, </span><span style='color:#330072;'>Hospital Onset, </span><span style='color:#AE2573;'>Community Onset</span>",
       caption = paste0("Data source: IPC Team, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " ")) 

# Save Output
ggsave(file = paste0("Outputs/IPC/EColiPlot.png"),
       plot = EColiPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



# 	MRSA ------------------------------------------------------
  
  MRSAPlot <-
  ggplot(data = IPC %>% filter(Organism == "MRSA"),
         aes(x = `Date`,
             y = `Count`,
             colour = factor(`Provider`,
                             level = c('Primary Care',
                                       'Hospital Onset',
                                       'Community Onset')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("Primary Care" = NHSAquaGreen,
                                 "Hospital Onset" = NHSPurple,
                                 "Community Onset" = NHSPink)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(expand = c(0,
                                0),
                     labels = comma) +
  scale_x_date(breaks= seq(min(IPC$Date), max(IPC$Date), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Count of MRSA"),
       subtitle = "<span style='color:#00A499;'>Primary Care, </span><span style='color:#330072;'>Hospital Onset, </span><span style='color:#AE2573;'>Community Onset</span>",
       caption = paste0("Data source: IPC Team, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " ")) 

# Save Output
ggsave(file = paste0("Outputs/IPC/MRSAPlot.png"),
       plot = MRSAPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



# 	MSSA ------------------------------------------------------
  
  MSSAPlot <-
  ggplot(data = IPC %>% filter(Organism == "MSSA"),
         aes(x = `Date`,
             y = `Count`,
             colour = factor(`Provider`,
                             level = c('Primary Care',
                                       'Hospital Onset',
                                       'Community Onset')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("Primary Care" = NHSAquaGreen,
                                 "Hospital Onset" = NHSPurple,
                                 "Community Onset" = NHSPink)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(expand = c(0,
                                0),
                     labels = comma) +
  scale_x_date(breaks= seq(min(IPC$Date), max(IPC$Date), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Count of MSSA"),
       subtitle = "<span style='color:#00A499;'>Primary Care, </span><span style='color:#330072;'>Hospital Onset, </span><span style='color:#AE2573;'>Community Onset</span>",
       caption = paste0("Data source: IPC Team, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " ")) 

# Save Output
ggsave(file = paste0("Outputs/IPC/MSSAPlot.png"),
       plot = MSSAPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")




# 	Klebsiella ------------------------------------------------------
  
  KlebsiellaPlot <-
  ggplot(data = IPC %>% filter(Organism == "Klebsiella"),
         aes(x = `Date`,
             y = `Count`,
             colour = factor(`Provider`,
                             level = c('Primary Care',
                                       'Hospital Onset',
                                       'Community Onset')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("Primary Care" = NHSAquaGreen,
                                 "Hospital Onset" = NHSPurple,
                                 "Community Onset" = NHSPink)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(expand = c(0,
                                0),
                     labels = comma) +
  scale_x_date(breaks= seq(min(IPC$Date), max(IPC$Date), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Count of Klebsiella"),
       subtitle = "<span style='color:#00A499;'>Primary Care, </span><span style='color:#330072;'>Hospital Onset, </span><span style='color:#AE2573;'>Community Onset</span>",
       caption = paste0("Data source: IPC Team, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " ")) 

# Save Output
ggsave(file = paste0("Outputs/IPC/KlebsiellaPlot.png"),
       plot = KlebsiellaPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")





# 	Pseudomonas ------------------------------------------------------
  
  PseudomonasPlot <-
  ggplot(data = IPC %>% filter(Organism == "Pseudomonas"),
         aes(x = `Date`,
             y = `Count`,
             colour = factor(`Provider`,
                             level = c('Primary Care',
                                       'Hospital Onset',
                                       'Community Onset')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("Primary Care" = NHSAquaGreen,
                                 "Hospital Onset" = NHSPurple,
                                 "Community Onset" = NHSPink)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(expand = c(0,
                                0),
                     labels = comma) +
  scale_x_date(breaks= seq(min(IPC$Date), max(IPC$Date), by = "3 months"),
               date_labels = "%b %Y")+
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Count of Pseudomonas"),
       subtitle = "<span style='color:#00A499;'>Primary Care, </span><span style='color:#330072;'>Hospital Onset, </span><span style='color:#AE2573;'>Community Onset</span>",
       caption = paste0("Data source: IPC Team, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " ")) 

# Save Output
ggsave(file = paste0("Outputs/IPC/PseudomonasPlot.png"),
       plot = PseudomonasPlot,
       width = PNGWidth,
       height = PNGHeight,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")



# 	Combined ------------------------------------------------------

IPCPlot <-
  ggplot(data = IPC,
         aes(x = `Date`,
             y = `Count`,
             colour = factor(`Provider`,
                             level = c('Primary Care',
                                       'Hospital Onset',
                                       'Community Onset')))) +
  geom_line(size = 0.7) +
  scale_colour_manual("legend",
                      values = c("Primary Care" = NHSAquaGreen,
                                 "Hospital Onset" = NHSPurple,
                                 "Community Onset" = NHSPink)) +
  geom_hline(yintercept = 0,
             size = 0.5,
             colour = "#000000") +
  scale_y_continuous(expand = c(0,
                                0),
                     labels = comma) +
  scale_x_date(breaks= seq(min(IPC$Date), max(IPC$Date), by = "3 months"),
               date_labels = "%b %Y")+
  facet_wrap(~Organism, ncol=2)+
  LinePlotTheme() +
  theme(plot.subtitle = element_markdown()) +
  labs(title = paste0("Infection Prevention Control"),
       subtitle = "<span style='color:#00A499;'>Primary Care, </span><span style='color:#330072;'>Hospital Onset, </span><span style='color:#AE2573;'>Community Onset</span>",
       caption = paste0("Data source: IPC Team, updated on ",
                        format(Sys.time(),
                               '%A %d %B %Y'),
                        sep = " ")) 


# Save Output
ggsave(file = paste0("Outputs/IPC/IPCPlot.png"),
       plot = IPCPlot,
       width = 16,
       height = 15,
       units = PNGUnits,
       dpi = PNGDPI,
       type = "cairo")