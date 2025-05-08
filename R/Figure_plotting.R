## Code for generating plots for publication and supplementary material

# required packages
library(ggplot2)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(RColorBrewer)
library(gridExtra)
library(patchwork)


setwd('C:/Users/dcpit/OneDrive/Documents/GRADUATE SCHOOL/PhD-yr1/Project 4/SEAIR_Model/Results/Plots/')

# MUST RUN CUMULATIVE INFECTION CENTRAL TENDENCY CODE FIRST AND HAVE RESULTS IN GLOBAL ENVIRONMENT #

#Figure 1
# Plot 1 (Swine fair duration)
p1 <- ggplot(long_swined, aes(x = Day, y = Infected, fill = Day)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = 'black', alpha = 0.2) +
  scale_fill_brewer(palette = "Set3") +
  theme_ipsum() +
  labs(title = "Swine", x = "Fair Duration (Days)", y = "Infection Count") +
  theme(
    legend.position = "none",
    plot.tag = element_text(face = "bold", size = 14),
    axis.title.x = element_text(hjust = 0.5, size = 14),
    axis.title.y = element_text(hjust = 0.5, size = 14),
    axis.text = element_text(color = 'black')
  )
ggsave("FD_swine.jpeg",plot = p1, units = "in", width = 6, height = 4, dpi = 600)

# Plot 2 (Human fair duration)
p2 <- ggplot(long_humand, aes(x = Day, y = Infected, fill = Day)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = 'black', alpha = 0.2) +
  scale_fill_brewer(palette = "Set3") +
  theme_ipsum() +
  labs(title = "Human", x = "Fair Duration (Days)", y = "Infection Count") +
  theme(
    legend.position = "none",
    plot.tag = element_text(face = "bold", size = 14),
    axis.title.x = element_text(hjust = 0.5, size = 14),
    axis.title.y = element_text(hjust = 0.5, size = 14),
    axis.text = element_text(color = 'black')
  )
ggsave("FD_human.jpeg",plot = p2, units = "in", width = 6, height = 4, dpi = 600)

# Plot 3 (Swine biosecurity)
p3<-ggplot(long_swineR0, aes(x=R0, y=Infected, fill=R0))+
  geom_violin()+
  geom_boxplot(width=0.1, color='black', alpha=0.2)+
  scale_fill_brewer(palette = "Set3")+
  theme_ipsum()+
  theme(legend.position = "none",
        plot.tag = element_text(face = "bold", size = 14))+
  labs(title = "Swine", x = "Reproductive Number(R0)", y = "Infection Count") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 14),
        axis.title.y = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(color = 'black'))
ggsave("Biosec_swine.jpeg", plot = p3, units = "in", width = 6, height = 4, dpi = 600)

# Plot 4 (Human biosecurity)
p4<-ggplot(long_humanR0, aes(x=R0, y= Infected, fill=R0))+
  geom_violin()+
  geom_boxplot(width=0.1, color='black', alpha=0.2)+
  scale_fill_brewer(palette = "Set3")+
  theme_ipsum()+
  theme(legend.position = "none",
        plot.tag = element_text(face = "bold", size = 14))+
  labs(title = "Human", x = "Reproductive Number(R0)", y = "Infection Count") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 14),
        axis.title.y = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(color = 'black'))
ggsave("Biosec_human.jpeg", plot = p4, units = "in", width = 6, height = 4, dpi = 600)

# Combine with patchwork: side-by-side, auto-labeled A and B
fig1 <- (p1 | p2) / (p3 | p4) + plot_annotation(tag_levels = 'A')

print(fig1)

ggsave("Figure_1_2x2.jpeg",plot = fig1, units = "in", width = 12, height = 8, dpi = 600)

# Supplementary Figure 4
p5 <- ggplot(long_swineAs, aes(x=Initial, y=Infected, fill=Initial))+
  geom_violin()+
  geom_boxplot(width=0.1, color='black', alpha=0.2)+
  scale_fill_brewer(palette = "Set3")+
  theme_ipsum()+
  theme(legend.position = "none",
        plot.tag = element_text(face = "bold", size = 14))+
  labs(title = "Swine", x = "Initially Infected Swine", y = "Infection Count") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 14),
        axis.title.y = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(color = 'black'))

ggsave("Initially_infected_Swine.jpeg", units="in", width = 6, height = 4, dpi=600)

p6 <- ggplot(long_humanAs, aes(x=Initial, y= Infected, fill=Initial))+
  geom_violin()+
  geom_boxplot(width=0.1, color='black', alpha=0.2)+
  scale_fill_brewer(palette = "Set3")+
  theme_ipsum()+
  theme(legend.position = "none",
        plot.tag = element_text(face = "bold", size = 14))+
  labs(title = "Human", x = "Initially Infected Swine", y = "Infection Count") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 14),
        axis.title.y = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(color = 'black'))

ggsave("Initially_infected_Human.jpeg", units="in", width = 6, height = 4, dpi=600)

supfig4 <- (p5)/(p6) + plot_annotation(tag_levels = 'A')

print(supfig4)

ggsave("Supplementary_F4.jpeg", plot = supfig4, units = "in", width = 6, height = 6, dpi = 600)


#Supplementary Figure 3
p7 <-ggplot(long_swineT17, aes(x=Chi, y=Infected, fill=Chi))+
  geom_violin()+
  geom_boxplot(width=0.1, color='black', alpha=0.2)+
  scale_fill_brewer(palette = "Set3")+
  theme_ipsum()+
  theme(legend.position = "none",
        plot.tag = element_text(face = "bold", size = 14))+
  labs(title = "Swine", x = "Infection Detection", y = "Infection Count") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 14),
        axis.title.y = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(color = 'black'))

ggsave("Quarentine_Detection_17_Swine.jpeg", units="in", width = 6, height = 4, dpi=600)

p8 <-ggplot(long_humanT17, aes(x=Chi, y= Infected, fill=Chi))+
  geom_violin()+
  geom_boxplot(width=0.1, color='black', alpha=0.2)+
  scale_fill_brewer(palette = "Set3")+
  theme_ipsum()+
  theme(legend.position = "none",
        plot.tag = element_text(face = "bold", size = 14))+
  labs(title = "Human", x = "Infection Detection", y = "Infection Count") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 14),
        axis.title.y = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(color = 'black'))

ggsave("Quarentine_Detection_35_Human.jpeg", units="in", width = 6, height = 4, dpi=600)

p9 <-ggplot(long_swineT35, aes(x=Chi, y=Infected, fill=Chi))+
  geom_violin()+
  geom_boxplot(width=0.1, color='black', alpha=0.2)+
  scale_fill_brewer(palette = "Set3")+
  theme_ipsum()+
  theme(legend.position = "none",
        plot.tag = element_text(face = "bold", size = 14))+
  labs(title = "Swine", x = "Infection Detection", y = "Infection Count") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 14),
        axis.title.y = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(color = 'black'))

ggsave("Quarentine_Detection_35_Swine.jpeg", units="in", width = 6, height = 4, dpi=600)

p10 <-ggplot(long_humanT35, aes(x=Chi, y= Infected, fill=Chi))+
  geom_violin()+
  geom_boxplot(width=0.1, color='black', alpha=0.2)+
  scale_fill_brewer(palette = "Set3")+
  theme_ipsum()+
  theme(legend.position = "none",
        plot.tag = element_text(face = "bold", size = 14))+
  labs(title = "Human", x = "Infection Detection", y = "Infection Count") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 14),
        axis.title.y = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(color = 'black'))

ggsave("Quarentine_Detection_35_Human.jpeg", units="in", width = 6, height = 4, dpi=600)

supfig3 <- (p7|p8)/(p9|p10) + plot_annotation(tag_levels = 'A')

print(supfig3)

ggsave("Supplementary_F3.jpeg", plot = supfig3, units = "in", width = 12, height = 8, dpi = 600)


# Figure 2
p11 <-ggplot(long_swineR0D, aes(x=R0.D, y=Infected, fill=R0.D))+
  geom_violin()+
  geom_boxplot(width=0.1, color='black', alpha=0.2)+
  scale_fill_brewer(palette = "Set3")+
  theme_ipsum()+
  theme(legend.position = "none",
        plot.tag = element_text(face = "bold", size = 14))+
  labs(title = "Swine", x = "Scenario", y = "Infection Count") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 14),
        axis.title.y = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(color = 'black'))

ggsave("Biosecurity_Duration_Swine.jpeg", units="in", width = 6, height = 4, dpi=600)

p12 <-ggplot(long_HumanR0D, aes(x=R0.D, y= Infected, fill=R0.D))+
  geom_violin()+
  geom_boxplot(width=0.1, color='black', alpha=0.2)+
  scale_fill_brewer(palette = "Set3")+
  theme_ipsum()+
  theme(legend.position = "none",
        plot.tag = element_text(face = "bold", size = 14))+
  labs(title = "Human", x = "Scenario", y = "Infection Count") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 14),
        axis.title.y = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(color = 'black'))

ggsave("Biosecurity_Duration_Human.jpeg", units="in", width = 6, height = 4, dpi=600)

p13<-ggplot(long_swineBioTest, aes(x=Scenario, y=Infected, fill=Scenario))+
  geom_violin()+
  geom_boxplot(width=0.1, color='black', alpha=0.2)+
  scale_fill_brewer(palette = "Set3")+
  theme_ipsum()+
  theme(legend.position = "none",
        plot.tag = element_text(face = "bold", size = 14))+
  labs(title = "Swine", x = "Scenario", y = "Infection Count") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 14),
        axis.title.y = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(color = 'black'))

ggsave("Prefair_Biosecurity_Quarentine_Swine.jpeg", units="in", width = 6, height = 4, dpi=600)

p14<-ggplot(long_HumanBioTest, aes(x=Scenario, y= Infected, fill=Scenario))+
  geom_violin()+
  geom_boxplot(width=0.1, color='black', alpha=0.2)+
  scale_fill_brewer(palette = "Set3")+
  theme_ipsum()+
  theme(legend.position = "none",
        plot.tag = element_text(face = "bold", size = 14))+
  labs(title = "Human", x = "Scenario", y = "Infection Count") +
  theme(axis.title.x = element_text(hjust = 0.5, size = 14),
        axis.title.y = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(color = 'black'))

ggsave("Prefair_Biosecurity_Quarentine_Human.jpeg", units="in", width = 6, height = 4, dpi=600)

fig2 <- (p11 | p12) / (p13 | p14) + plot_annotation(tag_levels = 'A')

print(fig2)

ggsave("Figure_2_2x2.jpeg",plot = fig2, units = "in", width = 12, height = 8, dpi = 600)