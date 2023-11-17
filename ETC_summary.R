#Loading the appropriate libraries
# loading the appropriate libraries
library(readr)
library(ggplot2)
# loading and checking the data
data_summary <- read_csv("ETC_summary.csv")
print(data_summary)

#Basic plot
# coloured barplot
ggplot(data_summary, aes(x = factor(Temp), y = mean)) + 
  geom_bar(stat = "identity")
#Using colour to split the results
# coloured barplot
ggplot(data_summary, aes(x = factor(Temp), y = mean, fill = Explant, colour = Explant)) + 
  geom_bar(stat = "identity")

# coloured barplot
ggplot(data_summary, aes(x = factor(Temp), y = mean, fill = Explant, colour = Explant)) + 
  geom_bar(stat = "identity", position = "dodge")
#Error bars
# coloured barplot
ggplot(data_summary, aes(x = factor(Temp), y = mean, fill = Explant, colour = Explant)) + 
  geom_bar(stat = "identity", position = "dodge")  +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd))
# coloured barplot
ggplot(data_summary, aes(x = factor(Temp), y = mean, fill = Explant, colour = Explant)) + 
  geom_bar(stat = "identity", position = "dodge")  +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position = position_dodge(0.9), width = 0.25)
# coloured barplot
ggplot(data_summary, aes(x = factor(Temp), y = mean, fill = Explant, colour = Explant)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5)  +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position = position_dodge(0.9), width = 0.25)
#Customising x and y titles
# coloured barplot
ggplot(data_summary, aes(x = factor(Temp), y = mean, fill = Explant, colour = Explant)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5)  +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position = position_dodge(0.9), width = 0.25) +
  labs(x="Temperature (˚C)", y="Androgenic response")
#Customising the theme and legend
# coloured barplot
ggplot(data_summary, aes(x = factor(Temp), y = mean, fill = Explant, colour = Explant)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5)  +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position = position_dodge(0.9), width = 0.25) +
  labs(x="Temperature (˚C)", y="Androgenic response") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = c(0.1, 0.75))
#Adding the compact letter display
# coloured barplot
ggplot(data_summary, aes(x = factor(Temp), y = mean, fill = Explant, colour = Explant)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5)  +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position = position_dodge(0.9), width = 0.25) +
  labs(x="Temperature (˚C)", y="Androgenic response") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = c(0.1, 0.75)) +
  geom_text(aes(label=Tukey), position = position_dodge(0.90), size = 3, 
            vjust=-0.8, hjust=-0.5, colour = "gray25")
#Customising the y limits
# coloured barplot
ggplot(data_summary, aes(x = factor(Temp), y = mean, fill = Explant, colour = Explant)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5)  +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position = position_dodge(0.9), width = 0.25) +
  labs(x="Temperature (˚C)", y="Androgenic response") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = c(0.1, 0.75)) +
  geom_text(aes(label=Tukey), position = position_dodge(0.90), size = 3, 
            vjust=-0.8, hjust=-0.5, colour = "gray25") +
  ylim(0, 15)
#Adding text labels
# coloured barplot
ggplot(data_summary, aes(x = factor(Temp), y = mean, fill = Explant, colour = Explant)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5)  +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position = position_dodge(0.9), width = 0.25) +
  labs(x="Temperature (˚C)", y="Androgenic response") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = c(0.1, 0.75)) +
  geom_text(aes(label=Tukey), position = position_dodge(0.90), size = 3, 
            vjust=-0.8, hjust=-0.5, colour = "gray25") +
  ylim(0, 15) +
  geom_text(aes(label=Explant, y = 2.5), position = position_dodge(0.90))
