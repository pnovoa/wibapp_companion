library(tidyverse)
library(Hmisc)
library(corrplot)
library(corrr)
library(ggbump)
library(scales)
library(latex2exp)
library(ggthemes)

colors_for_bars <- c("#00AFBB", "#E7B800", "#FC4E07", "#00BA38", "#B2B2B2") #c("#F5D547", "#64A8D1", "#E87A72", "#81C784")

levels_of_measures <- c("Spearman", "Kendall", "Blest's coeff.", "Weight. coeff.", "WS coeff.")
levels_of_attitudes <- c("Lower bound", "Upper bound", "Pessimistic attitude", "Neutral attitude", "Optimistic attitude")

label_par <- c(
  "TRDF" = TeX("$O_1: w_T \\geq w_R \\geq w_D \\geq w_F$"),
  "RTDF" = TeX("$O_2: w_R \\geq w_T \\geq w_D \\geq w_F$"),
  "TRFD" = TeX("$O_3: w_T \\geq w_R \\geq w_F \\geq w_D$"),
  "RTFD" = TeX("$O_4: w_R \\geq w_T \\geq w_F \\geq w_D$")
)


the_geom_text_for_labels <- geom_text(aes(label=Label, hjust=Hjust), angle=90, vjust=0.5, size=2.5, fontface="bold")


final_look <- theme_clean(base_size = 10) +
  theme(strip.text = element_text(face = "bold", size = 9),
        axis.text.x = element_text(angle=45, vjust = 1., hjust = 1.),
        panel.grid.major.y = element_line(colour = "gray60", linewidth = 0.3, linetype = "dotted"),
        plot.background = element_blank(),
        axis.line.y = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                       ends = "last")),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                       ends = "last")),
        axis.title = element_text(face = "bold", size = 11),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.background = element_rect(color = NA)
  ) 