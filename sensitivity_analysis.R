source("common.R")


df_pref_ranks <- readxl::read_xls(path = "univ_ranks.xls", sheet = "sensit") %>%
  mutate(Measure = factor(Measure, levels=levels_of_measures), 
         Attitude = factor(Attitude, levels=levels_of_attitudes), 
         Preference = factor(Preference, levels=names(label_par)),
         Label = format(round(Mean, 3), nsmall = 3)) %>%
  mutate(Strength = paste0("s = ", Strength))


df_pref_ranks <- df_pref_ranks %>%
  dplyr::group_by(Attitude, Measure, Strength) %>%
  dplyr::mutate(MaxBest = max(Mean), 
                Best = ifelse(Mean == MaxBest, "Best", "Others")) %>%
  dplyr::ungroup()


df_pref_ranks$Preference_label <- factor(df_pref_ranks$Preference, levels = names(label_par), labels = label_par)


latex_labels <- c("TRDF" = TeX(r"(\textbf{O}$_1: w_T \geq w_R \geq w_D \geq w_F$)"),
                  "RTDF" = TeX(r"(\textbf{O}$_2: w_R \geq w_T \geq w_D \geq w_F$)"),
                  "TRFD" = TeX(r"(\textbf{O}$_3: w_T \geq w_R \geq w_F \geq w_D$)"),
                  "RTFD" = TeX(r"(\textbf{O}$_4: w_R \geq w_T \geq w_F \geq w_D$)"))



p <- df_pref_ranks %>%
  ggplot(aes(x=Preference, y=Mean, label=Label, fill=Strength)) + # label=Label)) +
  geom_bar(stat = "identity", width = 0.85, position = "dodge") +
  #geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=0.25, linewidth=0.25, colour="black", alpha=0.9) + 
  geom_text(angle=90, vjust=0.5, hjust=1.2, size=2, position = position_dodge(width = 0.85)) +
  xlab("Preference order") + 
  ylab("Similarity (mean from perturbations)") +
  scale_y_continuous(expand = c(0.00, 0.00), breaks = seq(0,1,0.25), limits = c(0,1.2)) +
  scale_x_discrete(labels = latex_labels) + 
  scale_fill_manual(values = colors_for_bars) +
  guides(fill = guide_legend(title = "Swap strength")) + 
  facet_grid(Measure~Attitude) + 
  final_look + 
  theme(
    legend.title = element_text(face = "bold")
  )


ggsave(filename = "07_sensitivity_comparison.pdf", plot = p, width = 9, height = 6.5)