source("common.R")
source("kendall_summary.R")
source("sim_measures.R")


df_rur_rank <- readxl::read_xls(path = "univ_ranks.xls", sheet = "rank_comparison") %>%
  select(all_of(c("RUR", "RUR_Score"))) %>%
  mutate(RankRUR = rank(-RUR_Score)) %>%
  rename("Alternative" = RUR)

df_wiba_ranks <- readxl::read_xls(path = "univ_ranks.xls", sheet = "wiba_ranks") %>%
  select(-starts_with("W_")) %>%
  pivot_longer(cols = -c("Preference", "Alternative"), names_to = "Indicator", values_to = "Score") %>%
  group_by(Preference, Indicator) %>%
  mutate(Rank = rank(-Score)) %>%
  inner_join(df_rur_rank, by = "Alternative")

rank_results <- df_wiba_ranks %>%
  group_by(Preference, Indicator) %>%
  summarise(
    Spearman = cor(RankRUR, Rank, method = "spearman"),
    Kendall = cor(RankRUR, Rank, method = "kendall"),
    `Blest's coeff.` = blest_rank_correlation(RankRUR, Rank),
    `Weight. coeff.` = weighted_rank_measure_of_correlation(RankRUR, Rank),
    `WS coeff.` = ws_coefficient(RankRUR, Rank)
  ) %>% ungroup() %>%
  pivot_longer(cols = -c(Preference, Indicator), names_to = "Measure", values_to = "Similarity") %>%
  mutate(Measure = factor(Measure, levels=levels_of_measures),
         Preference = factor(Preference, levels=names(latex_labels))
  )


rank_results_to_plot <- rank_results %>% 
  filter(Indicator %in% levels_of_attitudes, Measure %in% levels_of_measures) %>%
  mutate(Indicator = factor(Indicator, levels=levels_of_attitudes)) %>%
  mutate(Measure = factor(Measure, levels=levels_of_measures)) %>%
  mutate(Label = format(round(Similarity, 3), nsmall = 3)) %>%
  mutate(Hjust = ifelse(Similarity < 0.5, -0.1, 1.2)) %>%
  group_by(Indicator,Measure) %>%
  mutate(BestSim = max(Similarity)) %>% ungroup() %>%
  mutate(Fill = ifelse(Similarity == BestSim, "Best", "Others")) 


p_rank_comp <- rank_results_to_plot %>% 
  ggplot(aes(x=Preference, y=Similarity)) + # label=Label)) +
  geom_bar(aes(fill=Fill), stat = "identity", width = 0.5, position = "dodge") +
  the_geom_text_for_labels +
  ylab("Similarity to RUR ranking") +
  xlab("Preference order") + 
  scale_y_continuous(expand = c(0.00, 0.00), breaks = seq(0,1,0.25), limits = c(0,1.2)) +
  scale_x_discrete(labels = latex_labels) + 
  scale_fill_manual(values = colors_for_bars[c(2,1)]) +
  # facet_grid(Preference_label~Attitude, labeller = label_parsed) + 
  facet_grid(Measure~Indicator) + 
  final_look

ggsave(filename = "08_rur_comparison.pdf", plot = p_rank_comp, width = 9, height = 6.5)



df_kendalls <- df_wiba_ranks %>%
  filter(Indicator %in% levels_of_attitudes) %>%
  mutate(Indicator = factor(Indicator, levels=levels_of_attitudes)) %>%
  group_by(Preference, Indicator) %>%
  group_split() %>%
  lapply(function(sub_df) {
    result <- kendall_summary(sub_df$RankRUR, sub_df$Rank)
    
    # Agregar la columna de grupos para que los resultados puedan ser identificados
    result$Preference <- sub_df$Preference[1]
    result$Indicator <- sub_df$Indicator[1]
    
    return(result)
  }) %>%
  bind_rows() %>% 
  group_by(Preference, Indicator) %>%
  mutate(Perc = N/sum(N) * 100) 



df_kendalls$Preference_label <- factor(df_kendalls$Preference, levels = names(latex_labels), labels = latex_labels)

df_kendalls <- df_kendalls %>%
  mutate(Label = paste0(format(round(Perc, 1), nsmall = 1), "%")) %>%
  mutate(Hjust = ifelse(Perc < 25, -0.1, 1.2))

p_kendall <- df_kendalls %>%
  ggplot(aes(x=Type, y=Perc)) +
  geom_bar(aes(fill=Type), 
           stat = "identity", 
           width = 0.5, position = "dodge",
           show.legend = FALSE) +
  the_geom_text_for_labels +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     expand = c(0.00, 0.00), limits = c(0,110)) +
  scale_fill_manual(values = colors_for_bars) +
  ylab("Percentage of total pairs (n = 401,856)") + 
  xlab("Type of pairs") + 
  facet_grid(Preference_label~Indicator, labeller = labeller(Preference_label = label_parsed)) + 
  final_look


print(p_kendall)

ggsave(filename = "10_rur_comparison_kendall.pdf", plot = p_kendall, width = 8.1, height = 6)


# GRÁFICO DE DISTRIBUCIÓN DE LA DIFERENCIA ENTRE RANKINGS

n_univ <- nrow(df_rur_rank)

df_rank_diff_plot <- df_wiba_ranks %>%
  filter(Indicator %in% levels_of_attitudes) %>%
  mutate(Indicator = factor(Indicator, levels=levels_of_attitudes)) %>%
  group_by(Preference, Indicator) %>%
  mutate(RankPercent = 100*RankRUR/n_univ) %>%
  mutate(RankPercentGroup = cut(
    x = RankPercent,
    breaks = seq(0, 100, 25),
    include.lowest = TRUE,
    ordered_result = TRUE,
    labels = paste0("Q", 1:4)
  )) %>%
  mutate(Difference = abs(RankRUR - Rank))

df_rank_diff_plot$Preference_label <- factor(df_rank_diff_plot$Preference, levels = names(latex_labels), labels = latex_labels)



p_rank_diff <- df_rank_diff_plot %>%
  ggplot(aes(x=RankPercentGroup, y=Difference)) +
  geom_boxplot(aes(fill=RankPercentGroup), width=0.5, 
               color="black", 
               outlier.alpha = 0.5, 
               outlier.size = 1, 
               linewidth=0.2, 
               show.legend = FALSE) + 
  scale_fill_manual(values = colors_for_bars) +
  ylab("Absolute difference of ranks (with respect to RUR)") + 
  xlab("Ranking quartiles by RUR approach") + 
  facet_grid(Preference_label~Indicator, labeller = labeller(Preference_label = label_parsed)) + 
  final_look

print(p_rank_diff)

ggsave(filename = "09_rank_diff_dist.pdf", plot = p_rank_diff, width = 8.1, height = 6)
