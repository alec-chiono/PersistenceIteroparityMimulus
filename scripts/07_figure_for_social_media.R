# Figure for social media post

source("scripts/04_predict.R")
librarian::shelf(ggdist)
theme_set(ggthemes::theme_tufte() + theme(text=element_text(size=15), panel.border=element_rect(color="black", fill=NA, linewidth=1)))
if (!file.exists("figures")) dir.create("figures")

persistence_cond_pred %<>%
  filter(water_potential=="0") %>%
  mutate(
    ecotype=case_when(
    ecotype=="Semelparous" ~ "Annual",
    ecotype=="Iteroparous" ~ "Perennial",
    TRUE ~ NA)
    )

persistence_raw_proportions <- germ_df %>%
  filter(water_potential=="0") %>%
  group_by(site, seed_family, cold_stratification, water_potential, ecotype) %>%
  summarize(p=num_persisting_seeds/num_viable_seeds,
            ecotype=case_when(
              ecotype=="Semelparous" ~ "Annual",
              ecotype=="Iteroparous" ~ "Perennial",
              TRUE ~ NA),
            .groups="drop") %>%
  arrange(cold_stratification) %>%
  group_by(ecotype, seed_family, water_potential) %>%
  mutate(
    slope=case_when(
      length(p)!=2 ~ NA,
      diff(p)<0 ~ "neg",
      diff(p)>0 ~ "pos",
      TRUE ~ "zero"
    ),
  ) %>% ungroup()


## Sample spaghetti lines to show model prediction of effects of cold stratification on individual seed families
draws_sample <- sample(1:max(persistence_cond_pred$.draw), 100)
persistence_cond_spag <- persistence_cond_pred %>%
  filter(.draw %in% draws_sample) %>%
  arrange(cold_stratification) %>%
  group_by(.draw, ecotype, seed_family, water_potential) %>%
  mutate(
    diff_p=diff(p),
    slope=case_when(
      diff_p<0 ~ "neg",
      diff_p>0 ~ "pos",
      TRUE ~ "zero"
    )
  ) %>% ungroup()

## Setting some plotting parameters
pdot <- 0.012
pscale <- 0.75
palpha <- .9
cold_strat_effect_colors <- c("pos"="red4", "neg"="skyblue3", "zero"="grey50")
ecotype_colors <- c("Annual"="grey75", "Perennial"="grey25")

## Plot Fig 2
fig <- ggplot(mapping=aes(x=cold_stratification, y=1-p, fill=ecotype)) +
  stat_slab(data=filter(persistence_cond_pred, cold_stratification=="Not cold stratified") %>%
              mutate(water_potential=fct_rev(water_potential)),
            side="left", normalize="groups", scale=pscale, alpha=.8) +
  stat_slab(data=filter(persistence_cond_pred, cold_stratification=="Cold stratified"),
            aes(side=cold_stratification),
            side="right", normalize="groups", scale=pscale, alpha=.8) +
  geom_vline(xintercept=c("Not cold stratified", "Cold stratified"), color="black", linetype=2) +
  geom_line(data=filter(persistence_cond_spag, slope=="pos"), aes(group=paste0(.draw, seed_family)), color=cold_strat_effect_colors["pos"], alpha=.1, linewidth=.1, linetype=1) +
  geom_line(data=filter(persistence_cond_spag, slope=="neg"), aes(group=paste0(.draw, seed_family)), color=cold_strat_effect_colors["neg"], alpha=.1, linewidth=.1, linetype=1) +
  geom_line(data=filter(persistence_cond_spag, slope=="zero"), aes(group=paste0(.draw, seed_family)), color=cold_strat_effect_colors["zero"], alpha=.1, linewidth=.1, linetype=1) +
  geom_line(data=filter(persistence_raw_proportions, slope=="pos"), aes(group=seed_family), color=cold_strat_effect_colors["pos"], linetype=2) +
  geom_line(data=filter(persistence_raw_proportions, slope=="neg"), aes(group=seed_family), color=cold_strat_effect_colors["neg"], linetype=2) +
  geom_line(data=filter(persistence_raw_proportions, slope=="zero"), aes(group=seed_family), color=cold_strat_effect_colors["zero"], linetype=2) +
  geom_dots(data=filter(persistence_raw_proportions, cold_stratification=="Not cold stratified"),
            side="left", scale=1.25*pscale, binwidth=unit(c(-Inf, 1.25*pdot), "npc"), overflow="keep",
            linewidth=.2, color="black") +
  geom_dots(data=filter(persistence_raw_proportions, cold_stratification=="Cold stratified"),
            side="right", scale=1.25*pscale, binwidth=unit(c(-Inf, 1.25*pdot), "npc"), overflow="keep",
            linewidth=.2, color="black") +
  scale_x_discrete(labels=c("Not cold\nstratified", "Cold\nstratified")) +
  scale_y_continuous(name="Probability of Germination", limits=c(0,1)) +
  scale_fill_manual(values=ecotype_colors, guide="none") +
  facet_grid(~ ecotype) +
  theme(
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_text(size=10)
    )

fig

## Write Fig 2
ggsave("figures/socials_figure.pdf", fig, width=6, height=3, units="in", dpi=600)
