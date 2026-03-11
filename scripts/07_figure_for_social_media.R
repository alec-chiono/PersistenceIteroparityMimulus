# Figure for social media post

source("scripts/04_predict.R")
librarian::shelf(ggdist)
theme_set(ggthemes::theme_tufte() + theme(text=element_text(size=20, family="sans"), panel.border=element_rect(color="black", fill=NA, linewidth=1)))
if (!file.exists("figures")) dir.create("figures")

wp <- "0" #water potential treatment that we'll filter for

# Wrangle posterior predictions
persistence_cond_pred %<>%
  filter(water_potential==wp) %>% #filter to only one water potential treatment
  mutate(
    ecotype=case_when( #Change ecotype labels to be more intuitive
    ecotype=="Semelparous" ~ "Annual Populations",
    ecotype=="Iteroparous" ~ "Perennial Populations",
    TRUE ~ NA)
    )

# Get germination proportions from raw data
persistence_raw_proportions <- germ_df %>%
  filter(water_potential==wp) %>%
  group_by(site, seed_family, cold_stratification, water_potential, ecotype) %>%
  summarize(p=num_persisting_seeds/num_viable_seeds,
            ecotype=case_when(
              ecotype=="Semelparous" ~ "Annual Populations",
              ecotype=="Iteroparous" ~ "Perennial Populations",
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
pdot <- 0.013
pscale <- .85
palpha <- .9
cold_strat_effect_colors <- c("pos"="red4", "neg"="skyblue3", "zero"="grey50")
ecotype_colors <- c("Annual"="grey75", "Perennial"="grey25")

## Plot Fig 2
fig <- ggplot(mapping=aes(x=cold_stratification, y=1-p, fill=ecotype, color=ecotype)) +
  geom_vline(xintercept=c("Not cold stratified", "Cold stratified"), color="black", linetype=2) +
  geom_line(data=filter(persistence_cond_spag, slope=="pos"), aes(group=paste0(.draw, seed_family)), alpha=.1, linewidth=.1, linetype=1) +
  geom_line(data=filter(persistence_cond_spag, slope=="neg"), aes(group=paste0(.draw, seed_family)), alpha=.1, linewidth=.1, linetype=1) +
  geom_line(data=filter(persistence_cond_spag, slope=="zero"), aes(group=paste0(.draw, seed_family)), alpha=.1, linewidth=.1, linetype=1) +
  geom_line(data=filter(persistence_raw_proportions, slope=="pos"), aes(group=seed_family), linetype=2) +
  geom_line(data=filter(persistence_raw_proportions, slope=="neg"), aes(group=seed_family), linetype=2) +
  geom_line(data=filter(persistence_raw_proportions, slope=="zero"), aes(group=seed_family), linetype=2) +
  geom_dots(data=filter(persistence_raw_proportions, cold_stratification=="Not cold stratified"),
            side="right", scale=1.25*pscale, binwidth=unit(c(-Inf, 1.25*pdot), "npc"), overflow="keep",
            linewidth=.2) +
  geom_dots(data=filter(persistence_raw_proportions, cold_stratification=="Cold stratified"),
            side="left", scale=1.25*pscale, binwidth=unit(c(-Inf, 1.25*pdot), "npc"), overflow="keep",
            linewidth=.2) +
  scale_x_discrete(labels=c("\n \nNot cold\nstratified", "\n \nCold\nstratified")) +
  scale_y_continuous(name="Proportion Seeds Germinated", limits=c(0,1)) +
  scale_fill_manual(values=c("skyblue3", "red4"), aesthetics=c("fill", "color"), guide="none") +
  facet_grid(~ ecotype) +
  theme(
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.x=element_text(size=10),
    axis.title.y=element_text(size=13),
    axis.text.y=element_text(size=10),
    )

fig

## Write Fig 2
ggsave("figures/socials_figure_raw.pdf", fig, width=6, height=3, units="in", dpi=600)

