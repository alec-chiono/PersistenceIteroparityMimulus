# Visualize model results
# Alec Chiono, alec.chiono@colorado.edu
source("scripts/04_predict.R")
librarian::shelf(ggdist, patchwork, shadowtext, "rmcelreath/rethinking")
theme_set(ggthemes::theme_tufte() + theme(text=element_text(size=15), panel.border=element_rect(color="black", fill=NA, linewidth=1)))
if (!file.exists("figures")) dir.create("figures")

# Set some variables for plotting later
water_potential_labeller <- paste(as.numeric(as.character(levels(germ_df$water_potential))), "MPa", sep=" ")
names(water_potential_labeller) <- levels(germ_df$water_potential)
ecotype_colors <- c("Semelparous"="grey75", "Iteroparous"="grey25")

# Figure 1 ---------------------------------------------------------------------
## Wrangle raw data into persistence proportions
persistence_raw_proportions <- germ_df %>%
  group_by(site, seed_family, cold_stratification, water_potential, ecotype) %>%
  summarize(p=num_persisting_seeds/num_viable_seeds,
            .groups="drop") %>%
  arrange(cold_stratification) %>%
  group_by(seed_family, water_potential) %>%
  mutate(
    slope=case_when(
      length(p)!=2 ~ NA,
      diff(p)<0 ~ "neg",
      diff(p)>0 ~ "pos",
      TRUE ~ "zero"
    ),
  ) %>% ungroup()

## Setting some plotting parameters
pdot <- 0.0225
pscale <- 0.75
palpha <- .9

## Fig 1A: Persistence proportions for each treatment combo
fig1A <- ggplot(mapping=aes(x=water_potential, y=p, fill=ecotype)) +
  geom_vline(xintercept=c("-1", "-0.5", "0"), color="black", linetype=2) +
  stat_slab(data=filter(persistence_marg_pred, ecotype=="Semelparous"), alpha=palpha,
               side="left", normalize="groups", scale=pscale*.75, color=NA) +
  geom_dots(data=filter(persistence_raw_proportions, ecotype=="Semelparous"), color="black",
            side="left", scale=pscale*.75, binwidth=unit(c(-Inf, pdot*.5), "npc"),
            linewidth=.15, , show.legend=FALSE) +
  stat_slab(data=filter(persistence_marg_pred, ecotype=="Iteroparous"), alpha=palpha,
               side="right", normalize="groups", scale=pscale*.75, , color=NA) +
  geom_dots(data=filter(persistence_raw_proportions, ecotype=="Iteroparous"), color="black",
            side="right", scale=pscale*.75, binwidth=unit(c(-Inf, pdot*.5), "npc"),
            linewidth=.15, show.legend=FALSE) +
  scale_y_continuous(name="Persistence\nProportion", limits=c(0,1)) +
  scale_x_discrete(name="Water Potential (MPa)") +
  scale_color_manual(aesthetics=c("fill", "color"), values=ecotype_colors) +
  facet_grid(~ cold_stratification) +
  theme(legend.key.size=unit(.1, 'cm'),
        legend.text=element_text(size=6, margin=margin(r=30, unit="pt")),
        legend.title=element_blank(),
        legend.position="inside",
        legend.position.inside=c(.987,.93),
        axis.title.y=element_text(angle=0, vjust=0.5)
  )

## Fig 1B: Contrasts between ecotypes
### Create data.frame for text that will show direction of relationship
fig1B_text <- data.frame(
  water_potential=3.8,
  diff_p=c(-0.65, 0.65),
  label=c("More\nSemelparous\nPersistence", "More\nIteroparous\nPersistence"),
  ecotype=factor(c("Semelparous", "Iteroparous")),
  cold_stratification="Cold stratified"
  )
### Create data.frame for arrows that will show direction of relationship
fig1B_arrow <- data.frame(
  water_potential=3.9,
  diff_p=c(-0.4, 0.4),
  yend=c(-.9,.9),
  ecotype=factor(c("Semelparous", "Iteroparous")),
  cold_stratification="Cold stratified"
)

### Find proportions of distribution above and below zero
fig1B_dist_percents <- persistence_marg_pred %>%
  arrange(ecotype) %>%
  group_by(.draw, cold_stratification, water_potential) %>%
  summarize(diff_p=diff(p), .groups="drop") %>%
  group_by(cold_stratification, water_potential) %>%
  summarize(
    above=mean(diff_p>0),
    below=mean(diff_p<0),
    .groups="drop"
  ) %>%
  pivot_longer(cols=c(above, below), names_to="response", values_to="label") %>%
  mutate(
    water_potential=case_when(
      water_potential=="-1" ~ 1.08,
      water_potential=="-0.5" ~ 2.08,
      water_potential=="0" ~ 3.08
    ),
    label=paste0(format(round(label*100, 1), nsmall=1), "%"),
    label=str_replace(label, " ", ""),
    diff_p=c(0.15, -0.18, 0.1, -0.8, 0.1, -0.8, 0.15, -0.15, 0.12, -0.12, 0.28, -0.12)
    )

### Plot Fig 1B
fig1B <- persistence_marg_pred %>%
  arrange(ecotype) %>%
  group_by(.draw, cold_stratification, water_potential) %>%
  summarize(diff_p=diff(p), .groups="drop") %>%
  ggplot(aes(x=water_potential, y=diff_p)) +
  geom_vline(xintercept=c("-1", "-0.5", "0"), color="black", linetype=2) +
  geom_hline(yintercept=0, linetype=2, color="black") +
  stat_slab(aes(fill=after_stat(y>0)), alpha=palpha, normalize="groups") +
  geom_text(data=fig1B_dist_percents, aes(label=label), family="serif", size=3, hjust=0,
            color=c(ecotype_colors[2], ecotype_colors[1], ecotype_colors[2], "white", ecotype_colors[2], "white",
                    ecotype_colors[2], ecotype_colors[1], "white", "white", ecotype_colors[2], ecotype_colors[1])
            ) +
  geom_text(data=fig1B_text, aes(label=label, color=ecotype), family="serif",
            size=2, lineheight=.75, hjust=1) +
  geom_segment(data=fig1B_arrow, aes(yend=yend, color=ecotype),
               arrow=arrow(length=unit(0.2, "cm"))) +
  scale_y_continuous(name="Difference\nbetween\nEcotypes", limits=c(-1,1)) +
  scale_x_discrete(name="Water Potential (MPa)") +
  scale_fill_manual(values=setNames(ecotype_colors, c(FALSE, TRUE))) +
  scale_color_manual(values=ecotype_colors) +
  facet_grid(~ fct_relevel(cold_stratification, "Not cold stratified")) +
  theme(legend.position="none",
        axis.title.y=element_text(angle=0, vjust=0.5))


## Fig 1C: Effects of cold stratification
fig1C_df <- persistence_marg_pred %>%
  arrange(cold_stratification) %>%
  group_by(.draw, ecotype, water_potential) %>%
  summarize(diff_p=diff(p), .groups="drop")

### Find proportions of distribution above and below zero
fig1C_dist_percents <- fig1C_df %>%
  group_by(ecotype, water_potential) %>%
  summarize(
    above=mean(diff_p>0),
    below=mean(diff_p<0),
    .groups="drop"
  ) %>%
  pivot_longer(cols=c(above, below), names_to="response", values_to="label") %>%
  mutate(
    label=paste0(format(round(label*100, 1), nsmall=1), "%"),
    label=str_replace(label, "  ", ""),
    label=str_replace(label, " ", ""),
    water_potential=case_when(
      ecotype=="Semelparous"&water_potential=="-1" ~ 0.95,
      ecotype=="Semelparous"&water_potential=="-0.5" ~ 1.95,
      ecotype=="Semelparous"&water_potential=="0" ~ 2.95,
      ecotype=="Iteroparous"&water_potential=="-1" ~ 1.05,
      ecotype=="Iteroparous"&water_potential=="-0.5" ~ 2.05,
      ecotype=="Iteroparous"&water_potential=="0" ~ 3.05
    ),
    diff_p=c(0.15, -0.15, 0.15, -0.15, 0.15, -0.75, 0.17, -0.15, 0.35, -0.15, 0.22, -0.15),
    hjust=case_when(
      ecotype=="Semelparous" ~ 1,
      ecotype=="Iteroparous" ~ 0
    )
  )

### Create data.frame for text that will show direction of relationships
fig1C_text <- data.frame(
  water_potential=.1,
  diff_p=c(0.65, -0.65),
  label=c("Stratification\nIncreased\nPersistence", "Stratification\nDecreased\nPersistence"),
  ecotype=factor(c("Semelparous", "Iteroparous"))
)

### Create data.frame for arrows that will show direction of relationship
fig1C_arrow <- data.frame(
  water_potential=0.075,
  diff_p=c(-0.4, 0.4),
  yend=c(-.9,.9),
  ecotype=factor(c("Semelparous", "Iteroparous"))
  )

### Plot Fig 1C
fig1C <- ggplot(mapping=aes(y=diff_p, x=water_potential, fill=ecotype)) +
  geom_vline(xintercept=c("-1", "-0.5", "0"), color="black", linetype=2) +
  geom_hline(yintercept=0, linetype=2, color="black") +
  stat_slab(data=filter(fig1C_df, ecotype=="Semelparous"), alpha=palpha,
           side="left", scale=pscale*.75, normalize="groups") +
  stat_slab(data=filter(fig1C_df, ecotype=="Iteroparous"), alpha=palpha,
           side="right", scale=pscale*.75, normalize="groups") +
  geom_text(data=fig1C_dist_percents, aes(label=label), hjust=fig1C_dist_percents$hjust, family="serif", size=3,
            color=c(ecotype_colors[1], ecotype_colors[1], ecotype_colors[1], "white", ecotype_colors[1], "white",
                    ecotype_colors[2], ecotype_colors[2], "white", ecotype_colors[2], ecotype_colors[2], ecotype_colors[2])
            ) +
  geom_segment(data=fig1C_arrow, aes(yend=yend), color="black",
               arrow=arrow(length=unit(0.2, "cm"))) +
  geom_text(data=fig1C_text, aes(label=label), color="black", family="serif",
            size=2, lineheight=.75, hjust=0) +
  scale_y_continuous(name="Effect of Cold\nStratification", limits=c(-1,1)) +
  scale_x_discrete(name="Water Potential (MPa)") +
  scale_fill_manual(values=ecotype_colors) +
  theme(legend.key.size=unit(.1, 'cm'),
        legend.text=element_text(size=6, margin=margin(r=30, unit="pt")),
        legend.title=element_blank(),
        legend.position="inside",
        legend.position.inside=c(.985,.94),
        axis.title.y=element_text(angle=0, vjust=0.5)
  )

## Write Figure 1
png("figures/figure1.png", width=7, height=8, units="in", res=1500)
fig1A + fig1B + fig1C +
  plot_layout(ncol=1) +
  plot_annotation(tag_levels="a")
dev.off()

# Fig 2 -----------------------------------------------------------------------
# Within and between population variation in persistence in response to treatments
## Wrangle raw data into persistence proportions
persistence_raw_proportions <- germ_df %>%
  group_by(site, seed_family, cold_stratification, water_potential, ecotype) %>%
  summarize(p=num_persisting_seeds/num_viable_seeds,
            .groups="drop") %>%
  arrange(cold_stratification) %>%
  group_by(seed_family, water_potential) %>%
  mutate(
    slope=case_when(
      length(p)!=2 ~ NA,
      diff(p)<0 ~ "neg",
      diff(p)>0 ~ "pos",
      TRUE ~ "zero"
    ),
  ) %>% ungroup()

## Setting some plotting parameters
pdot <- 0.0225
pscale <- 0.75
palpha <- .9
cold_strat_effect_colors <- c("pos"="red4", "neg"="skyblue3", "zero"="grey50")

## Sample spaghetti lines to show model prediction of effects of cold stratification on individual seed families
draws_sample <- sample(1:max(persistence_cond_pred$.draw), 100)
persistence_cond_spag <- persistence_cond_pred %>%
  mutate(water_potential=fct_rev(water_potential)) %>%
  arrange(cold_stratification) %>%
  group_by(ecotype:water_potential) %>%
  group_by(.draw, seed_family, water_potential) %>%
  mutate(
    diff_p=diff(p),
    slope=case_when(
      diff_p<0 ~ "neg",
      diff_p>0 ~ "pos",
      TRUE ~ "zero"
      )
    ) %>% ungroup() %>%
  filter(.draw %in% draws_sample)

## Plot Fig S1
fig2 <- ggplot(mapping=aes(x=cold_stratification, y=p, fill=ecotype)) +
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
  scale_y_continuous(name="Probability of Persistence", limits=c(0,1)) +
  scale_fill_manual(values=ecotype_colors) +
  facet_grid(water_potential ~ site, labeller=labeller(water_potential=water_potential_labeller)) +
  theme(legend.key.size=unit(.05, 'cm'),
        legend.text=element_text(size=6),
        legend.title=element_blank(),
        legend.position="inside",
        legend.position.inside=c(.96,.0319),
        legend.box.background=element_rect(color="white"),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_text(size=8))

## Write Fig S1
png("figures/figure2.png", width=10, height=6, units="in", res=1000)
fig2
dev.off()

# Fig S2 -----------------------------------------------------------------------
viability_raw_proportions <- germ_df %>%
  group_by(site, seed_family, cold_stratification, water_potential, ecotype) %>%
  summarize(p=num_viable_seeds/(num_viable_seeds+num_inviable_seeds),
            .groups="drop")

## Setting some plotting parameters
s2dot <- 0.01
s2scale <- 0.5
s2alpha <- .9

## Plot Fig S2
figS2 <- ggplot(mapping=aes(x=water_potential, y=p, fill=ecotype, color=ecotype)) +
  geom_vline(xintercept=c("-1", "-0.5", "0"), color="black", linetype=2) +
  stat_slab(data=filter(viability_pred, ecotype=="Semelparous"), alpha=s2alpha,
               side="left", normalize="groups", scale=s2scale) +
  geom_dots(data=filter(viability_raw_proportions, ecotype=="Semelparous"), color="black",
            side="left", scale=s2scale, binwidth=unit(c(-Inf, s2dot), "npc"),
            linewidth=.15, , show.legend=FALSE) +
  stat_slab(data=filter(viability_pred, ecotype=="Iteroparous"), alpha=s2alpha,
               side="right", normalize="groups", scale=s2scale) +
  geom_dots(data=filter(viability_raw_proportions, ecotype=="Iteroparous"), color="black",
            side="right", scale=s2scale, binwidth=unit(c(-Inf, s2dot), "npc"),
            linewidth=.15, show.legend=FALSE) +
  scale_y_continuous(name="Viable\nProportion", limits=c(0,1)) +
  scale_x_discrete(name="Water Potential (MPa)") +
  scale_color_manual(aesthetics=c("fill", "color"), values=ecotype_colors) +
  facet_grid(~ cold_stratification) +
  theme(legend.key.size=unit(.1, 'cm'),
        legend.text=element_text(size=6, margin=margin(r=30, unit="pt")),
        legend.title=element_blank(),
        legend.position="inside",
        legend.position.inside=c(.987,0.05),
        axis.title.y=element_text(angle=0, vjust=0.5)
  )

## Write Fig S2
png("figures/figureS2.png", width=8, height=4, units="in", res=1500)
figS2
dev.off()

# Fig S3 -----------------------------------------------------------------------
# Timing of germination

## Set some plotting parameters
tdot <- 0.01
tscale <- 0.5

# Plot Fig S3
figS3 <- ggplot(mapping=aes(y=water_potential, x=Day, fill=ecotype)) +
  geom_hline(yintercept=c("0", "-0.5", "-1"), color="black", linetype=2) +
  stat_slab(data=filter(time_pred, ecotype=="Semelparous"), side="top", normalize="groups", scale=tscale, alpha=palpha) +
  geom_dots(data=filter(time_df, ecotype=="Semelparous"), color="black", side="top", scale=tscale, binwidth=unit(c(-Inf, tdot), "npc"),  linewidth=.05) +
  stat_slab(data=filter(time_pred, ecotype=="Iteroparous"), side="bottom", normalize="groups", scale=tscale, alpha=palpha) +
  geom_dots(data=filter(time_df, ecotype=="Iteroparous"), color="black", side="bottom", scale=tscale, binwidth=unit(c(-Inf, tdot), "npc"), linewidth=.05) +
  scale_x_continuous(name="Day of Germination", limits=c(0,25)) +
  scale_y_discrete(name="Water Potential (MPa)") +
  scale_fill_manual(values=ecotype_colors) +
  facet_wrap(~ cold_stratification, ncol=1) +
  theme(legend.key.size=unit(.4, 'cm'),
        legend.text=element_text(size=9),
        legend.title=element_blank(),
        legend.position="inside",
        legend.position.inside=c(.85,.96)
  )

# Write Fig S3
png("figures/figureS3.png", width=4, height=5, units="in", res=1500)
figS3
dev.off()
