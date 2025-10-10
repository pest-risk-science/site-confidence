library(akima)
library(viridis)
library(ggplot2)
library(ggridges)
library(ggpubr)
library(khroma)

### Run analysis.R first
figures_dir <- file.path(root_dir, "figures")

khroma_palette <- colour("bright")(length(unique(var_imp_df$Model)))


trap_labs <- c("1 trap","3 traps","5 traps", "10 traps")
names(trap_labs) <- c(1,3,5,10)

lure_labs <- c("\u03bb = 7", "\u03bb = 14", "\u03bb = 25", "\u03bb = 36", "\u03bb = 50")
names(lure_labs) <- c(7,14,25,36,50)

########################
# Pest simulation plot #
########################

cluster_dat <- simulate_clustered_poisson(total_points=100,
                           lambda_parent=3)
rand_dat <- data.frame(x=runif(100,0,316.2278),y=runif(100,0,316.2278))


png(filename = file.path(figures_dir, "example_simulation.png"),
    width = 7, height = 4, units = "in", res=400)
par(mfrow=c(1,2),mar=c(1,1,2,0))
plot(rand_dat,xlim=c(0,316.2278),ylim=c(0,316.2278),pch=16,xaxt="n",yaxt="n",
     xlab="",ylab="",frame=FALSE,
     main="Non-clustered simulation")
rect(xleft = 0, xright=316.2278,ybottom=0,ytop=316.2278)
plot(cluster_dat,xlim=c(0,316.2278),ylim=c(0,316.2278),pch=16,xaxt="n",yaxt="n",
     xlab="",ylab="",frame=FALSE,
     main="Simulation with 3 clusters")
rect(xleft = 0, xright=316.2278,ybottom=0,ytop=316.2278)
dev.off()

###################
# Validation Plot #
###################
valid_plot2 <- ggplot(meta_df2) +
  geom_point(aes(x=p, y=p_hat, color=species, shape=as.factor(grid_d)), size=4) +
  geom_errorbar(aes(x=p,ymin=p_lb,ymax=p_ub, color=species),size=1) +
  geom_abline(slope=1,intercept=0) +
  scale_colour_bright() +
  guides(shape=guide_legend("Grid res (m)"),
         color=guide_legend("Species")) +
  labs(x = "True proportion recaptured", y = "Estimated proportion recaptured" #title = "(a)",
       ) +
  theme_bw() +
  theme(text=element_text(size=20))
valid_plot2
ggsave(filename = file.path(figures_dir, "valid_plot.png"), width = 7, height = 5)


#valid_plot <- ggplot(meta_df) +
#  geom_point(aes(x=p, y=p_hat, color=species, shape=as.factor(grid_d)), size=4) +
#  geom_errorbar(aes(x=p,ymin=p_lb,ymax=p_ub, color=species),size=1) +
#  geom_abline(slope=1,intercept=0) +
#  scale_colour_bright() +
#  guides(shape=guide_legend("Grid res (m)"),
#         color=guide_legend("Species")) +
#  labs(x = "True proportion recaptured", y = " ",
#       title = "(b)") +
#  theme_bw() +
#  theme(text=element_text(size=20))
#valid_plot

ggpubr::ggarrange(valid_plot2, common.legend = TRUE, #valid_plot,
                  legend = "bottom")
ggsave(filename = file.path(figures_dir, "valid_plot_combined.png"), width = 5, height = 5)


######################
# Random Forest Plot #
######################
rf_plot <- ggplot(var_imp_df, aes(x=reorder(Variable,value), y=value, fill=Model, order=Model)) +
  geom_bar(stat="identity", position = position_dodge(-.9)) +
  scale_y_continuous(breaks=c(.02,.04,.06,.08,.1,.12)) +
  scale_fill_manual(values = khroma_palette) +
  coord_flip() +
  ylab("Variable Importance") +
  xlab("") +
  theme_bw() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        text=element_text(size=20))
rf_plot
ggsave(filename = file.path(figures_dir, "rf_plot.png"), width = 10, height = 8)


##################
# Step Size Plot #
##################

step_size_df <- res_df %>%
  filter(n_traps == 3,
         lure_attract == 14,
         same_spot == FALSE,
         attract_area == FALSE) %>%
  dplyr::select(num_pests,step_size,ftw1,cat_ftw1) %>%
  dplyr::mutate(pests_per_ha = num_pests/10)

step_size_df_clust <- res_df_clust %>%
  filter(n_traps == 3,
         lure_attract == 14,
         num_clust == 2) %>%
  dplyr::select(num_pests,step_size,ftw1,cat_ftw1) %>%
  dplyr::mutate(pests_per_ha = num_pests/10)


ss_1 <- step_size_df %>%
  summarise(ftw1_mn = mean(ftw1),
            ftw1_lb = quantile(ftw1,.05),
            ftw1_ub = quantile(ftw1,.95),
            .by = c(pests_per_ha,step_size)) %>%
  mutate(step_size = as.factor(step_size)) %>%
  ggplot(aes(x=pests_per_ha,y=ftw1_mn,color=step_size)) +
  geom_line(size=2) +
  geom_ribbon(aes(ymin=ftw1_lb,ymax=ftw1_ub,fill=step_size,color=step_size),
              alpha=.2) +
  scale_color_bright()+
  scale_fill_bright() +
  xlab("True pest prevalence (pests per hectare)") +
  ylab("Trap catch (P/T/W)") +
  labs(fill = "Step size (m)", color = "Step size (m)") +
  theme_bw() +
  ggtitle("(b)")

ss_2 <- step_size_df %>%
  filter(ftw1 <= 100) %>%
  mutate(cat_ftw1 = factor(cat_ftw1,
                           levels=c("negligible", "very low", "low", "moderate", "high")),
         step_size = as.factor(step_size)) %>%
  ggplot(aes(x=cat_ftw1)) +
  #geom_density_ridges(aes(x=num_pests, fill = step_size),alpha=.5) +
  geom_boxplot(aes(y=pests_per_ha, fill = step_size), outlier.alpha = 0.01, outlier.size = 0.8) +
  scale_color_bright()+
  scale_fill_bright() +
  ylab("True pest prevalence (pests per hectare)") +
  xlab("Trap catch (P/T/W)") +
  theme_bw() +
  labs(fill = "Step size (m)") +
  ggtitle("(a)")

ss_3 <- step_size_df_clust %>%
  summarise(ftw1_mn = mean(ftw1),
            ftw1_lb = quantile(ftw1,.05),
            ftw1_ub = quantile(ftw1,.95),
            .by = c(pests_per_ha,step_size)) %>%
  mutate(step_size = as.factor(step_size)) %>%
  ggplot(aes(x=pests_per_ha,y=ftw1_mn,color=step_size)) +
  geom_line(size=2) +
  geom_ribbon(aes(ymin=ftw1_lb,ymax=ftw1_ub,fill=step_size,color=step_size),
              alpha=.2) +
  scale_color_bright()+
  scale_fill_bright() +
  xlab("True pest prevalence (pests per hectare)") +
  ylab("Trap catch (P/T/W)") +
  labs(fill = "Step size (m)", color = "Step size (m)") +
  theme_bw() +
  ggtitle("Clustered Case") +
  ggtitle("(d)")

ss_4 <- step_size_df_clust %>%
  filter(ftw1 <= 100) %>%
  mutate(cat_ftw1 = factor(cat_ftw1,
                           levels=c("negligible", "very low", "low", "moderate", "high")),
         step_size = factor(step_size,levels=c(3,7,20))) %>%
  ggplot(aes(x=cat_ftw1)) +
  #geom_density_ridges(aes(x=num_pests, fill = step_size),alpha=.5) +
  geom_boxplot(aes(y=pests_per_ha, fill = step_size), outlier.alpha = 0.01, outlier.size = 0.8) +
  scale_color_bright()+
  scale_fill_bright() +
  ylab("True pest prevalence (pests per hectare)") +
  xlab("Trap catch (P/T/W)") +
  theme_bw() +
  labs(fill = "Step size (m)") +
  ggtitle("(c)")

ggarrange(ss_2,ss_1,ss_4,ss_3,nrow=2,ncol=2)
ggsave(filename = file.path(figures_dir, "step_size_plot.png"), width = 10, height = 8)




########################
# Trap Parameter Plots #
########################

trap_df <- res_df %>%
  filter(same_spot == FALSE,
         attract_area == FALSE) %>%
  dplyr::select(num_pests, n_traps, step_size, lure_attract, ftw1, cat_ftw1) %>%
  dplyr::mutate(pests_per_ha = num_pests/10)



t1 <- trap_df %>%
  summarise(ftw1_mn = mean(ftw1),
            ftw1_lb = quantile(ftw1,.05),
            ftw1_ub = quantile(ftw1,.95),
            .by = c(pests_per_ha,n_traps,lure_attract)) %>%
  mutate(n_traps = as.factor(n_traps)) %>%
  mutate(lure_attract = as.factor(lure_attract)) %>%
  ggplot(aes(x=pests_per_ha, y=ftw1_mn, color=lure_attract)) +
  geom_line(size=2) +
  geom_ribbon(aes(ymin=ftw1_lb,ymax=ftw1_ub,fill=lure_attract,color=lure_attract),
              alpha=.2) +
  scale_color_bright()+
  scale_fill_bright() +
  facet_grid(~n_traps, labeller = labeller(n_traps = trap_labs)) +
  xlab("True pest prevalence (pests per hectare)") +
  ylab("Trap catch (P/T/W)") +
  labs(fill = "Lure attractiveness (\u03bb)", color = "Lure attractiveness (\u03bb)") +
  theme_bw() +
  theme(legend.position = "bottom")

t2 <- trap_df %>%
  summarise(ftw1_mn = mean(ftw1),
            ftw1_lb = quantile(ftw1,.05),
            ftw1_ub = quantile(ftw1,.95),
            .by = c(pests_per_ha,n_traps,lure_attract)) %>%
  mutate(n_traps = as.factor(n_traps)) %>%
  mutate(lure_attract = as.factor(lure_attract)) %>%
  ggplot(aes(x=pests_per_ha, y=ftw1_mn, color=n_traps)) +
  geom_line(size=2) +
  geom_ribbon(aes(ymin=ftw1_lb,ymax=ftw1_ub,fill=n_traps,color=n_traps),
              alpha=.2) +
  scale_color_bright()+
  scale_fill_bright() +
  facet_grid(~lure_attract) +
  xlab("Pests per hectare") +
  ylab("P/T/W") +
  labs(fill = "Number of traps", color="Number of traps") +
  theme_bw() +
  theme(legend.position = "bottom")

t3 <- trap_df %>%
  filter(ftw1 <= 100) %>%
  mutate(cat_ftw1 = factor(cat_ftw1,
                           levels=c("negligible", "very low", "low", "moderate", "high"))) %>%
  mutate(cat_ftw1 = replace(cat_ftw1, cat_ftw1=="very low","low")) %>%
  mutate(n_traps = as.factor(n_traps)) %>%
  mutate(lure_attract = as.factor(lure_attract)) %>%
  ggplot(aes(x=cat_ftw1, y=pests_per_ha, fill=lure_attract)) +
  geom_boxplot(outlier.alpha = .01, outlier.size = .8) +
  #geom_boxplot(outlier.shape = NA) +
  scale_color_bright()+
  scale_fill_bright() +
  facet_grid(~n_traps, labeller = labeller(n_traps = trap_labs)) +
  ylab("True pest prevalence (pests per hectare)") +
  xlab("Trap catch (P/T/W)") +
  labs(fill = "Lure attractiveness (\u03bb)") +
  theme_bw() +
  theme(legend.position = "bottom")


t4 <- trap_df %>%
  mutate(cat_ftw1 = factor(cat_ftw1,
                           levels=c("negligible", "very low", "low", "moderate", "high"))) %>%
  mutate(cat_ftw1 = replace(cat_ftw1, cat_ftw1=="very low","low")) %>%
  mutate(n_traps = as.factor(n_traps)) %>%
  mutate(lure_attract = as.factor(lure_attract)) %>%
  ggplot(aes(y=cat_ftw1, x=pests_per_ha, fill=n_traps)) +
  geom_boxplot() +
  #geom_boxplot(outlier.shape = NA) +
  scale_color_bright()+
  scale_fill_bright() +
  facet_grid(~lure_attract) +
  xlab("Pests per hectare") +
  ylab("P/T/W") +
  labs(fill = "Number of traps") +
  theme_bw() +
  theme(legend.position = "bottom")

#ggpubr::ggarrange(t1,t3,t4,nrow=3)
#ggsave(filename = file.path(figures_dir, "traps_plot.png"), width = 11, height = 11)

#ggpubr::ggarrange(t2,t4,nrow=2)
#ggsave(filename = file.path(figures_dir, "lure_plot.png"), width = 11, height = 9)

ggpubr::ggarrange(t1)
ggsave(filename = file.path(figures_dir, "traps_plot.png"), width = 11, height = 5)

ggpubr::ggarrange(t3)
ggsave(filename = file.path(figures_dir, "traps_cat_plot.png"), width = 11, height = 5)



t5 <- trap_df %>%
  summarise(ftw1_mn = mean(ftw1),
            ftw1_lb = quantile(ftw1,.05),
            ftw1_ub = quantile(ftw1,.95),
            .by = c(pests_per_ha,n_traps,lure_attract)) %>%
  mutate(n_traps = as.factor(n_traps)) %>%
  filter(pests_per_ha == 5) %>%
  ggplot(aes(x=lure_attract, y = ftw1_mn, group = n_traps,
             color=n_traps)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=ftw1_lb,ymax=ftw1_ub,fill=n_traps,color=n_traps),
              alpha=.2) +
  scale_color_bright()+
  scale_fill_bright() +
  xlab("Lure attractiveness (\u03bb)") +
  ylab("Trap catch (P/T/W)") +
  labs(fill = "Number of traps", color="Number of traps") +
  theme_bw() +
  theme(legend.position = "bottom")

t5
ggsave(filename = file.path(figures_dir, "ptw_vs_lure_plot.png"),width=6,height=6)


# Closest points on line to 20 PTW
trap_sum_df <-  trap_df %>%
  summarise(ftw1_mn = mean(ftw1),
            ftw1_lb = quantile(ftw1,.05),
            ftw1_ub = quantile(ftw1,.95),
            .by = c(num_pests,n_traps,lure_attract)) %>%
  mutate(n_traps = as.factor(n_traps))

closest_points <- trap_sum_df %>%
  group_by(n_traps, lure_attract) %>%
  slice_min(abs(ftw1_mn - 20), n = 1) %>%
  ungroup()

closest_points2 <- trap_sum_df %>%
  group_by(n_traps, lure_attract) %>%
  slice_min(abs(num_pests - 100), n = 1) %>%
  ungroup()


t1_ <- trap_df %>%
  summarise(pests_mn = mean(num_pests),
            pests_lb = quantile(num_pests,.05),
            pests_ub = quantile(num_pests,.95),
            .by = c(ftw1,n_traps,lure_attract)) %>%
  mutate(n_traps = as.factor(n_traps)) %>%
  mutate(lure_attract = as.factor(lure_attract)) %>%
  ggplot(aes(y=ftw1, x=pests_mn, color=lure_attract)) +
  geom_line(size=2) +
  geom_ribbon(aes(xmin=pests_lb,xmax=pests_ub,fill=lure_attract,color=lure_attract),
              alpha=.2) +
  facet_grid(~n_traps) +
  xlab("Number of pests") +
  ylab("P/T/W") +
  labs(fill = "Lure attractiveness", color = "Lure attractiveness") +
  theme_bw() +
  theme(legend.position = "bottom")
####

trap_df %>%
  summarise(ftw1_mn = mean(ftw1),
            ftw1_lb = quantile(ftw1,.05),
            ftw1_ub = quantile(ftw1,.95),
            .by = c(num_pests,n_traps,lure_attract)) %>%
  mutate(n_traps = as.factor(n_traps)) %>%
  filter(num_pests == 100) %>%
  ggplot(aes(x=lure_attract, y = ftw1_mn, group = n_traps,
             color=n_traps)) +
  geom_point() +
  theme_bw()



t1_p <- trap_df %>%
  filter(num_pests > 0) %>%
  summarise(ftw1_mn = mean(ftw1/num_pests*n_traps),
            ftw1_lb = quantile(ftw1/num_pests*n_traps,.05),
            ftw1_ub = quantile(ftw1/num_pests*n_traps,.95),
            .by = c(pests_per_ha,n_traps,lure_attract)) %>%
  mutate(n_traps = as.factor(n_traps)) %>%
  mutate(lure_attract = as.factor(lure_attract)) %>%
  ggplot(aes(x=pests_per_ha, y=ftw1_mn, color=lure_attract)) +
  geom_line(size=2) +
  geom_ribbon(aes(ymin=ftw1_lb,ymax=ftw1_ub,fill=lure_attract,color=lure_attract),
              alpha=.2) +
  scale_color_bright()+
  scale_fill_bright() +
  facet_grid(~n_traps, labeller = labeller(n_traps = trap_labs)) +
  xlab("True pest prevalence (pests per hectare)") +
  ylab("Trap catch (P/T/W)") +
  labs(fill = "Lure attractiveness (\u03bb)", color = "Lure attractiveness (\u03bb)") +
  theme_bw() +
  theme(legend.position = "bottom")

t1_p



################
# Cluster Plot #
################

c1 <- res_df_clust %>%
  filter(step_size==3, lure_attract==14) %>%
  filter(num_clust%in%c(1,3,5)) %>%
  mutate(pests_per_ha = num_pests/10) %>%
  summarise(ftw1_mn = mean(ftw1),
            ftw1_lb = quantile(ftw1,.05),
            ftw1_ub = quantile(ftw1,.95),
            .by = c(pests_per_ha,n_traps,num_clust, lure_attract)) %>%
  mutate(n_traps = as.factor(n_traps)) %>%
  mutate(lure_attract = as.factor(lure_attract)) %>%
  mutate(num_clust = as.factor(num_clust)) %>%
  ggplot(aes(x=pests_per_ha, y=ftw1_mn, color=num_clust)) +
  geom_line(size=2) +
  geom_ribbon(aes(ymin=ftw1_lb,ymax=ftw1_ub,fill=num_clust,color=num_clust),
              alpha=.2) +
  scale_color_bright()+
  scale_fill_bright() +
  facet_grid(~n_traps, labeller = labeller(n_traps = trap_labs))  +
  xlab("True pest prevalence (pests per hectare)") +
  ylab("Trap catch (P/T/W)") +
  labs(fill = "Number of pest clusters", color = "Number of pest clusters") +
  theme_bw() +
  theme(legend.position = "bottom")

c2 <- res_df_clust %>%
  filter(ftw1 <= 100) %>%
  filter(step_size==3, lure_attract==14) %>%
  filter(num_clust%in%c(1,3,5)) %>%
  mutate(pests_per_ha = num_pests/10) %>%
  mutate(cat_ftw1 = factor(cat_ftw1,
                           levels=c("negligible", "very low", "low", "moderate", "high"))) %>%
  mutate(cat_ftw1 = replace(cat_ftw1, cat_ftw1=="very low","low")) %>%
  mutate(n_traps = as.factor(n_traps),
         lure_attract = as.factor(lure_attract),
         num_clust = as.factor(num_clust)) %>%
  ggplot(aes(x=cat_ftw1, y=pests_per_ha, fill=num_clust)) +
  geom_boxplot(outlier.alpha = .01, outlier.size = .8) +
  #geom_boxplot(outlier.shape = NA) +
  facet_grid(~n_traps, labeller = labeller(n_traps = trap_labs))  +
  scale_color_bright()+
  scale_fill_bright() +
  ylab("True pest prevalence (pests per hectare)") +
  xlab("Trap catch (P/T/W)") +
  labs(fill = "Number of pest clusters") +
  theme_bw() +
  theme(legend.position = "bottom")


c1_append <- res_df_clust %>%
  filter(step_size==3) %>%
  mutate(pests_per_ha = num_pests/10) %>%
  summarise(ftw1_mn = mean(ftw1),
            ftw1_lb = quantile(ftw1,.05),
            ftw1_ub = quantile(ftw1,.95),
            .by = c(pests_per_ha,n_traps,num_clust, lure_attract)) %>%
  mutate(n_traps = as.factor(n_traps)) %>%
  mutate(lure_attract = as.factor(lure_attract)) %>%
  mutate(num_clust = as.factor(num_clust)) %>%
  ggplot(aes(x=pests_per_ha, y=ftw1_mn, color=num_clust)) +
  geom_line(size=2) +
  geom_ribbon(aes(ymin=ftw1_lb,ymax=ftw1_ub,fill=num_clust,color=num_clust),
              alpha=.2) +
  facet_grid(lure_attract~n_traps, labeller = labeller(n_traps = trap_labs,
                                                       lure_attract=lure_labs)) +
  scale_color_bright()+
  scale_fill_bright() +
  ylab("True pest prevalence (pests per hectare)") +
  xlab("Trap catch (P/T/W)") +
  labs(fill = "Number of pest clusters", color = "Number of pest clusters") +
  theme_bw() +
  theme(legend.position = "bottom")

c2_append <- res_df_clust %>%
  filter(ftw1 <= 100) %>%
  filter(step_size==3) %>%
  mutate(pests_per_ha = num_pests/10) %>%
  mutate(cat_ftw1 = factor(cat_ftw1,
                           levels=c("negligible", "very low", "low", "moderate", "high"))) %>%
  mutate(cat_ftw1 = replace(cat_ftw1, cat_ftw1=="very low","low")) %>%
  mutate(n_traps = as.factor(n_traps),
         lure_attract = as.factor(lure_attract),
         num_clust = as.factor(num_clust)) %>%
  ggplot(aes(x=cat_ftw1, y=pests_per_ha, fill=num_clust)) +
  geom_boxplot(outlier.alpha = .01, outlier.size = .8) +
  scale_color_bright()+
  scale_fill_bright() +
  #geom_boxplot(outlier.shape = NA) +
  facet_grid(lure_attract~n_traps, labeller = labeller(n_traps = trap_labs,
                                                       lure_attract=lure_labs)) +
  ylab("True pest prevalence (pests per hectare)") +
  xlab("Trap catch (P/T/W)") +
  labs(fill = "Number of clusters") +
  theme_bw() +
  theme(legend.position = "bottom")


ggpubr::ggarrange(c1)
ggsave(filename = file.path(figures_dir, "clust_plot.png"), width = 11, height = 5)

ggpubr::ggarrange(c2)
ggsave(filename = file.path(figures_dir, "clust_cat_plot.png"), width = 11, height = 5)

c1_append
ggsave(filename = file.path(figures_dir, "clust1_append_plot.png"), width = 11, height = 9)

c2_append
ggsave(filename = file.path(figures_dir, "clust2_append_plot.png"), width = 11, height = 9)







ggpubr::ggarrange(t1,c1,nrow=2, labels = c("(a)","(b)"))
ggsave(filename = file.path(figures_dir, "line_plots.png"), width = 11, height = 7)




























###
#
####
single_out_df <- res_df %>%
  filter(n_traps == 3,
         lure_attract == 14,
         same_spot == TRUE,
         attract_area == FALSE) %>%
  dplyr::select(num_pests,step_size,ftw1,cat_ftw1)



single_out_df %>%
  summarise(ftw1_mn = mean(ftw1),
            ftw1_lb = quantile(ftw1,.05),
            ftw1_ub = quantile(ftw1,.95),
            .by = c(num_pests,step_size)) %>%
  mutate(step_size = as.factor(step_size)) %>%
  ggplot(aes(x=num_pests,y=ftw1_mn,color=step_size)) +
  geom_line(size=2) +
  geom_ribbon(aes(ymin=ftw1_lb,ymax=ftw1_ub,fill=step_size,color=step_size),
              alpha=.2) +
  xlab("Number of pests") +
  ylab("P/T/W")








# num_traps <- c(1,3,5,10)
# step_sizes <- c(5,20,43,50,62.6)
# lure_attract <- c(7,14,25,36,50)
# same_spots <- c(TRUE,FALSE)
# attract_areas <- c(TRUE,FALSE)
# negligible, very low, low, moderate, high

res_df_sub <- res_df[res_df$same_spot==FALSE,]

df_fig3 <- res_df_sub %>%
  filter(step_size %in% c(20,43,62.6)) %>%
  group_by(lure_attract, n_traps, step_size, cat_ftw1) %>%
  summarise(mn = mean(num_pests),
            ub = quantile(num_pests,.975))



#mn: 0, 810
#ub: 0,1000

############
# FIGURE 3 #
############

df_fig3a <- df_fig3 %>%
  filter(step_size == 20,
         cat_ftw1 == "negligible")
df_fig3a <- interp(x=df_fig3a$n_traps,y=df_fig3a$lure_attract,z=df_fig3a$mn,
                   nx=10,ny=20)
df_fig3a <- as.data.frame(interp2xyz(df_fig3a))

df_fig3b <- df_fig3 %>%
  filter(step_size == 43,
         cat_ftw1 == "negligible")
df_fig3b <- interp(x=df_fig3b$n_traps,y=df_fig3b$lure_attract,z=df_fig3b$mn,
                   nx=10,ny=20)
df_fig3b <- as.data.frame(interp2xyz(df_fig3b))

df_fig3c <- df_fig3 %>%
  filter(step_size == 62.6,
         cat_ftw1 == "negligible")
df_fig3c <- interp(x=df_fig3c$n_traps,y=df_fig3c$lure_attract,z=df_fig3c$mn,
                   nx=10,ny=20)
df_fig3c <- as.data.frame(interp2xyz(df_fig3c))


df_fig3d <- df_fig3 %>%
  filter(step_size == 20,
         cat_ftw1 %in% c("negligible", "very low"))
df_fig3d <- interp(x=df_fig3d$n_traps,y=df_fig3d$lure_attract,z=df_fig3d$mn,
                   nx=10,ny=20, duplicate = "mean")
df_fig3d <- as.data.frame(interp2xyz(df_fig3d))

df_fig3d$step_size <- 20
df_fig3d$cat_ftw1 <- "<= very low"

df_fig3e <- df_fig3 %>%
  filter(step_size == 43,
         cat_ftw1 %in% c("negligible", "very low"))
df_fig3e <- interp(x=df_fig3e$n_traps,y=df_fig3e$lure_attract,z=df_fig3e$mn,
                   nx=10,ny=20,  duplicate = "mean")
df_fig3e<- as.data.frame(interp2xyz(df_fig3b))

df_fig3e$step_size <- 43
df_fig3e$cat_ftw1 <- "<= very low"

df_fig3f <- df_fig3 %>%
  filter(step_size == 62.6,
         cat_ftw1 %in% c("negligible", "very low"))
df_fig3f <- interp(x=df_fig3f$n_traps,y=df_fig3f$lure_attract,z=df_fig3f$mn,
                   nx=10,ny=20,  duplicate = "mean")
df_fig3f <- as.data.frame(interp2xyz(df_fig3f))

df_fig3f$step_size <- 62.6
df_fig3f$cat_ftw1 <- "<= very low"

df_fig3g <- df_fig3 %>%
  filter(step_size == 20,
         cat_ftw1 == "low")
df_fig3g <- interp(x=df_fig3g$n_traps,y=df_fig3g$lure_attract,z=df_fig3g$mn,
                   nx=10,ny=20)
df_fig3g <- as.data.frame(interp2xyz(df_fig3g))

df_fig3g$step_size <- 20
df_fig3g$cat_ftw1 <- "low"

df_fig3h <- df_fig3 %>%
  filter(step_size == 43,
         cat_ftw1 == "low")
df_fig3h <- interp(x=df_fig3h$n_traps,y=df_fig3h$lure_attract,z=df_fig3h$mn,
                   nx=10,ny=20)
df_fig3h <- as.data.frame(interp2xyz(df_fig3h))

df_fig3h$step_size <- 43
df_fig3h$cat_ftw1 <- "low"

df_fig3i <- df_fig3 %>%
  filter(step_size == 62.6,
         cat_ftw1 == "low")
df_fig3i <- interp(x=df_fig3i$n_traps,y=df_fig3i$lure_attract,z=df_fig3i$mn,
                   nx=10,ny=20)
df_fig3i <- as.data.frame(interp2xyz(df_fig3i))

df_fig3i$step_size <- 62.6
df_fig3i$cat_ftw1 <- "low"

df_fig3j <- df_fig3 %>%
  filter(step_size == 20,
         cat_ftw1 == "moderate")
df_fig3j <- interp(x=df_fig3j$n_traps,y=df_fig3j$lure_attract,z=df_fig3j$mn,
                   nx=10,ny=20)
df_fig3j <- as.data.frame(interp2xyz(df_fig3j))

df_fig3j$step_size <- 20
df_fig3j$cat_ftw1 <- "moderate"

df_fig3k <- df_fig3 %>%
  filter(step_size == 43,
         cat_ftw1 == "moderate")
df_fig3k <- interp(x=df_fig3k$n_traps,y=df_fig3k$lure_attract,z=df_fig3k$mn,
                   nx=10,ny=20)
df_fig3k <- as.data.frame(interp2xyz(df_fig3k))

df_fig3k$step_size <- 43
df_fig3k$cat_ftw1 <- "moderate"

df_fig3l <- df_fig3 %>%
  filter(step_size == 62.6,
         cat_ftw1 == "moderate")
df_fig3l <- interp(x=df_fig3l$n_traps,y=df_fig3l$lure_attract,z=df_fig3l$mn,
                   nx=10,ny=20)
df_fig3l <- as.data.frame(interp2xyz(df_fig3l))

df_fig3l$step_size <- 62.6
df_fig3l$cat_ftw1 <- "moderate"

df_fig3m <- df_fig3 %>%
  filter(step_size == 20,
         cat_ftw1 == "high")
df_fig3m <- interp(x=df_fig3m$n_traps,y=df_fig3m$lure_attract,z=df_fig3m$mn,
                   nx=10,ny=20)
df_fig3m <- as.data.frame(interp2xyz(df_fig3m))

df_fig3m$step_size <- 20
df_fig3m$cat_ftw1 <- "high"

df_fig3n <- df_fig3 %>%
  filter(step_size == 43,
         cat_ftw1 == "high")
df_fig3n <- interp(x=df_fig3n$n_traps,y=df_fig3n$lure_attract,z=df_fig3n$mn,
                   nx=10,ny=20)
df_fig3n <- as.data.frame(interp2xyz(df_fig3n))

df_fig3n$step_size <- 43
df_fig3n$cat_ftw1 <- "high"

df_fig3o <- df_fig3 %>%
  filter(step_size == 62.6,
         cat_ftw1 == "high")
df_fig3o <- interp(x=df_fig3o$n_traps,y=df_fig3o$lure_attract,z=df_fig3o$mn,
                   nx=10,ny=20)
df_fig3o <- as.data.frame(interp2xyz(df_fig3o))

df_fig3o$step_size <- 62.6
df_fig3o$cat_ftw1 <- "high"


df_fig3_int <- rbind(df_fig3d,
                     df_fig3e,
                     df_fig3f,
                     df_fig3g,
                     df_fig3h,
                     df_fig3i,
                     df_fig3j,
                     df_fig3k,
                     df_fig3l,
                     df_fig3m,
                     df_fig3n,
                     df_fig3o)

df_fig3_int$cat_ftw1 <- factor(df_fig3_int$cat_ftw1,
                               levels = c("<= very low","low","moderate","high"))

ggplot(df_fig3_int, aes(x=x,y=y,fill=z)) +
  geom_tile() +
  scale_fill_viridis(option="H") +

  facet_grid(cat_ftw1~step_size) +
  xlab("Number of Traps") +
  ylab("Lure attractiveness") +
  labs(fill="Mean pests")

###########
# Figure 4#
###########

df_fig4a <- df_fig3 %>%
  filter(step_size == 20,
         cat_ftw1 == "negligible")
df_fig4a <- interp(x=df_fig4a$n_traps,y=df_fig4a$lure_attract,z=df_fig4a$ub,
                   nx=10,ny=20)
df_fig4a <- as.data.frame(interp2xyz(df_fig4a))

df_fig4b <- df_fig3 %>%
  filter(step_size == 43,
         cat_ftw1 == "negligible")
df_fig4b <- interp(x=df_fig4b$n_traps,y=df_fig4b$lure_attract,z=df_fig4b$ub,
                   nx=10,ny=20)
df_fig4b <- as.data.frame(interp2xyz(df_fig4b))

df_fig4c <- df_fig3 %>%
  filter(step_size == 62.6,
         cat_ftw1 == "negligible")
df_fig4c <- interp(x=df_fig4c$n_traps,y=df_fig4c$lure_attract,z=df_fig4c$ub,
                   nx=10,ny=20)
df_fig4c <- as.data.frame(interp2xyz(df_fig4c))


df_fig4d <- df_fig3 %>%
  filter(step_size == 20,
         cat_ftw1 %in% c("negligible", "very low"))
df_fig4d <- interp(x=df_fig4d$n_traps,y=df_fig4d$lure_attract,z=df_fig4d$ub,
                   nx=10,ny=20, duplicate = "mean")
df_fig4d <- as.data.frame(interp2xyz(df_fig4d))

df_fig4d$step_size <- 20
df_fig4d$cat_ftw1 <- "<= very low"

df_fig4e <- df_fig3 %>%
  filter(step_size == 43,
         cat_ftw1 %in% c("negligible", "very low"))
df_fig4e <- interp(x=df_fig4e$n_traps,y=df_fig4e$lure_attract,z=df_fig4e$ub,
                   nx=10,ny=20,  duplicate = "mean")
df_fig4e<- as.data.frame(interp2xyz(df_fig4b))

df_fig4e$step_size <- 43
df_fig4e$cat_ftw1 <- "<= very low"

df_fig4f <- df_fig3 %>%
  filter(step_size == 62.6,
         cat_ftw1 %in% c("negligible", "very low"))
df_fig4f <- interp(x=df_fig4f$n_traps,y=df_fig4f$lure_attract,z=df_fig4f$ub,
                   nx=10,ny=20,  duplicate = "mean")
df_fig4f <- as.data.frame(interp2xyz(df_fig4f))

df_fig4f$step_size <- 62.6
df_fig4f$cat_ftw1 <- "<= very low"

df_fig4g <- df_fig3 %>%
  filter(step_size == 20,
         cat_ftw1 == "low")
df_fig4g <- interp(x=df_fig4g$n_traps,y=df_fig4g$lure_attract,z=df_fig4g$ub,
                   nx=10,ny=20)
df_fig4g <- as.data.frame(interp2xyz(df_fig4g))

df_fig4g$step_size <- 20
df_fig4g$cat_ftw1 <- "low"

df_fig4h <- df_fig3 %>%
  filter(step_size == 43,
         cat_ftw1 == "low")
df_fig4h <- interp(x=df_fig4h$n_traps,y=df_fig4h$lure_attract,z=df_fig4h$ub,
                   nx=10,ny=20)
df_fig4h <- as.data.frame(interp2xyz(df_fig4h))

df_fig4h$step_size <- 43
df_fig4h$cat_ftw1 <- "low"

df_fig4i <- df_fig3 %>%
  filter(step_size == 62.6,
         cat_ftw1 == "low")
df_fig4i <- interp(x=df_fig4i$n_traps,y=df_fig4i$lure_attract,z=df_fig4i$ub,
                   nx=10,ny=20)
df_fig4i <- as.data.frame(interp2xyz(df_fig4i))

df_fig4i$step_size <- 62.6
df_fig4i$cat_ftw1 <- "low"

df_fig4j <- df_fig3 %>%
  filter(step_size == 20,
         cat_ftw1 == "moderate")
df_fig4j <- interp(x=df_fig4j$n_traps,y=df_fig4j$lure_attract,z=df_fig4j$ub,
                   nx=10,ny=20)
df_fig4j <- as.data.frame(interp2xyz(df_fig4j))

df_fig4j$step_size <- 20
df_fig4j$cat_ftw1 <- "moderate"

df_fig4k <- df_fig3 %>%
  filter(step_size == 43,
         cat_ftw1 == "moderate")
df_fig4k <- interp(x=df_fig4k$n_traps,y=df_fig4k$lure_attract,z=df_fig4k$ub,
                   nx=10,ny=20)
df_fig4k <- as.data.frame(interp2xyz(df_fig4k))

df_fig4k$step_size <- 43
df_fig4k$cat_ftw1 <- "moderate"

df_fig4l <- df_fig3 %>%
  filter(step_size == 62.6,
         cat_ftw1 == "moderate")
df_fig4l <- interp(x=df_fig4l$n_traps,y=df_fig4l$lure_attract,z=df_fig4l$ub,
                   nx=10,ny=20)
df_fig4l <- as.data.frame(interp2xyz(df_fig4l))

df_fig4l$step_size <- 62.6
df_fig4l$cat_ftw1 <- "moderate"

df_fig4m <- df_fig3 %>%
  filter(step_size == 20,
         cat_ftw1 == "high")
df_fig4m <- interp(x=df_fig4m$n_traps,y=df_fig4m$lure_attract,z=df_fig4m$ub,
                   nx=10,ny=20)
df_fig4m <- as.data.frame(interp2xyz(df_fig4m))

df_fig4m$step_size <- 20
df_fig4m$cat_ftw1 <- "high"

df_fig4n <- df_fig3 %>%
  filter(step_size == 43,
         cat_ftw1 == "high")
df_fig4n <- interp(x=df_fig4n$n_traps,y=df_fig4n$lure_attract,z=df_fig4n$ub,
                   nx=10,ny=20)
df_fig4n <- as.data.frame(interp2xyz(df_fig4n))

df_fig4n$step_size <- 43
df_fig4n$cat_ftw1 <- "high"

df_fig4o <- df_fig3 %>%
  filter(step_size == 62.6,
         cat_ftw1 == "high")
df_fig4o <- interp(x=df_fig4o$n_traps,y=df_fig4o$lure_attract,z=df_fig4o$ub,
                   nx=10,ny=20)
df_fig4o <- as.data.frame(interp2xyz(df_fig4o))

df_fig4o$step_size <- 62.6
df_fig4o$cat_ftw1 <- "high"


df_fig4_int <- rbind(df_fig4d,
                     df_fig4e,
                     df_fig4f,
                     df_fig4g,
                     df_fig4h,
                     df_fig4i,
                     df_fig4j,
                     df_fig4k,
                     df_fig4l,
                     df_fig4m,
                     df_fig4n,
                     df_fig4o)

df_fig4_int$cat_ftw1 <- factor(df_fig4_int$cat_ftw1,
                               levels = c("<= very low","low","moderate","high"))



ggplot(df_fig4_int, aes(x=x,y=y,fill=z)) +
  geom_tile() +
  scale_fill_viridis(option="H") +

  facet_grid(cat_ftw1~step_size) +
  xlab("Number of Traps") +
  ylab("Lure attractiveness") +
  labs(fill="95th percentile pests")



############
# Figure 5 #
############
res_df_sub$cat_ftw1 <- factor(res_df_sub$cat_ftw1,
                              levels = c("negligible","very low","low","moderate","high"))

res_df_sub %>%
  filter(n_traps %in% c(1,5,10)) %>%
  mutate(cat_ftw1 = as.factor(cat_ftw1)) %>%
  mutate(lure_attract = factor(lure_attract)) %>%
  mutate(step_size = as.factor(step_size)) %>%
  ggplot(aes(y=cat_ftw1)) +
  geom_density_ridges(aes(x=num_pests,fill = paste0(lure_attract))) +
  facet_grid(n_traps ~ step_size) +
  xlab("Number of Pests in Site") +
  ylab("Trap Catch") +
  labs(fill="Lure attractiveness")


############
# Figure 6 #
############

res_df_sub %>%
  filter(n_traps %in% c(1,10)) %>%
  filter(step_size == 43) %>%
  mutate(cat_ftw1 = as.factor(cat_ftw1)) %>%
  mutate(lure_attract = factor(lure_attract)) %>%
  mutate(step_size = as.factor(step_size)) %>%
  ggplot(aes(y=cat_ftw1)) +
  geom_density_ridges(aes(x=num_pests,fill = paste0(lure_attract))) +
  facet_grid(n_traps ~ step_size) +
  xlab("Number of Pests in Site") +
  ylab("Trap Catch") +
  labs(fill="Lure attractiveness")


############
# Figure 7 #
############

res_df_sub %>%
  filter(step_size > 0) %>%
  #
  #group_by(n_traps, step_size, lure_attract)%>%
  summarise(ftw1_mn = mean(ftw1),
            ftw1_lb = quantile(ftw1,.05),
            ftw1_ub = quantile(ftw1,.95),
            .by = c(num_pests, n_traps, step_size, lure_attract)) %>%
  mutate(n_traps = as.factor(n_traps)) %>%
  ggplot(aes(x=num_pests)) +
  geom_line(aes(y = ftw1_mn, color = n_traps)) +
  geom_ribbon(aes(ymin=ftw1_lb,ymax=ftw1_ub, fill = n_traps, color = n_traps),
              alpha=.2) +
  xlab("Number of pests") +
  ylab("Flies per trap per week") +
  labs(fill = "Number of traps", color = "Number of traps") +
  facet_grid(lure_attract ~ step_size)


############
# Figure 8 #
############

res_df_sub %>%
  filter(step_size > 0) %>%
  filter(lure_attract>7 & lure_attract < 50) %>%
  filter(n_traps %in% c(1,5,10)) %>%
  summarise(ftw1_mn = mean(ftw1),
            ftw1_lb = quantile(ftw1,.05),
            ftw1_ub = quantile(ftw1,.95),
            .by = c(num_pests, n_traps)) %>%
  mutate(n_traps = as.factor(n_traps)) %>%
  ggplot(aes(x=num_pests, y=ftw1_mn, color=n_traps)) +
  geom_line(size=2) +
  geom_ribbon(aes(ymin=ftw1_lb,ymax=ftw1_ub, fill = n_traps, color =n_traps),
              alpha=.2)+
  xlab("Number of pests") +
  ylab("Flies per trap per week") +
  labs(fill = "Number of traps", color = "Number of traps")



