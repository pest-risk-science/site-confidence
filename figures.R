
### Author: D. Gladish
### Note: run analysis.R first.  This script generates figures.

############
# Preamble #
############

# Packages
library(akima)
library(viridis)
library(ggplot2)
library(ggridges)
library(ggpubr)
library(khroma)

# Directories
root_dir <- getwd()
figures_dir <- file.path(root_dir, "figures")

# Label info
khroma_palette <- colour("bright")(length(unique(var_imp_df$Model)))

trap_labs <- c("1 trap","3 traps","5 traps", "10 traps")
names(trap_labs) <- c(1,3,5,10)

lure_labs <- c("1/\u03bb = 7", "1/\u03bb = 14", "1/\u03bb = 25", "1/\u03bb = 36", "1/\u03bb = 50")
names(lure_labs) <- c(7,14,25,36,50)

########################
# Pest simulation plot #
########################

# Figure S1
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

# Figure 1
valid_plot <- ggplot(meta_df2) +
  geom_point(aes(x=p, y=p_hat, color=species, shape=as.factor(grid_d)), size=4) +
  geom_errorbar(aes(x=p,ymin=p_lb,ymax=p_ub, color=species),size=1) +
  geom_abline(slope=1,intercept=0) +
  scale_colour_bright() +
  guides(shape=guide_legend("Grid res (m)"),
         color=guide_legend("Species")) +
  labs(x = "Observed proportion recaptured", y = "Simulated proportion recaptured" #title = "(a)",
       ) +
  theme_bw() +
  theme(text=element_text(size=20))
valid_plot2
ggsave(filename = file.path(figures_dir, "valid_plot.png"), width = 7, height = 5)

ggpubr::ggarrange(valid_plot, common.legend = TRUE,
                  legend = "bottom")
ggsave(filename = file.path(figures_dir, "valid_plot_combined.png"), width = 5, height = 5)


######################
# Random Forest Plot #
######################

# Figure 2
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

# Figure S4
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
  ylab("Trap catch (PTW)") +
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
  xlab("Trap catch (PTW)") +
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
  ylab("Trap catch (PTW)") +
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
  xlab("Trap catch (PTW)") +
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


# Figure 3a
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
  ylab("Trap catch (PTW)") +
  labs(fill = "Lure attractiveness (1/\u03bb)", color = "Lure attractiveness (1/\u03bb)") +
  theme_bw() +
  theme(legend.position = "bottom")


# Figure 4
t3 <- trap_df %>%
  filter(ftw1 <= 100) %>%
  mutate(cat_ftw1 = factor(cat_ftw1,
                           levels=c("negligible", "very low", "low", "moderate", "high"))) %>%
  mutate(cat_ftw1 = replace(cat_ftw1, cat_ftw1=="very low","low")) %>%
  mutate(n_traps = as.factor(n_traps)) %>%
  mutate(lure_attract = as.factor(lure_attract)) %>%
  ggplot(aes(x=cat_ftw1, y=pests_per_ha, fill=lure_attract)) +
  geom_boxplot(outlier.alpha = .01, outlier.size = .8) +
  scale_color_bright()+
  scale_fill_bright() +
  facet_grid(~n_traps, labeller = labeller(n_traps = trap_labs)) +
  ylab("True pest prevalence (pests per hectare)") +
  xlab("Trap catch (PTW)") +
  labs(fill = "Lure attractiveness (1/\u03bb)") +
  theme_bw() +
  theme(legend.position = "bottom")

ggpubr::ggarrange(t1)
ggsave(filename = file.path(figures_dir, "traps_plot.png"), width = 11, height = 5)

ggpubr::ggarrange(t3)
ggsave(filename = file.path(figures_dir, "traps_cat_plot.png"), width = 11, height = 5)



################
# Cluster Plot #
################

# Figure 3b
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
  ylab("Trap catch (PTW)") +
  labs(fill = "Number of pest clusters", color = "Number of pest clusters") +
  theme_bw() +
  theme(legend.position = "bottom")

# Figure 5
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
  facet_grid(~n_traps, labeller = labeller(n_traps = trap_labs))  +
  scale_color_bright()+
  scale_fill_bright() +
  ylab("True pest prevalence (pests per hectare)") +
  xlab("Trap catch (PTW)") +
  labs(fill = "Number of pest clusters") +
  theme_bw() +
  theme(legend.position = "bottom")

# Figure S2
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
  xlab("True pest prevalence (pests per hectare)") +
  ylab("Trap catch (PTW)") +
  labs(fill = "Number of pest clusters", color = "Number of pest clusters") +
  theme_bw() +
  theme(legend.position = "bottom")

# Figure S3
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
  facet_grid(lure_attract~n_traps, labeller = labeller(n_traps = trap_labs,
                                                       lure_attract=lure_labs)) +
  ylab("True pest prevalence (pests per hectare)") +
  xlab("Trap catch (PTW)") +
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











###############
# Old Figures #
###############
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
  ylab("PTW") +
  labs(fill = "Number of traps", color="Number of traps") +
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
  ylab("PTW") +
  labs(fill = "Number of traps") +
  theme_bw() +
  theme(legend.position = "bottom")

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
  xlab("Lure attractiveness (1/\u03bb)") +
  ylab("Trap catch (PTW)") +
  labs(fill = "Number of traps", color="Number of traps") +
  theme_bw() +
  theme(legend.position = "bottom")




