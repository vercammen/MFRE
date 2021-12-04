rm(list = ls()) 
graphics.off()

# load packages and data
pacman::p_load(here, dplyr, ggplot2)
profits <- readRDS(file = here("Data", "compare.rds"))
head(profits)

# Create histograms
long_corn <- ggplot(profits, aes(x=long_corn)) +  xlab("Profits, Long in Corn ($/bu)") +
  geom_histogram(color="black", fill="white", bins = 30) + scale_x_continuous(limits = c(-1.5, 1.5), breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5)) +
  theme(axis.title=element_text(size=15))
long_corn

short_hedge <- ggplot(profits, aes(x=short_hedge)) +  xlab("Profits, Short Hedge ($/bu)") +
  geom_histogram(color="black", fill="white", bins = 30) + scale_x_continuous(limits = c(-1.5, 1.5), breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5))+
  theme(axis.title=element_text(size=15))
short_hedge

crush_hedged <- ggplot(profits, aes(x=crushH)) +  xlab("Normalized Crush Margin, Hedged ($/bu)") +
  geom_histogram(color="black", fill="white", bins = 30) + scale_x_continuous(limits = c(-1.5, 1.5), breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5))+
  theme(axis.title=element_text(size=15))
crush_hedged

crush_nohedged <- ggplot(profits, aes(x=crushNH)) +  xlab("Normalized Crush Margin, No Hedge ($/bu)") +
  geom_histogram(color="black", fill="white", bins = 30) + scale_x_continuous(limits = c(-1.5, 1.5), breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5))+
  theme(axis.title=element_text(size=15))
crush_nohedged

basis_change <- ggplot(profits, aes(x=BasisChange)) +  xlab("Change in Basis over Hedging Period ($/bu)") +
  geom_histogram(color="black", fill="white", bins = 30) + scale_x_continuous(limits = c(-1.5, 1.5), breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5)) + theme_classic(base_size = 15)
basis_change


# overlay hist
overlay_deferred <- ggplot(profits) +
  geom_histogram(aes(x= long_corn), fill = "red", alpha = 0.2) +
  geom_histogram(aes(x = short_hedge), fill = "blue", alpha = 0.2) + 
  scale_x_continuous(limits = c(-1.5, 1.5), breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5)) +
  xlab("Profits, $/bu") + labs(caption = "red = long spot, blue = short hedge")
overlay_deferred

overlay_TCM <- ggplot(profits) +
  geom_histogram(aes(x= crushNH), fill = "red", alpha = 0.2) +
  geom_histogram(aes(x = crushH), fill = "blue", alpha = 0.2) + 
  scale_x_continuous(limits = c(-1.5, 1.5), breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5)) +
  xlab("Profits, $/bu") + labs(caption = "red = unhedged crush, blue = hedged crush")
overlay_TCM 

# save images - uncomment to run and overwrite files 
ggsave(long_corn, file = here("Images", "hist_long_corn.png"))
ggsave(short_hedge, file = here("Images", "hist_short_hedge.png"))
# ggsave(overlay_deferred, file = here("Images", "overlay_deferred.png"))
ggsave(crush_hedged, file = here("Images", "hist_crush_hedged.png"))
ggsave(crush_nohedged, file = here("Images", "hist_crush_nohedged.png"))
ggsave(basis_change, file = here("Images", "hist_basischange.png"))


crushH_percentile <- quantile(profits$crushH, probs = c(0.05,0.1,0.2,0.3,0.4,0.5))
crushNH_percentile <- quantile(profits$crushNH, probs = c(0.05,0.1,0.2,0.3,0.4,0.5))
basischange_percentile <- quantile(profits$BasisChange, probs = c(0.05,0.1,0.2,0.3,0.4,0.5))
longcorn_percentile <- quantile(profits$long_corn, probs = c(0.05,0.1,0.2,0.3,0.4,0.5))
shorthedge_percentile <- quantile(profits$short_hedge, probs = c(0.05,0.1,0.2,0.3,0.4,0.5))
longhedge_percentile <- quantile(profits$long_hedge, probs = c(0.05,0.1,0.2,0.3,0.4,0.5))

crushH_percentile
crushNH_percentile
basischange_percentile

longhedge_percentile
shorthedge_percentile

perc <- rbind(crushH_percentile, crushNH_percentile, basischange_percentile, longhedge_percentile, shorthedge_percentile)

# rename rows
rownames(perc) <- c("Crush Hedged", "Crush No Hedged", "Basis Change", "Long Hedge", "Short Hedge")
perc




