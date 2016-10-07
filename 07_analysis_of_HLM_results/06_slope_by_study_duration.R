library(dplyr)
library(ggplot2)

params <- read.csv("../06_HLM_output/site_slopes_3_points.csv")
params <- params %>% filter(parameter=="site_slope")

site_slopes <- params %>% 
  filter(Period=="1900-2015", grouping=="Ecoregion")

orig_data <- read.csv("../05_HLM_analysis_code/formatted_data_3points.csv")

orig_summary <- orig_data %>%
  group_by(study_ID, Study, Site, trajectory_ID) %>%
  dplyr::summarize(Latitude=Latitude[1], Longitude=Longitude[1], mean_Depth=mean(mean_Depth),
            Duration =  max(year) - min(year), ECOREGION=ECOREGION[1]) %>%
  ungroup() %>%
  mutate(SiteName = paste0(Study, "-", Site, "-", study_ID, "-", trajectory_ID,
                           ":", Study))


site_slopes_merged <- left_join(site_slopes, orig_summary) %>%
  filter(!is.na(Duration)) %>%
  filter(Duration>2) %>%
  mutate(durBox=cut(Duration, breaks=c(-1,20,  70))) %>%
  mutate(posNeg=sign(mean)) %>%
  mutate(posNeg = ifelse(p>0.9 | p<0.1,posNeg, 0))


site_slopes_merged$pn2 <- factor(site_slopes_merged$posNeg)
levels(site_slopes_merged$pn2) <- c("Negative", "Zero", "Positive")
site_slopes_merged$pn2 <- factor(site_slopes_merged$pn2, levels=c("Positive", "Zero", "Negative"))

levels(site_slopes_merged$durBox) <- c("≤20 years", ">20 years")
levels(site_slopes_merged$durBox) <- c(expression(phantom(0) <= "20 years"),
                                        expression(phantom(0) > "20 years"))

plot(mean ~ Duration, data=site_slopes_merged)
plot(mean ~ posNeg, data=site_slopes_merged)

ggplot(site_slopes_merged,
       aes(x=durBox, fill=factor(posNeg))) +
  geom_bar(position="dodge") +
  scale_y_log10()


###FOR SUPPLEMENT


tab1 <- ggplot(site_slopes_merged,
       aes(x=Duration, y=mean, ymin=mean-se, ymax=mean+se, color=pn2))+
  geom_point(size=2.5) +
  geom_linerange(size=0.5) + theme_bw(base_size=24) +
  ylab(expression(Slope %+-% 1*SE)) +
  xlab("Duration (years)") +
  scale_color_manual(guide = guide_legend(title="Sign\n"), 
                       values=c("blue", "grey", "red")) +
  annotate(geom="text", x=0,y=0.3, label="A", size=8)


#from http://stackoverflow.com/questions/3472980/ggplot-how-to-change-facet-labels
dur_names <- list(
  "≤20 years"=expression(phantom(0) <= "20 years"),
  ">20 years"=expression(phantom(0) > "20 years")
)

dur_labeller <- function(variable,value){
  return(dur_names[value])
}


tab2 <- ggplot(site_slopes_merged,
       aes(x=mean, fill=pn2)) +
  facet_wrap(~durBox, scale="free_y", labeller=dur_labeller) +
  geom_histogram(position = "identity", bins=30)+
  scale_fill_manual(guide = "none", 
                    values=c("blue", "grey", "red")) +
  xlab("Slope") +
  ylab("Count") +
  theme_bw(base_size=24)+
  annotate(geom="text", x=-0.17,y=100, label=c("B",""), size=8) +
  ylim(c(0,115))

library(gridExtra)

jpeg("../Figures/slope_duration.jpg", height=850, width=768, type = c("quartz"))
grid.arrange(tab1, tab2, ncol=1)
dev.off()

pdf.options(encoding='ISOLatin2.enc')
pdf("../Figures/slope_duration.pdf", height=12, width=11)
grid.arrange(tab1, tab2, ncol=1)
dev.off()
