source("parse_sheet.R")
library(ggplot2)
library(tidyr)

tests %>%
  filter(correct==FALSE) %>%
  group_by(Initial_Training,Pre_Post,Defect) %>%
  summarize(Acid=sum(Acid!=0),
            Astringent=sum(Astringent!=0),
            Barny=sum(Barny!=0),
            Bitter=sum(Bitter!=0),
            Cooked=sum(Cooked!=0),
            Feed=sum(Feed!=0),
            Flat=sum(Flat!=0),
            Foreign_Chemical=sum(Foreign_Chemical!=0),
            Fruity_Fermented=sum(Fruity_Fermented!=0),
            Lacks_Freshness=sum(Lacks_Freshness!=0),
            Light_Oxidized=sum(Light_Oxidized!=0),
            Lipid_Oxidized=sum(Lipid_Oxidized!=0),
            Malty=sum(Malty!=0),
            Milk_Carton=sum(Milk_Carton!=0),
            Rancid=sum(Rancid!=0),
            Unclean=sum(Unclean!=0),
            Coagulated=sum(Coagulated!=0),
            Other=sum(Other!=0),
            Control=sum(Control!=0)) %>%
  ungroup() %>%
  mutate(Initial_Text="Re-Training") %>%
  #mutate(Initial_Training=factor(Initial_Training,levels=c("TRUE","FALSE"))) %>%
  gather(incorrect_defect,num_marked,Acid:Control) -> incorrect_marks

incorrect_marks$Initial_Text[incorrect_marks$Initial_Training] <- "Initial Training"

#for( i in unique(incorrect_marks$Defect) ) {
#  ggplot(incorrect_marks %>% filter(Defect==i),
#         aes(x=incorrect_defect,
#             y=num_marked,
#             color=Pre_Post,
#             fill=Pre_Post)) +
#    theme_bw() +
#    labs(title=paste(i,"Samples"),
#         x="Marked Defect",
#         y="Number Marked",
#         fill="Pre- or Post-Training",
#         color="Pre- or Post-Training") +
#    #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
#    scale_y_continuous(breaks=1:(incorrect_marks %>% filter(Defect==i) %>% select(num_marked) %>% max())) +
#    geom_bar(stat="identity", position="dodge") +
#    coord_polar() +
#    facet_grid(~Initial_Text) -> g
#  ggsave(filename=paste("Polar_Incorrect_",i,".pdf",sep=""),
#         plot=g,
#         width=11,
#         height=8.5,
#         units="in")
#}