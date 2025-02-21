# file: OrganWt90a010325.R
# purpose: statistical analysis Organ Wt Data 90-day (CSS401.6.7)
# 1/15/25 - EMK edit to make this file for generating graphics only, use in ms & poster
# uses data file OrganWeightsPFAS.xlsx
# The lines below for the 4 below will have to be changed at the code completion.
# MHFPK lines 19-378
# PFHI lines 382 - 736
# CTFPA lines 740 - 1096
# MFHPK lines  1099 - 1846

#Load needed R packages
library(statsr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(xlsx)
library(readxl)
library(flextable)
library(performance)
library(tidyr)
library(DescTools)
library(ToxicR)

# Load required packages
library(ggplot2)
library(dplyr)
library(readxl)

## 1H,1H,9H-PERFLUORONONYL AcRYLATE (PFNAC)
## read in the data file
PFNACo <- read_excel("OrganWeightsPFAS.xlsx", sheet='PFNAC', na = c("", "NA")) 
View(PFNACo)

#remove rows 63 through last row, eliminate Females
PFNACom <- PFNACo[-c(61:nrow(PFNACo)), ]

#BOX PLOTS Male Rat Organ Weight Data PFNAC

LiverAbs1m <- ggplot(
  
  PFNACom, 
  
  aes(
    
    x = factor(Group), 
    
    y = LiverAbs, 
    
    fill = factor(Group) # Fill for violin plot
    
  )
  
) +
  
  geom_violin(trim = TRUE, alpha = 0.7, color = "black") + # Violin plot
  
  geom_boxplot(
    
    aes(fill = factor(Group)), # Separate fill for box plot
    
    width = 0.1, 
    
    color = "black", 
    
    alpha = 0.9
    
  ) +
  
  labs(
    
    title = "PFNAC - Male Rat Liver", 
    
    x = "Dose Group (mg/kg-day)", 
    
    y = "Liver Weight (grams)"
    
  ) +
  
  scale_fill_brewer(palette = "Set2", name = "Group") + # Colorful palette
  
  theme_bw(base_size = 18) +
  
  theme(legend.position = "none", plot.title=element_text(hjust=0.5))



# Save the plot as a PNG file, units in inches

ggsave(
  
  filename = "LiverAbs1m.png",
  
  plot = LiverAbs1m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)

# Display the plot

LiverAbs1m

LiverRel1m <- ggplot(
  PFNACom, 
  aes(
    x = factor(Group), 
    y = LiverRel, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(
    aes(fill = factor(Group)),#separate fill for boxplot
    width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  labs(title = "PFNAC - Male Rat Liver/Body Weight",
       x = "Dose Group (mg/kg-day)", y = "Liver/BW Ratio (%)") +
  #scale_fill_discrete(name = "Group") +
  scale_fill_brewer(palette = "Set2", name ="Group")+ #colorful palette
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5))
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
# Save the plot as a PNG file, units in inches

ggsave(
  
  filename = "LiverRel1m.png",
  
  plot = LiverRel1m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)

LiverRel1m

LiverBrn1m <- ggplot(
  PFNACom, 
  aes(
    x = factor(Group), 
    y = LiverBrn, 
    fill = factor(Group)# fill for violin plot
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(
    aes(fill = factor(Group)),# Separate fill for box plot
    width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  labs(title = "PFNAC - Male Rat Liver/Brain Weight",
       x = "Dose Group (mg/kg-day)", y = "Liver/Brain Weight (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  #scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal() +
ggsave(
  
  filename = "LiverBrn1m.png",
  
  plot = LiverBrn1m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)  
  
LiverBrn1m

#BOX PLOTS Male Rat Organ Weight Data - Kidney
KidneyAbs1m <- ggplot(
  PFNACom, 
  aes(
    x = factor(Group), 
    y = KidneyAbs, 
    fill = factor(Group)
  )
) +
 # geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "PFNAC - Male Rat Kidney",
       x = "Dose Group (mg/kg-day)", y = "Kidney Weight (grams)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  ggsave(
    
    filename = "KidneyAbs1m.png",
    
    plot = KidneyAbs1m,
    
    width = 5,
    
    height = 6,
    
    dpi = 300
    
  )  
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
KidneyAbs1m

KidneyRel1m <- ggplot(
  PFNACom, 
  aes(
    x = factor(Group), 
    y = KidneyRel, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "PFNAC - Male Rat Kidney/Body Weight",
       x = "Dose Group (mg/kg-day)", y = "Kidney/BW Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "KidneyRel1m.png",
  
  plot = KidneyRel1m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)  
KidneyRel1m

KidneyBrn1m <- ggplot(
  PFNACom, 
  aes(
    x = factor(Group), 
    y = KidneyBrn, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "PFNAC - Male Rat Kidney/Brain Weight",
       x = "Dose Group (mg/kg-day)", y = "Kidney/Brain Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
 # scale_fill_manual(values=rep("gray",6))
 # theme_minimal()
ggsave(
  
  filename = "KidneyBrn1m.png",
  
  plot = KidneyBrn1m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)  
KidneyBrn1m


#BEGIN ANALYSIS FEMALE RAT ORGAN WEIGHT DATA PFNAC
#Include only females rows 61-120
PFNACof <- PFNACo[c(61:120),]

#BOX PLOTS FEMALE Rat Organ Weight Data PFNAC
#Absolute Liver Weights, Female Rats
LiverAbs1f <- ggplot(
  PFNACof, 
  aes(
    x = factor(Group), 
    y = LiverAbs, 
    fill = factor(Group)
  )
) +
 # geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "PFNAC - Female Rat Liver",
       x = "Dose Group (mg/kg-day)", y = "Liver Weight (grams)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 

  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()

ggsave(
  
  filename = "LiverAbs1f.png",
  
  plot = LiverAbs1f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)  
LiverAbs1f

#Relative Liver Weights, Female Rats
LiverRel1f <- ggplot(
  PFNACof, 
  aes(
    x = factor(Group), 
    y = LiverRel, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "PFNAC - Female Rat Liver/Body Weight",
       x = "Dose Group (mg/kg-day)", y = "Liver/BW Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6)) 
  #theme_minimal()
  ggsave(
    
    filename = "LiverRel1f.png",
    
    plot = LiverRel1f,
    
    width = 5,
    
    height = 6,
    
    dpi = 300
    
  )  
LiverRel1f

#Liver/Brain Weight Ratio Female Rats
LiverBrn1f <- ggplot(
  PFNACof, 
  aes(
    x = factor(Group), 
    y = LiverBrn, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "PFNAC - Female Rat Liver/Brain Weight",
       x = "Dose Group (mg/kg-day)", y = "Liver/Brain Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "LiverBrn1f.png",
  
  plot = LiverBrn1f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)

LiverBrn1f

#BOX PLOTS FEMALE Rat Organ Weight Data - Kidney
#Absolute Kidney Weights, Female Rats
KidneyAbs1f <- ggplot(
  PFNACof, 
  aes(
    x = factor(Group), 
    y = KidneyAbs, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "PFNAC - Female Rat Kidney Weight",
       x = "Dose Group (mg/kg-day)", y = "KidneyAbs (grams)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5))
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "KidneyAbs1f.png",
  
  plot = KidneyAbs1f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
KidneyAbs1f

#Relative Kidney Weights, Female Rats
KidneyRel1f <- ggplot(
  PFNACof, 
  aes(
    x = factor(Group), 
    y = KidneyRel, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "PFNAC - Female Rat Kidney/Body Weight",
       x = "Dose Group (mg/kg-day)", y = "Kidney/BW Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "KidneyRel1f.png",
  
  plot = KidneyRel1f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
KidneyRel1f

#Kidney/Brain Weight Ratio Female Rats
KidneyBrn1f <- ggplot(
  PFNACof, 
  aes(
    x = factor(Group), 
    y = KidneyBrn, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "PFNAC - Female Rat Kidney/Brain Weight",
       x = "Dose Group (mg/kg-day)", y = "Kidney/Brain Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "KidneyBrn1f.png",
  
  plot = KidneyBrn1f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
KidneyBrn1f


##******************************************************************************
### 1H,1H,2H,2H-Perfluorohexyl Iodide (PFHI)
## read in the data file
PFHIo <- read_excel("OrganWeightsPFAS.xlsx", sheet='PFHI', na = c("", "NA")) 
View(PFHIo)

#remove rows 63 through last row, eliminate Females
PFHIom <- PFHIo[-c(61:nrow(PFHIo)), ]

#BOX PLOTS Male Rat Organ Weight Data PFHI
LiverAbs2m <- ggplot(
  PFHIom, 
  aes(
    x = factor(Group), 
    y = LiverAbs, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "PFHI - Male Rat Liver Weight",
       x = "Dose Group (mg/kg-day)", y = "Liver Weight (grams)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "LiverAbs2m.png",
  
  plot = LiverAbs2m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
LiverAbs2m

LiverRel2m <- ggplot(
  PFHIom, 
  aes(
    x = factor(Group), 
    y = LiverRel, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "PFHI - Male Rat Liver/Body Weight",
       x = "Dose Group (mg/kg-day)", y = "Liver/BW Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "LiverRel2m.png",
  
  plot = LiverRel2m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
LiverRel2m

LiverBrn2m <- ggplot(
  PFHIom, 
  aes(
    x = factor(Group), 
    y = LiverBrn, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "PFHI - Male Rat Liver/Brain Weight",
       x = "Dose Group (mg/kg-day)", y = "Liver/Brain Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "LiverBrn2m.png",
  
  plot = LiverBrn2m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
LiverBrn2m

#BOX PLOTS Male Rat Organ Weight Data - Kidney
KidneyAbs2m <- ggplot(
  PFHIom, 
  aes(
    x = factor(Group), 
    y = KidneyAbs, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "PFHI - Male Rat Kidney",
       x = "Dose Group (mg/kg-day)", y = "KidneyAbs (grams)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "KidneyAbs2m.png",
  
  plot = KidneyAbs2m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
KidneyAbs2m

KidneyRel2m <- ggplot(
  PFHIom, 
  aes(
    x = factor(Group), 
    y = KidneyRel, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "PFHI - Male Rat Kidney/Body Weight",
       x = "Dose Group (mg/kg-day)", y = "Kidney/BW Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "KidneyRel2m.png",
  
  plot = KidneyRel2m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
KidneyRel2m

KidneyBrn2m <- ggplot(
  PFHIom, 
  aes(
    x = factor(Group), 
    y = KidneyBrn, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "PFHI - Male Rat Kidney/Brain Weight",
       x = "Dose Group (mg/kg-day)", y = "Kidney/Brain Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "KidneyBrn2m.png",
  
  plot = KidneyBrn2m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
KidneyBrn2m


#BEGIN ANALYSIS FEMALE RAT ORGAN WEIGHT DATA PFHI
#Include only females rows 61-120
PFHIof <- PFHIo[c(61:120),]

#BOX PLOTS FEMALE Rat Organ Weight Data PFHI
#Absolute Liver Weights, Female Rats, PFHI
LiverAbs2f <- ggplot(
  PFHIof, 
  aes(
    x = factor(Group), 
    y = LiverAbs, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "PFHI - Female Rat Liver",
       x = "Dose Group (mg/kg-day)", y = "Liver Weight (grams)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
  ggsave(
    
    filename = "LiverAbs2f.png",
    
    plot = LiverAbs2f,
    
    width = 5,
    
    height = 6,
    
    dpi = 300
    
  )
LiverAbs2f

#Relative Liver Weights, Female Rats, PHFI
LiverRel2f <- ggplot(
  PFHIof, 
  aes(
    x = factor(Group), 
    y = LiverRel, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "PFHI - Female Rat Liver/Body Weight",
       x = "Dose Group (mg/kg-day)", y = "Liver/BW Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()

ggsave(
  
  filename = "LiverRel2f.png",
  
  plot = LiverRel2f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
LiverRel2f

#Liver/Brain Weight Ratio Female Rats, PFHI
LiverBrn2f <- ggplot(
  PFHIof, 
  aes(
    x = factor(Group), 
    y = LiverBrn, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "PFHI - Female Rat Liver/Brain Weight",
       x = "Dose Group (mg/kg-day)", y = "Liver/Brain Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "LiverBrn2f.png",
  
  plot = LiverBrn2f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
LiverBrn2f

#BOX PLOTS FEMALE Rat Organ Weight Data PFHI - Kidney
#Absolute Kidney Weights, Female Rats, PFHI
KidneyAbs2f <- ggplot(
  PFHIof, 
  aes(
    x = factor(Group), 
    y = KidneyAbs, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "PFHI - Female Rat Kidney",
       x = "Dose Group (mg/kg-day)", y = "KidneyAbs (grams)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "KidneyAbs2f.png",
  
  plot = KidneyAbs2f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
KidneyAbs2f

#Relative Kidney Weights, Female Rats, PHFI
KidneyRel2f <- ggplot(
  PFHIof, 
  aes(
    x = factor(Group), 
    y = KidneyRel, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "PFHI - Female Rat Kidney/Body Weight",
       x = "Dose Group (mg/kg-day)", y = "Kidney/BW Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "KidneyRel2f.png",
  
  plot = KidneyRel2f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)

KidneyRel2f

#Kidney/Brain Weight Ratio Female Rats, PFHI
KidneyBrn2f <- ggplot(
  PFHIof, 
  aes(
    x = factor(Group), 
    y = KidneyBrn, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "PFHI - Female Rat Kidney/Brain Weight",
       x = "Dose Group (mg/kg-day)", y = "Kidney/Brain Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "KidneyBrn2f.png",
  
  plot = KidneyBrn2f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
KidneyBrn2f



##***************************************************************************##
## 2-chloro-2,3,3,3-tetrafluoropropanoicacid (CTFPA)
## read in the data file
CTFPAo <- read_excel("OrganWeightsPFAS.xlsx", sheet='CTFPA', na = c("", "NA")) 
View(CTFPAo)

#remove rows 63 through last row, eliminate Females
CTFPAom <- CTFPAo[-c(61:nrow(CTFPAo)), ]

#BOX PLOTS Male Rat Organ Weight Data CTFPA
LiverAbs3m <- ggplot(
  CTFPAom, 
  aes(
    x = factor(Group), 
    y = LiverAbs, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "CTFPA - Male Rat Liver",
       x = "Dose Group (mg/kg-day)", y = "LiverAbs (grams)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "LiverAbs3m.png",
  
  plot = LiverAbs3m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
LiverAbs3m

LiverRel3m <- ggplot(
  CTFPAom, 
  aes(
    x = factor(Group), 
    y = LiverRel, 
    fill = factor(Group)
  )
) +
 # geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "CTFPA - Male Rat Liver/Body Weight",
       x = "Dose Group (mg/kg-day)", y = "Liver/BW Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "LiverRel3m.png",
  
  plot = LiverRel3m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
LiverRel3m

LiverBrn3m <- ggplot(
  CTFPAom, 
  aes(
    x = factor(Group), 
    y = LiverBrn, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "CTFPA - Male Rat Liver/Brain Weight",
       x = "Dose Group (mg/kg-day)", y = "Liver/Brain Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "LiverBrn3m.png",
  
  plot = LiverBrn3m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
LiverBrn3m

#BOX PLOTS Male Rat Organ Weight Data - Kidney
KidneyAbs3m <- ggplot(
  CTFPAom, 
  aes(
    x = factor(Group), 
    y = KidneyAbs, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "CTFPA - Male Rat Kidney",
       x = "Dose Group (mg/kg-day)", y = "KidneyAbs (grams)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "KidneyAbs3m.png",
  
  plot = KidneyAbs3m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
KidneyAbs3m

KidneyRel3m <- ggplot(
  CTFPAom, 
  aes(
    x = factor(Group), 
    y = KidneyRel, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "CTFPA - Male Rat Kidney/Body Weight",
       x = "Dose Group (mg/kg-day)", y = "Kidney/BW Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "KidneyRel3m.png",
  
  plot = KidneyRel3m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
KidneyRel3m

KidneyBrn3m <- ggplot(
  CTFPAom, 
  aes(
    x = factor(Group), 
    y = KidneyBrn, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "CTFPA - Male Rat Kidney/Brain Weight",
       x = "Dose Group (mg/kg-day)", y = "Kidney/Brain Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "KidneyBrn3m.png",
  
  plot = KidneyBrn3m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
KidneyBrn3m


#BEGIN ANALYSIS FEMALE RAT ORGAN WEIGHT DATA CTFPA
#Include only females rows 61-120
CTFPAof <- CTFPAo[c(61:120),]

#BOX PLOTS FEMALE Rat Organ Weight Data CTFPA
#Absolute Liver Weights, Female Rats
LiverAbs4f <- ggplot(
  CTFPAof, 
  aes(
    x = factor(Group), 
    y = LiverAbs, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "CTFPA - Female Rat Liver",
       x = "Dose Group (mg/kg-day)", y = "Liver Weight (grams)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "LiverAbs4f.png",
  
  plot = LiverAbs4f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
LiverAbs4f

#Relative Liver Weights, Female Rats
LiverRel4f <- ggplot(
  CTFPAof, 
  aes(
    x = factor(Group), 
    y = LiverRel, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "CTFPA - Female Rat Liver/Body Weight",
       x = "Dose Group (mg/kg-day)", y = "Liver/BW Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "LiverRel4f.png",
  
  plot = LiverRel4f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
LiverRel4f

#Liver/Brain Weight Ratio Female Rats
LiverBrn4f <- ggplot(
  CTFPAof, 
  aes(
    x = factor(Group), 
    y = LiverBrn, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "CTFPA - Female Rat Liver/Brain Weight",
       x = "Dose Group (mg/kg-day)", y = "Liver/Brain Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "LiverBrn4f.png",
  
  plot = LiverBrn4f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
LiverBrn4f

#BOX PLOTS FEMALE Rat Organ Weight Data - Kidney
#Absolute Kidney Weights, Female Rats
KidneyAbs4f <- ggplot(
  CTFPAof, 
  aes(
    x = factor(Group), 
    y = KidneyAbs, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "CTFPA - Female Rat Kidney",
       x = "Dose Group (mg/kg-day)", y = "KidneyAbs (grams)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "KidneyAbs4f.png",
  
  plot = KidneyAbs4f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
KidneyAbs4f

#Relative Kidney Weights, Female Rats
KidneyRel4f <- ggplot(
  CTFPAof, 
  aes(
    x = factor(Group), 
    y = KidneyRel, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "CTFPA - Female Rat Kidney/Body Weight",
       x = "Dose Group (mg/kg-day)", y = "Kidney/BW Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "KidneyRel4f.png",
  
  plot = KidneyRel4f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
KidneyRel4f

#Kidney/Brain Weight Ratio Female Rats
KidneyBrn4f <- ggplot(
  CTFPAof, 
  aes(
    x = factor(Group), 
    y = KidneyBrn, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "CTFPA - Female Rat Kidney/Brain Weight",
       x = "Dose Group (mg/kg-day)", y = "Kidney/Brain Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "KidneyBrn4f.png",
  
  plot = KidneyBrn4f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
KidneyBrn4f


##***************************************************************************##
## Methyl Heptafluoropropyl ketone (MHFPK)
## read in the data file
MHFPKo <- read_excel("OrganWeightsPFAS.xlsx", sheet='MHFPK', na = c("", "NA")) 
View(MHFPKo)

#remove rows 63 through last row, eliminate Females
MHFPKom <- MHFPKo[-c(61:nrow(MHFPKo)), ]

#BOX PLOTS Male Rat Organ Weight Data MHFPK
LiverAbs4m <- ggplot(
  MHFPKom, 
  aes(
    x = factor(Group), 
    y = LiverAbs, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "MHFPK - Male Rat Liver",
       x = "Dose Group (mg/kg-day)", y = "Liver Weight (grams)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "LiverAbs4m.png",
  
  plot = LiverAbs4m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
LiverAbs4m

LiverRel4m <- ggplot(
  MHFPKom, 
  aes(
    x = factor(Group), 
    y = LiverRel, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "MHFPK - Male Rat Liver/Body Weight",
       x = "Dose Group (mg/kg-day)", y = "Liver/BW Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "LiverRel4m.png",
  
  plot = LiverRel4m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
LiverRel4m

LiverBrn4m <- ggplot(
  MHFPKom, 
  aes(
    x = factor(Group), 
    y = LiverBrn, 
    fill = factor(Group)
  )
) +
 # geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "MHFPK - Male Rat Liver/Brain Weight",
       x = "Dose Group (mg/kg-day)", y = "Liver/Brain Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "LiverBrn4m.png",
  
  plot = LiverBrn4m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
LiverBrn4m

#BOX PLOTS Male Rat Organ Weight Data - Kidney
KidneyAbs4m <- ggplot(
  MHFPKom, 
  aes(
    x = factor(Group), 
    y = KidneyAbs, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "MHFPK - Male Rat Kidney Weight",
       x = "Dose Group (mg/kg-day)", y = "KidneyAbs (grams)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "LiverBrn4m.png",
  
  plot = LiverBrn4m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
KidneyAbs4m

KidneyRel4m <- ggplot(
  MHFPKom, 
  aes(
    x = factor(Group), 
    y = KidneyRel, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "MHFPK - Male Rat Kidney/Body Weight",
       x = "Dose Group (mg/kg-day)", y = "Kidney/BW Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "KidneyRel4m.png",
  
  plot = KidneyRel4m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
KidneyRel4m

KidneyBrn4m <- ggplot(
  MHFPKom, 
  aes(
    x = factor(Group), 
    y = KidneyBrn, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "MHFPK - Male Rat Kidney/Brain Weight",
       x = "Dose Group (mg/kg-day)", y = "Kidney/Brain Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "KidneyBrn4m.png",
  
  plot = KidneyBrn4m,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
KidneyBrn4m


#BEGIN ANALYSIS FEMALE RAT ORGAN WEIGHT DATA MHFPK
#Include only females rows 61-120
MHFPKof <- MHFPKo[c(61:120),]

#BOX PLOTS FEMALE Rat Organ Weight Data MHFPK
#Absolute Liver Weights, Female Rats
LiverAbs5f <- ggplot(
  MHFPKof, 
  aes(
    x = factor(Group), 
    y = LiverAbs, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "MHFPK - Female Rat Liver",
       x = "Dose Group (mg/kg-day)", y = "Liver Weight (grams)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "LiverAbs5f.png",
  
  plot = LiverAbs5f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
LiverAbs5f

#Relative Liver Weights, Female Rats
LiverRel5f <- ggplot(
  MHFPKof, 
  aes(
    x = factor(Group), 
    y = LiverRel, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "MHFPK - Female Rat Liver/Body Weight",
       x = "Dose Group (mg/kg-day)", y = "Liver/BW Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "LiverRel5f.png",
  
  plot = LiverRel5f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
LiverRel5f

#Liver/Brain Weight Ratio Female Rats
LiverBrn5f <- ggplot(
  MHFPKof, 
  aes(
    x = factor(Group), 
    y = LiverBrn, 
    fill = factor(Group)
  )
) +
 # geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "MHFPK - Female Rat Liver/Brain Weight",
       x = "Dose Group (mg/kg-day)", y = "Liver/Brain Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "LiverBrn5f.png",
  
  plot = LiverBrn5f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
LiverBrn5f

#BOX PLOTS FEMALE Rat Organ Weight Data - Kidney
#Absolute Kidney Weights, Female Rats
KidneyAbs5f <- ggplot(
  MHFPKof, 
  aes(
    x = factor(Group), 
    y = KidneyAbs, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "MHFPK - Female Rat Kidney",
       x = "Dose Group (mg/kg-day)", y = "KidneyAbs (grams)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "KidneyAbs5f.png",
  
  plot = KidneyAbs5f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
KidneyAbs5f

#Relative Kidney Weights, Female Rats
KidneyRel5f <- ggplot(
  MHFPKof, 
  aes(
    x = factor(Group), 
    y = KidneyRel, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "MHFPK - Female Rat Kidney/Body Weight",
       x = "Dose Group (mg/kg-day)", y = "Kidney/BW Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "KidneyRel5f.png",
  
  plot = KidneyRel5f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
KidneyRel5f

#Kidney/Brain Weight Ratio Female Rats
KidneyBrn5f <- ggplot(
  MHFPKof, 
  aes(
    x = factor(Group), 
    y = KidneyBrn, 
    fill = factor(Group)
  )
) +
  #geom_boxplot() +
  geom_violin(trim = TRUE, alpha=0.7, color= "black") + #violin plot
  geom_boxplot(width = 0.1, color ="black", alpha = 0.9) +# boxplot overlay
  aes(fill = factor(Group))+ # Separate fill for box plot
  labs(title = "MHFPK - Female Rat Kidney/Brain Weight",
       x = "Dose Group (mg/kg-day)", y = "Kidney/Brain Ratio (%)") +
  scale_fill_brewer(palette = "Set2", name = "Group")+ #Colorful palette
  scale_fill_discrete(name = "Group") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none", plot.title=element_text(hjust=0.5)) 
  #scale_fill_manual(values=rep("gray",6))
  #theme_minimal()
ggsave(
  
  filename = "KidneyBrn5f.png",
  
  plot = KidneyBrn5f,
  
  width = 5,
  
  height = 6,
  
  dpi = 300
  
)
KidneyBrn5f

