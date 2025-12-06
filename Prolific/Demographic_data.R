
library(readxl)
sub <- read_excel("Prolific/data/Participant_info.xlsx")

# Age:
mean(sub$Age)
sd(sub$Age)
range(sub$Age)


# Gender:
table(sub$Sex)

# Ethnicity:
table(sub$`Ethnicity simplified`)/ nrow(sub)*100


