


big <- read.csv("C:/Users/Peter Ellis/Documents/MED/coprlookup_vij.csv")
head(big)

missing <- big[Country13(big$COPRDetail) == "Other", ]
missing[order(missing[,2]), 1]


# notes for Vij
# Hong Kong, Taiwan need to be in rest of Asia
# Virgin Island US - you have two treatments of it, prob just put in other Americas
# Azerbaijan and Georgia should be in rest of Asia