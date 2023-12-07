Evaluations Notes

#Mammals

I want to correct for differential sampling between gear types in the dataset for the fixed gears. I am ignoring year for now, but that could be fixed (added) later.

So mean can still be calculated by gear but save n.
Calculate sample variance for each gear type in two parts: numerator and denominator
Use n for weights for every observation (divide 1/n).  Use weighted.mean for mean calc across all gears.
Add variance across gears.
