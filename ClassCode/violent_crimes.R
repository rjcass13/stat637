#####################
# Prospective Study #
#####################
# Prosepective Study: Select people based on exposure, then follow them forward

# X: Exposure to lead as a child (0 = no, 1 = yes)
# Y: Violent crime (0 = no, 1 = yes)
dt <- as.data.frame(matrix(
  c(0, 0, 59950, 
  0, 1, 50, 
  1, 0, 39781,
  1, 1, 219), ncol = 3, byrow = TRUE))
colnames(dt) = c("X", "Y", "Count")

# P violent crime | lead exposure
p1 <- dt$Count[dt$X == 1 & dt$Y == 1] / sum(dt$Count[dt$X == 1])
# P violent crime | no lead exposure
p0 <- dt$Count[dt$X == 0 & dt$Y == 1] / sum(dt$Count[dt$X == 0])
# Relative Risk
RR <- p1/p0
# Odds of violent crime
O1 <- p1/(1-p1)
# Odds of no violent crime
O0 <- p0/(1-p0)
# Odds Ratio
OR <- O1/O0
# OR very close to RR in this case

# Odds of violent crime involvement given high lead exposure 
# are ~6.6 times the same odds given low lead exposure


#######################
# Retrospective Study #
#######################
# Restrospective Study: pick people based on outcome, then look at exposure

dt2 <- as.data.frame(matrix(
  c(0, 0, 5410, 
  0, 1, 186, 
  1, 0, 3590,
  1, 1, 814), ncol = 3, byrow = TRUE))
colnames(dt2) = c("X", "Y", "Count")

# odds(X=1|Y=1)
O12 <- dt2$Count[dt2$X == 1 & dt2$Y == 1] / sum(dt2$Count[dt2$X == 0 & dt2$Y == 1])
# odds(X=1|y=0)
O02 <- dt2$Count[dt2$X == 1 & dt2$Y == 0] / sum(dt2$Count[dt2$X == 0 & dt2$Y == 0])
# Odds Ratio
OR2 <- O12/O02
# We can't make the assumption that 814/3590 represents the odds of violent crime | lead exposure because
# this does not account for prevalence of violent crime involvement in the population

# Odds of high lead exposure given violent crime involvement 
# are ~6.6 time the same odds but not involved in violent crime

# Odds Ratios are 'reversible', odds are not