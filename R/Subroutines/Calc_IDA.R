### Index Decomposition Analysis for China
# Note: Maybe the decomposition does not work correctly because of new column for source region of flow. Needs to be checked.

# Define order of regions that are compared with China
reg_order <- c(10,9,4,2,3,11,6,7,8,5,13,12)

r <- 1
for(r in 1:length(reg_order))
{
  tmp <- MEME_decomposition(reg_1 = 1, reg_2 = reg_order[r])
  if(r == 1) result_IDA <- tmp
  if(r != 1) result_IDA <- cbind(result_IDA,tmp$value)
  
}

colnames(result_IDA)[2:13] <- region_agg[reg_order]
# write.xlsx(x = result_IDA, file = "./output/IDA_eHANPP_per_cap_result_2014_China_vs_all_other_regions.xlsx")

