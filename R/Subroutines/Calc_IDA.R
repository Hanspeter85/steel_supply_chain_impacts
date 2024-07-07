### Index Decomposition Analysis for China
# Note: Maybe the decomposition does not work correctly because of new column for source region of flow. Needs to be checked.

## IDA for eHANPP per capita of China

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
result_IDA_China <- result_IDA


## IDA for eHANPP per capita of Europe

# Define order of regions that are compared with China
reg_order <- c(1,10,9,4,3,11,6,7,8,5,13,12)

r <- 1
for(r in 1:length(reg_order))
{
  tmp <- MEME_decomposition(reg_1 = 2, reg_2 = reg_order[r])
  if(r == 1) result_IDA <- tmp
  if(r != 1) result_IDA <- cbind(result_IDA,tmp$value)
  
}

colnames(result_IDA)[2:13] <- region_agg[reg_order]
result_IDA_Europe <- result_IDA


## IDA for eHANPP per capita of India

# Define order of regions that are compared with China
reg_order <- c(1,10,9,4,2,3,11,6,7,8,5,13)

r <- 1
for(r in 1:length(reg_order))
{
  tmp <- MEME_decomposition(reg_1 = 12, reg_2 = reg_order[r])
  if(r == 1) result_IDA <- tmp
  if(r != 1) result_IDA <- cbind(result_IDA,tmp$value)
  
}

colnames(result_IDA)[2:13] <- region_agg[reg_order]
result_IDA_India <- result_IDA

