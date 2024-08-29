### Index Decomposition Analysis for China, Europe and the world average

## Prepare data for IDA and add data for world average for  the composition

tmp <- pop_agg 
tmp[nrow(pop_agg)+1,1] <- "Global"  
tmp[nrow(pop_agg)+1,2] <- sum(pop_agg$value)

pop_IDA <- tmp %>% 
  mutate("stressor" = "POP") %>% 
  rename("destination_region_group" = "region") %>% 
  select(destination_region_group, stressor, value)

tmp <- Results_agg %>% 
  group_by(destination_region_group, stressor) %>% 
  summarise(value = sum(value), .groups = 'drop')

global  <- tmp %>% 
  group_by(stressor) %>% 
  summarise(value = sum(value), .groups = 'drop') %>% 
  mutate("destination_region_group" = "Global") %>% 
  select(destination_region_group, stressor, value)

IDA_data <<- tmp %>% 
  bind_rows(global, pop_IDA)

remove(tmp, global)

# Define general order of regions 
reg_order <- c(10,9,4,2,3,11,6,7,8,5,13,12)
region_list_IDA <<- region_agg[reg_order]
region_list_IDA <<- c("China",region_list_IDA[1:7],"Global",region_list_IDA[8:12])

## IDA for eHANPP per capita of China

main_reg <- "China"
r <- 1
# other_reg <- "Canada"
for(other_reg in region_list_IDA[region_list_IDA != main_reg])
{
  tmp <- MEME_decomposition(reg_1 = main_reg, reg_2 = other_reg)
  if(r == 1) result_IDA <- tmp
  if(r != 1) result_IDA <- cbind(result_IDA,tmp$value)
  r <- r + 1
}

colnames(result_IDA)[2:14] <- region_list_IDA[region_list_IDA != main_reg]
result_IDA_China <- result_IDA


## IDA for eHANPP per capita of Europe

main_reg <- "Europe"
r <- 1
# other_reg <- "Canada"
for(other_reg in region_list_IDA[region_list_IDA != main_reg])
{
  tmp <- MEME_decomposition(reg_1 = main_reg, reg_2 = other_reg)
  if(r == 1) result_IDA <- tmp
  if(r != 1) result_IDA <- cbind(result_IDA,tmp$value)
  r <- r + 1
}

colnames(result_IDA)[2:14] <- region_list_IDA[region_list_IDA != main_reg]
result_IDA_Europe <- result_IDA


## IDA for eHANPP per capita of the global average

main_reg <- "Global"
r <- 1
# other_reg <- "Canada"
for(other_reg in region_list_IDA[region_list_IDA != main_reg])
{
  tmp <- MEME_decomposition(reg_1 = main_reg, reg_2 = other_reg)
  if(r == 1) result_IDA <- tmp
  if(r != 1) result_IDA <- cbind(result_IDA,tmp$value)
  r <- r + 1
}

colnames(result_IDA)[2:14] <- region_list_IDA[region_list_IDA != main_reg]
result_IDA_Global <- result_IDA

