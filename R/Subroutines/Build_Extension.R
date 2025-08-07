## Load pre-processed extensions and create single data frame

# Disaggregate iron extraction into biomes
E1 <- Build_Extension_Biomes()
E1 <- t( ( t(E1) / colSums(E1) ) * IOT$e[,4] )
E1[is.na(E1)] <- 0
print( str_c("Sum of iron ore extraction by biome in 2014: ", sum(E1) ," t/year") )

# Load other extensions
E2 <- Build_Extension_AREA_HANPP_AWARE()
E2 <- as.data.frame(E2)
print( str_c("Sum of HANPP of iron ore extraction in 2014: ", rowSums(E2[1,]) ," tC/year") )
print( str_c("Sum of direct land use for iron ore extraction in 2014: ", rowSums(E2[2,]) ," km2/year") )
E3 <- as.data.frame( Build_Extension_Biodiversity_Carbon_Water_Score() )
colnames(E3) <- "Biodiversity_Carbon_Water"

# integrate extensions and discern flow- from index-based extensions
E_flow <- t( rbind(E1, E2)[-15,] )
E_index <- as.matrix( cbind( E3, t( E2[3,] ) ) )

# Adjust for data gaps in polygons
tmp_full <- Code$Z %>% 
  select(Index, RegionName, SectorCode) %>% 
  bind_cols(E_flow %>% as.data.frame()) %>% 
  left_join(base$region %>% 
              select(Name, Region),
            by = c("RegionName" = "Name")) %>% 
  mutate(DE = rowSums(E_flow[,1:12]))

tmp_miss <- tmp_full %>% 
  filter(DE > 1000 & AREA == 0) %>% 
  select(Index, RegionName, Region, DE)

expert_correct <- data.frame("RegionName" = c("Greece","Slovakia","South Korea","Indonesia"),
                             "share_AREA" = c(0.15,-0.15,-0.15,0.15),
                             "share_HANPP" = c(-0.1,0.1,-0.1,0.1))
  
HANPP_AREA_DE <- tmp_full %>% 
  filter(Region %in% unique(tmp_miss$Region)) %>% 
  group_by(Region) %>% 
  summarize(AREA = sum(AREA),
            DE = sum(DE),
            HANPP = sum(HANPP)) %>% 
  mutate(factor_AREA = AREA/DE,
         factor_HANPP = HANPP/AREA) %>% 
  select(-AREA, -DE, -HANPP)

tmp_fill <- tmp_full %>% 
  filter(Index %in% tmp_miss$Index) %>% 
  left_join(expert_correct, by = "RegionName") %>% 
  left_join(HANPP_AREA_DE, by = "Region") %>% 
  mutate(AREA = (1+share_AREA) * DE * factor_AREA,
         HANPP = (1+share_HANPP) * AREA * factor_HANPP) %>% 
  select(colnames(tmp_full))

tmp_NEW <- tmp_full %>% 
  filter(!(Index %in% tmp_fill$Index)) %>% 
  bind_rows(tmp_fill) %>% 
  arrange(-desc(Index)) %>% 
  select(colnames(E_flow)) %>% 
  as.matrix()

E_flow <- tmp_NEW

## Create extension for the EXIOBASE model run
# ex_iron <- 33  # Product code of iron ore in EXIOBASE

tmp <- data.frame("row_reg" = rep(1:32,each = 200),
                     "row_sec" = rep(1:200, 32),
                     "row_index" = 1:(200*32),
                     "col_reg" = rep(1:32,each = (200*32*30)),
                     "col_sec" = rep(1:30, each = (200*32)),
                     "col_index" = rep(1:(30*32), each = (200*32))) %>% 
  mutate(c = case_when(row_reg == col_reg & row_sec == 33 & col_sec == 1 ~ 1,
                       .default = 0))
# Create sparse matrix
disagg <- sparseMatrix(
  i = tmp$row_index, 
  j = tmp$col_index, 
  x = tmp$c,         
  dims = c((200*32),(30*32)) )

# Transform into EXIOBASE 
E_flow_EXIO <- disagg %*% E_flow %>% as.matrix()

diff <- colSums(E_flow)[14] - 5593

Code$Z %>% 
  filter(RegionName == "China",SectorCode == 1) %>% 
  pull(SectorIndex)

E_flow[481,14] <- E_flow[481,14] - diff

tmp %>% 
  filter(col_reg == 17,
         c == 1) %>% 
  pull(row_index)

E_flow_EXIO[3233,14] <- E_flow_EXIO[3233,14] - diff


colSums(E_flow_EXIO)[14]
colSums(E_flow)[14]

remove(tmp, disagg, E1,E2,E3, tmp_fill, tmp_full, tmp_miss, tmp_NEW, expert_correct, HANPP_AREA_DE)
