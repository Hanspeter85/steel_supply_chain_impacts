

# Load/compute EXIOBASE monetary MRIO
  
if(!file.exists(str_c(path$EXIO,"Parsed/L_agg_fin_2014.csv")))
{
  # Technology matrix (products by product):
  tmp <- fread(paste0(path$EXIO,"/IOT_2014_pxp/A.txt"), skip = 3)
  A <- as.matrix(tmp[,3:9802])
  
  
  # Final demand (products by category):
  Y <- fread(paste0(path$EXIO,"/IOT_2014_pxp/Y.txt"), skip = 3)[,c(-1,-2)] %>% 
    as.matrix()
  
  # Total output 
  x <- fread(paste0(path$EXIO,"/IOT_2014_pxp/x.txt"))[,c(-1,-2)] %>% 
    as.matrix() %>% c()
  
  # Transaction matrix
  Z  <- t(t(A)*x)
  
  Conco[["EXIO_2_base_long"]] <- Conco$EXIO_2_base %>% 
    as.data.frame() %>% 
    mutate("reg_exio" = 1:49) %>% 
    pivot_longer(cols = base$region$Abbrev,
                 names_to = "reg_pio") %>% 
    filter(value != 0) %>% 
    select(-value) %>% arrange(-desc(reg_exio))
  
  agg_key <- str_c(rep(Conco$EXIO_2_base_long$reg_pio, each = 200),"_", rep(as.character(1:200), 49) )  
  agg_key_y <- rep(Conco$EXIO_2_base_long$reg_pio, each = 7)
  
  
  Z_agg <- Agg( Agg(Z, agg_key, 1), agg_key, 2)
  Y_agg <- Agg( Agg(Y, agg_key, 1), agg_key_y, 2)
  
  sort <- list("z" = data.frame("reg" = rep(base$region$Abbrev, each = 200),
                                "sec" = rep(as.character(1:200), 32),
                                "index" = 1:(200*32)) %>% 
                 mutate(key = str_c(reg,"_",sec)) %>% 
                 left_join(data.frame("pos_key" = colnames(Z_agg),
                                      "pos_index" = 1:(32*200)),
                           by = c("key" = "pos_key")),
               "y" = data.frame("reg" = base$region$Abbrev,
                                "index" = 1:32) %>% 
                 left_join(data.frame("pos_key" = colnames(Y_agg),
                                      "pos_index" = 1:32),
                           by = c("reg" = "pos_key")))
  
  fwrite(sort$z %>% select(index, reg, sec), str_c(path$EXIO,"Parsed/Zcode_agg.csv"))
  
  
  Z_agg_fin <- Z_agg[sort$z$pos_index,sort$z$pos_index]
  Y_agg_fin <- Y_agg[sort$z$pos_index,sort$y$pos_index]
  
  x_agg_fin <- rowSums(Z_agg_fin) + rowSums(Y_agg_fin)
  
  A_agg_fin <- t(t(Z_agg_fin)/x_agg_fin)
  A_agg_fin[is.na(A_agg_fin)] <- 0
  
  fwrite(Z_agg_fin, str_c(path$EXIO,"Parsed/Z_agg_fin_2014.csv"))
  fwrite(Y_agg_fin, str_c(path$EXIO,"Parsed/Y_agg_fin_2014.csv"))
  fwrite(A_agg_fin, str_c(path$EXIO,"Parsed/A_agg_fin_2014.csv"))
  
  I <- diag(nrow(A_agg_fin))
  
  L <- solve(I - A_agg_fin)
  fwrite(L, str_c(path$EXIO,"Parsed/L_agg_fin_2014.csv"))
  
  EXIO <- list("L" = L,
               "Y" = Y_agg_fin)
  
  remove(I, L, sort, tmp, Y, Y_agg, Y_agg_fin, Z, Z_agg, Z_agg_fin, agg_key, agg_key_y, x, x_agg_fin)
  
}else
{
  EXIO <- list("L" = fread(str_c(path$EXIO,"Parsed/L_agg_fin_2014.csv")) %>% 
                 as.matrix(),
               "Y" = fread(str_c(path$EXIO,"Parsed/Y_agg_fin_2014.csv")) %>% 
                 as.matrix(),
               "Zcode" = fread(str_c(path$EXIO,"Parsed/Zcode_agg.csv")))
}
  
  