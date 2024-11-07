## Create supply use tables from raw data and compile IO model
SUT <- Load_SUT(type = "Results")                    
IOT <- Build_IOT(SUT = SUT,mode = "ixi")                          
## Build extensions
source( paste0(path$subroutines,"/Build_Extension.R") )


Z <- IOT$Z
x <- IOT$x
y <- rowSums(IOT$y)

str(Z)
str(x)

A <- t(t(Z)/x)
base$product$Name

# Define sectors to extract
extract_sec <- 4:6

# Get filter vector
filt <- Code$Z %>% 
  mutate(v = case_when(SectorCode %in% extract_sec ~ 0,
                       .default = 1)) %>% 
  pull(v)

A_star <- A * filt
A_0 <- A - A_star
Y_star <- y * filt 
I <- diag(1, nrow(A),nrow(A))
L_star <- solve(I - A_star)
L <- solve(I - A)
sum(L_star)
sum(L)

term_1 <- c( A_0 %*% L_star %*% Y_star )
FP <- L %*% diag( term_1 )

sum(FP)

final <- Agg(FP, aggkey = Code$Z$SectorName, dim = 1)
final <- Agg(final, aggkey = Code$Z$SectorName, dim = 2)

rowSums(final)
