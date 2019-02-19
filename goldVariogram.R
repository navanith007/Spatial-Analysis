getwd()
library(dplyr)
library(xlsx)
library(data.table)
library(sf)
library(sp)
library(mapview)
library(gstat)
library(ggplot2)
library(gridExtra)
library(flexclust)
library(spatstat.utils)
# loading the data
gold <- fread("gold_samples.dat")


str(gold)
var <- variogram(gold_grade ~ 1, ~ X + Y, gold[1:nrow(gold)/10, ])
g1 <- ggplot(var, aes(x = dist, y = gamma)) +
        geom_line()
# extracting the coords
gold_coord <- gold[,1:2]

#Converting to sf object  

gold.sf.point <- st_as_sf(x = gold_coord, 
                        coords = c("X", "Y"),
                        crs = "+proj=longlat +datum=WGS84")
# plot the gold coords
plot(gold.sf.point)

# map view the gold plot  
mapview(gold.sf.point)

#-----------------------variogram-----------------------
gold_coord.mat <- as.matrix(gold_coord)
length <- nrow(gold_coord.mat)
mat <- gold_coord.mat[1:(length/10), ]
dist_matrix <- dist2(mat, mat)




row <- seq(0.2,400, by = 20)

gamma <- lapply(row, function(row){
        
        
        tollerance <- c(row-10,row+10)
        points <- which(inside.range(dist_matrix, tollerance), arr.ind = TRUE)
        points
        value = sum((gold$gold_grade[points[,1]] - gold$gold_grade[points[, 2]] )^2)
        return (value/(nrow(points) * 2))
})
gam <- unlist(gamma)

var_scratch <- data.frame(distance = row, gam)

g2 <- ggplot(var_scratch, aes(x = distance, gam)) +
        geom_point()
grid.arrange(g1, g2)

#-------------------fitting the sample variance to a model
C <- var(gold$gold_grade[1:nrow(gold)/10])
a <- 100
# 1) Spherical model  
gamma <- function(h){
        if(h >= a){
                gamma = C
        }
        else{
                gamma = C * ( (1.5 * (h/a) ) - (0.5 * (h/a)^3))
        }
        return (gamma)
}
h <- seq(0, 400, by = 0.1)

spherical_gamma <- lapply(h,gamma)
spherical_gamma <- unlist(spherical_gamma)
var_spherical <- as.data.frame(h, spherical_gamma)
g3 <- ggplot(var_spherical, aes(x = h, spherical_gamma)) +
        geom_line()
#g4 <- ggplot() +
       # geom_point(var_scratch, aes(x = distance, gam)) +
        #geom_line(var_spherical, aes(x = h, spherical_gamma))
grid.arrange(g1, g2, g3)


