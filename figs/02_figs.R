# Figures for lecture on data cleaning

# Load packages ----
library(dplyr)
library(ggplot2)
library(amt)
library(mvtnorm)
library(ragg)

# Diffusion vs ballistic motion ----

# ... ballistic motion ----
st <- c(446200, 4625800)
ball <- data.frame(x = st[1] + 707.1 * 0:50, 
                   y = st[2] + 707.1 * 0:50) %>% 
  mutate(t = as.POSIXct("2021-06-20 12:00:00") +
           0:50 * 60 * 60) %>% 
  make_track(x, y, t, crs = 32612) %>% 
  steps() %>% 
  mutate(hour = 1:nrow(.))

ball_plot <- ball %>% 
  ggplot(aes(x = x1_, y = y1_, xend = x2_, yend = y2_, color = hour)) +
  geom_segment() +
  geom_point() +
  coord_sf(crs = 32612) +
  scale_color_viridis_c(name = "Hour") +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Ballistic Motion") +
  theme_bw()

ggsave("figs/ballistic.png", plot = ball_plot, device = agg_png,
       width = 600, height = 600, units = "px", dpi = 110)

# ... diffusive motion ----
dif <- data.frame(x = NA, y = NA, t = ball$t1_)
dif$x[1] <- st[1]
dif$y[1] <- st[2]

# Step length always 1000 m
# Heading uniformly distributed (thus turn angle uniform)
set.seed(123456)
for (i in 2:nrow(dif)) {
  # Assume 0 is north and we measure clockwise
  angle <- runif(1, min = -1 * pi, max = pi)
  dif$x[i] <- dif$x[i - 1] + sin(angle) * 1000
  dif$y[i] <- dif$y[i - 1] + cos(angle) * 1000
}

dif <- dif %>% 
  make_track(x, y, t, crs = 32612) %>% 
  steps() %>% 
  mutate(hour = 1:nrow(.))

diff_plot <- dif %>% 
  ggplot(aes(x = x1_, y = y1_, xend = x2_, yend = y2_, color = hour)) +
  geom_segment() +
  geom_point() +
  coord_sf(crs = 32612) +
  scale_color_viridis_c(name = "Hour") +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Diffusive Motion") +
  theme_bw()

ggsave("figs/diffusive.png", plot = diff_plot, device = agg_png,
       width = 600, height = 600, units = "px", dpi = 110)

# ... combine ----
comb <- rbind(mutate(ball, type = "Ballistic"),
              mutate(dif, type = "Diffusive"))

comb_plot <- comb %>% 
  ggplot(aes(x = x1_, y = y1_, xend = x2_, yend = y2_, color = type)) +
  geom_segment() +
  geom_point() +
  coord_sf(crs = 32612) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Ballistic vs. Diffusive Motion") +
  theme_bw()

ggsave("figs/combined.png", plot = comb_plot, device = agg_png,
       width = 600, height = 600, units = "px", dpi = 110)

# ... one step ----
one_step <- ball[1, ] %>% 
  ggplot(aes(x = x1_, y = y1_, xend = x2_, yend = y2_, color = hour)) +
  geom_segment(arrow = arrow(angle = 15, 
                             length = unit(0.15, "inches"),
                             type = "closed"),
               linewidth = 1) +
  geom_point(size = 3) +
  geom_label(aes(x = 446500, y = 4626200, label = "1.0 km/h"), 
             inherit.aes = FALSE) +
  coord_sf(crs = 32612) +
  scale_color_viridis_c(name = "Hour") +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Constant Speed (1.0 km/h)") +
  theme_bw()

ggsave("figs/one_step.png", plot = one_step, device = agg_png,
       width = 600, height = 500, units = "px", dpi = 120)
