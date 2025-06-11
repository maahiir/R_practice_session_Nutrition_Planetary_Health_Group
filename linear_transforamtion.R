Sys.setenv(LANGUAGE = "en")
install.packages("gganimate", repos = "https://cloud.r-project.org/")
library(ggplot2)
library(gganimate)
library(dplyr)



# Define two vectors (you can change these!)
v1 <- c(1, 2)
v2 <- c(2, 1)

# Create a grid of linear combinations: a * v1 + b * v2
coeff_grid <- expand.grid(a = seq(-2, 2, 0.2), b = seq(-2, 2, 0.2))
span_points <- coeff_grid %>%
  rowwise() %>%
  mutate(x = a * v1[1] + b * v2[1],
         y = a * v1[2] + b * v2[2])

# Set up base plot
ggplot() +
  # Axes
  geom_hline(yintercept = 0, color = "gray50") +
  geom_vline(xintercept = 0, color = "gray50") +
  
  # Span points (shows filled 2D space)
  geom_point(data = span_points, aes(x = x, y = y), alpha = 0.3, color = "blue") +
  
  # Basis vectors from origin
  geom_segment(aes(x = 0, y = 0, xend = v1[1], yend = v1[2]),
               arrow = arrow(length = unit(0.1, "inches")),
               color = "red", size = 1.2) +
  geom_segment(aes(x = 0, y = 0, xend = v2[1], yend = v2[2]),
               arrow = arrow(length = unit(0.1, "inches")),
               color = "green", size = 1.2) +
  
  coord_fixed(xlim = c(-5, 5), ylim = c(-5, 5)) +
  labs(title = "Span of Two Vectors in 2D Space") +
  theme_minimal()




# Create smooth parametric path through 2D space
N <- 200  # total frames
t <- seq(0, 2*pi, length.out = N)
t1_vals <- 2 * cos(t)    # smoothly circle through coefficient space
t2_vals <- 2 * sin(t)

# Build dataframe
data <- tibble(
  frame = 1:N,
  t1 = t1_vals,
  t2 = t2_vals,
  x1 = t1 * v1[1],
  y1 = t1 * v1[2],
  x2 = t2 * v2[1],
  y2 = t2 * v2[2],
  xr = x1 + x2,
  yr = y1 + y2
)

# Plot
p <- ggplot(data) +
  geom_hline(yintercept = 0, color = "gray80") +
  geom_vline(xintercept = 0, color = "gray80") +
  
  # Vector 1 (red)
  geom_segment(aes(x = 0, y = 0, xend = x1, yend = y1),
               color = "red", arrow = arrow(length = unit(0.15, "inches")),
               size = 1.2) +
  
  # Vector 2 (green)
  geom_segment(aes(x = 0, y = 0, xend = x2, yend = y2),
               color = "green", arrow = arrow(length = unit(0.15, "inches")),
               size = 1.2) +
  
  # Resultant vector (blue)
  geom_segment(aes(x = 0, y = 0, xend = xr, yend = yr),
               color = "blue", arrow = arrow(length = unit(0.15, "inches")),
               size = 1.2) +
  
  coord_fixed(xlim = c(-6, 6), ylim = c(-6, 6)) +
  theme_minimal(base_size = 14) +
  labs(title = "Smooth Span Sweep of Two Vectors",
       subtitle = "Red = v1, Green = v2, Blue = v1 + v2") +
  transition_manual(frame)

# Render smooth animation
animate(p, fps = 30, duration = 7, width = 600, height = 600,
        renderer = gifski_renderer("smooth_sweep_span.gif"))


