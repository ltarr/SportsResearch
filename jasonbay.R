library(tidyverse)
library(pitchRx)


nym11gids <- gids[grepl("nyn", gids) & grepl("2011", gids)]
dat <- scrape(game.ids = nym11gids)
pitch2011 <- dat[["pitch"]]
ab2011 <- dat[["atbat"]]

pitch2011 <- pitch2011[,1:49] %>%
  select(num, url, type, px, pz)

ab2011 <- ab2011[,1:34] %>%
  select(num, url, batter)

mydat <- inner_join(ab2011, pitch2011, by = c("num", "url"))

people <- read_csv("https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv")
bayid <- people %>%
  filter(name_first == "Jason", name_last == "Bay") %>%
  select(key_mlbam)

bayactions <- mydat %>%
  filter(batter == bayid$key_mlbam)

bayactions <- bayactions %>%
  mutate(swung = if_else(type == "S" | type == "X", TRUE, FALSE))

bay_sample <- bayactions %>%
  sample_n(600)

k_zone_plot <- ggplot(bay_sample, aes(x = px, y = pz)) +
  geom_rect(xmin = -0.947, xmax = 0.947, ymin = 1.5,
            ymax = 3.6, fill = "lightgray", alpha = 0.01) +
  coord_equal() +
  scale_x_continuous("Horizontal location (ft.)",
                     limits = c(-2, 2)) +
  scale_y_continuous("Vertical location (ft.)",
                     limit = c(0, 5))

bay_loess <- loess(swung ~ px + pz, data = bayactions,
                   control = loess.control(surface = "direct"))

pred_area <- expand.grid(px = seq(-2, 2, by = 0.1),
                         pz = seq(0, 6, by = 0.1))

pred_area_fit <- pred_area %>%
  mutate(fit = as.numeric(predict(bay_loess,
                                  newdata = .)))

bay_plot <- k_zone_plot %+%
  filter(pred_area_fit, fit >= 0, fit <= 1) +
  stat_contour(aes(z = fit, color = stat(level)),
               binwidth = 0.2) +
  scale_color_gradient(low = "white", high = "darkblue")

bay_plot <- bay_plot %>%
  directlabels::direct.label(method = "bottom.pieces")

bay_plot