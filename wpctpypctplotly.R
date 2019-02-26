library(plotly)
library(Lahman)

# Only take into consideration teams from 2010 and onwards.
my_teams <- Teams %>%
  filter(yearID >= 2010) %>%
  mutate(w_pct = W / G, py_pct = (R^2) / (R^2 + RA^2))

# Create linear model, with Py % being response variable and W % being explanatory variable.
fit <- lm(py_pct ~ w_pct, data = my_teams)

# Using plot_ly to create an interactive scatterplot.
# Notice the editing of the hoverinfo, to make interactivity even better.
# No legend is needed.
p <- my_teams %>%
  plot_ly(x = ~w_pct, y = ~py_pct) %>%
  add_markers(hoverinfo = "text",
              text = ~paste(teamID, yearID, "<br>",
                            "Win %: ", round(w_pct * 100, 1), "<br>",
                            "Pythag %: ", round(py_pct * 100, 1), "<br>")) %>%
  add_lines(x = ~w_pct, y = fitted(fit), name = "Linear Fit") %>%
  layout(title = "Pythagorean % vs. Win %",
         showlegend = FALSE)
  
#api_create(p, filename="py_vs_w")
