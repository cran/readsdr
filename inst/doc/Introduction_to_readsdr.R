## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=FALSE, fig.cap="Figure 1. SIR", out.width = '80%'------------------
filepath <- system.file("models/", "SIR_diagram.png", package = "readsdr")
knitr::include_graphics(filepath)

## -----------------------------------------------------------------------------
library(readsdr)

filepath <- system.file("models/", "SIR.stmx", package = "readsdr")
mdl      <- read_xmile(filepath, graph = TRUE) 

## -----------------------------------------------------------------------------
description <- mdl$description

model_summary <- data.frame(n_stocks    = length(description$levels),
                            n_variables = length(description$variables),
                            n_consts    = length(description$constants))
print(model_summary)

## -----------------------------------------------------------------------------
deSolve_components <- mdl$deSolve_components

all.equal(deSolve_components, xmile_to_deSolve(filepath))

library(deSolve)

simtime <- seq(deSolve_components$sim_params$start,
               deSolve_components$sim_params$stop,
               deSolve_components$sim_params$dt)

output_deSolve <- ode(y      = deSolve_components$stocks,
                      times  = simtime,
                      func   = deSolve_components$func,
                      parms  = deSolve_components$consts, 
                      method = "euler")

result_df <- data.frame(output_deSolve)

head(result_df)

result_df2 <- sd_simulate(deSolve_components)
identical(result_df, result_df2)

## ---- message = FALSE, warning = FALSE, fig.align='center', fig.width = 6-----
library(dplyr)
library(tidyr)
library(ggplot2)

tidy_result_df <- result_df %>% 
  select(time, Susceptible, Infected, Recovered) %>%
  pivot_longer(-time, names_to = "Variable") 

ggplot(tidy_result_df, aes(x = time, y = value)) +
  geom_line(aes(group = Variable, colour = Variable)) +
  theme_classic() +
  theme(legend.position = "bottom")

## ---- fig.align='center', fig.height=7, fig.width=7, message=FALSE, warning=FALSE----
library(igraph)

gr <- graph_from_data_frame(mdl$graph_dfs$edges, directed = TRUE, 
                               vertices = mdl$graph_dfs$nodes)

V(gr)$shape <- ifelse(V(gr)$type == "stock", "rectangle", "circle")

plot.igraph(gr, edge.arrow.size = 0.25,
            vertex.label.cex = 1, layout = layout.lgl(gr))

