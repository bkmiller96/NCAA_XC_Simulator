splash_theme <- function(legend = FALSE){
  st <- theme(plot.background = element_rect(fill = "gray95"),
              panel.background = element_rect(fill = "gray95"),
              plot.title = element_markdown(lineheight = 1.1, family = "Open Sans"),
              plot.title.position = "plot",
              axis.line = element_line(color = "gray50"),
              axis.title = element_text(family = "Open Sans", size = 12),
              axis.text = element_text(family = "Open Sans", size = 11),
              strip.text.x = element_text(family = "Open Sans"),
              strip.text.y = element_text(family = "Open Sans"),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(color = "gray90"),
              axis.ticks = element_line(color = "gray50"))
  if(legend){
    st +
      theme(legend.position = "top",
            legend.justification = "left",
            legend.direction = "horizontal",
            legend.background = element_rect(fill = "gray95"),
            legend.key = element_rect(fill = "gray95"),
            legend.text = element_text(family = "Open Sans", margin = margin(r = 5, l = -5, unit = "pt")),
            legend.box.margin = margin(-5,-10,-5,-5),
            legend.margin = margin(t = 0,
                                   r = 0,
                                   b = 0,
                                   l = -1.5,
                                   unit="cm"),
            legend.title = element_blank())
  } else {
    st +
      theme(legend.position = "none")
  }
}

splash_colors <- c(
  "Splash blue" = "#090C9B",
  "orange" = "#3C3744",
  "green" = "#3D52D5",
  "light blue" = "#B4C5E4",
  "dark blue" = "#FBFFF1",
  "red orange" = "#F95738",
  "dark green" = "#3E5622",
  "yellow" = "#F4D35E",
  "light yellow" = "#FAF0CA",
  "purple" = "#e274c5"
  
)

splash_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (splash_colors)
  
  splash_colors[cols]
}

splash_palettes <- list(
  "qualitative" = splash_cols("Splash blue", "yellow", "orange", "green", "red orange", "dark green", "light yellow", "dark blue", "light blue"),
  "diverging" = splash_cols("Splash blue", "yellow"),
  "sequential" = splash_cols("Splash blue", "yellow")
)

splash_pal <- function(palette, reverse = FALSE, ...) {
  pal <- splash_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

scale_color_splash <- function(discrete = TRUE, sequential = FALSE, reverse = FALSE, mp = 0, ...) {
  color_cat <- case_when(discrete & !sequential ~ "qual",
                         discrete & sequential ~ "ord",
                         !discrete & sequential ~ "seq",
                         TRUE ~ "div")
  
  palette <- case_when(color_cat == "qual" ~ "qualitative",
                       color_cat == "ord" ~ "sequential",
                       color_cat == "seq" ~ "sequential",
                       TRUE ~ "diverging")
  
  pal <- splash_pal(palette = palette, reverse = reverse)
  
  if (color_cat == "qual") {
    scale_color_manual(values = unname(splash_palettes$qualitative))
  } else if (color_cat == "ord") {
    discrete_scale("colour", paste0("splash_", palette), palette = pal, ...)
  } else if (color_cat == "seq") {
    scale_color_gradientn(colours = pal(256), ...)
  } else {
    scale_color_gradient2(low = splash_cols("Splash blue"), mid = "white", high = splash_cols("orange"), midpoint = mp)
  }
}

scale_fill_splash <- function(discrete = TRUE, sequential = FALSE, reverse = FALSE, mp = 0, ...) {
  color_cat <- case_when(discrete & !sequential ~ "qual",
                         discrete & sequential ~ "ord",
                         !discrete & sequential ~ "seq",
                         TRUE ~ "div")
  
  palette <- case_when(color_cat == "qual" ~ "qualitative",
                       color_cat == "ord" ~ "sequential",
                       color_cat == "seq" ~ "sequential",
                       TRUE ~ "diverging")
  pal <- splash_pal(palette = palette, reverse = reverse)
  
  if (color_cat == "qual") {
    scale_fill_manual(values = unname(splash_palettes$qualitative))
  } else if (color_cat == "ord") {
    discrete_scale("colour", paste0("splash_", palette), palette = pal, ...)
  } else if (color_cat == "seq") {
    scale_fill_gradientn(colours = pal(256), ...)
  } else {
    scale_fill_gradient2(low = splash_cols("Splash blue"), mid = "white", high = splash_cols("orange"), midpoint = mp)
  }
}
