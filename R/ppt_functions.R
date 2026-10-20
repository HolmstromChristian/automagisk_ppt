# Skapa ett stapeldiagram

bar_chart <- function(data, x, y, group = NULL, diagramrubrik=""){
  
  generate_colors <- function(n) {
    palette <- colorRampPalette(c(
      "#EA8F12", 
      "#B12F26",  
      "#33414E",  
      "#62717F",  
      "#5B9BC3", 
      "#71B396", 
      "#795974"   
    ))(7)
    
    palette[1:n]
  }
  
  theme <- mschart_theme(grid_major_line_x = fp_border(style = "none"),
                         grid_minor_line_y = fp_border(style = "none"),
                         grid_major_line_y = fp_border(width = 0.75,
                                                       color = "#D9D9D9",
                                                       style = "solid"),
                         double_fmt = "#,##0",
                         axis_text_y = fp_text(color = "#595959",
                                               bold = FALSE, font.size = 12),
                         legend_text = fp_text(color = "#000000",
                                               bold = FALSE, font.size = 9))
  
  
  if(is.null(group)){
    
    my_barchart <- ms_barchart(data, x, y)
    
    my_barchart <- set_theme(my_barchart, theme)
    
    ## Only provide one color when there isn't a group
    color <- generate_colors(1)
    
    my_barchart <- chart_data_fill(my_barchart,
                                   values = color)
    
    my_barchart <- chart_data_stroke(my_barchart,
                                     values = color)
    
  } else {
    
    my_barchart <- ms_barchart(data, x, y, group)
    
    my_barchart <- set_theme(my_barchart, theme)
    
    ## mschart needs a named vector to color bars properly, below we generate these
    unique_series <- unique(data[[group]])
    
    # Generate colors for unique series
    colors <- generate_colors(length(unique_series))
    
    # Create a named vector with series as names and colors as values
    serie_colors <- setNames(colors, unique_series)
    
    my_barchart <- chart_data_fill(my_barchart,
                                   values = serie_colors)
    
    my_barchart <- chart_data_stroke(my_barchart,
                                     values = serie_colors)
    
  }
  
  my_barchart <- chart_data_line_width(my_barchart,
                                       values =0)
  
  my_barchart <- chart_settings(my_barchart,
                                grouping = "standard",
                                gap_width = 219, 
                                overlap = -27)
  
  my_barchart <- chart_ax_x(my_barchart, 
                            major_tick_mark = "none",
                            minor_tick_mark = "none",
                            rotation = -45)
  
  my_barchart <- chart_ax_y(my_barchart,
                            limit_min =0,
                            major_tick_mark = "none",
                            minor_tick_mark = "none",
                            num_fmt = "#,##0", rotation = 0)
  
  my_barchart <- chart_labels(my_barchart,
                              title = diagramrubrik)
  
  
  # my_barchart <- chart_data_labels(my_barchart,
  #                                  show_val = TRUE,
  #                                  num_fmt = "#,##0",
  #                                  position = 'outEnd')
  
  my_barchart <- chart_labels_text(my_barchart,
                                   values = fp_text(font.size = 9, color = "#000000"))
  
  my_barchart
  
}

add_bullet_points <- function(text, font_size = 18){
  
  font_size = font_size
  
  block_list(
    fpar(
      ftext(
        paste(text, collapse = "\n"),
        prop = fp_text(font.size = font_size, color = "#000000"))
    )
  )
}

# Skapar en temporär bildfil av en GGPLOT
till_png <- function(diagram, width = 5, height = 4, res = 300) {
  fil <- tempfile(fileext = ".png")
  png(fil, width = width, height = height, units = "in", res = res, bg = "white")
  print(diagram)
  dev.off()
  fil
}


# Rubrikbild ----
# Här bygger vi funktion för att skapa "Två delar" i en PowerPoint. "Två delar" som har följande ph_label element
# Rubrik 1	                    | Detta värde harm type: ctrTitle
# Underrubrik 2	                | Detta värde harm type: subTitle
# Platshållare för datum 3    	| Detta värde harm type: dt
# Platshållare för sidfot 4	    | Detta värde harm type: ftr
# Platshållare för bildnummer 5	| Detta värde harm type: sldNum

add_Rubrikbild <- function(ppt, 
                           Rubrik_1,
                           Underrubrik_2,
                           Platshållare_för_datum_3,
                           Platshållare_för_sidfot_4,
                           Platshållare_för_bildnummer_5 ){

  ppt |>
  add_slide(layout = "Rubrikbild", master = "Office-tema") |>
  ph_with(
    value = Rubrik_1,
    location = ph_location_label(ph_label = "Rubrik 1")) |> 
  ph_with(
    value = Underrubrik_2,
    location = ph_location_label(ph_label = "Underrubrik 2")) |> 
  ph_with(
   value = Platshållare_för_datum_3,
   location = ph_location_label(ph_label = "Platshållare för datum 3")) |> 
  ph_with(
    value = Platshållare_för_sidfot_4,
    location = ph_location_label(ph_label = "Platshållare för sidfot 4")) |> 
  # Här använder jag ph_location_type istället för ph_label. Jag vet ärligt talat inte när det är "bäst" att göra på det ena eller andra sättet.
  ph_with(
    value = Platshållare_för_bildnummer_5,
    location = ph_location_type(type = "sldNum"))
    
}

# Två delar ---- 

# Här bygger vi funktion för att skapa "Två delar" i en PowerPoint. "Två delar" som har följande ph_label element

# Rubrik 1                      | Detta värde har type= title  
# Platshållare för innehåll 2   | Detta värde har type=
# Platshållare för innehåll 3   | Detta värde har type=
# Platshållare för datum 4      | Detta värde har type= dt
# Platshållare för sidfot 5     | Detta värde har type= ftr
# Platshållare för bildnummer 6 | Detta värde har type= sldNum 

add_Två_delar_bullet_text <- function(ppt,
                                      diagramdata,
                                      Rubrik_1,
                                      Platshållare_för_innehåll_2,
                                      Platshållare_för_innehåll_3,
                                      typ_av_platshållare_3 = "Inte bild",
                                      Platshållare_för_datum_4,
                                      Platshållare_för_sidfot_5,
                                      Platshållare_för_bildnummer_6,
                                      font_size_bullets) {
  
  bullets <- add_bullet_points(Platshållare_för_innehåll_2)
  
  innehall_3 <- if (typ_av_platshållare_3 == "Bild") {
    
    sokvag <- if (inherits(Platshållare_för_innehåll_3, "ggplot")) {
      till_png(Platshållare_för_innehåll_3)
    } else {
      Platshållare_för_innehåll_3   #
    }
    
    external_img(sokvag)  
    
  } else {
    Platshållare_för_innehåll_3
  }
  
  ppt <- ppt |>
    add_slide(layout = "Två delar", master = "Office-tema") |>
    ph_with(value = Rubrik_1,
            location = ph_location_label(ph_label = "Rubrik 1")) |>
    ph_with(value = bullets,
            location = ph_location_label(ph_label = "Platshållare för innehåll 2"),
            level_list = c(1L)) |>
    ph_with(value = innehall_3,
            location = ph_location_label(ph_label = "Platshållare för innehåll 3"))
  
  ppt   # returnera presentationen
}



