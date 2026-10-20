
# Plockar ut en lista på antalet kommuner som vi behöver skapa PPT för
lista_kommuner <- folkmangd_scb |>
  distinct(region) |>
  slice(0:antal_rapporter) |> pull()

# Sota loopen som kör igenom listan av kommuner och skapar en PPT för varje
for (kommun in lista_kommuner){
  
  # Skapar diagramdata
  diagramdata_tidsserie <- folkmangd_scb |> 
    filter(region == kommun) |> 
    group_by(datum) |> 
    summarise(Befolkning = sum(folkmangd)) |> 
    ungroup() 
  
  
  # Skapar PPT för vald kommun
  ppt <- read_pptx("ppt/ppt_template.pptx")
  
  # Lägger till rubrikbild
  ppt <- add_Rubrikbild(ppt = ppt,
                       Rubrik_1 = paste0("Automagisk rapport för ", kommun),
                       Underrubrik_2 = "Skapad helt utan vett, sants eller smak",
                       Platshållare_för_datum_3 = dagens_datum,
                       Platshållare_för_sidfot_4 = "",
                       Platshållare_för_bildnummer_5 = "Slide 1"
                                             )


  # Skapar tidseriediagram native i excel
  diagram_formaterat <- bar_chart(data = diagramdata_tidsserie |> mutate(datum = format(datum,"%B-%y")),
              x = "datum",
              y = "Befolkning",
              diagramrubrik = "Befolkningsutveckling")
  
  ppt <- add_Två_delar_bullet_text(ppt=ppt,
                       diagramdata = diagramdata_tidsserie,
                       Rubrik_1 = "Befolkningsutveckling Exceldiagram",
                       Platshållare_för_innehåll_2 = text_till_Två_delar(diagramdata_tidsserie, kommun),
                       Platshållare_för_innehåll_3 = diagram_formaterat,
                       Platshållare_för_datum_4 = today(),
                       Platshållare_för_sidfot_5 = "Skapad av Christian Holmström",
                       Platshållare_för_bildnummer_6 = "Slide 2"
                       )
  
  # Skapar tidseriediagram med bilder istället för native excel diagram
  
  diagram_formaterat_bild <- ggplot(diagramdata_tidsserie, aes(x = datum, y = Befolkning)) +
    geom_line(color = "#EA8F12", linewidth = 1.1) +
    geom_point(color = "#EA8F12", size = 2) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y",
                 expand = expansion(mult = c(0.02, 0.05))) +
    scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE, trim = TRUE),
                       expand = expansion(mult = c(0.05, 0.1))) +
    labs(title = "Diagramrubrik", x = NULL, y = NULL,
         caption = "Källa: ...") +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor    = element_blank(),
      panel.grid.major.x  = element_blank(),
      panel.grid.major.y  = element_line(color = "#D9D9D9", linewidth = 0.4),
      axis.text           = element_text(color = "#595959"),
      axis.text.x         = element_text(angle = 45, hjust = 1),
      plot.title          = element_text(face = "bold", color = "#33414E", size = 14),
      plot.caption        = element_text(color = "#62717F", size = 8),
      plot.title.position = "plot"
    ) 
  
  bildfil <- diagram_formaterat_bild |> till_png()
  
  ppt <- add_Två_delar_bullet_text(ppt=ppt,
                                   diagramdata = diagramdata_tidsserie,
                                   Rubrik_1 = "Befolkningsutveckling GGPLOT bild",
                                   Platshållare_för_innehåll_2 = text_till_Två_delar(diagramdata_tidsserie, kommun),
                                   typ_av_platshållare_3 = "Bild",
                                   Platshållare_för_innehåll_3 = bildfil,
                                   Platshållare_för_datum_4 = today(),
                                   Platshållare_för_sidfot_5 = "Skapad av Christian Holmström",
                                   Platshållare_för_bildnummer_6 = "Slide 2"
  )
  
  
  print(ppt, paste0("output_ppt/", dagens_datum, "_",kommun, ".pptx"))

}
