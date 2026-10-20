pptx_path <- "ppt/ppt_template.pptx"
pres <- read_pptx(pptx_path)

ppt_struktur <- layout_properties(pres) |>
  as_tibble() |>
  select(master_name, name, type, id, ph_label, ph, everything()) |>
  arrange(master_name, name, id)

