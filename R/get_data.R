# Hämta befolkningsdata från SCB för att ha någtt att labba med. 100% CLAUDE SLOP för att få det till en dataframe, orkar inte lära mig JSON 

url <- "https://www.statistikdatabasen.scb.se/sq/179310"

js <- read_json(url)

dim_ids <- unlist(js$id)

dim_tabeller <- dim_ids |>
  set_names() |>
  map(\(id) {
    kat    <- js$dimension[[id]]$category
    koder  <- names(kat$index)[order(unlist(kat$index))]
    tibble(
      kod     = koder,
      etikett = unname(unlist(kat$label[koder]))
    )
  })

grid <- do.call(expand_grid, map(dim_tabeller, "kod"))

varden <- map_dbl(js$value, \(x) if (is.null(x)) NA_real_ else x)

stopifnot(nrow(grid) == length(varden))

folkmangd_scb <- grid |>
  mutate(folkmangd = varden) |>
  left_join(dim_tabeller$Region |> rename(Region = kod, region = etikett), by = "Region") |>
  left_join(dim_tabeller$Alder  |> rename(Alder  = kod, alder  = etikett), by = "Alder") |>
  transmute(
    region_kod = Region,
    region,
    alder_kod  = Alder,
    alder      = fct_inorder(alder),
    tid        = Tid,
    datum      = ym(str_replace(Tid, "M", "-")),   # första dagen i månaden
    folkmangd
  )

