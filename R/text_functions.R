# funktion för att omvanlda alla värden under asb(13) till bokstäver
siffror_till_text <- function(siffra) {
  return(case_when(
    abs(siffra) == 1 ~ "ett",
    abs(siffra) == 2 ~ "två",
    abs(siffra) == 3 ~ "tre",
    abs(siffra) == 4 ~ "fyra",
    abs(siffra) == 5 ~ "fem",
    abs(siffra) == 6 ~ "sex",
    abs(siffra) == 7 ~ "sju",
    abs(siffra) == 8 ~ "åtta",
    abs(siffra) == 9 ~ "nio",
    abs(siffra) == 10 ~ "tio",
    abs(siffra) == 11 ~ "elva",
    abs(siffra) == 12 ~ "tolv",
    TRUE ~ as.character(format(abs(siffra), big.mark = " ", scientific = FALSE))
  ))
}

datum_text <- function(datum) {
  datum <- as.Date(datum)
  
  dagar <- c("första", "andra", "tredje", "fjärde", "femte", "sjätte",
             "sjunde", "åttonde", "nionde", "tionde", "elfte", "tolfte",
             "trettonde", "fjortonde", "femtonde", "sextonde", "sjuttonde",
             "artonde", "nittonde", "tjugonde", "tjugoförsta", "tjugoandra",
             "tjugotredje", "tjugofjärde", "tjugofemte", "tjugosjätte",
             "tjugosjunde", "tjugoåttonde", "tjugonionde", "trettionde",
             "trettioförsta")
  
  manader <- c("januari", "februari", "mars", "april", "maj", "juni",
               "juli", "augusti", "september", "oktober", "november", "december")
  
  paste0("Den ", dagar[as.integer(format(datum, "%d"))], " ",
         manader[as.integer(format(datum, "%m"))], " år ",
         format(datum, "%Y"))
}

# Funktion för att skapa bullepoints av data till slide text_till_Två_delar

text_till_Två_delar <- function(df, kommun) {

  senaste_värdet <- df[[nrow(df), ncol(df)]]
  första_värdet <- df[[nrow(df) - 1, ncol(df)]]
  förändring <- senaste_värdet - första_värdet
  
  text_1 <- ""
  text_2 <- ""
  text_3 <- ""

        text_1 <- paste0(datum_text(max(df$datum)),
                         " uppgick befolknignen i ",
                         kommun,
                         " till ",
                         format(senaste_värdet, big.mark = " "))
        
        text_2 <- if (förändring == 0) {
          paste0("Sedan ", datum_text(min(df$datum)),
                 " har befolkningen varit oförändrad på ",
                 siffror_till_text(senaste_värdet), " personer.")
        } else {
          paste0("Sedan ", sub("^Den", "den", datum_text(min(df$datum))),
                 " har befolkningen ",
                 if (förändring > 0) "ökat" else "minskat",
                 " med ", siffror_till_text(abs(förändring)),
                 " personer, från ", siffror_till_text(första_värdet),
                 " till ", siffror_till_text(senaste_värdet), ".")
        }
        
        text_3 <- "Det är såklar väldigt kul för alla inblandade"
    
  bildtext <- c(text_1, text_2, text_3)
  
  # Remove empty strings
  bildtext <- bildtext[nzchar(bildtext)]
  
  return(bildtext)
}
