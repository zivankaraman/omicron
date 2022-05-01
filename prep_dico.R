dico <- read.csv(file = "data/dico.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
txt_fr <- dico$fr
txt_en <- dico$en
names(txt_fr) <- names(txt_en) <- dico$txt 
texte <- list(en = as.list(txt_en), fr = as.list(txt_fr))
saveRDS(texte, file = "data/dico.rds")

cat(sprintf('%s <- as.character(myText["%s"])', dico$txt, dico$txt), sep = "\n", file = "clipboard")

