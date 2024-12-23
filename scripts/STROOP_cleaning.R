library(dplyr)

# Percorso della cartella con i file
input_folder <- "C:/Users/sofia/OneDrive/Desktop/dataset studio piloti/STROOP"

# Percorso del file di output
output_file <- "C:/Users/sofia/OneDrive/Desktop/dataset studio piloti/STROOP/raw_stroop_updated.csv"

# Lista dei file da processare
file_paths <- list.files(path = input_folder, pattern = "*.csv", full.names = TRUE)

# Funzione per determinare il separatore corretto
get_separator <- function(file_path) {
  first_line <- readLines(file_path, n = 1)
  if (grepl(";", first_line)) {
    return(";")
  } else {
    return(",")
  }
}

# Funzione per elaborare ogni file
process_file <- function(file_path) {
  # Determina il separatore del file
  sep <- get_separator(file_path)
  
  # Estrai il codice ID dal nome del file
  code_id <- as.numeric(gsub("[^0-9]", "", basename(file_path)))
  
  # Leggi il file
  data <- tryCatch({
    cat("Leggendo il file:", file_path, "\n")
    read.table(file_path, header = TRUE, sep = sep, nrows = 69)
  }, error = function(e) {
    cat("Errore nel leggere il file", basename(file_path), ":", e$message, "\n")
    return(NULL)
  })
  
  # Se il file è vuoto o ha meno di 65 righe, ignoralo
  if (is.null(data) || nrow(data) <= 4) {
    cat("Il file", basename(file_path), "è vuoto o non leggibile.\n")
    return(NULL)
  }
  
  # Controlla se la colonna 'background' è presente e decide se ignorare le prime 4 righe
  if ("background" %in% names(data)) {
    data <- data[-(1:4), ]
  }
  
  # Normalizza i nomi delle colonne
  names(data) <- tolower(gsub("[[:space:]]", "", names(data)))
  
  # Controlla che le colonne necessarie esistano
  required_columns <- c("congruent", "correct", "response_time")
  if (!all(required_columns %in% names(data))) {
    cat("Il file", basename(file_path), "è stato ignorato (colonne mancanti).\n")
    return(NULL)
  }
  
  # Converte la colonna response_time in numerico e ignora errori
  data$response_time <- suppressWarnings(as.numeric(data$response_time))
  if(any(is.na(data$response_time))) {
    cat("Il file", basename(file_path), "ha valori mancanti nella colonna 'response_time'.\n")
  }
  
  # Filtra le risposte corrette
  correct_data <- data %>% filter(correct == 1)
  
  # Calcola ACC e RT
  acc_con <- sum(data$congruent == 1 & data$correct == 1, na.rm = TRUE)
  acc_inc <- sum(data$congruent == 0 & data$correct == 1, na.rm = TRUE)
  
  rt_con <- mean(correct_data$response_time[correct_data$congruent == 1], na.rm = TRUE)
  rt_inc <- mean(correct_data$response_time[correct_data$congruent == 0], na.rm = TRUE)
  
  # Ritorna un data frame con i risultati
  return(data.frame(
    ID = code_id,
    ACC_con = acc_con,
    ACC_inc = acc_inc,
    RT_con = rt_con,
    RT_inc = rt_inc
  ))
}

# Applica la funzione a tutti i file e ignora quelli problematici
results <- bind_rows(lapply(file_paths, process_file))

# Salva i risultati in un file CSV
tryCatch({
  write.csv(results, output_file, row.names = FALSE)
  cat("File di output salvato in:", output_file, "\n")
}, error = function(e) {
  cat("Errore nel salvare il file di output:", e$message, "\n")
})
