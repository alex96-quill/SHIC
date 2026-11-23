


# evalfis solo sulle righe con tutti gli input del modulo; le altre restano NA
valuta_fis <- function(input, fis) {
  input <- as.matrix(input)
  out <- rep(NA_real_, nrow(input))
  ok <- complete.cases(input)
  out[ok] <- as.vector(evalfis(input[ok, , drop = FALSE], fis))
  out
}




# TEST 100%, verifica varibili sabbia e argilla nuovo metodo prova su tutto il dataset ----

# # somma colonna riga per riga, se valore presente somma e ignora gli NA, se tutti NA darà NA
somma_na <- function(df) if_else(rowSums(!is.na(df)) == 0, NA_real_, rowSums(df, na.rm = TRUE)) 

# CONTROLLO COMPLETEZZA ----

# # Per ogni riga controlla se le variabili scelte sono presenti (non NA)
check_completezza <- function(df, vars) {
  df %>% 
    mutate(
      n_mancanti  = rowSums(is.na(pick(all_of(vars)))),           # quante variabili mancano nella riga
      perc_compl  = round(100 * (1 - n_mancanti / length(vars)), 1), # % di variabili presenti
      completezza = if_else(n_mancanti == 0, 1, 0),
      var_mancanti = apply(is.na(pick(all_of(vars))), 1,              # nomi delle variabili mancanti
                           function(r) paste(vars[r], collapse = ", "))
    )
}

# richiamo funzione nello script
# # Variabili che voglio siano presenti
# var_richieste <- c("SAND", "SILT", "clay", "org_car")
# 
# data_compl <- check_completezza(data_check_var, var_richieste)
# View(data_compl)
# 
# # # Riepilogo
# table(data_compl$completezza)
# 
# # # Solo le righe incomplete
# data_incompleti <- filter(data_compl, completezza == "Incompleto")
# View(data_incompleti)
# 
# 
# source("Script/funzioni.R")   # carica tutte le funzioni, devo mettere tutte le funzioni in un'unico script 




# CORREZIONE ORIZZONTI NA ----
# # Ultimo orizzonte di ogni profilo con lower NA: lower = upper + 30
correggi_lower_na <- function(df, prof = 30) {
  df %>% 
    arrange(point_id, upper) %>% 
    group_by(point_id) %>% 
    mutate(lower = if_else(row_number() == n() & is.na(lower), #condizione, se il lower l'ultimo orizzonte è NA
                           upper + prof, # se TRUE
                           lower)) %>%  # se FALSE
    ungroup()
}

# come richiamare nello script
# data_3 <- correggi_lower_na(data_2)





# CORREZIONE DISCONTINUITA' TRA ORIZZONTI (lower diverso da upper successivo) ----

# # Correzione discontinuità: lower e upper successivo devono coincidere

correggi_discontinuita <- function(df) {
  df %>% 
    arrange(point_id, upper) %>% # ordina righe per profilo e ogni profilo per upper (alto verso il basso)
    group_by(point_id) %>% # raggruppa per profilo
    mutate(
      lower = pmax(lower, lead(upper), na.rm = TRUE),   # corregge il lower, pmax confronta riga per riga e tiene il più grande. se low<upper sotto vince upper sotto, se low>upper sotto vince lower
      upper = coalesce(lag(lower), upper)               # lag(lower) -> lower orizzonte sopra, già corretto dalla riga precedente
    ) %>% 
    ungroup()
}

# come richiamare nello script
# data_3 <- data_2 %>% 
#   correggi_discontinuita()


# RULE FIRING ----
rule_firing <- function(data_matrix, fis) {
  rule <- fis$rule
  m    <- ncol(data_matrix)
  
  # # Gradi di appartenenza: per ogni input una matrice osservazioni x MF
  mf_deg <- lapply(seq_len(m), function(i)
    sapply(fis$input[[i]]$mf, function(mf) evalmf(data_matrix[, i], mf$type, mf$params))
  )
  
  # # Forza di attivazione di ogni regola
  firing <- sapply(seq_len(nrow(rule)), function(r) {
    usati <- which(rule[r, 1:m] != 0)              # input presenti nella regola
    vals  <- lapply(usati, function(i) {
      j  <- rule[r, i]
      mu <- mf_deg[[i]][, abs(j)]
      if (j < 0) 1 - mu else mu                    # j negativo = NOT
    })
    op <- if (rule[r, ncol(rule)] == 1) pmin else pmax   # 1 = AND, 2 = OR
    Reduce(op, vals) * rule[r, ncol(rule) - 1]           # moltiplico per il peso
  })
  
  # # Riepilogo
  activation_count <- colSums(firing > 0)
  summary_rules <- data.frame(
    rule          = seq_len(nrow(rule)),
    activations   = activation_count,
    pct           = round(100 * activation_count / nrow(data_matrix), 2),
    mean_strength = colMeans(firing)
  )
  
  list(
    firing      = firing,
    summary     = summary_rules[order(-activation_count), ],
    never_fired = which(activation_count == 0)
  )
}


# media e completezza
media_e_completezza <- function(dati, nome) {
  dati %>%
    mutate(thickness = lower_depth_cm - upper_depth_cm) %>%
    group_by(profile_id) %>%
    summarise(
      score_w     = weighted.mean(score, thickness, na.rm = TRUE),
      completezza = sum(thickness[!is.na(score)]) / sum(thickness) * 100,
      .groups = "drop"
    ) %>%
    mutate(score_w = ifelse(is.nan(score_w), NA, score_w)) %>%
    rename_with(~ paste0(.x, "_", nome), c(score_w, completezza))
}

