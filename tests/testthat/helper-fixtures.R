df_check_dups <- data.frame(
  ID           = c("GB024-lang1", "GB065-lang1", "GB065-lang1"),
  Language_ID  = c("lang1", "lang1", "lang1"),
  Parameter_ID = c("GB024", "GB065", "GB065"),
  Value        = c("1", "2", "1"),
  Code_ID      = c("GB024-1", "GB065-2", "GB065-1"),
  Comment      = c(NA, NA, NA), 
  Source       = c("griff1", "griff1", "griff1"),  
  Source_comment= c("griff1", "griff1", "griff1"),
  Coders        = c("HS", "HS", "HS"),
  stringsAsFactors = FALSE
)

df_check_dups_clean <- data.frame(
  ID           = c("GB024-lang1", "GB065-lang1", "GB130-lang1"),
  Language_ID  = c("lang1", "lang1", "lang1"),
  Parameter_ID = c("GB024", "GB065", "GB130"),
  Value        = c("1", "2", "3"),
  Code_ID      = c("GB024-1", "GB065-2", "GB130-3"),
  Comment      = c(NA, NA, NA), 
  Source       = c("griff1", "griff1", "griff1"),  
  Source_comment= c("griff1", "griff1", "griff1"),
  Coders        = c("HS", "HS", "HS"),
  stringsAsFactors = FALSE
)



df_check_multistate_binary_clashes <- data.frame(
  ID           = c("GB025-lang1", "GB025a-lang1", "GB025b-lang1"),
  Language_ID  = c("lang1", "lang1", "lang1"),
  Parameter_ID = c("GB025", "GB025a", "GB025b"),
  Value        = c("1", "1", "1"),
  Code_ID      = c("GB025-1", "GB025a-1", "GB025b-1"),
  Comment      = c(NA, NA, NA), 
  Source       = c("griff1", "griff1", "griff1"),  
  Source_comment= c("griff1", "griff1", "griff1"),
  Coders        = c("HS", "HS", "HS"),
  stringsAsFactors = FALSE
)


df_check_multistate_binary_clashes_clean <- data.frame(
  ID           = c("GB025-lang1", "GB025a-lang1", "GB025b-lang1"),
  Language_ID  = c("lang1", "lang1", "lang1"),
  Parameter_ID = c("GB025", "GB025a", "GB025b"),
  Value        = c("1", "1", "0"),
  Code_ID      = c("GB025-1", "GB025a-1", "GB025b-0"),
  Comment      = c(NA, NA, NA), 
  Source       = c("griff1", "griff1", "griff1"),  
  Source_comment= c("griff1", "griff1", "griff1"),
  Coders        = c("HS", "HS", "HS"),
  stringsAsFactors = FALSE
)


df_make_binary_solo <- data.frame(
ID           = c("GB025-lang1", "GB130-lang1", "GB065-lang1"),
Language_ID  = c("lang1", "lang1", "lang1"),
Parameter_ID = c("GB025", "GB130", "GB065"),
Value        = c("1", "1", "3"),
Code_ID      = c("GB025-1", "GB130-1", "GB065-3"),
Comment      = c(NA, NA, NA), 
Source       = c("griff1", "griff1", "griff1"),  
Source_comment= c("griff1", "griff1", "griff1"),
Coders        = c("HS", "HS", "HS"),
stringsAsFactors = FALSE
)

df_make_binary_multistate_mixed <- data.frame(
  ID           = c("GB025-lang1", "GB130-lang1", "GB065-lang1", "GB065a-lang1", "GB065b-lang1"),
  Language_ID  = c("lang1", "lang1", "lang1", "lang1", "lang1"),
  Parameter_ID = c("GB025", "GB130", "GB065", "GB065a", "GB065b"),
  Value        = c("1", "1", "3", "1", "0"),
  Code_ID      = c("GB025-1", "GB130-1", "GB065-3", "GB065a-1", "GB065b-0"),
  Comment      = c(NA, NA, NA, NA, NA), 
  Source       = c("griff1", "griff1", "griff1", "griff1", "griff1"),  
  Source_comment= c("griff1", "griff1", "griff1", "griff1", "griff1"),
  Coders        = c("HS", "HS", "HS", "HS", "HS"),
  stringsAsFactors = FALSE
)




#df_make_multistate_solo

#df_make_binary_multistate_mixed

