# =========================================================
# Archivo con funciones de apoyo para todos los ejercicios
# =========================================================

# Límite para que los prouctos sean exactos
.LIMITE_EXACTO <- 2^53

#Función para detectar el primer estado que se repite en la secuencia X
.detectar_ciclo <- function(x){
  primera <- match(x,x)
  rep_idx <- which(primera < seq_along(x))
  if (length(rep_idx) == 0) return(list(periodo = NA, inicio = NA, en = NA))
  j <- rep_idx[1]
  list(periodo = j - primera[j], inicio = primero[j], en = j)
}
