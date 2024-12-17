
#' Summary of Kendall correlation coefficient statistics
#'
#' @param ranking1 
#' @param ranking2 
#'
#' @returns A data frame with the frequency of three types of 
#' pairs (concordant, discordant, tied)
#' @export
#'
#' @examples
#' rank1 <- c(1,2,3,4,5,6)
#' rank2 <- c(2,1,3,4.5,4.5,6)
#' kendall_summary(rank1, rank2)
#' 
kendall_summary <- function(ranking1, ranking2) {
  # Verificar que los rankings tengan la misma longitud
  if (length(ranking1) != length(ranking2)) {
    stop("Rankings must be of equal size.")
  }
  
  # Crear un data.frame con todas las combinaciones de pares
  pairs <- expand.grid(i = 1:length(ranking1), j = 1:length(ranking1))
  pairs <- pairs[pairs$i < pairs$j, ]  # Filtrar pares únicos (i < j)
  
  # Identificar concordancia, discordancia o empate para cada par
  pairs$concordance <- with(pairs, {
    # Comparar diferencias de posiciones en ambos rankings
    diff1 <- ranking1[i] - ranking1[j]
    diff2 <- ranking2[i] - ranking2[j]
    
    # Clasificar pares
    ifelse(
      diff1 * diff2 > 0, "Concordant",  # Ambos tienen el mismo signo
      ifelse(diff1 * diff2 < 0, "Discordant", "Tied")  # Signos opuestos o cero
    )
  })
  
  # Calcular las frecuencias de cada tipo
  resumen <- table(pairs$concordance)
  resumen_df <- as.data.frame(resumen)
  colnames(resumen_df) <- c("Type", "N")
  
  resumen_df
}