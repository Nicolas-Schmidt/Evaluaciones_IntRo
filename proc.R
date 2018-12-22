
eval <- readxl::read_excel("eval.xlsx")
names(eval) <- stringr::str_extract(names(eval), "(?=¿).*")
eval <- eval[!is.na(names(eval))]
escala <- c("-", "1 a 5", "1 a 5", "1 a 10", "1 a 5", 
            "1 a 5", "-", "-", "1 a 5", "1 a 5", 
            "1 a 5", "1 a 5", "1 a 5", "1 a 5", 
            "1 a 5", "1 a 5", "1 a 5", "-")
preguntas <- data.frame(Pregunta = names(eval), 
                        Escala = escala, 
                        p = 1:ncol(eval), stringsAsFactors = FALSE)
names(eval) <- paste0("p", 1:ncol(eval))
vec <- ifelse(preguntas[,2]=="-", FALSE, TRUE)
out <- function(x){
        r <- apply(x, 2, range)
        data.frame(media = round(apply(x, 2, mean),2), 
                   min = r[1,], 
                   max = r[2,],
                   id = colnames(r), 
                   row.names = NULL, 
                   stringsAsFactors = FALSE)
}
tab <- merge(cbind(preguntas, id = paste0("p", 1:nrow(preguntas))), 
             out(eval[vec]), by = "id")
tab <- tab[order(tab$p),-c(1,4)]
rownames(tab) <- NULL



