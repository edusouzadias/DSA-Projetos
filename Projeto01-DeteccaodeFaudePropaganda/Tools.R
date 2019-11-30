


PlotarAtributos <- function(atributo, lista){
  p1 <- ggplot(df_downloaded, aes_string(x = atributo)) + geom_bar() + labs(title = "Fez download")
  p2<- df %>%
    filter(!!as.symbol(atributo) %in% lista) %>%
    ggplot(aes_string(x = atributo)) + geom_bar() + labs(title = "Total")
  grid.newpage()
  return(grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last")))
}


AtributosPoucoUtilizados <- function(atributo){
  nrow(df %>%
         group_by(!!as.symbol(atributo)) %>%
         summarise(total=n()/nrow(df)*100) %>%
         filter(total<1) %>%
         arrange(desc(total)))
}