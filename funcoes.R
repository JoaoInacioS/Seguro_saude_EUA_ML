# Funcoes para o trabalho ####

# ------------------------------------------------------------------------- ####
grafbarmedia <- function(d,v,v1,legx,legy,cores,cnum,leg){
  # d = banco de dados utilizado
  # cnum = T ou F, sendo T para conter valores nas colunas
  # leg = T ou F, sendo T para ter legenda
  # v = variavel categorica 
  # v1 = variavel numerica
  # entre aspas todos os outros 4: legx = "nome", ...
  d |>
    group_by( {{v}} ) |>
    summarise( "medy" = mean( {{v1}} )) |> 
    ggplot(aes(x = levels(factor( {{v}} )), y = medy,
               fill = levels(factor( {{v}} )))) +
    geom_col(colour="black",show.legend = leg) +
    scale_fill_brewer(palette = cores, direction = 1) +
    labs(
      x = legx,
      y = legy,
      fill = legx 
    ) + 
    theme_minimal() +
    if(cnum == T){
      #Para aparecer os valores de cada coluna:
      geom_text(aes(x = levels(factor( {{v}} )), y = medy, 
                     label = round(medy,2)),show.legend = leg,vjust=1.75)
    }}

grafboxgen <- function(d,v,v1,legx,legy,cores,leg){
  # d = banco de dados utilizado
  # leg = T ou F, sendo T para ter legenda
  # v = variavel 1
  # v1 = variavel 2
  # entre aspas todos os outros 4: legx = "nome", ...
  d |>
    ggplot(aes(x = {{v}}, y = {{v1}}, fill = Genero )) +
    geom_boxplot(show.legend = leg) +
    scale_fill_brewer(palette = cores, direction = 1) +
    labs(
      x = legx,
      y = legy,
      fill = " "
    ) + 
    theme_minimal()+
    theme(legend.position = "bottom")
}

#grafboxgen(d= dados, v = "", v1 = imc, cores = "Set1",leg = T,
 #            legx = "", legy = "IMC",
  #           titulo = "Titulo da tabela", subtitulo = "Subtitulo da tabela")

#grafbarmedia(d= dados2, v = faixa_etaria, v1 = imc, cores = "Spectral",cnum = F,
#             legx = "Faixa Etária", legy = "Média IMC",
#             titulo = "Titulo da tabela", subtitulo = "Subtitulo da tabela")

#grafbarmedia(d= dados2, v = faixa_etaria, v1 = imc, cores = "Set1", cnum = T,
#             legx = "Faixa Etária", legy = "Média IMC",
#             titulo = "Titulo da tabela", subtitulo = "Subtitulo da tabela")

