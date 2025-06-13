# Carregando bibliotecas;
library(spatialreg)
library(spdep)
library(spgwr)
library(VIM) # obs.: opcional

# Carregando dados;
data("columbus")

# Selecionando variáveis;
crime <- columbus$CRIME
inc <- columbus$INC
hoval <- columbus$HOVAL

# Assumindo coordenadas nas primeiras duas colunas;
coords <- columbus[, 1:2]

# Verificar e tratar valores ausentes - obs.: opcional;
if (any(is.na(coords))) {
  
  # Imputando valores ausentes usando KNNImpute 
  # obs.: considere métodos alternativos;
  coords <- knnImpute(coords)
  # Como alternativa, remova as linhas com coordenadas ausentes;
  # coords <- na.omit(coords) - obs.: não precisará do comando OMIT;
}

# Calculando os k-vizinhos mais próximos - ex.: k=5;
nn <- dnearneigh(coords, k = 5, longlat = FALSE, d1 = 0, d2 = max(dist(coords)))

# Calculando os k-vizinhos mais próximos - ex.: k=5;
nn <- knn2nb(knearneigh(coords, k = 5, longlat = FALSE))


# Criando a lista de vizinhos;
nb <- nb2listw(nn)

# Modelo SAR;
sar_model <- lagsarlm(crime ~ inc + hoval data = columbus, listw = nb)

# Adicionando as coordenadas ao conjunto de dados;
coordinates(columbus) <- c("X", "Y")

# Modelo GWR;
gwr_model <- gwr(crime ~ inc + hoval, data = columbus, handwidth = 0.5)

# Fazendo um resumo sobre o modelo SAR;
summary(sar_model)

# Fazendo um resumo sobre o modelo GWR;
summary(gwr_model)


# Imprimindo os parâmetros dos modelos;
print(gwr_model$SDF)