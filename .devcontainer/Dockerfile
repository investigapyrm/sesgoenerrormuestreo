# Usa la imagen oficial de Rocker con R y RStudio Server
FROM rocker/verse:4.3.1

# Instala system dependencies que puedas necesitar (por ejemplo pandoc)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libxml2-dev libssl-dev \
  && rm -rf /var/lib/apt/lists/*

# (Opcional) instala paquetes de TeX Live si renderizas PDF:
# RUN apt-get update && apt-get install -y texlive-latex-recommended texlive-latex-extra latexmk
