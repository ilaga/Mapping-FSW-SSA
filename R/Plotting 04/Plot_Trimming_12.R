
library(knitr)
library(magick)
file.names = list.files("../Figures/")

for(i in 1:length(file.names)){
  file.end = substr(file.names[i], nchar(file.names[i]) - 2, nchar(file.names[i]))
  if(file.end == "jpg"){
    knitr::plot_crop(paste0("../Figures/",file.names[i]))
  }
}
