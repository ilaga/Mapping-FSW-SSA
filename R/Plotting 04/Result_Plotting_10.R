

##########################################################################
##########################################################################
## This file generates the maps presented in the manuscript
## Additional plots not presented are also generated
##########################################################################
##########################################################################


library(tmap) # https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html
tmap_options(check.and.fix = TRUE)
## tmaptools::palette_explorer()
library(spData)
library(raster)
library(spdep)
library(cartography)
library(ggplot2)


# Load Results and shapes -------------------------------------------------

pse.dat = read.csv("../Data/FSW Spreadsheets/SSA_FSW_PSE_03.csv")
pse.country = subset(pse.dat, Validated_area == "National")

africa.iso = readxl::read_excel("../Data/Africa_ISO.xlsx")
africa.iso = africa.iso[,1:2]
ssa.iso = subset(africa.iso, Included == "TRUE")

country.shape.all = shapefile("../Data/Shapefiles/gadm36_0.shp", encoding = "UTF-8", use_iconv = TRUE)
country.shape.africa = subset(country.shape.all, GID_0 %in% africa.iso$ISO)
## Remove islands
nb = poly2nb(country.shape.africa, queen=TRUE,
                snap = 1e-4)
nb.mat = nb2mat(nb, zero.policy = TRUE, style = "B")
which.rm = which(rowSums(nb.mat) == 0)
country.shape.africa = country.shape.africa[-which.rm,]
country.shape.africa$na = NA

country.shape.continuous = aggregate(country.shape.africa, by = "na")
country.shape.sf = st_as_sf(country.shape.continuous)
country.shape.sf.2 = country.shape.sf %>%
  hatchedLayer(mode = "sfc", pattern = "left2right", density = 2)
country.shape.patterns <- st_sf(geometry = country.shape.sf.2)


gadm.raw = readRDS("../Data/Shapefile_Prep.rds")
gadm.district = readRDS("../Data/Results/Final_district_est.rds")
gadm.country = readRDS("../Data/Results/Final_country_est.rds")
pse.dat.fit = readRDS("../Data/Final_pse_dat_fit.rds")
pse.dat.sub = subset(pse.dat.fit, !is.na(pse))

gadm.district$Percent = round(gadm.district$Prev * 100, digits = 2)
gadm.district$Uncertainty = round(gadm.district$Uncertainty, digits = 2)
gadm.country$Percent = round(gadm.country$Percent, digits = 2)
gadm.country$Uncertainty = round(gadm.country$Uncertainty, digits = 2)


pse.prev.iso.dat = pse.dat.sub
pse.prev.iso.dat$ISO = factor(pse.prev.iso.dat$ISO, levels = levels(pse.dat.fit$ISO))

cbPalette <- c("#E69F00", "#56B4E9")
gg.prev.iso = ggplot(pse.dat.sub) +
  geom_jitter(aes(y = logit_prev, x = ISO, col = as.factor(City)), shape = 1, size = 2.5,
             stroke = 1.5) +
  theme_gray(base_size = 15) +
  xlab("Country ISO") + ylab("FSW Proportion (Logit)") +
  scale_x_discrete(drop = FALSE) + 
  scale_color_manual(values = cbPalette,
                     name = "\n", labels = c("Sub-national", "City")) +
  theme(legend.title=element_blank(),
        axis.text = element_text(size = 22, face = "bold"),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.text = element_text(size = 22, face = "bold"))
ggsave("../Figures/Prev_ISO.jpg", gg.prev.iso, width = 20, height = 10)


gg.prev.raw.iso = ggplot(pse.dat.sub) +
  geom_jitter(aes(y = 100*prev, x = ISO, col = as.factor(City)), shape = 1, size = 2.5,
             stroke = 1.5) +
  theme_gray(base_size = 15) +
  xlab("Country ISO") + ylab("FSW Percent") +
  scale_x_discrete(drop = FALSE) + 
  scale_color_manual(values = cbPalette,
                     name = "\n", labels = c("Sub-national", "City")) +
  theme(legend.title=element_blank(),
        axis.text = element_text(size = 22, face = "bold"),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.text = element_text(size = 22, face = "bold"))
ggsave("../Figures/Prev_raw_ISO.jpg", gg.prev.raw.iso, width = 20, height = 10)



gadm.country.df = gadm.country@data
national.df = subset(pse.dat, Validated_area == "National")

gg.country.est = ggplot() + 
  geom_point(aes(x = ISO, y = Percent), data = gadm.country.df, size = 5) +
  geom_errorbar(aes(x = ISO, ymin = Lower * 100, ymax = Upper * 100), data = gadm.country.df, size = 2) +
  geom_point(aes(x = ISO, y = 100 * Prev_comb), shape = 2, col = "red", data = national.df, size = 2, stroke = 2) +
  scale_shape_manual(values = c(1, 2),
                     name = "\n", labels = c("Model estimate", "National estimate\n from literature"),
                     guide = guide_legend(overrise.aes = list(shape = c(1, 2)))) +
  guides(shape = guide_legend(overrise.aes = list(shape = c(1, 2)))) +
  xlab("Country ISO") +
  ylab("Percent FSW") +
  theme_gray(base_size = 15) +
  theme(axis.text = element_text(size = 22, face = "bold"),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.text = element_text(size = 22, face = "bold"))
gg.country.est
ggsave("../Figures/Country_comp.jpg", gg.country.est, width = 20, height = 10)


pse.dat.sub$City = factor(pse.dat.sub$City, levels = c(0,1), labels = c("Sub-national", "City"))
gg.prev.refpop = ggplot(pse.dat.sub) + geom_point(aes(x = ref_pop_log, y = logit_prev)) +
  xlab("Log-transformed reference population") + ylab("Logit-transformed FSW proportion") +
  theme_grey(base_size = 15) + facet_wrap(~City) +
  theme(axis.text = element_text(size = 22, face = "bold"),
        axis.title = element_text(size = 22, face = "bold"),
        strip.text.x = element_text(size = 22, face = "bold"),
        aspect.ratio = 1)
gg.prev.refpop
ggsave("../Figures/FSW_prop_ref_pop.jpg", gg.prev.refpop, width = 20, height = 10)




gg.prev.pse = ggplot(pse.dat.sub) + geom_point(aes(y = logit_prev, x = log(pse))) +
  ylab("Logit-transformed FSW proportion") + xlab("Log-transformed FSW PSE") +
  theme_grey(base_size = 15) +
  theme(axis.text = element_text(size = 22, face = "bold"),
        axis.title = element_text(size = 22, face = "bold"),
        strip.text.x = element_text(size = 22, face = "bold"),
        aspect.ratio = 1)
gg.prev.pse

gg.refpop.pse = ggplot(pse.dat.sub) + geom_point(aes(y = ref_pop_log, x = log(pse))) +
  ylab("Log-transformed reference population") + xlab("Log-transformed FSW PSE") +
  theme_grey(base_size = 15) +
  theme(axis.text = element_text(size = 22, face = "bold"),
        axis.title = element_text(size = 22, face = "bold"),
        strip.text.x = element_text(size = 22, face = "bold"),
        aspect.ratio = 1)
gg.refpop.pse

gridExtra::grid.arrange(gg.prev.pse, gg.refpop.pse, gg.prev.refpop, nrow = 2)




# Get data availability ---------------------------------------------------

pse.city = subset(pse.dat.sub, City == "City")
pse.district = subset(pse.dat.sub, City == "Sub-national")

gadm.district$City_count = gadm.district$District_count = NA
for(admin.ind in 1:nrow(gadm.district)){
  gadm.district$City_count[admin.ind] = sum(pse.city$ID == gadm.raw$ID[admin.ind])
  gadm.district$District_count[admin.ind] = sum(pse.district$ID == gadm.raw$ID[admin.ind])
}
gadm.district$Total_count = gadm.district$City_count + gadm.district$District_count
gadm.district$City_count[gadm.district$City_count == 0] = NA
gadm.district$District_count[gadm.district$District_count == 0] = NA
gadm.district$Total_count[gadm.district$Total_count == 0] = NA

# Create plots ------------------------------------------------------------

all.avail.map = tm_shape(country.shape.africa) +
  tm_polygons("na", colorNA = "grey40", legend.show = FALSE) +
  tm_shape(country.shape.patterns) + 
  tm_lines(col = "grey20") +
  tm_shape(gadm.district) +
  tm_polygons("Total_count",
              breaks = seq(0.5, 11.5),
              labels = as.character(seq(1, 11)),
              palette = "Reds",
              contrast = c(0.3,1),
              colorNA = "grey70",
              textNA = "0",
              title = "Number of\ntotal PSE",
              border.col = "transparent",
              legend.show = TRUE) +
  tm_layout(frame = FALSE,
            legend.title.size = 1.4,
            legend.text.size = 1.2,
            legend.title.fontface = 2, asp = NA) +
  tm_shape(gadm.country) +
  tm_borders()

city.avail.map = tm_shape(country.shape.africa) +
  tm_polygons("na", colorNA = "grey40", legend.show = FALSE) +
  tm_shape(country.shape.patterns) + 
  tm_lines(col = "grey20") +
  tm_shape(gadm.district) +
  tm_polygons("City_count",
              breaks = seq(0.5, 11.5),
              labels = as.character(seq(1, 11)),
              palette = "Reds",
              contrast = c(0.3,1),
              colorNA = "grey70",
              textNA = "0",
              title = "Number of\ncity PSE",
              border.col = "transparent",
              legend.show = TRUE) +
  tm_layout(frame = FALSE,
            legend.title.size = 1.4,
            legend.text.size = 1.2,
            legend.title.fontface = 2, asp = NA) +
  tm_shape(gadm.country) +
  tm_borders()


district.avail.map = tm_shape(country.shape.africa) +
  tm_polygons("na", colorNA = "grey40", legend.show = FALSE) +
  tm_shape(country.shape.patterns) + 
  tm_lines(col = "grey20") +
  tm_shape(gadm.district) +
  tm_polygons("District_count",
              breaks = seq(0.5, 7.5),
              labels = as.character(seq(1,7)),
              palette = "Reds",
              contrast = c(0.3, 1),
              colorNA = "grey70",
              textNA = "0",
              title = "Number of\nsub-national PSE",
              border.col = "transparent",
              legend.show = TRUE) +
  tm_layout(frame = FALSE,
            legend.title.size = 1.5,
            legend.text.size = 1.5,
            legend.title.fontface = 2, asp = NA) +
  tm_shape(gadm.country) +
  tm_borders()






country.prev.map = tm_shape(country.shape.africa) +
  tm_polygons("na", colorNA = "grey40", legend.show = FALSE) +
  tm_shape(country.shape.patterns) + 
  tm_lines(col = "grey20") +
  tm_shape(gadm.country) +
  tm_polygons("Percent", title = "FSW Percent",
              n = 4, style = "quantile",
              textNA = NA) +
  tm_layout(frame = FALSE,
            legend.title.size = 1.5,
            legend.text.size = 1.5,
            legend.title.fontface = 2, asp = NA,
            legend.format = list(digits = 2))

country.uncertainty.map = tm_shape(country.shape.africa) +
  tm_polygons("na", colorNA = "grey40", legend.show = FALSE) +
  tm_shape(country.shape.patterns) + 
  tm_lines(col = "grey20") +
  tm_shape(gadm.country) +
  tm_polygons("Uncertainty", title = "Relative Uncertainty",
              n = 4, style = "quantile",
              palette = "Purples",
              textNA = NA) +
  tm_layout(frame = FALSE,
            legend.title.size = 1.5,
            legend.text.size = 1.5,
            legend.title.fontface = 2, asp = NA,
            legend.format = list(digits = 2))

district.prev.map = tm_shape(country.shape.africa) +
  tm_polygons("na", colorNA = "grey40", legend.show = FALSE) +
  tm_shape(country.shape.patterns) + 
  tm_lines(col = "grey20") +
  tm_shape(gadm.district) +
  tm_polygons("Percent", title = "FSW Percent", n = 10, style = "quantile",
              border.col = "transparent",
              showNA = FALSE) +
  tm_layout(frame = FALSE,
            legend.title.size = 1.5,
            legend.text.size = 1.5,
            legend.title.fontface = 2, asp = NA,
            legend.format = list(digits = 2)) +
  tm_shape(gadm.country) +
  tm_borders()

district.prev.even.map = tm_shape(country.shape.africa) +
  tm_polygons("na", colorNA = "grey40", legend.show = FALSE) +
  tm_shape(country.shape.patterns) + 
  tm_lines(col = "grey20") +
  tm_shape(gadm.district) +
  tm_polygons("Percent", title = "FSW Percent", breaks = seq(0, 0.11, by = 0.01),
              border.col = "transparent",
              showNA = FALSE) +
  tm_layout(frame = FALSE,
            legend.title.size = 1.5,
            legend.text.size = 1.5,
            legend.title.fontface = 2, asp = NA,
            legend.format = list(digits = 2)) +
  tm_shape(gadm.country) +
  tm_borders()

district.uncertainty.map = tm_shape(country.shape.africa) +
  tm_polygons("na", colorNA = "grey40", legend.show = FALSE) +
  tm_shape(country.shape.patterns) + 
  tm_lines(col = "grey20") +
  tm_shape(gadm.district) +
  tm_polygons("Uncertainty", title = "Relative Uncertainty", n = 10, style = "quantile",
              border.col = "transparent",
              palette = "Purples",
              showNA = FALSE) +
  tm_layout(frame = FALSE,
            legend.title.size = 1.5,
            legend.text.size = 1.5,
            legend.title.fontface = 2,
            legend.format = list(digits = 2)) +
  tm_shape(gadm.country) +
  tm_borders()

district.uncertainty.quartile.map = tm_shape(country.shape.africa) +
  tm_polygons("na", colorNA = "grey40", legend.show = FALSE) +
  tm_shape(country.shape.patterns) + 
  tm_lines(col = "grey20") +
  tm_shape(gadm.district) +
  tm_polygons("Uncertainty", title = "Relative Uncertainty", n = 4, style = "quantile",
              border.col = "transparent",
              showNA = FALSE) +
  tm_layout(frame = FALSE) +
  tm_shape(gadm.country) +
  tm_borders()

district.uncertainty.even.map = tm_shape(country.shape.africa) +
  tm_polygons("na", colorNA = "grey40", legend.show = FALSE) +
  tm_shape(country.shape.patterns) + 
  tm_lines(col = "grey20") +
  tm_shape(gadm.district) +
  tm_polygons("Uncertainty", title = "Relative Uncertainty",
              breaks = seq(0.5, 3, by = 0.25),
              border.col = "transparent",
              palette = "Purples",
              contrast = c(0.3, 1),
              showNA = FALSE) +
  tm_layout(frame = FALSE,
            legend.title.size = 1.5,
            legend.text.size = 1.5,
            legend.title.fontface = 2) +
  tm_shape(gadm.country) +
  tm_borders()









# Save maps ---------------------------------------------------------------

tmap_save(all.avail.map,
          filename = "../Figures/Total_Availability.jpg",
          dpi = 1000, asp = NA)
tmap_save(city.avail.map,
          filename = "../Figures/City_Availability.jpg",
          dpi = 1000, asp = NA)
tmap_save(district.avail.map,
          filename = "../Figures/District_Availability.jpg",
          dpi = 1000, asp = NA)
tmap_save(country.prev.map,
          filename = "../Figures/Country_Prevalence.jpg",
          dpi = 1000, asp = NA)
tmap_save(country.uncertainty.map,
          filename = "../Figures/Country_Uncertainty.jpg",
          dpi = 1000, asp = NA)
tmap_save(district.prev.map,
          filename = "../Figures/District_Prevalence.jpg",
          dpi = 1000, asp = NA)
tmap_save(district.uncertainty.map,
          filename = "../Figures/District_Uncertainty.jpg",
          dpi = 1000, asp = NA)


## Interactive plots: RUN WITH CARE
if(0){
  gadm.country$Lowerp = gadm.country$Lower * 100
  gadm.country$Upperp = gadm.country$Upper * 100
  
  gadm.district$Percent = gadm.district$Prev * 100
  gadm.district$Upperp = gadm.district$Upper * 100
  gadm.district$Lowerp = gadm.district$Lower * 100
  
  tmap_mode("view")
  both.map = tm_shape(country.shape.africa) +
    tm_borders(group = NULL) +
    tm_shape(gadm.country, name = "Country Estimates") +
    tm_polygons("Percent", title = "Country FSW Percent (%)",
                n = 4, style = "quantile",
                popup.vars = c("ISO-3" = "ISO",
                               "Estimated FSW Percent (%)" = "Percent",
                               "Lower Bound (%)" = "Lowerp",
                               "Upper Bound (%)" = "Upperp"),
                popup.format=list(Percent = list(digits = 2),
                                  Lowerp = list(digits = 2),
                                  Upperp = list(digits = 2)),
                id = "NAME_0") +
    tm_shape(gadm.district, name = "Sub-national Estimates") +
    tm_polygons("Percent", title = "Sub-national FSW Percent (%)", n = 10, style = "quantile",
                border.col = "transparent",
                popup.vars = c("Country" = "NAME_0",
                               "ISO-3" = "ISO",
                               "Estimated FSW Percent (%)" = "Percent",
                               "Lower Bound (%)" = "Lowerp",
                               "Upper Bound (%)" = "Upperp"),
                popup.format=list(Percent = list(digits = 2),
                                  Lowerp = list(digits = 2),
                                  Upperp = list(digits = 2)),
                id = "NAME_1") + 
    tm_layout(frame = FALSE,
              legend.title.size = 1.5,
              legend.text.size = 1.5,
              legend.title.fontface = 2, asp = NA) +
    tm_shape(gadm.country, name = "Country Borders") +
    tm_borders(group = NULL) +
    tm_facets(as.layers = TRUE)
  tmap_save(both.map,
            filename = "./Figures/FSW_Map_Interactive.html")
  
}
