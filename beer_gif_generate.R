# load libraries
gif_building = F

if(gif_building == F){#older version for interactive plot version, otherwise file too big for upload
  
  if(!require(devtools)){install.packages('devtools')} #developer version download
  library(devtools)
  devtools::install_version("plotly", version = "4.5.6",
                            repos = "http://cran.us.r-project.org")
  
} else {
  
  install.packages('plotly')#most up to date plotly version for gif building, otherwise 500 HTTP error
  
}

library(plotly)
###################################################################################################
# load data
data <- read.csv('beer_advocate_webscrapped.csv',sep = ',', header=TRUE)

selected_cols <- c("beer_name_og", "brewery_og", "abv", "ibu")
UT_dat <- data[selected_cols]

colnames(UT_dat) <- c('UT_beer_name', 'UT_brewery', 'UT_ABV', 'UT_IBU')

UT_dat$ALL_rating = data$avg_score #overall rating
UT_dat$ALL_raters = data$num_of_ratings #total number of raters

UT_dat$UT_IBU <- as.numeric(UT_dat$UT_IBU)
UT_dat$ALL_raters <- as.numeric(UT_dat$ALL_raters)

UT_dat$UT_ABV[UT_dat$UT_ABV<0] <- 0
UT_dat$UT_IBU[UT_dat$UT_IBU<0] <- 0

###################################################################################################
#The interaction of bitterness and alcohol content for beer ratings

xmax = 20#ABV
ymax = 310#IBU
interp_dens = 100#interpolation density for modeling

int_dat = UT_dat[complete.cases(UT_dat[c('UT_beer_name', 'UT_brewery','ALL_rating', 'UT_ABV', 'UT_IBU', 'ALL_raters')]),]

#Which individual beer is best?
head(int_dat[order(-int_dat[,'ALL_rating']),], 10)#order to see who is best in text
#quantile(int_dat$UT_IBU, seq(0, 1, 0.01))#put values in perspective

#generate LOESS model of beer ratings and predict interpolated ratings
m = loess(ALL_rating ~ UT_ABV * UT_IBU, data = int_dat, weights = ALL_raters)
x_marginal = seq(min(int_dat$UT_ABV), xmax, length = interp_dens)
y_marginal = seq(min(int_dat$UT_IBU), ymax, length = interp_dens)
data.fit <-  expand.grid(list(UT_ABV = x_marginal, UT_IBU = y_marginal))
pred_interp = predict(m, newdata = data.fit)#interpolated ratings    

#hover text for surface plot (LOESS model)
hover <- with(data.frame(x = rep(x_marginal, interp_dens),
                         y = rep(y_marginal, each = interp_dens),
                         p = as.vector(pred_interp)),
              paste('Modeled beer (LOESS)', '<br>',
                    "Predicted rating: ", round(p, digits = 2), "stars", '<br>',
                    "Alcohol content: ", round(x, digits = 2), '%', '<br>',
                    'Bitterness: ', round(y, digits = 2), 'IBU'))
hover_m = matrix(hover, nrow = interp_dens, ncol = interp_dens, byrow = T)

p <- plot_ly() %>%
  add_markers(data = int_dat, x = ~UT_ABV, y = ~UT_IBU, z = ~ALL_rating, size = ~ALL_raters,
              marker = list(symbol = 'circle', sizemode = 'area',
                            color = ~ALL_rating, colorscale = c('#708090', '#683531'), showscale = F,
                            zmin = 2, zmax = 5),
              sizes = c(50, 1000), opacity = 1,
              name = 'Beers',
              hoverinfo = 'text',
              text = ~paste('Actual beer',
                            '<br>Name: ', UT_beer_name,
                            '<br>Brewery: ', UT_brewery,
                            '<br>Type: ', UT_sub_style,
                            '<br>User rating: ', round(ALL_rating, digits = 2),
                            '<br>Raters: ', ALL_raters,
                            '<br>Alcohol content: ', UT_ABV, '%',
                            '<br>Bitterness: ', UT_IBU, 'IBU')) %>%
  add_surface(x = x_marginal, y = y_marginal,
              z = t(pred_interp),#add_surface() expects the x-values on the columns and the y-values on the rows (very confusing, I know)
              opacity = 0.7,
              name = 'LOESS model',
              hoverinfo = 'text',
              text = hover_m,
              showscale = F,
              colorscale = c('#708090', '#683531'),
              cmin = 2, cmax = 5) %>%
  layout(title = 'How bitterness and alcohol make good beer',
         scene = list(xaxis = list(title = '% Alcohol',
                                   gridcolor = 'rgb(255, 255, 255)',#white
                                   range = c(0, xmax)),
                      yaxis = list(title = 'Bitterness',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   range = c(0, ymax)),
                      zaxis = list(title = 'Rating',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   range = c(1, 5))),
         annotations = list(list(x = 0, y = 0,#bottom left corner of frame
                                 text = '<a href="https://twitter.com/rikunert">@rikunert</a>',
                                 xref = 'paper', yref = 'paper',
                                 xanchor = 'left',#left aligned
                                 showarrow = F),
                            list(x = 1, y = 0,#bottom right corner of frame
                                 text = 'Source: <a href="http://untappd.com">untappd.com</a> <br>and <a href="http://beeradvocate.com">beeradvocate.com</a>',
                                 xref = 'paper', yref = 'paper',
                                 xanchor = 'right',#right aligned
                                 showarrow = F)))

#save on plotly website
if(gif_building == F) plotly_POST(p, filename = "ABV_IBU_ratings")

#save images for gif building
#be sure to have most up to date version of plotly
if (gif_building) {
  
  
  for(i in seq(0,pi * 2, length = 60)){
    
    outfile <- paste('ABV_IBU_ratings',round(i,digits=2), sep = "_")
    #outfile = paste('ABV_IBU_ratings',round(i,digits=2), sep = "_")
    cam.zoom = 2
    
    p = p %>% layout(showlegend = F,
                     scene=list(camera = list(eye = list(x = cos(i)*cam.zoom, y = sin(i)*cam.zoom, z=0.8),
                                              center = list(x = 0,
                                                            y = 0,
                                                            z = 0
                                              ))))
    
    cat("Now rendering iteration:", i,"\n")
    plotly_IMAGE(p,
                 width = 700,
                 height = 650,
                 format = "png",
                 scale = 1,
                 out_file = paste(outfile,"png", sep="."))
    
  }
}

#now that images are up, combine them into a gif:
#download ImageMagick
#open windows terminal
#navigate to folder housing all the png files making up gif
#> "C:\Program Files\ImageMagick-7.0.5-Q16\magick" *.png -delay 5 -loop 0 name.gif

###################################################################################################
#Which kind of beer is best?

#weighted mean rating grouped by kind of beer
beer_kind_wmean = sapply(split(UT_dat, UT_dat$UT_sub_style), function(x) weighted.mean(x$ALL_rating, x$ALL_raters, na.rm = T))
beer_kind_mean = sapply(split(UT_dat, UT_dat$UT_sub_style), function(x) mean(x$ALL_rating, na.rm = T))

ordering = order(-beer_kind_wmean)
beer_kind_mean = beer_kind_mean[ordering]
beer_kind_wmean = beer_kind_wmean[ordering]

table_text = with(data.frame(x = beer_kind_wmean,
                             y = beer_kind_mean,
                             k = names(beer_kind_mean)),
                  paste(k, ' | ', round(x, digits = 2), ' | ', round(y, digits = 2)))
table_text