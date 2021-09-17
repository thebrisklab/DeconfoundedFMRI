

## Circle plot functions:

#### Function to Make Transparent Colors 
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}





# varname: a variable name corresponding to a column of results.ave
# thresh: threshold to use for displaying chords
#

myChordDiagram = function(data,varname,alpha=0.20,alphaname,title) {
  # alpha: pvalue threshold
  # alphaname: column of data with pvalues
  
  ## Set transparent color, used to mask unwanted chords
  trcol <- t_col("black", perc = 100, name = "lt.black")
  
  ## Set colors for negative and positive values
  poscol <- t_col("deepskyblue", perc = 15, name = "lt.blue") #positive correlation color
  negcol <- t_col("red", perc = 15, name = "lt.red") #negative correlation color
  
  
  ## Set names for networks (netLab)
  icInfo <- read_excel("componentLabels_pca85_ica30.xlsx")
  netLab <-icInfo$cognitive.domain[icInfo$signal==1]
  
  DATA = data.frame(images = list.files("ic_pngs",full.names = T), stringsAsFactors = F) %>% 
    mutate(
      names = gsub("[a-zA-Z]|[[:punct:]]","", images),
      values = sample(0:100, size=nrow(.), replace = T)
    )
  head(DATA)
  
  # Assign group in the original data frame
  DATA$Group = netLab %>% as.factor()
  
  # Get colours, turn the vector into characters, and then use the factor numbers of DATA$Group to assign colour.
  DATA$gColor = rainbow(7) %>% as.character %>% 
    .[as.numeric(DATA$Group)]
  
  #### arrange circle plot by group
  oDATA = DATA %>% arrange(Group)
  
  
  ############################################
  # Create the weighted adjacency matrix:
  
  # Construct the "from" edge:
  ic.from =   substr(data$EdgeName,5,6)
  ic.from = ifelse(substr(ic.from,2,2)=='.',paste0('0',substr(ic.from,1,1)),ic.from)
  # visually check it looks good:
  cbind(data$EdgeName,ic.from)
  
  # Construct the "to" edge
  ic.to = substr(data$EdgeName,nchar(data$EdgeName)-1,nchar(data$EdgeName))
  ic.to = ifelse(substr(ic.to,1,1)=='c',paste0('0',substr(ic.to,2,2)),ic.to)
  # visually check it looks good:
  cbind(data$EdgeName,ic.to)
  
  # modify the THIRD variable to create chords for different variables:
  PAIRS = data.frame(ic.from,ic.to,data[,varname])
  
  
  #### Index specific pairs by a threshold
  
  neg_ix = data[,alphaname] < alpha & PAIRS[,3]<0
  pos_ix = data[,alphaname] < alpha & PAIRS[,3]>0
  
  cols = rep(trcol,nrow(PAIRS))
  cols[neg_ix]=negcol
  cols[pos_ix]=poscol
  
  ## Set plot background to black
  par(bg = 'black')
  
  ##plot updated Chord diagram with new colors
  myPlot = chordDiagram(PAIRS,annotationTrack = c("grid","name"), annotationTrackHeight=c(0.05, 0.01), preAllocateTracks = list(track.height = 0.3), order=oDATA$names, grid.col = setNames(oDATA$gColor, oDATA$names),col=cols)
  title(title,col.main="white",cex.main=1.5)
  
  as_radians = function(x) x*pi/180
  
  ## Loop through images and plot brain maps
  u=0
  for(si in get.all.sector.index()){
    xplot=get.cell.meta.data("xplot",si)
    u=u+1
    
    # Small workaround because coordinate 0 should be 360
    if(xplot[1] == 0) xplot[1] = 360
    
    
    x=.86*cos(as_radians((xplot[2]+xplot[1])/2))
    y=.86*sin(as_radians((xplot[2]+xplot[1])/2))
    
    oDATA$images[grep(si, oDATA$images)] %>% 
      readPNG() %>% 
      rasterImage(x-0.09, y-0.09, x+0.09, y+0.09)
  }
  
  #### set labels to be white
  circos.track(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    xplot = get.cell.meta.data("xplot")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    
    circos.text(mean(xlim), ylim[1], sector.name, facing = "inside", 
                niceFacing = TRUE, adj = c(0.5, 0), col= "white")
  }, bg.border = NA)
  
  #    myPlot
}
