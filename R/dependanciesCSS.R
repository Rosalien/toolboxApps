# Ensemble des fonctions CSS, Javascript et ggplot2 pour la mise en forme de l'appli
#' @title googleAnalyticsParameter
#' @description googleAnalytics paramters 
#' @return String of java script
#' @examples
#' \dontrun{
#' googleAnalyticsParameter()
#' }
#' @export
# 

googleAnalyticsParameter <- function(){
'<!-- Global site tag (gtag.js) - Google Analytics -->
        <script async src="https://www.googletagmanager.com/gtag/js?id=UA-143674771-1"></script>
        <script>
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag(\'js\', new Date());
        gtag(\'set\',\'cookie_expires\',395*24*60*60); //Mesure RGPD : IP anonymisées et cookies réduit à 13mois max de conservation
        gtag(\'config\', \'UA-143674771-1\');
        </script>'
}

#' @title tweaks
#' @description Align checkboxs on multiple columns
#' @source https://stackoverflow.com/questions/36898492/aligning-checkbox-elements-in-many-coumns-in-a-shiny-app
#' @return String of java script
#' @examples
#' \dontrun{
#' tweaks()
#' }
#' @export
# 
tweaks <- function(){
".multicol { 
  height:auto;
  -webkit-column-count: 4;
  -moz-column-count: 4;
  column-count: 4;
  }
div.checkbox {margin-top: 0px;}
div.checkbox {margin-left: 0px;}"
}

#' @title tweaks2
#' @description Align checkboxs on multiple columns
#' @source https://stackoverflow.com/questions/42742191/align-checkboxgroupinput-vertically-and-horizontally
#' @return String of java script
#' @examples
#' \dontrun{
#' tweaks2()
#' }
#' @export
# 
tweaks2 <- function(){
".multicol .shiny-options-group{
                            -webkit-column-count: 4; /* Chrome, Safari, Opera */
                            -moz-column-count: 4;    /* Firefox */
                            column-count: 4;
                            -moz-column-fill: balanced;
                            -column-fill: balanced;
                            }
                            .checkbox{
                            margin-top: 0px !important;
                            -webkit-margin-after: 0px !important; 
                            }"
}

#' @title appCSS
#' @description CSS loading content MultiColumn in dygraph
#' @return String of css configuration
#' @examples
#' \dontrun{
#' appCSS()
#' }
#' @export
appCSS <- function(){
"
  #loading-content {
    position: absolute;
    padding: 0 0 0 0;
    background: #FFFFFF;
    opacity: 0.9;
    z-index: 100;
    left: 0;
    right: 0;
    height: 100%;
    text-align: center;
    color: #333;
  }
  "
}

#' @title dyMultiColumn
#' @description MultiColumn in dygraph
#' @source https://rstudio.github.io/dygraphs/gallery-custom-plotters.html
#' @param dygraph dygraph object
#' @importFrom dygraphs dygraph
#' @importFrom dygraphs dyPlotter
#' @return css configuration for dygraph
#' @examples
#' \dontrun{
#' dygraph(data)%>%
#' dyMultiColumn()
#' }
#' @export
dyMultiColumn <- function(dygraph) { 
      dyPlotter(dygraph = dygraph, 
                name = "MultiColumn", 
                path = system.file("plotters/multicolumn.js", 
                                   package = "dygraphs")) 
    }   

#' @title dyBarChart
#' @description Bar chart in dygraph
#' @source https://rstudio.github.io/dygraphs/gallery-custom-plotters.html
#' @param dygraph dygraph object
#' @importFrom dygraphs dygraph
#' @importFrom dygraphs dyPlotter
#' @return css configuration for dygraph
#' @examples
#' \dontrun{
#' dygraph(data)%>%
#' dyBarChart()
#' }
#' @export

dyBarChart <- function(dygraph) {
  dyPlotter(dygraph = dygraph,
            name = "BarChart",
            path = system.file("plotters/barchart.js",
                               package = "dygraphs"))
}

#' @title dyCSScool
#' @description css configuration for dygraph
#' @source https://github.com/rstudio/dygraphs/issues/227
#' @param dygraph dygraph object
#' @importFrom dygraphs dygraph
#' @return css configuration for dygraph
#' @examples
#' \dontrun{
#' dygraph(data)%>%
#' dyCSScool()
#' }
#' @export

dyCSScool <- function(dygraph){  
  dygraph$x$css <- '
  .dygraph-legend {
  width: auto !important;
  min-width: 150px;
  color: white;
  background-color: #BABABA !important;
  padding-left:5px;
  border-color:#BABABA;
  border-style:solid;
  border-width:thin;
  transition:0s 4s;
  z-index: 80 !important;
  box-shadow: 2px 2px 5px rgba(0, 0, 0, .3);
  border-radius: 3px;
  }
  
  .dygraph-legend:hover{
  transform: translate(-110%);
  transition: 0s;
  }
  
  .dygraph-legend > span {
  color: black;
  padding-left:5px;
  padding-right:2px;
  margin-left:-5px;
  background-color: white !important;
  display: block;
  }
  
  .dygraph-legend > span:first-child {
  margin-top:2px;
  }
  
  .dygraph-legend > span > span{
  display: inline;
  }
  
  .highlight {
  border-left: 2px solid #BABABA;
  padding-left:3px !important;
  }
  '
  dygraph
}

#' @title multiplot
#' @description # Multiple plot function
#'
#' ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' - cols:   Number of columns in layout
#' - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#'
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#' @param ... ggplot objects can be passed in ...
#' @param plotlist ggplot objects can be passed in ...
#' @param file ggplot parameter
#' @param cols ggplot parameter
#' @param layout ggplot parameter
#' @return ggplots
#' @import grid
#' @source https://stackoverflow.com/questions/33867301/dynamic-grid-plots-in-shiny-r
#' @export
#' 
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#' @title grid_arrange_shared_legend
#' @description Combined ggplots with a common legend
#' @param ... ggplot objects can be passed in ...
#' @param nrow ggplot parameter
#' @param ncol ggplot parameter
#' @param position legend position ("bottom", "right","top","left")
#' @return ggplots
#' @importFrom ggplot2 ggplotGrob
#' @importFrom ggtern arrangeGrob
#' @importFrom grid grid.newpage
#' @importFrom grid grid.draw
#' @source http://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
#' @export
 
grid_arrange_shared_legend <- function(...,nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {

  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)

  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  return(combined)
}

#' @title theme_chart2
#' @description ggplot2 theme
#' @param ... ggplot objects can be passed in ...
#' @param position String of position of legend (ex. "bottom")
#' @return ggplot2 theme configuration
#' @import ggplot2
#' @export
theme_chart <- function(position="bottom",...) {
 theme_minimal() +
  theme(
    text = element_text(family="Lato",color = "#22211d"),#22211d
    axis.line = element_blank(),
    axis.text.x = element_text(size = 9,face = "bold"),
    axis.text.y = element_text(size = 9,face = "bold"),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_line(color = "#dbdbd9", size = 0.2),
    panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),#Couleur de la grille. Initialement (#ebebe5)
    plot.background = element_rect(fill = "white", color = NA), 
    panel.background = element_rect(fill = "white", color = NA), 
    legend.background = element_rect(fill = "white", color = NA),
    legend.text=element_text(size = 9,colour = "#22211d"),
    legend.title=element_text(colour = "#22211d",face = "bold",size = 9),
    legend.position = position,
    strip.background =element_rect(fill="#dbdbd9",color="#dbdbd9"),#0071b7
    strip.text = element_text(size = 9,face = "bold")
  )
}

#' @title theme_chart2
#' @description ggplot2 theme
#' @param ... ggplot objects can be passed in ...
#' @param position String of position of legend (ex. "bottom")
#' @return ggplot2 theme configuration
#' @import ggplot2
#' @export
theme_chart2 <- function(position="bottom",...){
theme_classic()+
theme(
      text = element_text(family="Lato",color = "#22211d",size=12),#22211d
      axis.text.x = element_text(size = 11,face = "bold"),#,face = "bold"),
      axis.text.y = element_text(size = 11,face = "bold"),#,face = "bold"),face = "bold"),
      axis.line.x = element_line(colour = "#22211d", size = 0.7),
      axis.line.y = element_line(colour = "#22211d", size = 0.7),
      plot.title = element_text(size = 14, face = "bold"), 
      axis.title = element_text(face="bold"),
      legend.title=element_text(colour = "#22211d",face = "bold",size = 12),
      legend.position = position,
      strip.background =element_rect(fill="#dbdbd9",color="#dbdbd9"),#0071b7
      strip.text = element_text(size = 10,face = "bold")
    )
}