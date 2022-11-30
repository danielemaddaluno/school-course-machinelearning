# https://stackoverflow.com/questions/54876731/inline-latex-equations-in-shiny-app-with-mathjax
# https://github.com/rstudio/shiny/issues/1506

# Clean enviroment
rm(list=ls())

library(shiny)
library(plotly)
library(devtools)
# this library is installed from github
if (!require(katexR)) {
  devtools::install_github("timelyportfolio/katexR")
  library(katexR)
}

## Only run examples in interactive R sessions
if (interactive()) {
  options(device.ask.default = FALSE)
  
  ui <- fluidPage(
    
    tags$head(
      tags$link(rel="stylesheet", 
                href="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css", 
                integrity="sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ",
                crossorigin="anonymous"),
      HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js" integrity="sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij" crossorigin="anonymous"></script>'),
      HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous"></script>'),
      HTML('
    <script>
      document.addEventListener("DOMContentLoaded", function(){
        renderMathInElement(document.body, {
          delimiters: [{left: "$", right: "$", display: false}]
        });
      })
    </script>')
    ),
    
    titlePanel("Gaussian $\\mathcal{N}(v\\vert\\mu, C)$:"),
    
    sidebarLayout(
      
      sidebarPanel(
        sliderInput("mu_x", "Mean x ($\\mu_x$):",
                    min = -5, max = 5, value = 0
        ),
        sliderInput("mu_y", "Mean y ($\\mu_y$):",
                    min = -5, max = 5, value = 0
        ),
        sliderInput("sigma2_x", "Variance x ($\\sigma_x^2$):",
                    min = 1, max = 24, value = 16
        ),
        sliderInput("sigma2_y", "Variance y ($\\sigma_y^2$):",
                    min = 1, max = 24, value = 4
        ),
        sliderInput("cor_xy", "Correlation x y ($\\sigma_{xy}$):",
                    min = -5, max = 5, value = 0
        )
      ),
      
      mainPanel(
        uiOutput('latexFormula'),
        plotlyOutput("distPlot")
      )
    )
  )
  
  # Server logic
  server <- function(input, output) {
    
    output$latexFormula <- renderUI({
      tagList(
        helpText(paste0("$", 
                        "\\mathcal{N}(v\\vert\\mu, C) = (2\\pi)^{-\\frac{D}{2}} {\\vert C\\vert}^{-\\frac{1}{2}} e^{\\frac{-(z-\\mu)^T C^{-1} (z-\\mu)}{2}}",
                        "$")),
        helpText(paste0("$\\qquad$")),
        helpText(paste0("Where:")),
        helpText(paste0("$",
                        "D = 2",
                        " \\qquad ",
                        " ; ",
                        "\\qquad ",
                        "\\mu ", 
                        "=", 
                        "\\begin{pmatrix} \\mu_x \\\\ \\mu_y \\end{pmatrix}", 
                        "=", 
                        "\\begin{pmatrix} ", input$mu_x, " \\\\ ", input$mu_y, " \\end{pmatrix}", 
                        
                        " \\qquad ",
                        " ; ",
                        "\\qquad ",
                        
                        "C ", 
                        "=",
                        "\\Sigma ", 
                        "=",
                        "\\begin{pmatrix} \\sigma_x^2 & \\sigma_{xy} \\\\ \\sigma_{xy} & \\sigma_y^2 \\end{pmatrix}", 
                        "=",
                        "\\begin{pmatrix} ", input$sigma2_x," & ", input$cor_xy," \\\\ ", input$cor_xy," & ", input$sigma2_y," \\end{pmatrix}",
                        
                        
                        "$")),
        
        tags$script('renderMathInElement(document.getElementById("latexFormula"), {delimiters: [{left: "$", right: "$", display: false}]});')
      )
    })
    
    output$distPlot <- renderPlotly({
      ########################
      ##### COMPUTE PDF ######
      ########################
      library(ds4psy)
      data = make_grid(x_min = -100, x_max = 100, y_min = -100, y_max = 100)/10
      N = dim(data)[1]
      mu = c(input$mu_x, input$mu_y)
      Sigma = rbind(c(input$sigma2_x, input$cor_xy), c(input$cor_xy, input$sigma2_y))
      library(LaplacesDemon)
      data$z = dmvn(as.matrix(data), mu, Sigma)
      
      
      #####################
      ##### PLOT PDF ######
      #####################
      
      # Plot with mesh
      # library(plotly)
      # plot_ly() %>% 
      #   add_trace(data = data,  x=data$x, y=data$y, z=data$z, type="mesh3d")
      
      w <- sqrt(N)
      fig <- plot_ly(data = data, 
                     x=array(data$x, dim=c(w, w)), 
                     y=array(data$y, dim=c(w, w)), 
                     z=array(data$z, dim=c(w, w))
      )
      
      fig <- fig %>% add_surface(
        # add level curves
        contours = list(
          x = list(
            show=TRUE,
            usecolormap=TRUE,
            highlightcolor="#ff0000",
            project=list(x=TRUE)
          ),
          y = list(
            show=TRUE,
            usecolormap=TRUE,
            highlightcolor="#ff0000",
            project=list(y=TRUE)
          ),
          z = list(
            show=TRUE,
            usecolormap=TRUE,
            highlightcolor="#ff0000",
            project=list(z=TRUE)
          )
        )
      )
      return(fig)
    })
  }
  
  # Complete app with UI and server components
  shinyApp(ui, server)
}
