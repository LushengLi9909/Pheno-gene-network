library(shiny)
library(shinythemes)
library(DT)
library(shinyWidgets)
source("visnetwork_cross.R")
source("vis_cross_cell.R")

u_mapping <- read.delim("data/ph_mp_mapping.txt",header = TRUE,sep=",")
ui <- fluidPage(
  navbarPage(theme = shinytheme("flatly"),title = h2("Cell-Phenotype"),
             tabPanel(h3("Cross-specise Phenotype"),fluidRow(
               column(1),
               column(4, wellPanel(
                 selectizeInput('pheno', label = h4("Enter phenotype:"), choices = c(unique(u_mapping$MPterm),unique(u_mapping$HPterm)),
                                selected = "increased circulating ammonia level",options = list(create = TRUE)),
                 h5("Note: Only phenotypes that have the phenotypic orthologue relation are on the choice list")
                 )),
               column(7,
                 br(),
                 h4("The cross-species phenotype is phenotype that have the phenotypic 
                  orthologue relation and SSSOM mapping between MPO and HPO terms."),
                 h4("The phenotypic orthologue relation originated from unified phenotype ontology (uPheno)."),
                 h4("The mapping file contains 3208 pair phenotypes that have phenotypic orthologue relation and SSSOM mapping, that is avaliable at ")
                 )),
               fluidRow(
                 column(2),
                 column(10,h4("Phenotype-Gene Network:"),
                        h5("(If the number of genes is large, this will take longer to load and generate the network)")),
               visNetworkOutput("networkid", height = "700px"),
               column(2),
               column(10,h4("Cell-Phenotype Network:")),
               visNetworkOutput("networkcell", height = "450px"),
               fluidPage(DTOutput('celltable'))
             ))
  )
)

server <- function(input, output) {

  output$networkid <- renderVisNetwork({
    visnetwork(pheno = input$pheno)
  })
  output$networkcell <- renderVisNetwork({
    visnetcell(pheno = input$pheno)
  })
  output$celltable <- renderDT(cell_table(pheno = input$pheno), 
                              rownames = FALSE,extensions = 'Buttons', 
                              options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
}

shinyApp(ui = ui, server = server)
