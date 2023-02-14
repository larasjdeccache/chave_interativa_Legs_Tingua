library(shiny)
library(shinyjs)
library(shinyTree)
library(shinyWidgets)
library(shinythemes)
library(markdown)
library(htmltools)
library(ggplot2)  # for the diamonds dataset
library (readxl)

#################################
### Data
#################################

read.csv("matrix_3.csv", row.names = 1, sep= "\t") -> mat
read.csv("chatacters.CSV", stringsAsFactors = F, fileEncoding = "latin1", sep= ";") -> char.dat
data.frame(ID=c(1:ncol(mat)), Family=colnames(mat)) -> fam

#################################
### Identificação - options
#################################

unique(char.dat$Check.box.id) -> ident.ids
vector("list", length=length(ident.ids)) -> cb.dat.id 
names(cb.dat.id) <- ident.ids
for (i in 1:length(ident.ids)) {
  ident.ids[i] -> id0
  char.dat[which(char.dat$Check.box.id == id0),] -> dat0
  dat0$Check.box.label[1] -> label0
  dat0$ID -> choices0
  dat0$Character -> names(choices0)
  list(id=id0, label=label0, choices=choices0) -> cb.dat.id[[i]]
}

#################################
### Comparação - options
#################################

fam$ID -> fam.choices
names(fam.choices) <- fam$Family
fam.choices[order(names(fam.choices))] -> fam.choices

### caracteres

unique(char.dat$Check.box.label) -> char.types.opts

#################################
### UI
#################################

ui <- fluidPage(
  list(
    tags$head(HTML('<link rel="icon", href="legs.png", type="image/png" />')),
    
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
          title="", windowTitle="Leguminosas Tinguá"
        )),
    tags$style(HTML(".navbar-fixed-top{background-color: #CBD1B9; color: #5E7656; font-size: 16px;}",
                    ".navbar-default .navbar-nav>li>a {color: #5E7656}",
                    ".navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:hover, .navbar-default .navbar-nav>.active>a:focus {color: #2F362A}",
                    ".navbar-default .navbar-brand {color: #5E7656}",
                    ".navbar-default:hover .navbar-nav>li>a:hover {color: #2F362A;}",
                    ".navbar-default {border-color: #5E7656}",
                    ".container-fluid >.nav >li>a[data-value='Chave Dicotômica'] {color: #5E7656}",
                    ".container-fluid >.nav >li>a[data-value='Chave Interativa'] {color: #5E7656}",
                    ".container-fluid >.nav >li>a[data-value='Imagens'] {color: #5E7656;}",
                    ".jstree-proton .jstree-hovered:hover {background-color: #5E7656; box-shadow: #C66D5B;}",
                    ".jstree-proton.jstree-checkbox-selection .jstree-clicked {background-color: #ED9345; box-shadow: #C66D5B;}"
    ))
    ),

  navbarPage(theme = shinytheme("lumen"),
             title=div(img(src="legs_verde.png", style="margin-top: -14px; padding-right: 0px; padding-bottom:10px", height = 55)), position="fixed-top", windowTitle = "Leguminosas Tinguá",
             ### About
             tabPanel("Leguminosas arbóreas da Reserva Biológica do Tinguá", fluidPage(includeMarkdown("about.Rmd"))),
             tabPanel("Chave Dicotômica", icon = icon(name = "pagelines", lib = "font-awesome"), fluidPage(includeMarkdown("chave.Rmd"))) , collapsible = TRUE
             ,
             ##################################    
             ### Identificação
             ##################################
                 tabPanel("Chave Interativa", icon = icon(name = "pagelines", lib = "font-awesome"),
                          titlePanel(""),
                          ##################################
                          ### Side Panel
                          sidebarPanel(
                            useShinyjs(),
                            titlePanel("Caracteres"),
                            helpText("Selecione os caracteres presentes no seu espécime: Inicialmente deve-se apertar o ícone + e a marcação do estado de caráter é realizada na parte interior da lista. Conforme os caracteres são adicionados, as espécies que não possuem tais características são eliminadas."),
                            helpText("\n", "\n"),
                            
                            ### Checkbox tree
                            
                            uiOutput("chars.tree"),
                            
                            helpText("\n", "\n"),
                            helpText(""),
                            
                            ### Buttons
                            actionButton("clean.button.1", "Limpa", icon=icon(name = "eraser", lib="font-awesome")),
                            helpText("\n", "\n"),
                                                       
                          ),
                          ##################################
                          ### Main Panel
                          mainPanel( 
                            #titlePanel("Espécie(s)"),
                            helpText("\n", "\n"),
                            tabPanel("familias", tableOutput("familias.n")),
                            helpText("\n", "\n", "\n"),
                            tabPanel("familias", tableOutput("caracteres.n")),
                            helpText("\n", "\n", "\n"),
                            tabPanel("familias", tableOutput("familias.keep")),
                          )
                 ),
          ##################################
          ### Comparação
          ##################################
#             tabPanel("Comparação", 
#                      titlePanel("Comparação"),
#                      ##################################
#                      ### Side Panel
#                      sidebarPanel(
#                        useShinyjs(),
#                        titlePanel("Famílias"),
#                        helpText("Selecione 1 ou mais espécies para comparar suas características."),
#                        helpText("\n", "\n"),
#                        # Famílias
#                        dropdown(checkboxGroupInput(inputId = "familias.sel",
#                                                    label = "Espécies:",
#                                                    choices = fam.choices), 
#                                                    label = "Espécies"),
#                        helpText("\n", "\n"),
#                        
#                        # Characters
#                        radioButtons(inputId = "chars.sel",
#                                           label = "Caracteres:",
#                                           choices = c(
#                                             "Distintivos" = "distintivos",
#                                             "Semelhantes" = "semelhantes",
#                                             "Todos" = "todos"), selected="distintivos", inline=F
#                                     ),
#                        helpText("\n", "\n"),
#                        
#                        ### Checkbox characters 
#                        checkboxGroupInput(inputId = "char.type",
#                                           label = "Caracteres:",
#                                           choices = char.types.opts),
#                        
#                        ### Clean
#                        actionButton("clean.button.2", "Limpa", icon=icon(name="thumbs-up", lib="glyphicon")),
#                        
#                        helpText("\n", "\n"),
#                        helpText("")
#                      ),
#                      ##################################
#                      ### Main Panel
#                      mainPanel( 
#                        titlePanel("Tabela comparativa"),
#                        helpText("\n", "\n"),
#                        tabPanel("table.comp", tableOutput("familias.sel.n")),
#                        helpText("\n", "\n", "\n"),
#                        tabPanel("table.comp", tableOutput("chars.sel.n")),
#                        helpText("\n", "\n", "\n"),
#                        tabPanel("table.comp", tableOutput("familias.comp")),
#                        helpText("\n", "\n", "\n")
#                      )
#             ),
             
             ### About
             tabPanel("Imagens", icon=icon(name="pagelines", lib="font-awesome"), fluidPage(includeHTML("fotos.html")))
  )
)


