library(shiny)
library(shinyjs)
library(shinyTree)
library(shinyWidgets)
library(shinythemes)
library(markdown)
library(htmltools)
library(readxl)

#################################
### Options
#################################

taxa.in.italics = T

ui.taxa.remaining = "espécie(s) restante(s)"
ui.characters.selected = "caractere(s) selecionado(s)"

#################################
### Data
#################################

read.csv("matrix_3.csv", row.names = 1, sep= "\t") -> mat
read.csv("chatacters.CSV", row.names = 1, stringsAsFactors = F, fileEncoding = "latin1", sep= ";") -> char.dat
data.frame(ID=c(1:ncol(mat)), Family=colnames(mat), stringsAsFactors = F) -> fam

#stringsAsFactors = transforma as strings em fatores, no caso do código está como False, para não transformar em fatores

#################################
### Identificação - variables 
#################################

fam$Family -> keep
names(keep) <- fam$ID
sort(keep) -> keep
max.c = 3 #números de colunas apresentadas no site

#################################
### Identificação - options
#################################

unique(char.dat$Check.box.label) -> ident.ids
vector("list", length=length(ident.ids)) -> char.list
names(char.list) <- ident.ids
for (i in 1:length(ident.ids)) {
  ident.ids[i] -> id0
  char.dat[which(char.dat$Check.box.label == id0),] -> dat0
  vector('list', length=nrow(dat0)) -> sub.list
  rownames(dat0) -> choices0
  sub.list[1:nrow(dat0)] <- choices0
  dat0$Character -> names(sub.list)
  sub.list -> char.list[[i]]
}

names(as.data.frame(char.list)) -> x
sub("^[^.]*.","", x) -> char.dat$Character.lab

#################################
### Comparação - variables
#################################

matrix(nrow=nrow(char.dat), ncol=1) -> comp.out
rownames(comp.out) <- char.dat$Character
colnames(comp.out) <- " "
comp.out[] <- ""
fam.n = 0

#################################
### Comparação - options
#################################

fam$ID -> fam.choices
names(fam.choices) <- fam$Family
fam.choices[order(names(fam.choices))] -> fam.choices
### caracteres
unique(char.dat$Check.box.label) -> char.types.opts

#################################
### Server
#################################

server <- function(input, output, session) {
  
  #################################
  ### Identificação
  sort(keep) -> keep
  fam.out <- matrix(nrow=ceiling(length(keep)/max.c), ncol=max.c)
  fam.out[] <- ""
  fam.out[1:length(keep)] <- keep
  char.n <- vector()
  
  if (taxa.in.italics) {
    gsub(".", " ", fam.out, fixed=T) -> fam.out
    paste("<i>", fam.out, "</i>", sep="") -> fam.out[1:length(fam.out)]
    output$familias.keep <- renderTable({ fam.out },sanitize.text.function=function(x){x}, bordered = F, colnames=F)
  } else {
    output$familias.keep <- renderTable({ fam.out }, bordered = F, colnames=F)
  }
  output$familias.n <- renderText(paste(length(keep), ui.taxa.remaining))
  output$caracteres.n <- renderText(paste(length(char.n), ui.characters.selected))
  
  # check box tree
  output$chars.tree <-  renderUI({ 
    shinyTree("tree", checkbox = TRUE, theme="proton", themeIcons = F)
  })
  output$tree <- renderTree({ char.list })
  
  # check box data
  observeEvent(input$tree, {
    x <- names(as.data.frame(get_selected(input$tree, format = "slices")))
    #gsub(".", " ", x, fixed=T) -> x
    output$familias.keep <- renderText(x)
    if (length(x) > 0) {
      na.omit(match(x, char.dat$Character.lab)) -> x
      as.numeric(x) -> x
      length(x) -> char.n
      mat[x,] -> m0
      colSums(m0)/nrow(m0) -> m0
      which(m0 == 1) -> k0
      keep[match(k0, names(keep))] -> keep
      sort(keep) -> keep
      updateTabsetPanel(session, inputId="familias")
      fam.out <- matrix(nrow=ceiling(length(keep)/max.c), ncol=max.c)
      fam.out[] <- ""
      fam.out[1:length(keep)] <- keep
      if (taxa.in.italics) {
        gsub(".", " ", fam.out, fixed=T) -> fam.out
        paste("<i>", fam.out, "</i>", sep="") -> fam.out[1:length(fam.out)]
        output$familias.keep <- renderTable({ fam.out },sanitize.text.function=function(x){x}, bordered = F, colnames=F)
      } else {
        output$familias.keep <- renderTable({ fam.out }, bordered = F, colnames=F)
      }
      output$familias.n <- renderText(paste(length(keep), ui.taxa.remaining))
      output$caracteres.n <- renderText(paste(char.n, ui.characters.selected))
      
      ### Atualizar a aba de Comparação
      as.numeric(names(keep)) -> fams.sel
      names(fams.sel) <- keep
      sort(fams.sel) -> fams.sel
      updateCheckboxGroupInput(session=session, inputId="familias.sel",
                               choices = fam.choices, selected = fams.sel)
    } else {
      fam$Family -> keep
      names(keep) <- fam$ID
      sort(keep) -> keep
      fam.out <- matrix(nrow=ceiling(length(keep)/max.c), ncol=max.c)
      fam.out[] <- ""
      fam.out[1:length(keep)] <- keep
      char.n <- vector()
      
      if (taxa.in.italics) {
        gsub(".", " ", fam.out, fixed=T) -> fam.out
        paste("<i>", fam.out, "</i>", sep="") -> fam.out[1:length(fam.out)]
        output$familias.keep <- renderTable({ fam.out },sanitize.text.function=function(x){x}, bordered = F, colnames=F)
      } else {
        output$familias.keep <- renderTable({ fam.out }, bordered = F, colnames=F)
      }
      output$familias.n <- renderText(paste(length(keep), ui.taxa.remaining))
      output$caracteres.n <- renderText(paste(length(char.n), ui.characters.selected))
      
    } 
    
    
  })
  
  # clean 
  observeEvent(input$clean.button.1, { 
    if (input$clean.button.1 > 0) {
      fam$Family -> keep
      names(keep) <- fam$ID
      sort(keep) -> keep
      fam.out <- matrix(nrow=ceiling(length(keep)/max.c), ncol=max.c)
      fam.out[] <- ""
      fam.out[1:length(keep)] <- keep
      char.n <- vector()
      
      if (taxa.in.italics) {
        gsub(".", " ", fam.out, fixed=T) -> fam.out
        paste("<i>", fam.out, "</i>", sep="") -> fam.out[1:length(fam.out)]
        output$familias.keep <- renderTable({ fam.out },sanitize.text.function=function(x){x}, bordered = F, colnames=F)
      } else {
        output$familias.keep <- renderTable({ fam.out }, bordered = F, colnames=F)
      }
      output$familias.n <- renderText(paste(length(keep), ui.taxa.remaining))
      output$caracteres.n <- renderText(paste(length(char.n), ui.characters.selected))
      ## tree
      updateTree(session, "tree", data = char.list)
    }
  })
  
  
  #################################
  ### Comparacao
  
  output$familias.sel.n <- renderText(paste(fam.n, ui.taxa.remaining))
  output$chars.sel.n <- renderText(paste(nrow(comp.out), ui.characters.selected))
  output$familias.comp <- renderTable({ comp.out }, bordered = T, colnames=T, rownames = T)
  
  observeEvent(c(input$familias.sel, input$chars.sel, input$char.type), {
    input$chars.sel -> chars.sel.option
    input$familias.sel -> f0
    input$char.type -> chars.type.keep
    fam$Family[match(f0, fam$ID)] -> families
    if (length(families) > 0) {
      length(families) -> fam.n
      data.frame(mat[,match(f0, fam$ID)]) -> m0
      colnames(m0) <- families
      rownames(m0) <- char.dat$Character
      if (chars.sel.option == "todos") {
        m0 -> comp.out
      }
      if (chars.sel.option == "semelhantes") {
        if (length(families) > 1) {
          rowSums(m0) -> s0
          which(s0 == ncol(m0)) -> c1
          which(s0 == 0) -> c2
          m0[sort(c(c1,c2)),] -> comp.out
        } else {
          m0 -> comp.out
        }
      }
      if (chars.sel.option == "distintivos") {
        if (length(families) > 1) {
          rowSums(m0) -> s0
          which(s0 == ncol(m0)) -> c1
          which(s0 == 0) -> c2
          m0[-sort(c(c1,c2)),] -> comp.out
        } else {
          m0 -> comp.out
        }
        
      }
      if (length(chars.type.keep) > 0) {
        char.dat[match(rownames(comp.out), char.dat$Character),] -> char.dat.0
        keep.chars <- which(is.na(match(char.dat.0$Check.box.label, chars.type.keep))==F)
        data.frame(comp.out[keep.chars,], stringsAsFactors = F) -> m1
        rownames(m1) <- rownames(comp.out)[keep.chars]
        colnames(m1) <- colnames(comp.out)
        m1 -> comp.out
      }
      comp.out[comp.out == 0] <- ""
      comp.out[comp.out == 1] <- "x"
      output$familias.sel.n <- renderText(paste(fam.n, ui.taxa.remaining))
      output$familias.comp <- renderTable({ comp.out }, bordered = T, colnames=T, rownames = T)
      output$chars.sel.n <- renderText(paste(nrow(comp.out), ui.characters.selected))
      
    }
  })
  
  # clean 
  observeEvent(input$clean.button.2, { 
    if (input$clean.button.2 > 0) {
      updateCheckboxGroupInput(session=session, inputId="familias.sel",
                               choices = fam.choices, selected = c())
      updateCheckboxGroupInput(session=session, inputId="char.type",
                               choices = char.types.opts, selected = c())
      output$familias.sel.n <- renderText(paste(fam.n, ui.taxa.remaining))
      output$chars.sel.n <- renderText(paste(nrow(comp.out), ui.characters.selected))
      output$familias.comp <- renderTable({ comp.out }, bordered = T, colnames=T, rownames = T)
    }
  })
  
  # reset 
  #observeEvent(input$reload.button.1, { session$reload() })
  
}