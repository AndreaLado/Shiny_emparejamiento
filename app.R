# install.packages("tidyverse")
# install.packages("shiny")
# install.packages("shinyWidgets")
# install.packages("ebal")
# install.packages("shinydashboard")
# install.packages("MatchIt")
# install.packages("optmatch")
# install.packages("DT")


library(tidyverse)
library(shiny)
library(shinyWidgets)
library(ebal)
library(shinydashboard)
library(MatchIt)
library(Matching)
library(optmatch)
library(DT)

################IMPORTACIÓN######################## 


# install.packages("tidyverse")
library(tidyverse)

#Importo según el tipo de origen de la base. Sacar comentario y adaptar según corresponda 

#STATA:
# library(haven)
# base <- read_dta("ubicación de la base")

#R:
#base <- read.csv("ubicación de la base", sep=";")

#EXCEL
#library(readxl)
#base<- read_excel("ubicación de la base")

#######PREPARACIÓN DE LA BASE DE DATOS##############

#La base de datos tiene que tener un grupo de tratados identificados a partir de una columna llamada "T" con 0 y 1
#el 1 indica tratados


################vectores usados##########################

meanTest <- function(Tr,Co,weights.Tr,weights.Co) {
  ttest=data.frame()
  for (i in 1:ncol(Tr)){
    b=balanceUV(Tr[is.na(Tr[,i])==FALSE,i],Co[is.na(Co[,i])==FALSE,i], weights.Tr=weights.Tr, weights.Co=weights.Co,exact = TRUE, ks=FALSE,nboots = 1000, paired=TRUE, match=FALSE,estimand="ATT")
    ttest[i,1]=round(b$mean.Tr,2)
    ttest[i,2]=round(b$mean.Co,2)
    ttest[i,3]=round(b$tt$estimate,2)
    ttest[i,4]=round(b$tt$p.value,3)
    ttest[i,5]= sum(weights.Tr[is.na(Tr[,i])==FALSE])
    ttest[i,6]= round(sum(weights.Co[is.na(Co[,i])==FALSE]))
    #ttest[i,5]=nrow(Tr[is.na(Tr[,i])==FALSE,i])
    #ttest[i,6]=nrow(Co[is.na(Co[,i])==FALSE ,i])
  }
  names(ttest)=c("mean.Tr","mean.Co","dif","p.value","N.Tr","N.Co")
  rownames(ttest)=names(Tr)
  return(ttest) # diferencia de medias 
}

#A continuación se seleccionan las variables que queramos testear en el emparejamiento

#Previo a este paso se puede realizar un renombramiento para que el nombre sea mas amigable en la APP:
# base<-base %>%
#   rename(
#     Nombrenuevo = nombreviejo
#     )
    
    
v_emp=c( "Var1", 
         "var2",
         "Var3",
         "..."
        )
#es importante que las variables de emparejamiento sean numéricas y no sean colineales entre sí. 
#tampoco puede haber missings, por lo que si la base los tiene hay que lidiar con ese problema antes. 



vbles=c("Variables que queramos ver en los tests de medias") 
#recomendable repetir emparejamiento y se pueden agregar otras. Tienen que ser numéricas

Tr=base[base$T==1,vbles]
Co=base[base$T==0,vbles]

base=base %>%
  arrange(T,id)


##################UI##########################

header <-  dashboardHeader(
  title = "Emparejamiento XXX"
)

sidebar<- dashboardSidebar(
  sidebarMenu(
    menuItem("Introducción", icon = icon("angle-right"), tabName= "intro"), 
    menuItem("Parámetros de emparejamiento", icon = icon("angle-right"), tabName = "parametros"), 
    menuItem("Tests de medias", icon = icon("angle-right"), tabName = "tests"), 
    menuItem("Ponderadores usados", icon = icon("angle-right"), tabName = "ponderadores")
  )
)

body<- dashboardBody(
  tabItems(
    tabItem(
      tabName = "intro",
      fluidRow( 
      
        valueBox(1.486 , "Tamaño de la muestra", icon = icon("check"), width= 6), 
        valueBox(1133, "Muestra depurada", icon= icon("check-double"), width= 6), 
        valueBox(352, "Tratados", icon= icon("user-check"), color="yellow"), 
        valueBox(781, "Potenciales controles", icon= icon("user-minus"), color="yellow"), 
        valueBox("?", "Controles efectivos", icon= icon("question"), color="yellow"), 
        valueBox("Más", "info", href = "PFIS_RM.html", icon = icon("align-justify"), color="red")
    
        )
      
    ), 
    tabItem(
      tabName = "parametros",
      column(width = 12,
      fluidRow(
        box(
          title = "Emparejamiento y funciones de densidad", status = "primary", solidHeader = TRUE,
          pickerInput(inputId = "emparejamiento",
                      label = span( "Variables emparejamiento:"),
                      choices = sort(unique(v_emp)),
                      selected = sort(unique(v_emp)),
                      options = list(
                        `deselect-all-text` = "Ninguno",
                        `select-all-text` = "Todos",
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        size = 19),
                      multiple = TRUE),
          pickerInput(inputId = "var",
                      label = span( "Variable densidad:"),
                      choices = sort(c("Var1", "Var2" ,"Var3",  "Var4", "Var5", "Var6")), #Aquí se agregan las variables que queramos observar en las funciones de densidad
                      selected = sort(c("Var1")), #la función de densidad por defecto
                      options = list(
                        `deselect-all-text` = "Ninguno",
                        
                        `actions-box` = TRUE,
                        `live-search` = TRUE,
                        size = 19),
                      multiple = FALSE)
        )
        ),
      column(width = 12,
             fluidRow(
               
               box(
                 title= "Entropy balance matching",  status = "primary", solidHeader = TRUE,
                 plotOutput(
                   "densidad_1_eb"
                 )
               ), 
               box(
                 title= "Genetic matching", status = "primary", solidHeader = TRUE,
                 plotOutput(
                   "densidad_1_gm"
                 )
               ), 
               box(
                 title= "PSM matching", status = "primary", solidHeader = TRUE,
                 plotOutput(
                   "densidad_1_psm"
                 )
               )
             )
             )
      
      )
    ), 
    tabItem(
      tabName = "tests",
      fluidRow(
        box(
          title ="Tests de medias entropy balance",  status = "primary", solidHeader = TRUE, width = 12,
          dataTableOutput(
            "test_eb"
          )
        ), 
        box(
          title ="Tests de medias genetic matching",  status = "primary", solidHeader = TRUE, width = 12,
          dataTableOutput(
            "test_gm"
          )
        ), 
        box(
          title ="Tests de medias psm matching",  status = "primary", solidHeader = TRUE, width = 12,
          dataTableOutput(
            "test_psm"
          )
        )
      )
    ),
    tabItem(
      tabName = "ponderadores", 
      fluidRow(
        box(
          title= "Entropy balance matching",  status = "primary", solidHeader = TRUE,
          plotOutput(
            "pond_eb"
          )
        ), 
        box(
          title= "Genetic matching", status = "primary", solidHeader = TRUE,
          plotOutput(
            "pond_gm"
          )
        )
        , 
        box(
          title= "Propensity score matching", status = "primary", solidHeader = TRUE,
          plotOutput(
            "pond_psm"
          )
      )
    )
  )
)
)
ui<- dashboardPage(skin= "purple", header, sidebar, body)

#################server#######################

server<- function(input, output){
  
  
  #entropy balance
  output$pond_eb<- renderPlot({
    elegidos <- base[, input$emparejamiento]
    
    e=ebalance(base$T, elegidos)
    t=table(base$T)[[2]]
    base$w_eb=c(e$w,rep(1,t))
    
    
    base%>%
      ggplot() + geom_histogram(data=base, aes(x=w_eb, fill=T))+
      facet_grid(~T)
  }) 
  
  output$test_eb <- DT::renderDataTable(rownames=TRUE,
    # striped = TRUE, spacing = 'xs',digits = 2,
    {
    elegidos <- base[, input$emparejamiento]
    e=ebalance(base$T, elegidos)
    t=table(base$T)[[2]]
    base$w_eb=c(e$w,rep(1,t))
    
    
    
    meanTest(Tr,Co,weights.Tr=base$w_eb[base$T==1],weights.Co=base$w_eb[base$T==0]) %>%
        dplyr::select(-N.Co, -N.Tr)%>%
        # dplyr::arrange(p.value)%>%
       rename("Media tratados"= mean.Tr, "Media Controles"= mean.Co, "Diferencia"=dif, 
               "p-valor"=p.value)
     
  })
  
  
  output$densidad_1_eb <- renderPlot({
    elegidos <- base[, input$emparejamiento]
    e=ebalance(base$T, elegidos)
    t=table(base$T)[[2]]
    base$w_eb=c(e$w,rep(1,t))

  plot(density(base[[input$var]][base$T==1], kernel = c("gaussian")),col="red",main=paste("Densidad", toupper(input$var)))
  lines(density(base[[input$var]][base$T==0], kernel = c("gaussian")),col="blue",main="")
  lines(density(base[[input$var]][base$T==0], kernel = c("gaussian"),weight=(base$w_eb[base$T==0]/sum(base$w_eb[base$T==0]))),col="green",main="")
   
    }
  )
    #genetic matching
  
  
  output$pond_gm<- renderPlot({
    
    elegidos <- base[, c(input$emparejamiento, "T")]
   
    
    variable<- paste("T ~ ", paste(input$emparejamiento, collapse = " + "))
   
    gm <- matchit(as.formula(variable),
                   data=elegidos, method = "optimal", distance = "logit")
    
    base$w_gm=gm$weights
    
    
    base%>%
      ggplot() + geom_histogram(data=base, aes(x=w_gm, fill=T))+
      facet_grid(~T)
  }) 
  
  output$test_gm <- DT::renderDataTable(rownames=TRUE,
                                                        {
    elegidos <- base[, c(input$emparejamiento, "T")]


    variable<- paste("T ~ ", paste(input$emparejamiento, collapse = " + "))
 
    gm <- matchit(as.formula(variable),
                   data=elegidos, method = "optimal", distance = "logit")
    
    base$w_gm=gm$weights
    
    
    meanTest(Tr,Co,weights.Tr=base$w_gm[b$T==1],weights.Co=base$w_gm[base$T==0])%>%
       dplyr::select(-starts_with("N"))%>%
      # dplyr::arrange(p.value)%>%
      rename("Media tratados"= mean.Tr, "Media Controles"= mean.Co, "Diferencia"=dif, 
             "p-valor"=p.value)
      
    
  })
  
  
  output$densidad_1_gm <- renderPlot({
    
    elegidos <- base[, c(input$emparejamiento, "T")]
   
    
    
    
    variable<- paste("T ~ ", paste(input$emparejamiento, collapse = " + "))
   
    gm<- matchit(as.formula(variable),
                   data=elegidos, method = "optimal", distance = "logit")
    
    base$w_gm=gm$weights
    
    plot(density(base[[input$var]][base$T==1], kernel = c("gaussian")),col="red",main=paste("Densidad", toupper(input$var)))
    lines(density(base[[input$var]][base$T==0], kernel = c("gaussian")),col="blue",main="")
    lines(density(base[[input$var]][base$T==0], kernel = c("gaussian"),weight=(base$w_gm[base$T==0]/sum(base$w_gm[base$T==0]))),col="green",main="")
  }
  )
  
  
  #PSM matching
  
  
  output$pond_psm<- renderPlot({
    
    elegidos <- base[, c(input$emparejamiento, "T")]
    
    variable<- paste("T ~ ", paste(input$emparejamiento, collapse = " + "))
    
    psm <- matchit(as.formula(variable),
                  data=elegidos, method = "nearest", distance = "logit")
    
    base$w_psm=psm$weights
    
    
    base%>%
      ggplot() + geom_histogram(data=base, aes(x=w_psm, fill=T))+
      facet_grid(~T)
  }) 
  
  output$test_psm <-DT::renderDataTable(rownames=TRUE,
                                {
    elegidos <- base[, c(input$emparejamiento, "T")]
    
    
    
    
    variable<- paste("T ~ ", paste(input$emparejamiento, collapse = " + "))
    
    psm <- matchit(as.formula(variable),
                  data=elegidos, method = "nearest", distance = "logit")
    
    base$w_psm=psm$weights
    
    
    meanTest(Tr,Co,weights.Tr=base$w_psm[b$T==1],weights.Co=base$w_psm[b$T==0]) %>%
    dplyr::select(-starts_with("N"))%>%
      # dplyr::arrange(p.value)%>%
      rename("Media tratados"= mean.Tr, "Media Controles"= mean.Co, "Diferencia"=dif, 
             "p-valor"=p.value)
    
  })
  
  
  output$densidad_1_psm <- renderPlot({
    
    elegidos <- base[, c(input$emparejamiento, "T")]
    
    variable<- paste("T ~ ", paste(input$emparejamiento, collapse = " + "))
    
    psm<- matchit(as.formula(variable),
                 data=elegidos, method = "nearest", distance = "logit")
    
    base$w_psm=psm$weights
    
    
    plot(density(base[[input$var]][base$T==1], kernel = c("gaussian")),col="red",main=paste("Densidad", toupper(input$var)))
    lines(density(base[[input$var]][base$T==0], kernel = c("gaussian")),col="blue",main="")
    lines(density(base[[input$var]][base$T==0], kernel = c("gaussian"),weight=(base$w_psm[base$T==0]/sum(base$w_psm[base$T==0]))),col="green",main="")
  }
  )
  
  
  
}


################shinyapp######################
shinyApp(ui = ui, server = server)






