library(tidyverse)
library(rvest)
library(stringr)
library(lubridate)
library(shiny)
library(shinythemes)

ScoresA = c(25.4347,0.14354,51.39,0.8465,1.53775,5.74352,12.91,0.2797,10.14,0.03768)
ScoresB = c(18,220,1.5,75,82,28.5,4,100,7,480)
ScoresC = c(1.81,1.4,1.05,1.42,1.81,1.92,1.1,1.35,1.08,1.85)

get_performances = function(athletelink){
  
  performances = read_html(athletelink) |> 
    html_element(xpath='//*[@id="cphBody_pnlPerformances"]/table[2]') |> 
    html_table() |> 
    as.data.frame() |> 
    setNames(c("Event","Performance","IorW",
               "Windval","Col5","Pos","Data",
               "Col8","Col9","Venue","Meeting","Date"))
  
  AgeColumns=which(performances$Event =="Event")-1
  Agelist = c()
  NewAge = NA
  for (i in (1:length(performances[,1]))){
    if (is.element(i,AgeColumns) == TRUE){
      NewAge = word(performances[i,1],2)}
    Agelist = append(Agelist,NewAge)
  }
  performances$AgeGroup = Agelist
  performances = performances[-(AgeColumns),]
  performances = performances[-(which(performances$Event =="Event")),]
  performances$Date = format(as.Date(performances$Date,format="%d %b %y"))
  performances$Date = as.Date(performances$Date)
  performances =  performances |> mutate(
    Performance=ifelse(str_count(Performance,":")>0,
                       period_to_seconds(ms(Performance)),
                       Performance)
  )
  return(performances)
}

search_athlete = function(surname,firstname,club=""){
  
  athlete_session = session("https://www.thepowerof10.info/athletes/athleteslookup.aspx")
  setform = html_form(athlete_session)[[1]] |> html_form_set("ctl00$cphBody$txtSurname"=surname,
                                                             "ctl00$cphBody$txtFirstName"=firstname,
                                                             "ctl00$cphBody$txtClub" = club)
  newurl = session_submit(athlete_session,setform)$response$url
  if (substr(newurl,1,62) == "https://www.thepowerof10.info/athletes/profile.aspx?athleteid="){
    return(data.frame("First"=firstname,
                      "Surname"=surname,
                      "Track"="?",
                      "Sex"="?",
                      "Club"="?",
                      "Links"=newurl))
  } else{
    athlete_search =  read_html("https://www.thepowerof10.info/athletes/athleteslookup.aspx") |> 
      html_form()
    
    athlete_search =  athlete_search[[1]] |> 
      html_form_set("ctl00$cphBody$txtSurname"=surname,
                    "ctl00$cphBody$txtFirstName"=firstname,
                    "ctl00$cphBody$txtClub" = club) |> 
      html_form_submit() |> 
      read_html() |> 
      html_element(xpath='//*[@id="cphBody_dgAthletes"]')
    
    searchtable = athlete_search |> 
      html_table(header=TRUE) |> 
      select(c(First,Surname,Track,Sex,Club))
    
    links = athlete_search |> 
      html_elements("a") |> 
      html_attr("href")
    links = links[substr(links,1,23)=="profile.aspx?athleteid="]
    
    searchtable$Links = paste0("https://www.thepowerof10.info/athletes/",links)
    
    return(searchtable)
  }
}

get_pbs = function(athletelink){
  
  pbs = read_html(athletelink) |> 
    html_element(xpath='//*[@id="cphBody_divBestPerformances"]/table') |> 
    html_table(header=TRUE) |> 
    as.data.frame() |> 
    select(c(1,2)) |> 
    filter(Event != "Event")
  
  #Tidy Data
  pbs[] = lapply(pbs,as.character)
  
  n_slash = max(str_count(pbs$PB,"/"), na.rm=TRUE)
  if (n_slash == 0){
    break
  }else{
    for (val in c(1:nrow(pbs))){
      pbs[val,c(2:(2+n_slash))] = as.list(str_split_fixed(pbs[val,2],"/",n=n_slash+1)) 
    }
    pbs[pbs == ""] = NA
    pbs = pbs |> 
      pivot_longer(c(2,(2+n_slash)),values_to="PB",values_drop_na=TRUE) |> 
      select(Event,PB)
  }
  
  pbs = pbs |>  mutate(Conditions = ifelse(
    substring(PB,nchar(PB))=="i",
    "i",
    "")) |> 
    mutate(PB=ifelse(
      substring(PB,nchar(PB))=="i",
      substring(PB,1,nchar(PB)-1),
      PB)) |> 
    mutate(Conditions = ifelse(
      substring(PB,nchar(PB))=="w",
      "w",
      Conditions)) |> 
    mutate(PB=ifelse(
      substring(PB,nchar(PB))=="w",
      substring(PB,1,nchar(PB)-1),
      PB)) |> 
    mutate(PB = ifelse(
      str_count(PB,":")>0,
      as.numeric(period_to_seconds(ms(PB))),
      as.numeric(PB)))
  return(pbs)
}

get_dec_pbs = function(athletelink){
  
  PBS = get_pbs(athletelink) |> filter(Conditions != "w")
  DecPBS = c()
  DecPBS = DecPBS |> 
    append(min(filter(PBS,Event=="100")$PB)) |> 
    append(max(filter(PBS,Event=="LJ")$PB)) |>
    append(max(filter(PBS,Event=="SP7.26K")$PB)) |>
    append(max(filter(PBS,Event=="HJ")$PB)) |>
    append(min(filter(PBS,Event=="400")$PB)) |>
    append(min(filter(PBS,Event=="110H")$PB)) |>
    append(max(filter(PBS,Event=="DT2K")$PB)) |>
    append(max(filter(PBS,Event=="PV")$PB)) |>
    append(max(filter(PBS,Event=="JT800")$PB)) |>
    append(min(filter(PBS,Event=="1500")$PB))
  DecPBS[which(DecPBS == Inf | DecPBS ==-Inf)] = NA
  return(DecPBS)
}

ui = navbarPage(
  "Athletics Tools",
  theme = shinytheme("superhero"),
  tabPanel(
    "Get Link",
    fluidPage(
      fluidRow(
        column(12,h1("Athletics Tools"),align="center")
      ),
      fluidRow(
        column(12,h4("Welcome to Athletics Tools! Use the applet below to find your Power of 10 link, then navigate the tabs to use other features.
                     Unfortunately the website may run slow, so plots may take several seconds to render.",align="center"))
      ),
      fluidRow(
        column(12,h2("Find Your Power Of 10 Link"),align="center")
      ),
      fluidRow(
        column(1),
        column(10,
               textInput(
                 inputId="LINK",
                 label="Input Link directly",
                 width="100%"
               ),
               align="center"
        ),
        column(1)
      ),
      fluidRow(
        column(3,
               textInput(
                 inputId="Surname",
                 label="Surname"
               ),
               align="center"
        ),
        column(3,
               textInput(
                 inputId="Firstname",
                 label="Firstname"
               ),
               align="center"
        ),
        column(3,
               textInput(
                 inputId="Club",
                 label="Club"
               ),
               align="center"
        ),
        column(3,
               actionButton(
                 inputId="ActivateSearch",
                 label = "Search"
               ),
               align="center"
        )
      ),
      fluidRow(
        column(12, dataTableOutput("SearchTable"),align="center")
      )
    )
  ),
  tabPanel("Plot Performances",
           sidebarLayout(
             sidebarPanel(
               uiOutput("eventoptions")
             ),
             mainPanel(
               plotOutput(outputId = "distplot")
             )
           )
  ),
  tabPanel(
    "Decathlon",
    fluidPage(
      fluidRow(
        column(12, h1("Points Calculator"), align="center")
      ),
      fluidRow(
        column(6, h2("Day 1"), align="center"),
        column(6, h2("Day 2"), align="center")
      ),
      fluidRow(
        column(3,
               textInput(
                 inputId="DecI1",
                 label="100M"
               )
        ),
        column(3, uiOutput("DecO1"), align="center"),
        column(3,
               textInput(
                 inputId="DecI6",
                 label="110h"
               )
        ),
        column(3, uiOutput("DecO6"), align="center")
      ),
      fluidRow(
        column(3,
               textInput(
                 inputId="DecI2",
                 label="Long Jump"
               )
        ),
        column(3, uiOutput("DecO2"), align="center"),
        column(3,
               textInput(
                 inputId="DecI7",
                 label="Discus"
               )
        ),
        column(3, uiOutput("DecO7"), align="center")
      ),
      fluidRow(
        column(3,
               textInput(
                 inputId="DecI3",
                 label="Shotput"
               )
        ),
        column(3, uiOutput("DecO3"), align="center"),
        column(3,
               textInput(
                 inputId="DecI8",
                 label="Pole Vault"
               )
        ),
        column(3, uiOutput("DecO8"), align="center")
      ),
      fluidRow(
        column(3,
               textInput(
                 inputId="DecI4",
                 label="High Jump"
               )
        ),
        column(3, uiOutput("DecO4"), align="center"),
        column(3,
               textInput(
                 inputId="DecI9",
                 label="Javelin"
               )
        ),
        column(3, uiOutput("DecO9"), align="center")
      ),
      fluidRow(
        column(3,
               textInput(
                 inputId="DecI5",
                 label="400m"
               )
        ),
        column(3, uiOutput("DecO5"), align="center"),
        column(3,
               textInput(
                 inputId="DecI10",
                 label="1500m"
               )
        ),
        column(3, uiOutput("DecO10"), align="center")
      ),
      fluidRow(
        column(12, uiOutput("DecTotal"),align="center")
      ),
      fluidRow(
        column(5),
        column(2,
               actionButton(
                 "Decpbs",
                 "Click to get PBs",
                 align="center"
               )
        ),
        column(5)
      )
    )
  )
)

server = function(input,output){
  
  get_search_items = eventReactive(input$ActivateSearch,{
    if(input$Surname != ""){
      updateTextInput(session=getDefaultReactiveDomain(),"LINK",value=search_athlete(input$Surname,input$Firstname,input$Club)$Links[1])
      return(search_athlete(input$Surname,input$Firstname,input$Club))
    }else{
      return(data.frame("Data Table"="A surname must be entered to search"))
    }
  })
  
  output$SearchTable = renderDataTable(get_search_items(),
                                       options=list(searching=FALSE,
                                                    paging=FALSE))
  
  PTenLink = eventReactive(input$LINK, {
    perf = get_performances(input$LINK)
    if(input$LINK != ""){
      return(unique(perf$Event))
    } else {
      return(c("100"))
    }
  })
  
  output$eventoptions = renderUI({
    shiny::validate(
      need(substring(input$LINK,0,62) =="https://www.thepowerof10.info/athletes/profile.aspx?athleteid=",
           "Enter Power of 10 Profile")
    )
    selectInput("element",
                label="Choose an Event",
                choices = as.list(sort(PTenLink()))
    )
  })
  
  output$distplot = renderPlot({
    shiny::validate(
      need(substring(input$LINK,0,62) =="https://www.thepowerof10.info/athletes/profile.aspx?athleteid=",
           "Enter Power of 10 Profile to show plot!")
    )
    perf =  get_performances(input$LINK) |> 
      filter(Event ==input$element)
    gg = ggplot(perf,aes(x=Date,y=as.numeric(Performance)))+geom_point(aes(color=IorW))+
      xlab("Date")+ylab("Performance")+labs(color="Conditions")
    if (nrow(perf) >10) {
      gg+geom_smooth()+theme_minimal()
    }else{
      gg
    }
  })
  
  Scores =  reactiveValues(a1=0,a2=0,a3=0,a4=0,a5=0,a6=0,a7=0,a8=0,a9=0,a10=0)
  
  output$DecO1 = renderUI({
    shiny::validate(
      need(is.na(as.numeric(input$DecI1)) == FALSE,
           "Enter Score s.ms")
    )
    if (as.numeric(input$DecI1) > ScoresB[1]){
      Scores$a1 = 0
      output$DecTotal = renderUI({
        h3(paste0("Score: ", sum(unlist(reactiveValuesToList(Scores)))))
      })
      h3(0)
    }else{
      Scores$a1 = round(ScoresA[1]*abs(as.numeric(input$DecI1)-ScoresB[1])**ScoresC[1])
      output$DecTotal = renderUI({
        h3(paste0("Score: ", sum(unlist(reactiveValuesToList(Scores)))))
      })
      h3(Scores$a1)
    }
  })
  
  output$DecO2= renderUI({
    shiny::validate(
      need(is.na(as.numeric(input$DecI2)) == FALSE,
           "Enter Score m.cm")
    )
    if (100*as.numeric(input$DecI2) < ScoresB[2]){
      Scores$a2 = 0
      output$DecTotal = renderUI({
        h3(paste0("Score: ", sum(unlist(reactiveValuesToList(Scores)))))
      })
      h3(0)
    }else{
      Scores$a2 = round(ScoresA[2]*abs(100*as.numeric(input$DecI2)-ScoresB[2])**ScoresC[2])
      output$DecTotal = renderUI({
        h3(paste0("Score: ", sum(unlist(reactiveValuesToList(Scores)))))
      })
      h3(Scores$a2)
    }
  })
  
  output$DecO3 = renderUI({
    shiny::validate(
      need(is.na(as.numeric(input$DecI3)) == FALSE,
           "Enter Score m.cm")
    )
    if (as.numeric(input$DecI3) < ScoresB[3]){
      Scores$a3 = 0
      output$DecTotal = renderUI({
        h3(paste0("Score: ", sum(unlist(reactiveValuesToList(Scores)))))
      })
      h3(0)
    }else{
      Scores$a3 = round(ScoresA[3]*abs(as.numeric(input$DecI3)-ScoresB[3])**ScoresC[3])
      output$DecTotal = renderUI({
        h3(paste0("Score: ", sum(unlist(reactiveValuesToList(Scores)))))
      })
      h3(Scores$a3)
    }
  })
  
  output$DecO4 = renderUI({
    shiny::validate(
      need(is.na(as.numeric(input$DecI4)) == FALSE,
           "Enter Score m.cm")
    )
    if (100*as.numeric(input$DecI4) < ScoresB[4]){
      Scores$a4 = 0
      output$DecTotal = renderUI({
        h3(paste0("Score: ", sum(unlist(reactiveValuesToList(Scores)))))
      })
      h3(0)
    }else{
      Scores$a4 = round(ScoresA[4]*abs(100*as.numeric(input$DecI4)-ScoresB[4])**ScoresC[4])
      output$DecTotal = renderUI({
        h3(paste0("Score: ", sum(unlist(reactiveValuesToList(Scores)))))
      })
      h3(Scores$a4)
    }
  })
  
  output$DecO5 = renderUI({
    shiny::validate(
      need(is.na(as.numeric(input$DecI5)) == FALSE,
           "Enter Score s.ms")
    )
    if (as.numeric(input$DecI5) > ScoresB[5]){
      Scores$a5 = 0
      output$DecTotal = renderUI({
        h3(paste0("Score: ", sum(unlist(reactiveValuesToList(Scores)))))
      })
      h3(0)
    }else{
      Scores$a5 = round(ScoresA[5]*abs(as.numeric(input$DecI5)-ScoresB[5])**ScoresC[5])
      output$DecTotal = renderUI({
        h3(paste0("Score: ", sum(unlist(reactiveValuesToList(Scores)))))
      })
      h3(Scores$a5)
    }
  })
  
  output$DecO6 = renderUI({
    shiny::validate(
      need(is.na(as.numeric(input$DecI6)) == FALSE,
           "Enter Score s.ms")
    )
    if (as.numeric(input$DecI6) > ScoresB[6]){
      Scores$a6 = 0
      output$DecTotal = renderUI({
        h3(paste0("Score: ", sum(unlist(reactiveValuesToList(Scores)))))
      })
      h3(0)
    }else{
      Scores$a6 = round(ScoresA[6]*abs(as.numeric(input$DecI6)-ScoresB[6])**ScoresC[6])
      output$DecTotal = renderUI({
        h3(paste0("Score: ", sum(unlist(reactiveValuesToList(Scores)))))
      })
      h3(Scores$a6)
    }
  })
  
  output$DecO7 = renderUI({
    shiny::validate(
      need(is.na(as.numeric(input$DecI7)) == FALSE,
           "Enter Score m.cm")
    )
    if (as.numeric(input$DecI7) < ScoresB[7]){
      Scores$a7 = 0
      output$DecTotal = renderUI({
        h3(paste0("Score: ", sum(unlist(reactiveValuesToList(Scores)))))
      })
      h3(0)
    }else{
      Scores$a7 = round(ScoresA[7]*abs(as.numeric(input$DecI7)-ScoresB[7])**ScoresC[7])
      output$DecTotal = renderUI({
        h3(paste0("Score: ", sum(unlist(reactiveValuesToList(Scores)))))
      })
      h3(Scores$a7)
    }
  })
  
  output$DecO8 = renderUI({
    shiny::validate(
      need(is.na(as.numeric(input$DecI8)) == FALSE,
           "Enter Score m.cm")
    )
    if (100*as.numeric(input$DecI8) < ScoresB[8]){
      Scores$a8 = 0
      output$DecTotal = renderUI({
        h3(paste0("Score: ", sum(unlist(reactiveValuesToList(Scores)))))
      })
      h3(0)
    }else{
      Scores$a8 = round(ScoresA[8]*abs(100*as.numeric(input$DecI8)-ScoresB[8])**ScoresC[8])
      output$DecTotal = renderUI({
        h3(paste0("Score: ", sum(unlist(reactiveValuesToList(Scores)))))
      })
      h3(Scores$a8)
    }
  })
  
  output$DecO9 = renderUI({
    shiny::validate(
      need(is.na(as.numeric(input$DecI9)) == FALSE,
           "Enter Score m.cm")
    )
    if (as.numeric(input$DecI9) < ScoresB[9]){
      Scores$a9 = 0
      output$DecTotal = renderUI({
        h3(paste0("Score: ", sum(unlist(reactiveValuesToList(Scores)))))
      })
      h3(0)
    }else{
      Scores$a9 = round(ScoresA[9]*abs(as.numeric(input$DecI9)-ScoresB[9])**ScoresC[9])
      output$DecTotal = renderUI({
        h3(paste0("Score: ", sum(unlist(reactiveValuesToList(Scores)))))
      })
      h3(Scores$a9)
    }
  })
  
  output$DecO10 = renderUI({
    shiny::validate(
      need(is.na(as.numeric(input$DecI10)) == FALSE,
           "Enter Score s.ms")
    )
    if (as.numeric(input$DecI10) > ScoresB[10]){
      Scores$a10 = 0
      output$DecTotal = renderUI({
        h3(paste0("Score: ", sum(unlist(reactiveValuesToList(Scores)))))
      })
      h3(0)
    }else{
      Scores$a10 = round(ScoresA[10]*abs(as.numeric(input$DecI10)-ScoresB[10])**ScoresC[10])
      output$DecTotal = renderUI({
        h3(paste0("Score: ", sum(unlist(reactiveValuesToList(Scores)))))
      })
      h3(Scores$a10)
    }
  })
  
  observeEvent(input$Decpbs,{
    if (substring(input$LINK,0,62) =="https://www.thepowerof10.info/athletes/profile.aspx?athleteid="){
      DecPBS = get_dec_pbs(input$LINK)
      updateTextInput(session=getDefaultReactiveDomain(),"DecI1",value=DecPBS[1])
      updateTextInput(session=getDefaultReactiveDomain(),"DecI2",value=DecPBS[2])
      updateTextInput(session=getDefaultReactiveDomain(),"DecI3",value=DecPBS[3])
      updateTextInput(session=getDefaultReactiveDomain(),"DecI4",value=DecPBS[4])
      updateTextInput(session=getDefaultReactiveDomain(),"DecI5",value=DecPBS[5])
      updateTextInput(session=getDefaultReactiveDomain(),"DecI6",value=DecPBS[6])
      updateTextInput(session=getDefaultReactiveDomain(),"DecI7",value=DecPBS[7])
      updateTextInput(session=getDefaultReactiveDomain(),"DecI8",value=DecPBS[8])
      updateTextInput(session=getDefaultReactiveDomain(),"DecI9",value=DecPBS[9])
      updateTextInput(session=getDefaultReactiveDomain(),"DecI10",value=DecPBS[10])
    }else{
      updateTextInput(session=getDefaultReactiveDomain(),"DecI1")
    }
  })
}
shinyApp(ui,server)
