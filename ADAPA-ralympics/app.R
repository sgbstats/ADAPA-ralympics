library(googlesheets4)
library(flextable)
library(tidyverse)
library(shiny)
library(shinyjs)
options(gargle_oauth_cache = ".secrets")
gs4_auth(
    cache = ".secrets",
    email = "sebastiangbate@gmail.com"
)

ui <- fluidPage(
    tags$head(
        tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }
    "))
    ),
    tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
    tabsetPanel(
        tabPanel(
            "ADAPA-ralympics", fluid=T,
            mainPanel(
                uiOutput("table",inline = TRUE, style = "margin:0px; padding:0px"),
                # actionButton("reload", "Get new data"),
                tags$p("Green cells denote scoring events."),
                tags$p("Note this is proof of concept dummy data")
            )
        ),
        
        tabPanel(
            "Rules", fluid=T,
            mainPanel(
                tags$h1("Rules"),
                tags$p("Points are awarded based on the number of people competing in each event. If there are 5 people then then first place will receive 5 points, 4 four second and so on."),
                tags$p("Overall total is the sum of each person's best scoring eveents up to two thirds of the total number of events (rounded down)."),
                tags$p("The scoring system for each event will be determined by those competing."),
                tags$p(tags$a(tags$b(href="https://docs.google.com/spreadsheets/d/1h0S27-69mZgoPEyer9InFWZ0rN2PRSdlvqT7xS7Ob6k/edit?usp=sharing", "Spreadsheet of the results"))),
                tags$p(tags$a(tags$b(href="https://github.com/sgbstats/ADAPA-ralympics", "Github"))),
                tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/SGIospD9QRU", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                
            )
            
        )
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    results=googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1h0S27-69mZgoPEyer9InFWZ0rN2PRSdlvqT7xS7Ob6k/edit#gid=0",
                                      sheet = "Results")
    events=googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1h0S27-69mZgoPEyer9InFWZ0rN2PRSdlvqT7xS7Ob6k/edit#gid=0",
                                     sheet = "Events")
    
    # observeEvent(input$reload, {
    #     shinyjs::js$refresh()
    # })
    # 
    output$table=renderUI({

        results2=results %>% pivot_longer(cols=-c(Name), names_to="Event", values_to = "Score") %>% 
            filter(!is.na(Score)) %>% 
            merge(events %>% dplyr::select(Event, Scoring, Date), by="Event") %>% 
            group_by(Event) %>% 
            mutate(points=if_else(Scoring=="High", rank(Score, ties.method = "average"), rank(-Score, ties.method = "average"))) %>% 
            ungroup() %>% 
            dplyr::select(-Scoring, -Score)
        
        resultswide=results2 %>% arrange(Date) %>% select(-Date) %>% pivot_wider(values_from = "points", names_from = "Event")
        
        scoringevents=floor(nrow(events)*(2/3))
        
        championship=results2 %>% 
            select(-Date, -Event) %>% 
            group_by(Name) %>% 
            slice_max(points, n=scoringevents, with_ties = F) %>% 
            summarise(Points=sum(points)) %>% 
            ungroup() %>% 
            mutate(Position=rank(-Points, ties.method = "min")) %>% 
            arrange(Position, Name)
        
        c2=championship %>% merge(resultswide, by="Name") %>% 
            arrange(Position, Name) %>% 
            dplyr::select(-Position)
        
        
        colours=results2 %>% 
            group_by(Name) %>% 
            slice_max(points, n=scoringevents, with_ties = F) %>% 
            mutate(scoring="#cceecc") %>% 
            merge(championship %>% select(Name, Position), by="Name") %>% 
            arrange(Date) %>% 
            select(-Date, -points) %>% 
            pivot_wider(values_from = "scoring", names_from = "Event") %>% 
            arrange(Position, Name) %>% 
            ungroup() %>% 
            select(-Name, -Position) %>% 
            as.matrix()
        
        
        x=championship %>%
            arrange(Position, Name) %>% 
            mutate(col=case_when(Position==1~"#FFD700",
                                 Position==2~"#C0C0C0",
                                 Position==3~"#CD7F32"))
        colour1=cbind(x$col, colours)
        
        sheet_write(c2, ss="https://docs.google.com/spreadsheets/d/1h0S27-69mZgoPEyer9InFWZ0rN2PRSdlvqT7xS7Ob6k/edit#gid=0",
                    sheet="Scores")
        
        c2 %>% flextable() %>% 
            bg(j=2:(nrow(events)+2), bg=colour1) %>% 
            autofit() %>%
            htmltools_value()
        
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
