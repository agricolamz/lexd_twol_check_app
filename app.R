library(shiny)
library(tidyverse)

generate_without_twol <- function(lexd) {
    system(
        paste(
            "lexd",
            lexd,
            "> /tmp/new.lexd.att;",
            "hfst-txt2fst /tmp/new.lexd.att -o /tmp/new.lexd.hfst;",
            "hfst-fst2strings /tmp/new.lexd.hfst"
        ),
        intern = TRUE
    )
}

generate_with_twol <- function(lexd, twol) {
    system(
        paste(
            "lexd",
            lexd,
            "> /tmp/new.lexd.att;",
            "hfst-txt2fst /tmp/new.lexd.att -o /tmp/new.lexd.hfst;",
            "hfst-twolc",
            twol,
            "> /tmp/new.twol.hfst;",
            "hfst-compose-intersect /tmp/new.lexd.hfst /tmp/new.twol.hfst > /tmp/new.hfst;",
            "hfst-fst2strings /tmp/new.hfst"
        ),
        intern = TRUE
    )
}


ui <- fluidPage(titlePanel("Test your lexd and twol"),
                fluidRow(column(
                    3, wellPanel(
                        fileInput("lexd", "Choose a .lexd File",
                                  accept = c(".lexd")),
                        fileInput("twol", "Choose a .twol File",
                                  accept = c(".twol"))
                    )
                ),
                column(5,
                       DT::dataTableOutput("result"))))

server <- function(input, output, session) {
    output$result <- DT::renderDataTable({
        req(input$lexd)
        if(is.null(input$twol)){
        generate_without_twol(input$lexd$datapath) %>%
            str_split(pattern = ":", simplify = TRUE) %>% 
            as.data.frame() %>% 
            rename(form = V2,
                   analysis = V1) %>%
            DT::datatable(
                class = "compact",
                filter = "top",
                options = list(pageLength = 20, dom = 'tip')
            )
        } else {
            generate_with_twol(input$lexd$datapath,
                               input$twol$datapath) %>%
                str_split(pattern = ":") %>%
                tibble(form = map_chr(., 2),
                       analysis = map_chr(., 1)) %>%
                select(-1) %>%
                DT::datatable(
                    class = "compact",
                    filter = "top",
                    options = list(pageLength = 20, dom = 'tip')
                )
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
