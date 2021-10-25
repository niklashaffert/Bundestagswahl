# this script containes the app

# settings ----------------------------------------------------------------

### functions
source("functions.R")

### data
first_tier <- read_rds("Data/first_tier.rds")
second_tier <- read_rds("Data/second_tier.rds")
seats_states <- read_rds("Data/seats_states.rds")
seats <- read_rds("Data/seats_BT_org.rds")
df_colors <- read_rds("Data/colors.rds")
df_cand <- read_rds("Data/candidates.rds")
cand_elected <- read_rds("Data/elected_candidates.rds")


# ui ----------------------------------------------------------------------
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      actionButton(
        inputId = "calc",
        label = "Berechnen",
        width = 130
      ),
      hr(),
      actionButton(inputId = "reset", "Zurücksetzen", width = 130),
      hr(),
      textOutput("text"),
    ),
    mainPanel(
      width = 10,
      tabsetPanel(
        tabPanel(
          title = "Info",
          br(),
          h1("Eine Applikation"),
          br(),
          p(
            "Mit dieser Applikation kann simuliert werden, wie sich verschiedene Wahlergebnisse bei der Bundestagswahl 2021 auf die Zusammensetzung des 20. Deutschen Bundestages auswirken würden. Dazu können die originalen Wahlergebnisse, sowohl die Erststimmen in allen 299 Wahlkreisen als auch die Zweitstimmen in den 16 Bundesländern, manuell verändert werden und nach einer Neuberechnung des Bundestages betrachtet werden, wie sich dessen Größe und Zusammensetzung verändern würden. Zusätzlich lässt sich anzeigen, wie sich dies auf mögliche Koalitionsoptionen auswirken würde und welche Abgeordneten zusätzlich in den Bundestag einziehen, beziehungsweise ihren Sitz verlieren würden."
          ),
          p(
            "Unter dem Reiter Erststimmen können die Erststimmenergebnisse in den Wahlkreisen verändert werden, dabei kann durch Klicken oder manuelle Eingabe für jede Partei die Anzahl der Stimmen verändert werden. Unter dem Reiter Zweitstimmen lassen sich auf die gleiche Weise die Zweitstimmen innerhalb der Bundesländer verändern. Durch das Klicken des Knopfes Berechnen lässt sich dann die Zusammensetzung des Bundestages basierend auf den veränderten Ergebnissen berechnen. Der Knopf Zurücksetzen ermöglicht es, dass die Ergebnisse wieder auf die originalen Wahlergebnisse zurückgesetzt werden. Unter den Reitern Sitzverteilung und Koalitionen finden sich die Zusammensetzung des Bundestages beziehungsweise ob politisch realistische Koalitionen eine Mehrheit haben. Die beiden letzten Reiter Neue Abgeordnete und Ausgeschiedene Abgeordnete zeigen welche Abgeordneten nach einer Neuverteilung im Vergleich zum originalen Bundestag hinzukommen oder ihren Sitz verlieren."
          ),
          p(
            "Alle verwendeten Daten stammen vom ",
            a("Bundeswahlleiter.", href = "https://www.bundeswahlleiter.de/bundeswahlleiter.html"),
            "Eine Erläuterung zum verwendeten Umrechnungsverfahren von Stimmen in Sitze kann hier gefunden werden. Die Ergebnisse der Bundestagswahl, Erst- und Zweitstimmen, können",
            a("hier", href = "https://www.bundeswahlleiter.de/dam/jcr/bf33c285-ee92-455a-a9c3-8d4e3a1ee4b4/btw21_sitzberechnung.pdf"),
            "gefunden werden. Eine Übersicht der Kandidierenden kann",
            a("hier", href = "https://www.bundeswahlleiter.de/bundestagswahlen/2021/wahlbewerber.html"),
            "gefunden werden."
          ),
          p(
            "Keine Garantie, dass die angezeigten Zusammensetzungen des Bundestages, die Größen der möglichen Koalitionen und die Veränderungen bei den Abgeordneten korrekt sind. Der Code für die Applikation kann hier () gefunden werden."
          )
        ),
        tabPanel(title = "Erstimmen", DTOutput(outputId = "first_tier")),
        # table for manipulating first tier
        tabPanel(title = "Zweitstimmen", DTOutput(outputId = "second_tier")),
        # table for manipulating second tier
        tabPanel(title = "Sitzverteilung",
                 plotOutput(outputId = "plot_seats", height = "500")),
        # plot for seat distribution
        tabPanel(title = "Koalitionen",
                 plotlyOutput(outputId = "plot_coal",
                              height = "500")),
        # plot for possible coalitions
        tabPanel(title = "Neue Abgeordnete", DTOutput(outputId = "new_mp")),
        # table for new mps
        tabPanel(title = "Ausgeschiedene Abgeordnete", DTOutput(outputId = "old_mp")) # table for mps no longer part of the Bundestag
      )
    )
  )
)


# server ------------------------------------------------------------------
server <- function(input, output, session) {
  ### data
  
  # first tier
  first_tier_org <- first_tier
  first_tier_react <- reactiveVal(first_tier)
  first_tier_rea <- reactiveValues(data = first_tier)
  first_tier_proxy <- dataTableProxy("first_tier")
  
  # second tier
  second_tier_org <- second_tier
  second_tier_react <- reactiveVal(second_tier)
  second_tier_rea <- reactiveValues(data = second_tier)
  second_tier_proxy <- dataTableProxy("second_tier")
  
  # seats
  seats_org <- seats
  
  # reset to original data
  observeEvent(input$reset, {
    # first tier
    first_tier <<- first_tier_org
    first_tier_rea$data <- first_tier_org
    replaceData(first_tier_proxy, first_tier_rea$data, resetPaging = FALSE)
    # second tier
    second_tier <<- second_tier_org
    second_tier_rea$data <- second_tier_org
    replaceData(second_tier_proxy, second_tier_rea$data, resetPaging = FALSE)
    # seats
    seats <<- seats_org
    # text
    output$text <-
      renderText({
        "Aktuell werden die ursprünglichen Wahlergebnisse verwendet."
      })
    # seat distribution
    output$plot_seats <- renderPlot({
      plot_parl(df = seats,
                df_seats_org = seats_org,
                df_colors = df_colors)
    })
    # coalitions
    output$plot_coal <- renderPlot({
      plot_coal(df = seats, df_colors = df_colors)
    })
    # new mps to NULL
    output$new_mp <- NULL
    # old mps to NULL
    output$old_mp <- NULL
  })
  
  # change first tier
  observeEvent(input$first_tier_cell_edit, {
    info <- input$first_tier_cell_edit
    
    info$value <- ifelse(info$value < 0 , "0", info$value)
    
    first_tier_rea$data[info$row, info$col] <-
      as.numeric(info$value)
    
    first_tier_rea$data <-
      first_tier_rea$data %>%
      mutate(votes = as.numeric(votes),
             org_votes = as.numeric(org_votes)) %>%
      group_by(con_id) %>%
      mutate(
        # votes for nonvoters as rest
        votes = ifelse(
          party == "Nichtwählende/Ungültige",
          sum(org_votes) -
            sum(votes[party != "Nichtwählende/Ungültige"]),
          votes
        ),
        
        # if non_voters < 0 restore original votes
        votes = ifelse(votes < 0, org_votes, votes),
        
        # if more votes distributed than eligible voters, votes back to original votes
        votes = case_when(sum(votes) > sum(org_votes) ~ org_votes, TRUE ~
                            votes),
        
        # vote share based on new votes
        vote_share = ifelse(
          party != "Nichtwählende/Ungültige",
          round(votes / sum(votes[party != "Nichtwählende/Ungültige"]) * 100, 2),
          0
        ),
        
        # difference between original and new votes
        diff = votes - org_votes,
        
        closeness = max(votes[party != "Nichtwählende/Ungültige"]) - Rfast::nth(
          x = votes[party != "Nichtwählende/Ungültige"],
          k = 2,
          descending = TRUE
        )
      ) %>%
      ungroup()
    
    replaceData(first_tier_proxy, first_tier_rea$data, resetPaging = FALSE)
    
    first_tier <<- first_tier_rea$data
    
  })
  
  # change second tier
  observeEvent(input$second_tier_cell_edit, {
    info <- input$second_tier_cell_edit
    
    info$value <- ifelse(info$value < 0 , "0", info$value)
    
    second_tier_rea$data[info$row, info$col] <-
      as.numeric(info$value)
    
    second_tier_rea$data <-
      second_tier_rea$data %>%
      mutate(votes = as.numeric(votes),
             org_votes = as.numeric(org_votes)) %>%
      group_by(state) %>%
      mutate(
        # votes for nonvoters as rest
        votes = ifelse(
          party == "Nichtwählende/Ungültige",
          sum(org_votes) -
            sum(votes[party != "Nichtwählende/Ungültige"]),
          votes
        ),
        
        # if non_voters < 0 restore original votes
        votes = ifelse(votes < 0, org_votes, votes),
        
        # if more votes distributed than eligible voters, votes back to original votes
        votes = case_when(sum(votes) > sum(org_votes) ~ org_votes, TRUE ~
                            votes),
        
        # vote share based on new votes
        vote_share = ifelse(
          party != "Nichtwählende/Ungültige",
          round(votes / sum(votes[party != "Nichtwählende/Ungültige"]) * 100, 2),
          0
        ),
        
        # difference between original and new votes
        diff = votes - org_votes
      ) %>%
      ungroup()
    
    replaceData(second_tier_proxy, second_tier_rea$data, resetPaging = FALSE)
    
    second_tier <<- second_tier_rea$data
    
  })
  
  ### output
  
  # original seat distribution graphic
  output$plot_seats <- renderPlot({
    plot_parl(df = seats,
              df_seats_org = seats_org,
              df_colors = df_colors)
  })
  
  # original coalition graphic
  output$plot_coal <- renderPlotly({
    plot_coal(df = seats, df_colors = df_colors)
  })
  
  # graphics after calculation
  observeEvent(input$calc,
               {
                 # calculate new Bundestag
                 seats <<- calc_seats(
                   first_tier = first_tier %>% filter(party != "Nichtwählende/Ungültige"),
                   second_tier = second_tier %>% filter(party != "Nichtwählende/Ungültige"),
                   seats_states = seats_states,
                   seats = 598
                 )
                 
                 # plot for seat distribution
                 output$plot_seats <- renderPlot({
                   plot_parl(df = seats,
                             df_seats_org = seats_org,
                             df_colors = df_colors)
                 })
                 
                 # plot for possible coalitions
                 output$plot_coal <- renderPlotly({
                   plot_coal(df = seats, df_colors = df_colors)
                 })
                 
                 # data for new mps after recalculation
                 output$new_mp <-
                   renderDT(
                     datatable(
                       data = find_mps(
                         first_tier = first_tier %>%
                           filter(party != "Nichtwählende/Ungültige"),
                         seats = seats,
                         df_cand = df_cand
                       ) %>%
                         anti_join(cand_elected),
                       filter = "top",
                       colnames = c(
                         "Titel" = "title",
                         "Name" = "name",
                         "Vorname" = "first_name",
                         "Partei" = "party",
                         "Geschlecht" = "gender",
                         "Geburtsjahr" = "year_birth",
                         "Wahlmodus" = "elected",
                         "Nummer" = "con_id",
                         "Wahlkreis" = "con_name",
                         "Land" = "state",
                         "Listenposition" = "list_pos"
                       ),
                       options = list(autoWidth = TRUE,
                                      columnDefs = list(list(
                                        visible = FALSE, targets = c(1, 8, 11)
                                      )))
                     )
                   )
                 
                 # data for mps losing their seat
                 output$old_mp <-
                   renderDT(
                     datatable(
                       data = cand_elected %>%
                         anti_join(
                           find_mps(
                             first_tier = first_tier %>%
                               filter(party != "Nichtwählende/Ungültige"),
                             seats = seats,
                             df_cand = df_cand
                           )
                         ),
                       filter = "top",
                       colnames = c(
                         "Titel" = "title",
                         "Name" = "name",
                         "Vorname" = "first_name",
                         "Partei" = "party",
                         "Geschlecht" = "gender",
                         "Geburtsjahr" = "year_birth",
                         "Wahlmodus" = "elected",
                         "Nummer" = "con_id",
                         "Wahlkreis" = "con_name",
                         "Land" = "state",
                         "Listenposition" = "list_pos"
                       ),
                       options = list(autoWidth = TRUE,
                                      columnDefs = list(list(
                                        visible = FALSE, targets = c(1, 8, 11)
                                      )))
                     )
                   )
                 
               })
  
  ### output
  # first tier to data table
  output$first_tier <-
    renderDT(
      datatable(
        data = first_tier_react(),
        selection = "single",
        filter = "top",
        editable = list(target = "cell", disable = list(columns = c(0:4, 6:9))),
        colnames = c(
          "Nummer" = "con_id",
          "Wahlkreis" = "con_name",
          "Land" = "state",
          "Partei" = "party",
          "Stimmen" = "votes",
          "Anteil" = "vote_share",
          "Enge" = "closeness",
          "Stimmen (original)" = "org_votes",
          "Differenz" = "diff"
        ),
        options = list(autoWidth = TRUE,
                       columnDefs = list(list(
                         visible = FALSE, targets = c(3, 8)
                       )))
      )
    )
  
  # second tier to data table
  output$second_tier <-
    renderDT(
      datatable(
        second_tier_react(),
        selection = "single",
        filter = "top",
        editable = list(target = "cell", disable = list(columns = c(0:2, 4:6))),
        colnames = c(
          "Land" = "state",
          "Partei" = "party",
          "Stimmen" = "votes",
          "Anteil" = "vote_share",
          "Stimmen (original)" = "org_votes",
          "Differenz" = "diff"
        ),
        options = list(autoWidth = TRUE,
                       columnDefs = list(list(
                         visible = FALSE, targets = 5
                       )))
      )
    )
  
  # text displaying change in votes and seats
  output$text <-
    renderText({
      "Aktuell werden die ursprünglichen Wahlergebnisse verwendet."
    })
  
  observeEvent(input$calc, {
    if (sum(seats$seats) == 736) {
      output$text <-
        renderText({
          sprintf(
            "Insgesamt wurden %i Stimmen verschoben, dadurch hat sich die Größe des Bundestages nicht verändert.",
            sum(first_tier$diff[first_tier$diff > 0]) +
              sum(second_tier$diff[second_tier$diff > 0])
          )
        })
    } else if (sum(seats$seats) > 736) {
      output$text <-
        renderText({
          sprintf(
            "Insgesamt wurden %i Stimmen verschoben, dadurch hat sich der Bundestag um %i Sitze vergrößert.",
            sum(first_tier$diff[first_tier$diff > 0]) +
              sum(second_tier$diff[second_tier$diff > 0]),
            abs(736 - sum(seats$seats))
          )
        })
    } else{
      output$text <-
        renderText({
          sprintf(
            "Insgesamt wurden %i Stimmen verschoben, dadurch hat sich der Bundestag um %i Sitze verkleinert.",
            sum(first_tier$diff[first_tier$diff > 0]) +
              sum(second_tier$diff[second_tier$diff > 0]),
            736 - sum(seats$seats)
          )
        })
    }
  })
  
}

shinyApp(ui, server)
