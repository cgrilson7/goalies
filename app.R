# goalies.R

library(readr)
library(dplyr)

goalies <- 
  # read_csv("https://moneypuck.com/moneypuck/playerData/seasonSummary/2022/regular/goalies.csv", col_names = FALSE) %>%
  # select(playerId = X1, season = X2, name = X3, team = X4, situation = X6, icetime = X8, xGoals = X9, goals = X10) %>%
  read_csv("https://moneypuck.com/moneypuck/playerData/seasonSummary/2022/regular/goalies.csv") %>%
  select(playerId, season, name, team, situation, icetime, xGoals, goals) %>%
  mutate(last_name = sub("^\\S+\\s+", '', name)) %>%
  mutate(last_name_plus_team = paste0(last_name, "\n", team)) %>%
  mutate(g60 = goals / (icetime/(60*60)),
         xg60 = xGoals / (icetime/(60*60)),
         gsax = xGoals - goals,
         gsax60 = gsax / (icetime/(60*60)))

teams <- unique(goalies$team) %>% sort()
names_to_ids <- goalies$playerId
names(names_to_ids) <- goalies$name

library(shiny)
library(ggplot2)

ui <- fluidPage(
  # title
  title = "Catalyst Data Science",
  # add favicon
  tags$head(tags$link(rel="icon", href="data:image/x-icon;base64,AAABAAEAEBAAAAEACABoBQAAFgAAACgAAAAQAAAAIAAAAAEACAAAAAAAAAEAAAAAAAAAAAAAAAEAAAAAAAAAAAAAv7+/ALCwsAChoaEAHBwcAHR0dADMzMwArq6uAJ+fnwCQkJAAKSkpANnZ2QAaGhoACwsLAMrKygCdnZ0Ajo6OAObm5gAYGBgAubm5AIyMjAAWFhYA1dXVAMbGxgCZmZkAIyMjAHt7ewAUFBQAbGxsAMTExAC1tbUAiIiIAODg4AASEhIA0dHRAMLCwgADAwMAW1tbAExMTAA9PT0AHx8fAHd3dwDPz88AEBAQAAEBAQA7OzsAk5OTANzc3ADNzc0ADg4OAGZmZgCvr68ASEhIAJGRkQAqKioA2traAAwMDADLy8sARkZGAPb29gCPj48AGRkZANjY2ABxcXEACgoKAMnJyQBiYmIAurq6AJycnAAmJiYAfn5+ABcXFwDHx8cAYGBgACQkJADU1NQAbW1tAAYGBgDFxcUAXl5eAE9PTwD///8AmJiYACIiIgB6enoA0tLSABMTEwAEBAQAw8PDAJaWlgDu7u4AICAgAAICAgCysrIA+/v7AJSUlADs7OwAHh4eAGdnZwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAALCwsLCwsUwoKKAAsLCwsLCwsLCw9ADowSAwAFQAsLCwsLFwAIQAYHVgUABVXLCwsLCwsQAAtQx1OXVsAOCwsLCxcAGEAGg5BBkgyABUsLCwsAAAEAEYWYBFVBQBTAFwsLCwZAA9RAkhECzsQAEoAXCxhACk3TjxDCQEuICUAYQAAWwAXUgcHEzUqJjMTADYAAFYAIlRGTA5fDkIfOQBhTQAMADlZAAADHD5PA04ARSQAUwAISwBKAAAeUEliAEcALDENABYjAAAnNDoAABsALCwARwBcMC8wXlpCAEoAJCwsXAASAAA/WQYQABkAXCwsLCxcACsoAAAAAFYALCwsLAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", type="image/x-icon")),

  fluidRow(width = 12, align = 'center',
           h1("NHL Goaltender Performance"),
           h2(tags$a("Source: MoneyPuck.com", href="https://moneypuck.com/goalies.htm", target="_blank"))
  ),
  fluidRow(align = 'center',
           uiOutput("select_rendered"),
           uiOutput("slider_rendered")
  ),
  fluidRow(column(width = 3, br()),
           column(width = 3, align = 'center', selectInput("player_highlight", "Highlight a Player", choices = names_to_ids, multiple = TRUE, selected = NULL)),
           column(width = 3, align = 'center', selectInput("team_highlight", "Highlight a Team", choices = c("", teams), multiple = FALSE, selected = "")),
           column(width = 3, br())),
  fluidRow(column(width = 6, 
                  align = 'center',
           h1("Performance (GA/60) vs. Expected (xGA/60)"),
           h3("How to Read This Chart"),
           h4("The vertical lines represent the average expected goals per 60 minutes (for chosen situation), and +/- one standard deviation around this average."),
           h4("The diagonal lines represent various levels of goaltender performance vs. their expected goals against:"),
           h4(strong("Purple - worse than expected", style = "color: #440154"), " (-1 s.d.)", "|",
              strong("Blue - performing as expected", style = "color: #31688e"), "|",
              strong("Green - better than expected", style = "color: #35b779"), " (+1 s.d.)"),
           plotOutput('plot_diagonal', height = "800px", width = "100%"
           #            , hover = hoverOpts(
           #   id = 'plot_hover',
           #   delay = 100,
           #   delayType = 'throttle'
           # )
           )),
           column(width = 6, align = 'center',
           h1("Goals Saved Above Expected (GSAX/60)"),
           h3("How to Read This Chart"),
           h4("The vertical line represents the average expected goals per 60 minutes (for chosen situation)."),
           h4("The horizontal lines represent various levels of goaltender performance vs. their expected goals against:"),
           h4(strong("Purple - worse than expected", style = "color: #440154"), " (-1 s.d.)", "|",
              strong("Blue - performing as expected", style = "color: #31688e"), "|",
              strong("Green - better than expected", style = "color: #35b779"), " (+1 s.d.)"),
           plotOutput('plot_gsax', height = "800px", width = "100%"
                      #            , hover = hoverOpts(
                      #   id = 'plot_hover',
                      #   delay = 100,
                      #   delayType = 'throttle'
                      # )
           )
           )
  )
)

server <- function(input, output, session) {
  
  output$select_rendered <- renderUI({
    tagList(
      tags$style(type = 'text/css', '#big_selector .irs-grid-text {font-size: 20px}'), 
      div(id = 'big_selector',
          selectInput('situation_filter', h3("Situation"), choices = c("all", "5on5", "4on5", "5on4", "other"), selected = "5on5", width = "20%")
      )#div close
    )#taglst close
  })
  
  output$slider_rendered <- renderUI({
    tagList(
      tags$style(type = 'text/css', '#big_slider .irs-grid-text {font-size: 20px}'), 
      div(id = 'big_slider',
          sliderInput('icetime_filter', h3("Minimum Icetime Percentile"), min = 0, max = 99.9, value = 40, post = "%", width = "80%")
      )#div close
    )#taglst close
  })

output$plot_diagonal <- renderPlot({
  
  req(input$situation_filter)
  
  goalies_situation_filtered <- goalies %>%
    dplyr::filter(situation == input$situation_filter)
  
  g60_avg      <- sum(goalies_situation_filtered$goals) / (sum(goalies_situation_filtered$icetime)/(60*60))
  xg60_avg     <- sum(goalies_situation_filtered$xGoals) / (sum(goalies_situation_filtered$icetime)/(60*60))
  g60_sd       <- sd(goalies_situation_filtered$g60, na.rm = T)
  xg60_sd      <- sd(goalies_situation_filtered$xg60, na.rm = T)
  gsax60_avg   <- sum(goalies_situation_filtered$gsax) / (sum(goalies_situation_filtered$icetime)/(60*60))
  gsax60_sd    <- sd(goalies_situation_filtered$gsax60, na.rm = T)
  
  req(input$icetime_filter)
  
  goalies_ongoal_filtered <- goalies_situation_filtered %>%
    dplyr::filter(icetime > quantile(icetime, (input$icetime_filter/100)) | playerId %in% as.integer(input$player_highlight) | team %in% as.character(input$team_highlight)) %>%
      left_join((tibble(playerId = as.integer(input$player_highlight)) %>% mutate(player_fill = "#ab8d2c", player_color = "#ab8d2c", player_fontface = "bold.italic", player_stroke = 2)), by = c("playerId" = "playerId")) %>%
      left_join((tibble(team = as.character(input$team_highlight)) %>% mutate(team_fill = "#ab8d2c", team_color = "#ab8d2c", team_fontface = "bold.italic", team_stroke = 2)), by = c("team" = "team")) %>%
      mutate(fill = coalesce(player_fill, team_fill, "grey10"),
             color = coalesce(player_color, team_color, "grey10"),
             fontface = coalesce(player_fontface, team_fontface, "plain"),
             stroke = coalesce(player_stroke, team_stroke, 1))
  
  xmin_calc =   floor(min(goalies_ongoal_filtered$xg60)*2)/2
  xmax_calc = ceiling(max(goalies_ongoal_filtered$xg60)*2)/2
  ymin_calc =    floor(min(goalies_ongoal_filtered$g60)*2)/2
  ymax_calc =  ceiling(max(goalies_ongoal_filtered$g60)*2)/2

  goalies_ongoal_filtered %>%
    ggplot(aes(
      x = xg60,
      y = g60,
      group = last_name_plus_team
    )) +
    coord_cartesian(xlim = c(xmin_calc, xmax_calc),
                    ylim = c(ymin_calc, ymax_calc)) +
    scale_x_continuous(breaks = round(c(seq(xmin_calc, xmax_calc, 0.5), xg60_avg - xg60_sd, xg60_avg, xg60_avg + xg60_sd), 2)) +
    scale_y_continuous(breaks = seq(ymin_calc, ymax_calc, 0.5)) +
    geom_abline(slope = 1, intercept = gsax60_sd,  color = "#440154", size = 2, linetype = 'dashed', alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0,  color = "#31688e", size = 2, alpha = 0.7) +
    geom_abline(slope = 1, intercept = -gsax60_sd,  color = "#35b779", size = 2, linetype = 'dashed', alpha = 0.7) +
    geom_vline(xintercept = xg60_avg - xg60_sd, color = "grey70", size = 2, linetype = 'dashed', alpha = 0.5) +
    geom_vline(xintercept = xg60_avg, color = "grey60", size = 2, alpha = 0.75) +
    geom_vline(xintercept = xg60_avg + xg60_sd, color = "grey70", size = 2, linetype = 'dashed', alpha = 0.5) +
    geom_point(aes(fill = fill, stroke = stroke), shape = 21, size = 5) +
    scale_fill_identity() +
    ggrepel::geom_text_repel(aes(label = last_name, color = color, fontface = fontface), size = 6, max.overlaps = 20) +
    scale_color_identity() +
    # annotate("polygon",
    #          x = c(xmin_calc, xg60_avg, xg60_avg, xmin_calc),
    #          y = c(xmin_calc, xg60_avg, ymin_calc, ymin_calc),
    #          fill = "#1f9e89", alpha = 0.1) +
    labs(x = "Expected Goals Against per 60",
         y = "Goals Against per 60",
         size = "Total Icetime (60 Minute Increments)") +
    theme(plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_text(color = "grey10", size = rel(1.8)),
          axis.title = element_text(color = "grey10", size = rel(1.8)),
          axis.line = element_blank(),
          panel.grid.major = element_line(color = "grey90"),
          panel.grid.minor = element_blank(),
          legend.text = element_text(color = "grey10", size = rel(1.5)),
          legend.background =  element_rect(fill = "white"),
          legend.title = element_text(color = "grey10", size = rel(1.8)),
          legend.key = element_rect(fill = "white"),
          legend.position = "top",
          title = element_text(color = "grey10"))
})

output$plot_gsax <- renderPlot({
  
  req(input$situation_filter)
  
  goalies_situation_filtered <- goalies %>%
    dplyr::filter(situation == input$situation_filter)
  
  g60_avg      <- sum(goalies_situation_filtered$goals) / (sum(goalies_situation_filtered$icetime)/(60*60))
  xg60_avg     <- sum(goalies_situation_filtered$xGoals) / (sum(goalies_situation_filtered$icetime)/(60*60))
  g60_sd       <- sd(goalies_situation_filtered$g60, na.rm = T)
  xg60_sd      <- sd(goalies_situation_filtered$xg60, na.rm = T)
  gsax60_avg   <- sum(goalies_situation_filtered$gsax) / (sum(goalies_situation_filtered$icetime)/(60*60))
  gsax60_sd    <- sd(goalies_situation_filtered$gsax60, na.rm = T)
  
  req(input$icetime_filter)
  
  goalies_ongoal_filtered <- goalies_situation_filtered %>%
    dplyr::filter(icetime > quantile(icetime, (input$icetime_filter/100)) | playerId %in% as.integer(input$player_highlight) | team %in% as.character(input$team_highlight)) %>%
    left_join((tibble(playerId = as.integer(input$player_highlight)) %>% mutate(player_fill = "#ab8d2c", player_color = "#ab8d2c", player_fontface = "bold.italic", player_stroke = 2)), by = c("playerId" = "playerId")) %>%
    left_join((tibble(team = as.character(input$team_highlight)) %>% mutate(team_fill = "#ab8d2c", team_color = "#ab8d2c", team_fontface = "bold.italic", team_stroke = 2)), by = c("team" = "team")) %>%
    mutate(fill = coalesce(player_fill, team_fill, "grey10"),
           color = coalesce(player_color, team_color, "grey10"),
           fontface = coalesce(player_fontface, team_fontface, "plain"),
           stroke = coalesce(player_stroke, team_stroke, 1))
  
  xmin_calc =   floor(min(goalies_ongoal_filtered$xg60)*2)/2
  xmax_calc = ceiling(max(goalies_ongoal_filtered$xg60)*2)/2
  ymin_calc =    floor(min(goalies_ongoal_filtered$gsax60)*2)/2
  ymax_calc =  ceiling(max(goalies_ongoal_filtered$gsax60)*2)/2
  
  goalies_ongoal_filtered %>%
    ggplot(aes(
      x = xg60,
      y = gsax60,
      group = last_name_plus_team
    )) +
    coord_cartesian(xlim = c(xmin_calc, xmax_calc),
                    ylim = c(ymin_calc, ymax_calc)) +
    scale_x_continuous(breaks = seq(xmin_calc, xmax_calc, 0.5)) +
    scale_y_continuous(breaks = seq(ymin_calc, ymax_calc, 0.5)) +
    geom_vline(xintercept = xg60_avg, color = "grey60", size = 2, alpha = 0.75) +
    geom_hline(yintercept = -gsax60_sd,  color = "#440154", size = 2, linetype = 'dashed', alpha = 0.7) +
    geom_hline(yintercept = 0,  color = "#31688e", size = 2, linetype = 'solid', alpha = 0.7) +
    geom_hline(yintercept = gsax60_sd,  color = "#35b779", size = 2, linetype = 'dashed', alpha = 0.7) +
    geom_point(aes(fill = fill, stroke = stroke), shape = 21, size = 5) +
    scale_fill_identity() +
    ggrepel::geom_text_repel(aes(label = last_name, color = color, fontface = fontface), size = 6, max.overlaps = 20) +
    scale_color_identity() +
    # annotate("polygon",
    #          x = c(xmin_calc, xg60_avg, xg60_avg, xmin_calc),
    #          y = c(xmin_calc, xg60_avg, ymin_calc, ymin_calc),
    #          fill = "#1f9e89", alpha = 0.1) +
  labs(x = "Expected Goals Against per 60",
       y = "Goals Saved Above Expected per 60",
       size = "Total Icetime (60 Minute Increments)") +
    theme(plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank(),
          axis.text = element_text(color = "grey10", size = rel(1.8)),
          axis.title = element_text(color = "grey10", size = rel(1.8)),
          axis.line = element_blank(),
          panel.grid.major = element_line(color = "grey90"),
          panel.grid.minor = element_blank(),
          legend.text = element_text(color = "grey10", size = rel(1.5)),
          legend.background =  element_rect(fill = "white"),
          legend.title = element_text(color = "grey10", size = rel(1.8)),
          legend.key = element_rect(fill = "white"),
          legend.position = "top",
          title = element_text(color = "grey10"))
})

}

shinyApp(ui, server)
