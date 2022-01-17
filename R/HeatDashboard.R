
yaxis=statevar
facet=agevar
library(dplyr)
df_y <- excess_output %>% pluck(y ) %>% map(~(transpose(.x) %>% 
                                                     map(~(.x %>% bind_rows())) %>% .[c(datevar, "y")] %>% 
                                                     map(~(.x %>% gather(statevar, y))) %>% bind_cols() %>% 
                                                     set_names(c(statevar, datevar, "rm", "y")) %>% 
                                                     dplyr::select(-rm))) %>% bind_rows(.id = agevar)

df_pred <- excess_output %>% pluck(outcome) %>% map(~(transpose(.x) %>% 
                                                        map(~(.x %>% bind_rows())) %>% .[c(datevar, "pred")] %>% 
                                                        map(~(.x %>% gather(statevar, pred))) %>% bind_cols() %>% 
                                                        set_names(c(statevar, datevar, "rm", "pred")) %>% 
                                                        dplyr::select(-rm))) %>% bind_rows(.id = agevar)

df_oe <- df_y %>% left_join(df_pred, by = c(agevar, statevar, 
                                            datevar)) %>% mutate(oe = y/pred, year = year(get(datevar)), 
                                                                 week = week(get(datevar)), oe_fac = cut(oe, breaks = c(-Inf, 
                                                                                                                        0.5, 0.9,1, 1.1, 1.2, 1.3, 1.4, 1.5, Inf), labels = c("0.5", '0.9',"1.0", "1.1", 
                                                                                                                                                                              "1.2", "1.3", "1.4", "1.5", 
                                                                                                                                                                              ">1.5")), oe_fac_rev = factor(oe_fac, 
                                                                                                                                                                                                            levels = rev(levels(oe_fac))))

dates <- as.Date(unique(df_oe[[datevar]]))

states <- unique(df_oe[[statevar]])

age_groups <- unique(df_oe[[agevar]])

last.date <- max(dates)

last.date.format <- format(last.date, "%b %d, %Y")

ui.heat <- fluidPage(shiny::titlePanel(paste0("Data through ", 
                                              last.date.format)), shiny::sidebarLayout(shiny::sidebarPanel(shiny::selectInput(input = "set.states", 
                                                                                                                              label = "State:", choice = states, selected = c("NY"),  
                                                                                                                              multiple = T), shiny::selectInput(input = "set.ages", 
                                                                                                                                                                label = "Age group:", choice = age_groups, selected = c("25-44 years","45-64 years","65-74 years",        "75-84 years","85 years and older"), multiple = T), 
                                                                                                           shiny::sliderInput(input = "display.dates", label = "Earliest date to display", 
                                                                                                                              min = min(dates), max = dates[length(dates) - 2], 
                                                                                                                              step = 7, value = dates[length(dates) - round(length(dates)/5)])), 
                                                                                       shiny::mainPanel(shiny::plotOutput("plot"))))
server.heat <- function(input, output) {
  library(ggplot2)
  dates_states_ages_sel <- reactive({
    req(input$display.dates, input$set.states, input$set.ages)
    df_oe %>% filter(get(datevar) >= input$display.dates & 
                       get(statevar) %in% c(input$set.states) & get(agevar) %in% 
                       c(input$set.ages))
  })
  output$plot = renderPlot({
    ggplot(data = dates_states_ages_sel(), aes(x = factor(get(datevar)), 
                                               y = get(yaxis))) + geom_raster(aes(fill = oe_fac_rev), 
                                                                              interpolate = F) + scale_fill_manual(values = c(`>1.5` = "#67000d", 
                                                                                                                              `1.5` = "#a50f15", 
                                                                                                                              `1.4` = "#cb181d", 
                                                                                                                              `1.3` = "#ef3b2c", 
                                                                                                                              `1.2` = "#fb6a4a", 
                                                                                                                              `1.1` = "#fc9272", 
                                                                                                                              `1.0` = "#fcbba1", 
                                                                                                                              `0.9` = "#fee0d2", 
                                                                                                                              `0.5` = "#fff5f0")) + xlab("Time") + 
      labs(fill = "O/E Ratio") + scale_x_discrete(expand = c(0, 
                                                             0)) + scale_y_discrete(expand = c(0, 0)) + facet_grid(get(facet) ~ 
                                                                                                                     .) + theme_bw() + theme(axis.title.y = element_blank(), 
                                                                                                                                             axis.text.x = element_text(size = 7, vjust = 1, 
                                                                                                                                                                        hjust = 0, angle = 90))
  })
}