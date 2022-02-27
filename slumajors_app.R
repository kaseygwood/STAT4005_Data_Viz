library(tidyverse)

library(readxl)
df <- read_excel("~/Desktop/data visualization/STAT4005_Data_Viz/data/slu_graduates_17_21.xlsx")


df <- df %>% mutate(across(everything(),
                           .fns = ~replace(., . ==  "STATS" , "STAT")))

df_long <- df %>% pivot_longer(3:8, names_to = "type", values_to = "discipline")
df_major <- df_long %>% 
  filter(type == "major1" | type == "major2" | type == "major3")

df_stat <- df_major %>% filter(discipline == "STAT") 
df_statfull <- semi_join(df_long, df_stat, by = "adm_id") %>%
  filter(type == "major1" |
           type == "major2" | 
           type == "major3")

df_nostat <- df_statfull %>% filter(discipline != "STAT" &
                                      !is.na(discipline)) %>%
  group_by(discipline) %>%
  summarise(nstudent = n()) %>%
  mutate(discipline = fct_reorder(discipline, nstudent))
ggplot(data = df_nostat, aes(x = discipline, y = nstudent)) +
  geom_col() +
  coord_flip()

library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(radioButtons(inputId = "majorchoice", 
                             label = "Choose a Major", 
                             choices = c("MATH", "CS", "STAT"))),
    mainPanel(plotOutput(outputId = "majorplot")),
  )
)

server <- function(input, output){
  
  df_update <- reactive({
    df_stat <- df_major %>% filter(discipline == input$majorchoice)
    df_statfull <- semi_join(df_long, df_stat, by = "adm_id") %>%
      filter(type == "major1" |
               type == "major2" | 
               type == "major3")
    df_nostat <- df_statfull %>% filter(discipline != input$majorchoice &
                                          !is.na(discipline)) %>%
      group_by(discipline) %>%
      summarise(nstudent = n()) %>%
      mutate(discipline = fct_reorder(discipline, nstudent))
  })
    
    output$majorplot <- renderPlot({
      ggplot(data = df_update(), aes(x = discipline, y = nstudent)) +
        geom_col() +
        coord_flip()
    
  })
}

    
shinyApp(ui, server)

