# The Vien Invitational Draft App
# Author: Bradley Vien
# Date: 03/02/2024
# Version: 1.0
# Differnce from previous versions:
#   added button functionality
#   created dynamically generated ui based on user selected number of teams
#   added summary data from previous years
#   added more options to the data table output to make it more user friendly
#   added sidebar panel that scrolls separately from make panel


library(dplyr)
library(purrr)
library(shiny)
library(DT)
library(shinyWidgets)

# ### set global variables
# current_year <- 2023

### functions for manipulating data ############################################

# calculate counts for various outcomes (birdies, pars, double pars, etc.)
add_outcome_counts <- function(
        df,
        par = c(4, 3, 4, 5, 4, 4, 5, 3, 4, 4, 3, 4, 4, 3, 4, 4, 5, 5)
) {
    
    hole_data <- df %>% 
        dplyr::select(dplyr::contains(match = "Hole", vars = names(.)))
    
    score2par <- hole_data
    doublePars <- hole_data
    for (i in 1:nrow(hole_data)) {
        score2par[i, ] <- hole_data[i, ] - par
        doublePars[i, ] <- hole_data[i, ] - 2*par
    }
    
    pars_fun <- function(x) {
        if(is.na(x)) {x <- NA} else if (x == 0) {x <- TRUE} else {x <- FALSE}
    }
    birdies_fun <- function(x) {
        if(is.na(x)) {x <- NA} else if (x == -1) {x <- TRUE} else {x <- FALSE}
    }
    eagles_fun <- function(x) {
        if(is.na(x)) {x <- NA} else if (x < -1) {x <- TRUE} else {x <- FALSE}
    }
    bogies_fun <- function(x) {
        if(is.na(x)) {x <- NA} else if (x == 1) {x <- TRUE} else {x <- FALSE}
    }
    doubles_fun <- function(x) {
        if(is.na(x)) {x <- NA} else if (x >= 2) {x <- TRUE} else {x <- FALSE}
    }
    
    
    pars <- apply(score2par, c(1,2), pars_fun)
    bogies <- apply(score2par, c(1,2), bogies_fun)
    birdies <- apply(score2par, c(1,2), birdies_fun)
    eagles <- apply(score2par, c(1,2), eagles_fun)
    doubles <- apply(score2par, c(1,2), doubles_fun)
    double_pars <- apply(doublePars, c(1,2), pars_fun)
    
    Pars <- rowSums(pars)
    Birdies <- rowSums(birdies)
    Eagles <- rowSums(eagles)
    Bogies <- rowSums(bogies)
    Double_And_Up <- rowSums(doubles)
    Double_Pars <- rowSums(double_pars)
    
    df <- cbind.data.frame(df, Eagles, Birdies, Pars, Bogies, Double_And_Up, Double_Pars)
    
    return(df)
}
# calculate the average score for each player on a certain hole type
hole_avg <- function(.data, 
                     hole = 3, 
                     par = c(4, 3, 4, 5, 4, 4, 5, 3, 4, 4, 3, 4, 4, 3, 4, 4, 5, 5)) {
    new_data <- .data %>% 
        dplyr::select(c("Name", paste0("Hole_", which(par == hole))))
    players <- unique(new_data$Name)
    
    summary_list <- lapply(players, function(x, dat = new_data){
        y <- dat %>% dplyr::filter(Name == x) %>%
            dplyr::select(-Name) %>%
            unlist() %>%
            mean(na.rm = T)
        df <- tibble(Name = x,
                     avg = y)
        
    })
    
    summary_df <- do.call(rbind.data.frame, summary_list)
    colnames(summary_df)[[2]] <- paste0("Par", hole, "_Avg")
    return(summary_df)
}
# calculate round averages based on data after 2016
averages <- function(all_data, round_digits = 2, cut_year = 2015) {
    summary_data <- all_data %>% 
        dplyr::filter(year > cut_year) %>%
        dplyr::select(-c("year")) %>% 
        dplyr::group_by(Name) %>%
        dplyr::summarise_all(mean, na.rm = T) %>%
        dplyr::mutate(dplyr::across(-c("Name"), round, digits = round_digits))
    colnames(summary_data)[2:ncol(summary_data)] <- 
        paste0(colnames(summary_data)[2:ncol(summary_data)], "_Avg")
    
    rounds_played <- all_data %>% dplyr::select(-c("year")) %>% 
        dplyr::group_by(Name) %>%
        dplyr::summarise(n = n())
    colnames(rounds_played)[[2]] <- "Rounds"
    
    hole_type_summary <- lapply(3:5, hole_avg, .data = all_data) %>%
        purrr::reduce(full_join, by = "Name") %>%
        dplyr::mutate(dplyr::across(-c("Name"), round, digits = round_digits))
    
    averages_df <- list(rounds_played, summary_data, hole_type_summary) %>%
        purrr::reduce(full_join, by = "Name")
    
    averages_df
}

### load data for the app ######################################################

# read in the .csv files, add a year column based upon the file name, and 
# combine into a dataframe, and then add the outcome counts

df <- paste0("www/data/", base::list.files(pattern = "*.csv", path = "www/data")) %>%
    rlang::set_names(nm = substr(sub(pattern = ".*/", "", .), 1, 4)) %>%
    purrr::map_dfr(read.csv, .id = "year") %>% # read .csv and create year column
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::select(-c("In", "Out")) %>%
    add_outcome_counts()

### set global variables
current_year <- max(df$year)

# setwd("C:/Users/bradl/Desktop/Vien Invitational/Data/Seed Round")
# df <- base::list.files(pattern = "*.csv") %>%
#     rlang::set_names(nm = substr(., 1, 4)) %>%
#     purrr::map_dfr(read.csv, .id = "year") %>% # read .csv and create year column
#     dplyr::mutate(year = as.numeric(year)) %>%
#     dplyr::select(-c("In", "Out")) %>%
#     add_outcome_counts()

my_summary_df <- averages(df)

draft_data <- df %>% filter(year == current_year)
draft_data <- draft_data %>%
    slice(order(draft_data$Total, draft_data$Hole_16, draft_data$Hole_3,
                draft_data$Hole_15, draft_data$Hole_2, draft_data$Hole_17,
                draft_data$Hole_4, draft_data$Hole_18, draft_data$Hole_7,
                draft_data$Hole_13, draft_data$Hole_5, draft_data$Hole_14,
                draft_data$Hole_1, draft_data$Hole_10, draft_data$Hole_8,
                draft_data$Hole_12, draft_data$Hole_6, draft_data$Hole_11,
                draft_data$Hole_9)) %>%
    left_join(my_summary_df, by = "Name") %>%
    select(-contains(match = "Hole", vars = names(.))) %>%
    select(-year)

### modules for the shiny app ##################################################

receiver_ui <- function(id, class) {
    ns <- NS(id)
    fluidRow(
        column(width = 12,
               h4(HTML(paste("<b>Team", sub("row","", x = id), "</b>"))),
               actionButton(ns("add"), 
                            label = HTML("<b>Draft Player</b>"),
                            icon("arrow-right"),
                            style = "background-color: red"),
               # actionButton(ns("add_all"), 
               #              label = NULL,
               #              icon("angle-double-right")),
               actionButton(ns("remove"),
                            label = "Remove Player",
                            icon("angle-left"),
                            style="float:right"),
               # actionButton(ns("remove_all"),
               #              label = NULL,
               #              icon("angle-double-left")),
               # p(paste("Team", sub("row","", x = id)))
               # p(HTML(paste("<b>Team", sub("row","", x = id), "</b>")))
        ),
        column(width = 12,
               offset = 0, 
               # h3(paste("Team", sub("row","", x = id))),
               dataTableOutput(ns("sink_table"))),
        class = class
    )
}

receiver_server <- function(input, output, session, selected_rows, full_page, blueprint) {
    ## data_exch contains 2 data.frames:
    ## send: the data.frame which should be sent back to the source
    ## receive: the data which should be added to this display
    data_exch <- reactiveValues(send    = blueprint,
                                receive = blueprint,
                                team = blueprint)
    
    ## trigger_delete is used to signal the source to delete the rows which just were sent
    trigger_delete <- reactiveValues(trigger = NULL, all = FALSE)
    
    ## render the table and remove .original_order, which is used to keep always the same order
    output$sink_table <- renderDataTable({
        dat <- data_exch$receive
        dat$.original_order <- NULL
        dat <- dat %>% select(c("Name", "Total", "Total_Avg"))
        DT::datatable(dat, 
                      rownames = FALSE, 
                      options = list(info = FALSE,
                                     paging = FALSE,
                                     searching = FALSE,
                                     scrollX = TRUE
                                     # columnDefs = list(list(
                                     #     visible=FALSE, 
                                     #     targets= 2:20))
                      ))
    })
    
    ## helper function to move selected rows from this display back 
    ## to the source via data_exch
    shift_rows <- function(selector) {
        data_exch$send <- data_exch$receive[selector, , drop = FALSE]
        data_exch$receive <- data_exch$receive[-selector, , drop = FALSE]
    }
    
    ## helper function to add the relevant rows
    add_rows <- function(all) {
        rel_rows <- if(all) req(full_page()) else req(selected_rows())
        data_exch$receive <- rbind(data_exch$receive, rel_rows)
        # data_exch$receive <- data_exch$receive[order(data_exch$receive$.original_order), ]
        ## trigger delete, such that the rows are deleted from the source
        old_value <- trigger_delete$trigger
        trigger_delete$trigger <- ifelse(is.null(old_value), 0, old_value) + 1
        trigger_delete$all <- all
    }
    
    observeEvent(input$add, {
        add_rows(FALSE)
    })
    
    observeEvent(input$add_all, {
        add_rows(TRUE)
    })
    
    observeEvent(input$remove, {
        shift_rows(req(input$sink_table_rows_selected))
    })
    
    observeEvent(input$remove_all, {
        shift_rows(req(input$sink_table_rows_current))
    })
    
    ## return the send reactive to signal the main app which rows to add back
    ## and the delete trigger to remove rows
    list(send   = reactive(data_exch$send),
         delete = trigger_delete,
         team = reactive(data_exch$receive))
}


### ui #########################################################################

ui <- fluidPage(
    tags$head(tags$style(HTML(".odd {background: #DDEBF7;}",
                              ".even {background: #BDD7EE;}",
                              ".btn-default {min-width:38.25px;}",
                              ".row {padding-top: 10px;padding-bottom: 10px;}",
                              ".dt-button.buttons-columnVisibility {background: black !important;
                              color: white !important; opacity: 0.5;}",
                              ".dt-button.buttons-columnVisibility.active {
                              background: green !important;
                              color: white !important;
                              opacity: 1;}"))),
    
    sidebarLayout(
        position = "right",
        # title = "side bar",
        sidebarPanel(
            width = 3,
            # NOTE::: width is manually set to be less than the allocated 4/12 = .3333
            style = "height: 95vh; overflow-y: auto;
                     position: fixed; width: 24%;",
            
            # add below to make the side bar panel scrollable
            # style = "height: 95vh; overflow-y: auto;",
            
            # add below to fix the side panel as the main panels scrolls down
            # style = "position:fixed; width:inherit;"
            
            # Use below in case of a 50 pixel Header!
            # style = "height: calc(100vh - 50px); overflow-y: auto;",
            # not sure the diff between height and max-height
            # style = "max-height: calc(100vh - 50px); overflow-y: auto;",
            
            
            fluidRow(
                column(width = 6, textInput(inputId = "file_name",
                                            label = "Draft name:", 
                                            value = "my draft")),
                column(width = 6, pickerInput(inputId = "num_teams", 
                                              label = "Number of teams",
                                              choices = 2:20)),
                column(width = 12, actionButton("add_table", 
                                                HTML("<b>Start Draft</b>"),
                                                style = "background-color: blue;
                                                color: white")),
                
            ),# end fluid row
            fluidRow(
                column(width = 12, div(id = "container"))
            ), # end fluid row
            fluidRow(
                column(width = 12, downloadButton(outputId = "downloadData", 
                                                  label = HTML("<b>Download Teams</b>"),
                                                  style = "background-color: blue;
                                                  color: white"))
                
            ) # end fluid row
        ), # end sidebar panel
        mainPanel(
            width = 9,
            # style = "position:fixed;width:inherit;",
            fluidRow(
                column(width = 12, dataTableOutput("source_table")),
            ), 
            fluidRow(
                column(width = 6, verbatimTextOutput("handlebars")),
                column(width = 6, verbatimTextOutput("num_teams"),
                       br(),
                       verbatimTextOutput("Teams"))
            )
        )# end main panel
    )# end sidebar layout
    
) # end fluid page

### Server #####################################################################

server <- function(input, output, session) {
    orig_data <- draft_data
    orig_data$.original_order <- seq(1, NROW(orig_data), 1)
    
    
    my_data <- reactiveVal(orig_data)
    
    handlers <- reactiveVal(list())
    
    n_teams <- reactive({
        n <- req(input$num_teams)
        n
    })
    
    selected_rows <- reactive({
        my_data()[req(input$source_table_rows_selected), , drop = FALSE]
    })
    
    all_rows <- reactive({
        my_data()[req(input$source_table_rows_current), , drop = FALSE]
    })
    
    
    observeEvent(input$add_table, {
        
        # add "if" so the button only works one time
        if (input$add_table < 2) { 
            lapply(1:n_teams(), function(x){
                old_handles <- handlers()
                n <- length(old_handles) + 1
                uid <- paste0("row", n)
                insertUI("#container", ui = receiver_ui(uid, ifelse(n %% 2, "odd", "even")))
                new_handle <- callModule(
                    receiver_server,
                    uid,
                    selected_rows = selected_rows,
                    full_page = all_rows,
                    ## select 0 rows data.frame to get the structure
                    blueprint = orig_data[0, ])
                
                handlers(c(old_handles, setNames(list(new_handle), uid)))
                # handlers(append(old_handles, setNames(list(new_handle), uid)))
                
                observeEvent(handlers()[[uid]]$delete$trigger, {
                    if (handlers()[[uid]]$delete$all) {
                        selection <- req(input$source_table_rows_current)
                    } else {
                        selection <- req(input$source_table_rows_selected)
                    }
                    my_data(my_data()[-selection, , drop = FALSE])
                })
                
                observe({
                    req(NROW(handlers()[[uid]]$send()) > 0)
                    dat <- rbind(isolate(my_data()), handlers()[[uid]]$send())
                    my_data(dat[order(dat$.original_order), ])
                })
                
            }) # end lapply
        } # end if
        
        
        output$num_teams <- renderPrint(input$add_table)
        output$handlebars <- renderPrint(handlers())
        
    }) # end observe event add_table
    
    
    teams_df <- reactive({
        if (input$add_table > 0) {
            # handlers()$row1$send()
            
            df_list<- lapply(1:n_teams(), function(x){
                team_name <- paste0("Team_", x)
                uid <- paste0("row", x)
                df <- data.frame(handlers()[[uid]]$team()[, 1])
                colnames(df) <- team_name
                df
            })
            
            # df_teams <- do.call("cbind.fill", df_list)
            df_teams <- df_list %>%
                purrr::imap(~setNames(.x, .y)) %>%
                purrr::map(tibble::rownames_to_column) %>%
                purrr::reduce(full_join, by = "rowname") %>%
                dplyr::select(-c("rowname"))
            colnames(df_teams) <- paste0("Team_", 1:n_teams())
            df_teams
        }
    })
    
    output$Teams <- renderPrint({
        if (input$add_table > 0) {
            teams_df()
        } else {"ho"}
        # input$`row1-sink_table_rows_all`
        
    })
    
    output$source_table <- renderDataTable({
        dat <- my_data()
        dat$.original_order <- NULL
        DT::datatable(dat, 
                      # caption = 'Players',
                      rownames = FALSE,
                      extensions = c('FixedColumns', 'Buttons'),
                      options = list(scrollX = TRUE,
                                     autoWidth = TRUE,
                                     # columnDefs = list(list(width = '75px', targets = "_all"))))
                                     columnDefs = list(list(width = '125px', targets = c(0))),
                                     fixedColumns = list(leftColumns = 1),
                                     dom = 'Bfrtip',
                                     buttons = list(list(extend = 'colvis', columns = 2:(ncol(dat) - 1)))
                      ) # end optoins list
        ) # end datatable 
    })
    
    output$downloadData <- downloadHandler(
        filename = function(name = input$file_name) {
            paste0(name, ".csv")
        },
        content = function(file) {
            write.csv(teams_df(), file)
        }
    )
    
} # end server function


shinyApp(ui, server)