source("R/ui_helpers.R")
source("app.R")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = paste0("styles.css?v=", icon_cache_bust)),
    tags$script(src = paste0("train.js?v=", icon_cache_bust))
  ),
  div(class = "app-shell",
    div(class = "title-row",
      tags$a(
        class = "app-logo-link",
        href = "https://analyticsff.shinyapps.io/last-war-store-guide/",
        title = "Last War Store Guide home",
        tags$img(
          class = "app-logo",
          src = paste0("icons/last-war-logo.png?v=", icon_cache_bust),
          alt = "Last War"
        )
      ),
      div(class = "title-copy",
        h1("Last War Store Guide"),
        div(class = "subtitle",
            "Compare store listings after translating each currency into diamond-equivalent value. The conversion model uses overlapping items and an adjustable exchange-rate strictness setting, so weak deals do not dominate the exchange rate."),
        div(class = "byline", "Compliments of [tAf]TheMathNinja [Server 2143]. DM with issues or suggestions."),
        div(class = "build-stamp", app_build_label)
      )
    ),
    div(class = "control-card hq-card",
      sliderInput("hq_level", "HQ Level", min = 20, max = 30, value = 29, step = 1),
      div(class = "note",
        "This currently only affects the reported value of buying loose resources directly in the Diamond Storefront, which is usually a bad deal. Resource chest multipliers use fixed tier ratios; unentered HQ levels fall back to HQ 29 values.")
    ),

    tabsetPanel(id = "main_tabs",

      #####################################################################################
      # Store View tab section
      # Allows user to view the items available in the selected store, with prices 
      # converted to DIA equivalents using the network model. The user can select which 
      # store to view, and see summary metrics for that store at the top of the page.
      tabPanel("Store View",
        fluidRow(
          column(3, 
            div(
              class = "control-card",
              selectInput("store", "Store", choices = sort(unique(initial_prices$store))),
              div(
                class = "note",
                "Values come from the network conversion model, which fits item and currency values from all store relationships together."
              )
            )
          ),
          column(9,
                fluidRow(
                  column(4, 
                    metric_box(
                      label = textOutput("store_currency_label", inline = TRUE),
                      value = textOutput("store_rate", inline = TRUE),
                      additional_class = "currency-name"
                    )
                  ),
                  column(4, 
                    metric_box("Converted listings",
                      textOutput("store_count", inline = TRUE)
                    )
                  ),
                  column(4, 
                    metric_box("Top item", 
                      textOutput("store_top_item", inline = TRUE)
                    ), 
                  )
                ),
                tableOutput("store_table")
          )
        )),

      #####################################################################################
      # Item View tab section
      # Allows user to view the prices for a given item across all stores, converted to 
      # DIA equivalents using the network model. User can select items to view, and see 
      # summary metrics for that item across stores.
      tabPanel("Item View",

        fluidRow(
          column(3, div(class = "control-card",
                        selectInput("item", "Item", choices = item_choices(initial_prices)),
                        div(class = "note",
                            "Universal/flexible items appear in related item views when they can satisfy the same need.")
          )),
          column(9,
            fluidRow(
              column(3,
                div(class = "metric", div(class = "label", "Best Store"), div(class = "value", uiOutput("item_best_store", inline = TRUE))))
                metric_box("Best Store",
                  uiOutput("item_best_store", inline = TRUE)
                ),
              column(3, 
                metric_box("Best DIA/unit",
                  textOutput("item_best_dia", inline = TRUE)
                ),
              column(3, 
                metric_box("Normal DIA/unit",
                  textOutput("item_normal_dia", inline = TRUE)
                )),
              column(3, 
                metric_box("DIA/unit range",
                  textOutput("item_dia_range", inline = TRUE)
                ))
              ),
              tableOutput("item_table")
            )
          )
        )
      ),

      #####################################################################################
      # Train Calculator tab section
      # Allows user to input the contents of a train car and see the expected value of that
      # train car in DIA equivalents, along with the probability of it being selected and 
      # its expected value in the selection queue.
      tabPanel("Train Calculator",
        div(class = "control-card note",
            "Enter how many train slots contain each reward. A train car should total exactly 6 item slots.",
            tags$br(),
            "This tool cannot suggest the best train car for maximizing in-game power; it only shows equivalent resource cost for a given train car across in-game stores."
        ),
        fluidRow(
          column(3, 
            metric_box("Items Loaded",
              textOutput("train_slot_count", inline = TRUE)
            ),
          column(3, 
            metric_box("Train car value",
              textOutput("train_total_dia", inline = TRUE)
            )),
          column(3, 
            metric_box("Selection probability",
              textOutput("train_selection_probability", inline = TRUE)
            )),
          column(3, 
            metric_box("Queue position EV",
              textOutput("train_queue_ev", inline = TRUE)
            ))
          ),
          uiOutput("train_warning"),
          uiOutput("train_save_control"),
          fluidRow(
            column(6,
                    div(class = "control-card hq-card",
                        numericInput("train_queue", "Commanders in Queue", value = 5, min = 0, max = 100, step = 1),
                        div(class = "note", "Already waiting, before you join.")
                    ),
                    tableOutput("train_table")),
            column(6, uiOutput("saved_train_cars"))
          ),
          uiOutput("train_inputs")
        )
      ),

      #####################################################################################
      # Currency Conversion Model tab section
      # Shows the inferred exchange rates from the network model, along with the
      # observations that inform the model and diagnostics on how well the model fits those
      # observations. User can filter the observations by currency to see how the model 
      # fits each currency's relationships.
      tabPanel("Currency Conversion Model",
        h3("Inferred Exchange Rates"),
        tableOutput("rates_table"),
        div(class = "model-explainer",
            strong("How the network model works"),
            div("The model estimates item values and currency exchange rates together from all connected store listings, with DIA fixed at 1."),
            div("Each listing contributes an observation: item value is expected to equal listed base-unit price times currency value."),
            div("Observation weight reflects available base-unit volume, softened with a square root and capped so high-limit rows matter more without dominating."),
            div("Rows far above or below the fitted network remain visible as diagnostics; they are not removed before the first fit.")),
        div(class = "filter-bar",
            selectInput("anchor_currency", "Observation currency",
                        choices = c("All currencies" = "__ALL__", sort(unique(initial_prices$curr[initial_prices$curr != "DIA"]))))),
        h3("Network Observations"),
        tableOutput("anchors_table"),
        h3("Network Diagnostics"),
        tableOutput("network_table")
      ),

      #####################################################################################
      # Raw Data tab section
      # Shows the raw source data table with all the observed listings and prices that inform
      # the model. This is included for transparency and to allow users to explore the raw data
      # themselves if they want to.
      tabPanel("All Store Listings",
        div(class = "control-card note",
            "This is the normalized source table from the workbook. Item keys smooth out a few spelling and naming variants so equivalent goods compare together."),
        tableOutput("raw_table")
      )
    )
  )
)