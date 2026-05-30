source("R/ui_helpers.R")
source("app.R")

# Load initial data to populate UI controls
# These will be updated reactively when the HQ level changes, 
# but this ensures the controls are populated on first render.
initial_prices <- load_prices(hq_level = 29)
initial_train_items <- train_items(hq_level = 29)

server <- function(input, output, session) {
  saved_train_cars <- reactiveVal(list())
  editing_train_car <- reactiveVal(NULL)

  prices <- reactive(load_prices(hq_level = input$hq_level))
  model_for <- function() build_value_model(prices())

  store_model <- reactive(model_for())
  item_model <- reactive(model_for())
  currency_model <- reactive(model_for())

  observeEvent(prices(), {
    updateSelectInput(session, "store", choices = sort(unique(prices()$store)), selected = input$store)
    updateSelectInput(session, "item", choices = item_choices(prices()), selected = input$item)
    currency_choices <- c("All currencies" = "__ALL__", sort(unique(prices()$curr[prices()$curr != "DIA"])))
    updateSelectInput(session, "anchor_currency", choices = currency_choices, selected = input$anchor_currency)
  }, ignoreInit = TRUE)

  observeEvent(session$clientData$url_search, {
    query <- parseQueryString(session$clientData$url_search)
    if (identical(query$view, "item") && !is.null(query$item) && query$item %in% prices()$item_canonical) {
      updateSelectInput(session, "item", selected = query$item)
      updateTabsetPanel(session, "main_tabs", selected = "Item View")
    }
    if (identical(query$view, "store") && !is.null(query$store) && query$store %in% prices()$store) {
      updateSelectInput(session, "store", selected = query$store)
      updateTabsetPanel(session, "main_tabs", selected = "Store View")
    }
    if (identical(query$view, "model")) {
      if (!is.null(query$currency) && query$currency %in% prices()$curr) {
        updateSelectInput(session, "anchor_currency", selected = query$currency)
      }
      updateTabsetPanel(session, "main_tabs", selected = "Currency Conversion Model")
    }
  }, ignoreInit = FALSE, once = TRUE)

  observeEvent(input$go_item, {
    if (input$go_item %in% prices()$item_canonical) {
      updateSelectInput(session, "item", selected = input$go_item)
      updateTabsetPanel(session, "main_tabs", selected = "Item View")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$go_store, {
    if (input$go_store %in% prices()$store) {
      updateSelectInput(session, "store", selected = input$go_store)
      updateTabsetPanel(session, "main_tabs", selected = "Store View")
    }
  }, ignoreInit = TRUE)

  store_currency <- reactive({
    first(prices()$curr[prices()$store == input$store])
  })

  output$store_currency_label <- renderText({
    curr <- store_currency()
    if (!length(curr) || is.na(curr)) {
      "Currency"
    } else {
      paste0(curr, " = ", currency_label(curr), " (currency)")
    }
  })

  output$store_rate <- renderText({
    rate <- store_model()$rates %>%
      filter(curr == store_currency()) %>%
      pull(dia_per_currency)
    curr <- store_currency()
    if (!length(rate) || is.na(rate) || !length(curr) || is.na(curr)) {
      "No rate"
    } else {
      paste0("1 ", curr, " = ", fmt_sig(rate, 3), " DIA")
    }
  })

  output$store_count <- renderText({
    n <- store_model()$valued %>%
      filter(store == input$store, !is.na(effective_dia_unit)) %>%
      nrow()
    format(n, big.mark = ",")
  })

  output$store_top <- renderText({
    x <- store_model()$valued %>%
      filter(store == input$store, !is.na(direct_value_vs_rate)) %>%
      collapse_visible_store_rows() %>%
      arrange(desc(direct_value_vs_rate), effective_dia_unit, flexibility_rank) %>%
      slice_head(n = 1)
    if (nrow(x) == 0) "No anchor" else x$item
  })

  collapse_visible_store_rows <- function(x) {
    x %>%
      group_by(row_id, store, item, price, curr, limit) %>%
      arrange(flexibility_rank, effective_dia_unit, .by_group = TRUE) %>%
      summarise(
        item_key = first(item_key),
        item_canonical = if_else(n_distinct(item_canonical) > 1, "Multiple choices", first(item_canonical)),
        item_link_target = first(item_canonical),
        qty = first(qty),
        comparable_qty = first(comparable_qty),
        comparable_unit = if_else(n_distinct(comparable_unit) > 1, "choice", first(comparable_unit)),
        anchor_group = first(anchor_group),
        flexibility_rank = min(flexibility_rank, na.rm = TRUE),
        dia_per_currency = first(dia_per_currency),
        direct_dia_unit = min(direct_dia_unit, na.rm = TRUE),
        direct_dia_sources = paste(sort(unique(direct_dia_sources[!is.na(direct_dia_sources)])), collapse = ", "),
        effective_dia_unit = min(effective_dia_unit, na.rm = TRUE),
        direct_value_vs_rate = max(direct_value_vs_rate, na.rm = TRUE),
        best_effective_dia_unit = min(best_effective_dia_unit, na.rm = TRUE),
        best_store = paste(sort(unique(best_store[!is.na(best_store)])), collapse = ", "),
        normal_effective_dia_unit = median(normal_effective_dia_unit, na.rm = TRUE),
        priority_score = max(priority_score, na.rm = TRUE),
        normal_value_ratio = max(normal_value_ratio, na.rm = TRUE),
        total_effective_dia = first(total_effective_dia),
        .groups = "drop"
      ) %>%
      mutate(across(
        c(direct_dia_unit, effective_dia_unit, direct_value_vs_rate, best_effective_dia_unit,
          normal_effective_dia_unit, priority_score, normal_value_ratio),
        ~ if_else(is.infinite(.x), NA_real_, .x)
      ))
  }

  collapse_visible_anchor_rows <- function(x) {
    x %>%
      group_by(row_id, store, item, price, curr, limit) %>%
      arrange(is_excluded, desc(selected_anchor), desc(observed_dia_per_currency), flexibility_rank, .by_group = TRUE) %>%
      summarise(
        item_key = first(item_key),
        qty = first(qty),
        comparable_qty = first(comparable_qty),
        comparable_unit = if_else(n_distinct(comparable_unit) > 1, "choice", first(comparable_unit)),
        direct_dia_sources = paste(sort(unique(direct_dia_sources[direct_dia_sources != "" & !is.na(direct_dia_sources)])), collapse = ", "),
        direct_dia_unit = safe_min(direct_dia_unit),
        observed_dia_per_currency = safe_max(observed_dia_per_currency),
        network_value_ratio = safe_max(network_value_ratio),
        network_weight = safe_max(network_weight),
        is_excluded = all(is_excluded),
        excluded_reason = first(excluded_reason),
        .groups = "drop"
      ) %>%
      mutate(across(
        c(direct_dia_unit, observed_dia_per_currency, network_value_ratio, network_weight),
        ~ if_else(is.infinite(.x), NA_real_, .x)
      ))
  }

  store_rows <- reactive({
    x <- store_model()$valued %>% filter(store == input$store)
    x <- x %>% filter(!is.na(effective_dia_unit))
    x <- collapse_visible_store_rows(x)
    x <- x %>% arrange(desc(normal_value_ratio), effective_dia_unit, flexibility_rank)
    x
  })

  output$store_table <- renderTable({
    price_col <- paste0("Price (", store_currency(), ")")
    dia_col <- dia_unit_col(store_currency())
    store_rows() %>%
      transmute(
        Item = item_link_cell(item, item_key, item_link_target),
        Qty = fmt_num(qty, 0),
        `Base Unit Qty` = base_unit_qty(comparable_qty, comparable_unit),
        !!price_col := fmt_num(price, 0),
        Limit = fmt_limit(limit, store),
        !!dia_col := bold_cell(fmt_num(effective_dia_unit, 2)),
        `Best DIA/unit` = fmt_num(best_effective_dia_unit, 2),
        `Best Store` = store_links_cell(best_store, input$store),
        `Normal DIA/unit` = fmt_num(normal_effective_dia_unit, 2),
        `Value vs normal` = deal_badge(normal_value_ratio)
      ) %>%
      head(80)
  }, sanitize.text.function = identity)

  item_rows <- reactive({
    key <- prices()$item_key[match(input$item, prices()$item_canonical)]
    item_model()$valued %>%
      filter(
        item_key == key |
          (key != "universal speed up hour" & item_key == "universal speed up hour" & str_detect(key, "speed up hour")) |
          (key %in% c("drone component level 1 equivalent", "drone component choice level 1 equivalent") &
             item_key %in% c("drone component level 1 equivalent", "drone component choice level 1 equivalent"))
      ) %>%
      group_by(network_item_key) %>%
      mutate(
        view_best_effective_dia_unit = min(effective_dia_unit, na.rm = TRUE),
        view_normal_effective_dia_unit = first(direct_dia_unit[!is.na(direct_dia_unit)]),
        view_priority_score = if_else(
          !is.na(effective_dia_unit) & is.finite(view_best_effective_dia_unit) & effective_dia_unit > 0,
          100 * view_best_effective_dia_unit / effective_dia_unit,
          NA_real_
        ),
        view_normal_value_ratio = if_else(
          !is.na(effective_dia_unit) & is.finite(view_normal_effective_dia_unit) & effective_dia_unit > 0,
          view_normal_effective_dia_unit / effective_dia_unit,
          NA_real_
        )
      ) %>%
      ungroup() %>%
      arrange(effective_dia_unit, flexibility_rank)
  })

  item_summary <- reactive({
    rows <- item_rows() %>%
      filter(!is.na(effective_dia_unit), is.finite(effective_dia_unit))
    if (nrow(rows) == 0) {
      return(tibble(
        best_dia = NA_real_,
        normal_dia = NA_real_,
        min_dia = NA_real_,
        max_dia = NA_real_,
        best_store = NA_character_
      ))
    }
    best_dia <- min(rows$effective_dia_unit, na.rm = TRUE)
    best_stores <- rows %>%
      filter(abs(effective_dia_unit - best_dia) < 1e-9) %>%
      pull(store) %>%
      unique() %>%
      sort()
    tibble(
      best_dia = best_dia,
      normal_dia = median(rows$view_normal_effective_dia_unit, na.rm = TRUE),
      min_dia = min(rows$effective_dia_unit, na.rm = TRUE),
      max_dia = max(rows$effective_dia_unit, na.rm = TRUE),
      best_store = paste(best_stores, collapse = ", ")
    )
  })

  output$item_best_store <- renderUI({
    stores <- item_summary()$best_store
    if (!length(stores) || is.na(stores) || stores == "") {
      HTML("No store")
    } else {
      HTML(store_links_cell(stores))
    }
  })

  output$item_best_dia <- renderText({
    fmt_sig(item_summary()$best_dia, 3)
  })

  output$item_normal_dia <- renderText({
    fmt_sig(item_summary()$normal_dia, 3)
  })

  output$item_dia_range <- renderText({
    summary <- item_summary()
    if (is.na(summary$min_dia) || is.na(summary$max_dia)) {
      ""
    } else {
      paste0(fmt_sig(summary$min_dia, 3), "-", fmt_sig(summary$max_dia, 3))
    }
  })

  output$item_table <- renderTable({
    item_rows() %>%
      transmute(
        Rank = row_number(),
        Store = store_link_cell(store),
        Item = item_cell(item, item_key),
        Qty = fmt_num(qty, 0),
        `Base Unit Qty` = base_unit_qty(comparable_qty, comparable_unit),
        Price = paste(fmt_num(price, 0), curr),
        Limit = fmt_limit(limit, store),
        `~DIA/unit` = bold_cell(fmt_num(effective_dia_unit, 2)),
        `Normal DIA/unit` = fmt_num(view_normal_effective_dia_unit, 2),
        `Value vs normal` = deal_badge(view_normal_value_ratio)
      )
  }, sanitize.text.function = identity)

  train_item_values <- reactive({
    items <- train_items(input$hq_level)
    model <- currency_model()
    item_values <- model$direct %>%
      select(item_key, dia_unit = direct_dia_unit)
    currency_values <- model$rates %>%
      select(curr, dia_per_currency)

    items %>%
      rowwise() %>%
      mutate(
        dia_each = case_when(
          value_kind == "flat" ~ flat_dia,
          value_kind == "currency" ~ comparable_qty * currency_values$dia_per_currency[match(currency, currency_values$curr)],
          value_kind == "item" ~ comparable_qty * item_values$dia_unit[match(item_key, item_values$item_key)],
          TRUE ~ NA_real_
        )
      ) %>%
      ungroup()
  })

  train_slot_input <- function(id) {
    value <- suppressWarnings(as.numeric(input[[paste0("train_", id)]]))
    if (length(value) == 0 || is.na(value)) {
      return(0)
    }
    value
  }

  clear_train_builder <- function(queue = 5) {
    ids <- train_items(input$hq_level)$id
    for (id in ids) {
      updateNumericInput(session, paste0("train_", id), value = 0)
    }
    updateNumericInput(session, "train_queue", value = queue)
    session$sendCustomMessage("updateTrainSlotColors", list())
  }

  load_train_builder <- function(car) {
    clear_train_builder(queue = car$queue)
    if (!is.null(car$counts) && length(car$counts)) {
      for (id in names(car$counts)) {
        updateNumericInput(session, paste0("train_", id), value = car$counts[[id]])
      }
    }
    session$sendCustomMessage("updateTrainSlotColors", list())
  }

  output$train_inputs <- renderUI({
    rows <- train_item_values()
    div(class = "train-grid",
        lapply(seq_len(nrow(rows)), function(i) {
          item <- rows[i, ]
          div(
            class = paste("train-item", paste0("train-item-", item$tier)),
            div(
              class = "train-qty-control train-slot-empty",
              numericInput(
                inputId = paste0("train_", item$id),
                label = HTML(train_item_cell(item$label, item$icon_item)),
                value = 0,
                min = 0,
                max = 6,
                step = 1
              )
            ),
            div(class = "train-item-value", paste0("Value: ~", fmt_dia_equiv(item$dia_each), " Diamonds"))
          )
        })
    )
  })

  train_rows <- reactive({
    train_item_values() %>%
      rowwise() %>%
      mutate(
        slot_count = train_slot_input(id),
        dia_total = slot_count * dia_each
      ) %>%
      ungroup()
  })

  train_slot_total <- reactive({
    sum(train_rows()$slot_count, na.rm = TRUE)
  })

  train_slot_state <- reactive({
    slots <- train_slot_total()
    case_when(
      slots > 6 ~ "over",
      slots == 6 ~ "ready",
      slots == 0 ~ "empty",
      TRUE ~ "under"
    )
  })

  output$train_slot_count <- renderText({
    fmt_num(train_slot_total(), 0)
  })

  output$train_total_dia <- renderText({
    paste0(fmt_dia_equiv(sum(train_rows()$dia_total, na.rm = TRUE)), " DIA")
  })

  train_queue_n <- reactive({
    q <- suppressWarnings(as.numeric(input$train_queue))
    q <- ifelse(is.na(q), 0, q)
    pmin(pmax(q, 0), 100)
  })

  train_total_value <- reactive({
    sum(train_rows()$dia_total, na.rm = TRUE)
  })

  train_selection_probability <- reactive({
    5 / max(train_queue_n() + 1, 5)
  })

  current_train_snapshot <- reactive({
    rows <- train_rows() %>%
      filter(slot_count > 0) %>%
      arrange(desc(tier == "top"), desc(dia_total), desc(dia_each))

    featured <- if (nrow(rows) > 0) rows[1, ] else NULL
    counts <- rows$slot_count
    names(counts) <- rows$id

    list(
      featured_label = if (is.null(featured)) "" else featured$label,
      featured_icon = if (is.null(featured)) "" else featured$icon_item,
      counts = as.list(counts),
      total_value = train_total_value(),
      queue = train_queue_n(),
      probability = train_selection_probability(),
      ev = train_selection_probability() * train_total_value()
    )
  })

  output$train_save_control <- renderUI({
    saved <- saved_train_cars()
    edit_index <- editing_train_car()
    slots <- train_slot_total()
    can_save <- slots == 6 && (!is.null(edit_index) || length(saved) < 4)
    save_note <- case_when(
      !is.null(edit_index) && slots == 6 ~ paste0("Ready to update Train Car ", edit_index, "."),
      !is.null(edit_index) ~ paste0("Editing Train Car ", edit_index, ". Load exactly 6 items to update."),
      length(saved) >= 4 ~ "Comparison is full. Clear saved cars to start over.",
      slots == 6 ~ "Ready to save this train car.",
      TRUE ~ paste0("Load exactly 6 items to save. Current total: ", fmt_num(slots, 0), ".")
    )

    save_label <- if (is.null(edit_index)) "Save Train Car" else paste0("Update Train Car ", edit_index)
    save_args <- list(inputId = "save_train_car", label = save_label, class = "btn-primary")
    if (!can_save) save_args$disabled <- "disabled"

    clear_args <- list(inputId = "clear_train_cars", label = "Clear All")
    if (length(saved) == 0) clear_args$disabled <- "disabled"

    div(
      class = "train-save-row",
      do.call(actionButton, save_args),
      do.call(actionButton, clear_args),
      span(class = "note", save_note)
    )
  })

  observeEvent(input$save_train_car, {
    if (train_slot_total() != 6) return()
    saved <- saved_train_cars()
    edit_index <- editing_train_car()
    if (is.null(edit_index) && length(saved) >= 4) return()
    snapshot <- current_train_snapshot()
    if (!is.null(edit_index) && edit_index >= 1 && edit_index <= length(saved)) {
      snapshot$name <- saved[[edit_index]]$name
      saved[[edit_index]] <- snapshot
      saved_train_cars(saved)
    } else {
      snapshot$name <- paste0("Train Car ", length(saved) + 1)
      saved_train_cars(append(saved, list(snapshot)))
    }
    editing_train_car(NULL)
    clear_train_builder()
  })

  observeEvent(input$clear_train_cars, {
    saved_train_cars(list())
    editing_train_car(NULL)
    clear_train_builder()
  })

  observeEvent(input$clear_train_car, {
    idx <- suppressWarnings(as.integer(input$clear_train_car))
    saved <- saved_train_cars()
    if (length(idx) == 0 || is.na(idx) || idx < 1 || idx > length(saved)) return()
    saved[[idx]] <- NULL
    if (length(saved)) {
      for (i in seq_along(saved)) {
        saved[[i]]$name <- paste0("Train Car ", i)
      }
    }
    current_edit <- editing_train_car()
    if (!is.null(current_edit)) {
      if (current_edit == idx) {
        editing_train_car(NULL)
        clear_train_builder()
      } else if (current_edit > idx) {
        editing_train_car(current_edit - 1)
      }
    }
    saved_train_cars(saved)
  })

  observeEvent(input$edit_train_car, {
    idx <- suppressWarnings(as.integer(input$edit_train_car))
    saved <- saved_train_cars()
    if (length(idx) == 0 || is.na(idx) || idx < 1 || idx > length(saved)) return()
    editing_train_car(idx)
    load_train_builder(saved[[idx]])
  })

  output$saved_train_cars <- renderUI({
    saved <- saved_train_cars()
    cards <- lapply(seq_len(4), function(i) {
      if (i > length(saved)) {
        return(div(class = "saved-train-card saved-train-placeholder", paste0("Train Car ", i)))
      }
      car <- saved[[i]]
      div(
        class = "saved-train-card",
        div(class = "saved-title", htmltools::htmlEscape(car$name)),
        div(class = "saved-row saved-row-featured",
            span(class = "saved-label", "Featured item"),
            span(class = "saved-value", HTML(train_item_cell(car$featured_label, car$featured_icon)))),
        div(class = "saved-row",
            span(class = "saved-label", "Total Item Value"),
            span(class = "saved-value", paste0(fmt_dia_compact(car$total_value), " Diamonds"))),
        div(class = "saved-row",
            span(class = "saved-label", "Queue Length"),
            span(class = "saved-value", fmt_num(car$queue, 0))),
        div(class = "saved-row",
            span(class = "saved-label", "Join EV"),
            span(class = "saved-value", paste0(fmt_dia_compact(car$ev), " Diamonds"))),
        div(
          class = "saved-train-actions",
          tags$button(
            type = "button",
            class = "btn btn-default btn-xs",
            onclick = sprintf("Shiny.setInputValue('edit_train_car', %s, {priority: 'event'});", i),
            "Edit"
          ),
          tags$span(" "),
          tags$button(
            type = "button",
            class = "btn btn-default btn-xs",
            onclick = sprintf("Shiny.setInputValue('clear_train_car', %s, {priority: 'event'});", i),
            "Clear"
          )
        )
      )
    })
    div(
      class = "saved-train-panel",
      h4("Saved Train Cars"),
      div(class = "saved-train-grid", cards)
    )
  })

  output$train_selection_probability <- renderText({
    paste0(fmt_num(100 * train_selection_probability(), 1), "%")
  })

  output$train_queue_ev <- renderText({
    paste0(fmt_dia_equiv(train_selection_probability() * train_total_value()), " DIA")
  })

  output$train_warning <- renderUI({
    slots <- train_slot_total()
    if (slots == 6) {
      div(class = "train-warning train-warning-ok", "Ready: this train car has exactly 6 item slots.")
    } else if (slots > 6) {
      div(class = "train-warning train-warning-over", paste0("Too many item slots. Train cars need exactly 6; current total: ", fmt_num(slots, 0), "."))
    } else {
      div(class = "train-warning train-warning-bad", paste0("Train cars need exactly 6 item slots. Current total: ", fmt_num(slots, 0), "."))
    }
  })

  output$train_table <- renderTable({
    rows <- train_rows() %>%
      filter(slot_count > 0) %>%
      arrange(desc(dia_total), desc(dia_each))
    if (nrow(rows) == 0) {
      return(tibble(Message = "Enter item counts below to calculate this train car."))
    }

    rows %>%
      transmute(
        Item = train_item_cell(label, icon_item),
        Count = fmt_num(slot_count, 0),
        `Diamond Equivalent` = fmt_dia_equiv(dia_each),
        `Total Diamond Value` = bold_cell(fmt_dia_equiv(dia_total))
      ) %>%
      bind_rows(tibble(
        Item = "<strong>Total Train Car Value</strong>",
        Count = "",
        `Diamond Equivalent` = "",
        `Total Diamond Value` = bold_cell(fmt_dia_equiv(train_total_value()))
      )) %>%
      bind_rows(tibble(
        Item = "<strong>Selection Probability</strong>",
        Count = "",
        `Diamond Equivalent` = "",
        `Total Diamond Value` = bold_cell(paste0(fmt_num(100 * train_selection_probability(), 1), "%"))
      )) %>%
      bind_rows(tibble(
        Item = "<strong>Queue Position EV</strong>",
        Count = "",
        `Diamond Equivalent` = "",
        `Total Diamond Value` = bold_cell(paste0(fmt_dia_equiv(train_selection_probability() * train_total_value()), " DIA"))
      ))
  }, sanitize.text.function = identity)

  output$rates_table <- renderTable({
    currency_model()$rates %>%
      arrange(dia_per_currency) %>%
      transmute(
        View = currency_anchor_link(curr),
        Currency = currency_cell(curr),
        `Network DIA/curr` = fmt_sig(dia_per_currency, 2),
        `Connected items` = if_else(is.na(anchor_items), "", as.character(anchor_items)),
        `Observed min` = fmt_sig(min_anchor, 2),
        `Observed median` = fmt_sig(median_anchor, 2),
        `Observed max` = fmt_sig(max_anchor, 2)
      )
  }, sanitize.text.function = identity)

  output$network_table <- renderTable({
    currency_model()$diagnostics %>%
      filter(direction != "Near network") %>%
      group_by(row_id, store, item, qty, price, curr, limit, direction) %>%
      summarise(
        item_key = first(item_key),
        network_value_ratio = safe_max(network_value_ratio),
        .groups = "drop"
      ) %>%
      arrange(desc(abs(log(network_value_ratio)))) %>%
      transmute(
        Direction = direction,
        Store = store_link_cell(store),
        Item = item_cell(item, item_key),
        Price = paste(fmt_num(price, 0), curr),
        Limit = fmt_limit(limit, store),
        `Value vs network` = deal_badge(network_value_ratio)
      ) %>%
      head(12)
  }, sanitize.text.function = identity)

  output$anchors_table <- renderTable({
    anchors <- currency_model()$anchor_candidates
    if (!is.null(input$anchor_currency) && input$anchor_currency != "__ALL__") {
      anchors <- anchors %>% filter(curr == input$anchor_currency)
    }
    anchors %>%
      collapse_visible_anchor_rows() %>%
      arrange(curr, desc(observed_dia_per_currency)) %>%
      transmute(
        Store = excluded_cell(store, is_excluded),
        Item = excluded_cell(item_cell(item, item_key), is_excluded),
        Qty = excluded_cell(fmt_num(qty, 0), is_excluded),
        `Base Unit Qty` = excluded_cell(base_unit_qty(comparable_qty, comparable_unit), is_excluded),
        Price = excluded_cell(paste(fmt_num(price, 0), curr), is_excluded),
        Limit = excluded_cell(fmt_limit(limit, store), is_excluded),
        `Model source` = excluded_cell(direct_dia_sources, is_excluded),
        `Network DIA / base unit` = excluded_cell(fmt_num(direct_dia_unit, 2), is_excluded),
        `DIA/curr` = excluded_cell(fmt_num(observed_dia_per_currency, 2), is_excluded),
        `Value vs network` = excluded_cell(deal_badge(network_value_ratio), is_excluded),
        Weight = excluded_cell(fmt_num(network_weight, 2), is_excluded)
      ) %>%
      head(400)
  }, sanitize.text.function = identity)

  output$raw_table <- renderTable({
    prices() %>%
      transmute(
        Store = store,
        Item = item_cell(item, item_key),
        Qty = fmt_num(qty, 0),
        `Base Unit Qty` = base_unit_qty(comparable_qty, comparable_unit),
        Price = paste(fmt_num(price, 0), curr),
        Limit = fmt_limit(limit, store),
        `Compared As` = item_canonical
      ) %>%
      head(300)
  }, sanitize.text.function = identity)
}