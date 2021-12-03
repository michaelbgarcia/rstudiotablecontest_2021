library(tidyverse)
library(glue)
library(gt)

source("functions.R")
source("data.R")

legend_df = bind_rows(
  ash_list,
  bangalore_list,
  bloodhound_list,
  caustic_list,
  crypto_list,
  fuse_list,
  gibraltar_list,
  horizon_list,
  lifeline_list,
  loba_list,
  mirage_list,
  octane_list,
  pathfinder_list,
  rampart_list,
  revenant_list,
  seer_list,
  valkyrie_list,
  wattson_list,
  wraith_list
)

gt_legend = function(df = legend_df, nrows = NULL) {
  if (!is.null(nrows)) {
    df = legend_df %>% slice(1:nrows)
    
  } else {
    df = legend_df
  }
  
  df %>%
    select(-type) %>%
    tibble::add_column(icon_place = "", .before = 1) %>%
    gt() %>%
    cols_hide(columns = c(ends_with("desc"))) %>%
    cols_hide(columns = c(cooldown, charge)) %>%
    tab_header(title = html(local_image("apex_data/logo.png", height = 120))) %>%
    tab_spanner(label = "Abilities",
                columns = c(passive, tactical, ultimate)) %>%
    gt_local_icon_banner(columns = icon_place, height = 80) %>%
    gt_local_icon_abilities(columns = passive,
                            type = "passive",
                            height = 60) %>%
    gt_local_icon_abilities(columns = tactical,
                            type = "tactical",
                            height = 60) %>%
    gt_local_icon_abilities(columns = ultimate,
                            type = "ultimate",
                            height = 60) %>%
    gt_merge_stack_name(col1 = name, col2 = alias, col3 = season) %>%
    cols_label(icon_place = "", name = "", season = "",
               cooldown = html("COOLDOWN</br>(SEC)"),
               charge = html("CHARGE</br>(MIN)")) %>%
    tab_source_note(
      source_note = md("All information presented, including icons and descriptions, is from **apexlegends.fandom.com**")
    ) %>%
    tab_source_note(
      source_note = "Apex Legends content and materials are the intellectual property of their respective owners."
    ) %>%
    tab_source_note(
      source_note = html(
        glue(
        "</br>{local_image(filename = glue(\"apex_data/apex-respawn-white-logo.png\"), height = 40)}")
      )
    ) %>%
    tab_source_note(
      source_note = md("&copy; 2021 Electronic Arts Inc.")
    ) %>%
    tab_options(
      data_row.padding = px(5),
      row.striping.background_color = "#dfdcd4",
      heading.background.color = "#952F36",
      heading.title.font.size = 30,
      heading.subtitle.font.size = 10,
      heading.padding = NULL,
      heading.border.bottom.style = NULL,
      heading.border.bottom.width = NULL,
      heading.border.bottom.color = NULL,
      heading.border.lr.style = NULL,
      heading.border.lr.width = NULL,
      heading.border.lr.color = NULL,
      table.border.top.width = px(2),
      table.border.bottom.width = px(2),
      table.border.left.width = px(2),
      table.border.right.width = px(2),
      source_notes.background.color = "#952F36"
      ) %>%
    tab_style(
      style = cell_borders(
        sides = c("left", "right"),
        color = "#d3d3d3",
        weight = px(2),
        style = "solid"
      ),
      locations = list(
        cells_body(
          columns = c(passive, tactical, ultimate),
          rows = everything()
        )#,
        # cells_column_labels(columns = c(passive, tactical, ultimate))
      )
    ) %>%
    opt_table_font(font = list(google_font(name = "teko"))) %>%
    opt_all_caps() %>%
    opt_align_table_header(align = "center") %>%
    opt_row_striping() %>%
    cols_align(align = "center", columns = everything()) %>%
    cols_width(c(passive, tactical, ultimate) ~ px(150)) %>%
    opt_table_outline(width = px(2))
}

gt_legend(nrows = NULL)

