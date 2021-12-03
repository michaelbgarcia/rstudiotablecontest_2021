gt_local_icon_banner = function (gt_object, columns, height = 30) {
  stopifnot(`Table must be of class 'gt_tbl'` = "gt_tbl" %in% 
              class(gt_object))
  column_names <- gt:::resolve_cols_c(expr = {
    {
      columns
    }
  }, data = gt_object)
  
  stub_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]] == 
                                                       "stub")]
  grp_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]] == 
                                                      "row_group")]
  
  legend_name = gt_object[["_data"]]$name
  
  gt_object %>% text_transform(locations = if (isTRUE(grp_var %in% 
                                                      column_names)) {
    cells_row_groups()
  }
  else if (isTRUE(stub_var %in% column_names)) {
    cells_stub(rows = !is.na({
      {
        columns
      }
    }))
  }
  else {
    cells_body({
      {
        columns
      }
    }, rows = !is.na({
      {
        columns
      }
    }))
  }, fn = function(x) {
    img_file = glue("apex_data/legends/{legend_name}/banner/{legend_name}.png")
    img = local_image(filename = img_file, height = height)
    return(img)
  }
  ) %>% text_transform(locations = if (isTRUE(stub_var %in% 
                                              column_names)) {
    cells_stub(rows = is.na({
      {
        columns
      }
    }))
  }
  else {
    cells_body({
      {
        columns
      }
    }, rows = is.na({
      {
        columns
      }
    }))
  }, fn = function(x) {
    ""
  })
}
gt_local_icon_type = function (gt_object, columns, height = 30) {
  stopifnot(`Table must be of class 'gt_tbl'` = "gt_tbl" %in% 
              class(gt_object))
  column_names <- gt:::resolve_cols_c(expr = {
    {
      columns
    }
  }, data = gt_object)
  
  stub_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]] == 
                                                       "stub")]
  grp_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]] == 
                                                      "row_group")]
  gt_object %>% text_transform(locations = if (isTRUE(grp_var %in% 
                                                      column_names)) {
    cells_row_groups()
  }
  else if (isTRUE(stub_var %in% column_names)) {
    cells_stub(rows = !is.na({
      {
        columns
      }
    }))
  }
  else {
    cells_body({
      {
        columns
      }
    }, rows = !is.na({
      {
        columns
      }
    }))
  }, fn = function(x) {
    img = local_image(filename = glue("apex_data/type/{x}.png"), height = height)
    glue("<div style='line-height:10px'>{img}</div>\n<div style='line-height:12px'><span style ='text-align:center;font-weight:bold;font-size:8px'>{str_to_upper(x)}</span></div>")
  }
  ) %>% text_transform(locations = if (isTRUE(stub_var %in% 
                                              column_names)) {
    cells_stub(rows = is.na({
      {
        columns
      }
    }))
  }
  else {
    cells_body({
      {
        columns
      }
    }, rows = is.na({
      {
        columns
      }
    }))
  }, fn = function(x) {
    ""
  })
}
gt_local_icon_season = function (gt_object, columns, height = 30) {
  stopifnot(`Table must be of class 'gt_tbl'` = "gt_tbl" %in% 
              class(gt_object))
  column_names <- gt:::resolve_cols_c(expr = {
    {
      columns
    }
  }, data = gt_object)
  
  stub_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]] == 
                                                       "stub")]
  grp_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]] == 
                                                      "row_group")]
  gt_object %>% text_transform(locations = if (isTRUE(grp_var %in% 
                                                      column_names)) {
    cells_row_groups()
  }
  else if (isTRUE(stub_var %in% column_names)) {
    cells_stub(rows = !is.na({
      {
        columns
      }
    }))
  }
  else {
    cells_body({
      {
        columns
      }
    }, rows = !is.na({
      {
        columns
      }
    }))
  }, fn = function(x) {
    if(is.na(x)) return()
    img = local_image(filename = glue("apex_data/season/{x}.png"), height = height)
    glue("<div>{img}</div><div><span style ='text-align:center;font-weight:bold;font-size:8px'>Season {x}</span></div>")
  }
  ) %>% text_transform(locations = if (isTRUE(stub_var %in% 
                                              column_names)) {
    cells_stub(rows = is.na({
      {
        columns
      }
    }))
  }
  else {
    cells_body({
      {
        columns
      }
    }, rows = is.na({
      {
        columns
      }
    }))
  }, fn = function(x) {
    ""
  })
}
gt_local_icon_abilities = function (gt_object, columns, type = c("passive", "tactical", "ultimate"), height = 30) {
  stopifnot(`Table must be of class 'gt_tbl'` = "gt_tbl" %in% 
              class(gt_object))
  column_names <- gt:::resolve_cols_c(expr = {
    {
      columns
    }
  }, data = gt_object)
  
  stub_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]] == 
                                                       "stub")]
  grp_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]] == 
                                                      "row_group")]
  
  legend_name = gt_object[["_data"]]$name
  passive_desc = gt_object[["_data"]]$passive_desc
  tactical_desc = gt_object[["_data"]]$tactical_desc
  ultimate_desc = gt_object[["_data"]]$ultimate_desc
  
  type = match.arg(type)
  
  gt_object %>% text_transform(locations = if (isTRUE(grp_var %in% 
                                                      column_names)) {
    cells_row_groups()
  }
  else if (isTRUE(stub_var %in% column_names)) {
    cells_stub(rows = !is.na({
      {
        columns
      }
    }))
  }
  else {
    cells_body({
      {
        columns
      }
    }, rows = !is.na({
      {
        columns
      }
    }))
  }, fn = function(x) {
    
    desc = sym(glue("{type}_desc"))
    
    img_path = glue("apex_data/legends/{legend_name}/abilities/{type}")
    
    img_file = list.files(img_path, full.names = TRUE)
    img = local_image(filename = img_file, height = height)
    
    img_glue = glue("<div style='line-height:6px;'>{img}</div>\n<div style='line-height:6px;padding-top:5px'><span style ='text-align:center;font-weight:bold;font-size:10px'>{str_to_upper(x)}</span></div><div style='line-height:6px;padding-top:5px'><span style ='text-align:center;font-weight:normal;font-size:6px'>{str_to_upper(get(desc))}</span></div>")
    
    return(img_glue)
    
    }
  ) %>% text_transform(locations = if (isTRUE(stub_var %in% 
                                              column_names)) {
    cells_stub(rows = is.na({
      {
        columns
      }
    }))
  }
  else {
    cells_body({
      {
        columns
      }
    }, rows = is.na({
      {
        columns
      }
    }))
  }, fn = function(x) {
    ""
  })
}
gt_merge_stack_name = function (gt_object, col1, col2, col3, colors = c("black", "grey")){
  stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in% 
              class(gt_object))
  stopifnot(`There must be two colors` = length(colors) == 
              2)
  
  col1_bare <- rlang::enexpr(col1) %>% rlang::as_string()
  row_name_var <- gt_object[["_boxhead"]][["var"]][which(gt_object[["_boxhead"]][["type"]] == 
                                                           "stub")]
  col2_bare <- rlang::enexpr(col2) %>% rlang::as_string()
  data_in <- gt_object[["_data"]][[col2_bare]]
  
  col3_bare <- rlang::enexpr(col3) %>% rlang::as_string()
  data_in_3 <- gt_object[["_data"]][[col3_bare]]
  
  gt_object %>% text_transform(locations = if (isTRUE(row_name_var == 
                                                      col1_bare)) {
    cells_stub(rows = gt::everything())
  }
  else {
    cells_body(columns = {
      {
        col1
      }
    })
  }, fn = function(x) {
    img = local_image(filename = glue("apex_data/season/{data_in_3}.png"), height = 15)
    glue::glue("<div style='line-height:10px; padding:10px'><span style='font-weight:bold;font-variant:small-caps;color:{colors[1]};font-size:24px'>{x}</div>\n<div style='line-height:10px'><span style ='font-weight:bold;color:{colors[2]};font-size:10px'>{str_to_upper(data_in)}</span></div>\n<div style='padding:5px;display:flex;align-content:flex-end;justify-content:center;align-items:center;flex-wrap: wrap;flex-direction: row;'>{img}<span style ='font-weight:bold;color:{colors[2]};font-size:10px'>SEASON {data_in_3}</span></div>")
  }) %>% cols_hide(columns = {
    {
      col2
    }
  }) %>% cols_hide(columns = {
    {
      col3
    }
  })
}