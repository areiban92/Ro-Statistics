#' apaFuntions 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd



apa <- function(x, title = " ", nota) {
  gt::gt(x,rownames_to_stub = FALSE) %>%
    gt::tab_options(
      table.border.top.color = "#FFFFFF00",
      heading.title.font.size = gt::px(10),
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = gt::pct(100),
      table.background.color = "#FFFFFF00",
      table.font.size = gt::px(12),
      data_row.padding = gt::px(1)
    ) %>%
    gt::cols_align(align="center") %>%
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("top", "bottom"),
          color = "#FFFFFF00",
          weight = gt::px(1)
        ),
        gt::cell_text(
          align="center"
        ),
        gt::cell_fill(color = "#FFFFFF00", alpha = NULL)
      ),
      locations = gt::cells_body(
        columns = gt::everything(),
        rows = gt::everything()
      )
    ) %>%
    #FOOTER SETUP
    gt::tab_source_note(
      source_note = nota
    )%>%
    
    #title setup
    gt::tab_header(
      title = gt::html("<i>", title, "</i>")
    ) %>%
    gt::opt_align_table_header(align = "left")
  
  
}

apaDescriptivos <- function(x, title = " ", nota,ID) {
  gt::gt(x,rownames_to_stub = FALSE,id = ID) %>%
    
    gt::tab_options(
      table.border.top.color = "#FFFFFF00",
      heading.title.font.size = gt::px(10),
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = gt::pct(50),
      table.background.color = "#FFFFFF00",
      table.font.size = gt::px(12),
      data_row.padding = gt::px(1)
    ) %>%
    gt::cols_align(align="center") %>%
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("top", "bottom","right"),
          color = "#FFFFFF00",
          weight = gt::px(1)
        ),
        gt::cell_text(
          align="center"
        ),
        gt::cell_fill(color = "#FFFFFF00", alpha = NULL)
      ),
      locations = list(gt::cells_body(
        columns = gt::everything(),
        rows = gt::everything()
      )
      
      
      )) %>%
    #FOOTER SETUP
    gt::tab_source_note(
      source_note = nota
    )%>%
    
    #title setup
    gt::tab_header(
      title = gt::html("<i>", title, "</i>")
    ) %>%
    gt::opt_align_table_header(align = "left") %>%
    gt::opt_css(
      css = paste0("#",ID," .gt_table {display : inline ;}")
    )
  
  
}

apaTtest <- function(x, title = " ", nota,ID) {
  
  
  gt::gt(x,rownames_to_stub = FALSE, id = ID)  %>%
    
    gt::tab_options(
      table.border.top.color = "#FFFFFF00",
      heading.title.font.size = gt::px(10),
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = gt::pct(100),
      table.background.color = "#FFFFFF00",
      table.font.size = gt::px(12)
    ) %>%
    gt::cols_align(align="center") %>%
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("top", "bottom","right"),
          color = "#FFFFFF00",
          weight = gt::px(1)
        ),
        gt::cell_text(
          align="center"
        ),
        gt::cell_fill(color = "#FFFFFF00", alpha = NULL)
      ),
      locations = list(gt::cells_body(
        columns = gt::everything(),
        rows = gt::everything()
      )
      ))  %>%
    #FOOTER SETUP
    gt::tab_source_note(
      source_note = nota
    )%>%
    
    #title setup
    gt::tab_header(
      title = gt::html("<i>", title, "</i>")
    ) %>%
    gt::opt_align_table_header(align = "left") %>%
    gt::opt_css(
      css = paste0("#",ID," .gt_table {display : inline ;}")
    )
}

apaTtestConfidencia <- function(x, title = " ", nota, confidencia,ID) {
  
  inferior <- NULL
  superior <- NULL
  
  if(confidencia == TRUE){
    gt::gt(x,rownames_to_stub = FALSE ,id = ID) %>%
      
      gt::tab_options(
        table.border.top.color = "#FFFFFF00",
        heading.title.font.size = gt::px(10),
        column_labels.border.top.width = 3,
        column_labels.border.top.color = "black",
        column_labels.border.bottom.width = 3,
        column_labels.border.bottom.color = "black",
        table_body.border.bottom.color = "black",
        table.border.bottom.color = "white",
        table.width = gt::pct(100),
        table.background.color = "#FFFFFF00",
        table.font.size = gt::px(12)
      ) %>%
      gt::cols_align(align="center") %>%
      gt::tab_style(
        style = list(
          gt::cell_borders(
            sides = c("top", "bottom","right"),
            color = "#FFFFFF00",
            weight = gt::px(1)
          ),
          gt::cell_text(
            align="center"
          ),
          gt::cell_fill(color = "#FFFFFF00", alpha = NULL)
        ),
        locations = list(gt::cells_body(
          columns = gt::everything(),
          rows = gt::everything()
        ),gt::cells_stub(
          #columns = gt::everything(),
          rows = gt::everything()
        )
        )) %>%
      gt::tab_spanner(
        label = "Mean Diferrences",
        columns = c(inferior,superior)
      ) %>%
      #FOOTER SETUP
      gt::tab_source_note(
        source_note = nota
      )%>%
      
      #title setup
      gt::tab_header(
        title = gt::html("<i>", title, "</i>")
      ) %>%
      gt::opt_align_table_header(align = "left")%>%
      gt::opt_css(
        css = paste0("#",ID," .gt_table {display : inline ;}")
      )
    
  } 
  else { 
    
    gt::gt(x,rownames_to_stub = FALSE,id = ID) %>%
      
      
      gt::tab_options(
        table.border.top.color = "#FFFFFF00",
        heading.title.font.size = gt::px(10),
        column_labels.border.top.width = 3,
        column_labels.border.top.color = "black",
        column_labels.border.bottom.width = 3,
        column_labels.border.bottom.color = "black",
        table_body.border.bottom.color = "black",
        table.border.bottom.color = "white",
        table.width = gt::pct(100),
        table.background.color = "#FFFFFF00",
        table.font.size = gt::px(12)
      ) %>%
      gt::cols_align(align="center") %>%
      gt::tab_style(
        style = list(
          gt::cell_borders(
            sides = c("top", "bottom","right"),
            color = "#FFFFFF00",
            weight = gt::px(1)
          ),
          gt::cell_text(
            align="center"
          ),
          gt::cell_fill(color = "#FFFFFF00", alpha = NULL)
        ),
        locations = list(gt::cells_body(
          columns = gt::everything(),
          rows = gt::everything()
        )
        ))  %>%
      #FOOTER SETUP
      gt::tab_source_note(
        source_note = nota
      )%>%
      
      #title setup
      gt::tab_header(
        title = gt::html("<i>", title, "</i>")
      ) %>%
      gt::opt_align_table_header(align = "left")%>%
      gt::opt_css(
        css = paste0("#",ID," .gt_table {display : inline ;}")
      )
    
  } 
}

apaContingencia <- function(x, title = " ", nota,ID) {
  gt::gt(x,id = ID) %>%
    gt::tab_options(
      table.border.top.color = "#FFFFFF00",
      heading.title.font.size = gt::px(10),
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = gt::pct(100),
      table.background.color = "#FFFFFF00",
      table.font.size = gt::px(12)
    ) %>%
    gt::cols_align(align="center") %>%
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("top", "bottom"),
          color = "#FFFFFF00",
          weight = gt::px(1)
        ),
        gt::cell_text(
          align="center"
        ),
        gt::cell_fill(color = "#FFFFFF00", alpha = NULL)
      ),
      locations = gt::cells_body(
        columns = gt::everything(),
        rows = gt::everything()
      )
    ) %>%
    #FOOTER SETUP
    gt::tab_source_note(
      source_note = nota
    )%>%
    
    #title setup
    gt::tab_header(
      title = gt::html("<i>", title, "</i>")
    ) %>%
    gt::opt_align_table_header(align = "left") %>%
    gt::opt_css(
      css = paste0("#",ID," .gt_table {display : inline ;}")
    )  
  
}

apaContingenciaCuadrado <- function(x, title = " ", nota,ID) {
  gt::gt(x,id = ID) %>%
    gt::tab_options(
      table.border.top.color = "#FFFFFF00",
      heading.title.font.size = gt::px(10),
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = gt::pct(50),
      table.background.color = "#FFFFFF00",
      table.font.size = gt::px(12)
    ) %>%
    gt::cols_align(align="center") %>%
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("top", "bottom"),
          color = "#FFFFFF00",
          weight = gt::px(1)
        ),
        gt::cell_text(
          align="center"
        ),
        gt::cell_fill(color = "#FFFFFF00", alpha = NULL)
      ),
      locations = gt::cells_body(
        columns = gt::everything(),
        rows = gt::everything()
      )
    ) %>%
    #FOOTER SETUP
    gt::tab_source_note(
      source_note = nota
    )%>%
    
    #title setup
    gt::tab_header(
      title = gt::html("<i>", title, "</i>")
    ) %>%
    gt::opt_align_table_header(align = "left")  %>%
    
    gt::text_transform(
      locations = gt::cells_body(),
      fn = function(x) {
        stringr::str_replace_all(x,
                                 pattern = "@",
                                 replacement = "<sup>") %>% 
          stringr::str_replace_all("~",
                                   "</sup>") }
    ) %>%
    gt::opt_css(
      css = paste0("#",ID," .gt_table {display : inline ;}")
    )
  
}

apaCorrelacion <- function(x, title = " ", nota,ID) {
  gt::gt(x,id = ID) %>%
    gt::tab_options(
      table.border.top.color = "#FFFFFF00",
      heading.title.font.size = gt::px(10),
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = gt::pct(60),
      table.background.color = "#FFFFFF00",
      table.font.size = gt::px(12)
    ) %>%
    gt::cols_align(align="center") %>%
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("top", "bottom"),
          color = "#FFFFFF00",
          weight = gt::px(1)
        ),
        gt::cell_text(
          align="center"
        ),
        gt::cell_fill(color = "#FFFFFF00", alpha = NULL)
      ),
      locations = gt::cells_body(
        columns = gt::everything(),
        rows = gt::everything()
      )
    ) %>%
    #FOOTER SETUP
    gt::tab_source_note(
      source_note = nota
    )%>%
    
    #title setup
    gt::tab_header(
      title = gt::html("<i>", title, "</i>")
    ) %>%
    gt::opt_align_table_header(align = "left") %>%
    gt::opt_css(
      css = paste0("#",ID," .gt_table {display : inline ;}")
    )
  
  
}

apaContingenciaFisher <- function(x, title = " ", nota,ID) {
  gt::gt(x,id = ID) %>%
    gt::tab_options(
      table.border.top.color = "#FFFFFF00",
      heading.title.font.size = gt::px(10),
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = gt::pct(70),
      table.background.color = "#FFFFFF00",
      table.font.size = gt::px(12)
    ) %>%
    gt::cols_align(align="center") %>%
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("top", "bottom"),
          color = "#FFFFFF00",
          weight = gt::px(1)
        ),
        gt::cell_text(
          align="center"
        ),
        gt::cell_fill(color = "#FFFFFF00", alpha = NULL)
      ),
      locations = gt::cells_body(
        columns = gt::everything(),
        rows = gt::everything()
      )
    ) %>%
    #FOOTER SETUP
    gt::tab_source_note(
      source_note = nota
    )%>%
    
    #title setup
    gt::tab_header(
      title = gt::html("<i>", title, "</i>")
    ) %>%
    gt::opt_align_table_header(align = "left") %>%
    gt::opt_css(
      css = paste0("#",ID," .gt_table {display : inline ;}")
    )
  
  
}


apaAnova <- function(x, title = " ", nota,ID) {
  gt::gt(x,rownames_to_stub = FALSE, id = ID) %>%
    gt::tab_options(
      table.border.top.color = "#FFFFFF00",
      heading.title.font.size = gt::px(10),
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = gt::pct(100),
      table.background.color = "#FFFFFF00",
      table.font.size = gt::px(12)
    ) %>%
    gt::cols_align(align="center") %>%
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("top", "bottom"),
          color = "#FFFFFF00",
          weight = gt::px(1)
        ),
        gt::cell_text(
          align="center"
        ),
        gt::cell_fill(color = "#FFFFFF00", alpha = NULL)
      ),
      locations = gt::cells_body(
        columns = gt::everything(),
        rows = gt::everything()
      )
    ) %>%
    #FOOTER SETUP
    gt::tab_source_note(
      source_note = nota
    )%>%
    
    #title setup
    gt::tab_header(
      title = gt::html("<i>", title, "</i>")
    ) %>%
    gt::opt_align_table_header(align = "left") %>%
    gt::opt_css(
      css = paste0("#",ID," .gt_table {display : inline ;}")
    )  
  
}

apaRegresion <- function(x, title = " ", nota,ID) {
  gt::gt(x,rownames_to_stub = FALSE, id = ID) %>%
    gt::tab_options(
      table.border.top.color = "#FFFFFF00",
      heading.title.font.size = gt::px(10),
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = gt::pct(100),
      table.background.color = "#FFFFFF00",
      table.font.size = gt::px(12)
    ) %>%
    gt::cols_align(align="center") %>%
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("top", "bottom"),
          color = "#FFFFFF00",
          weight = gt::px(1)
        ),
        gt::cell_text(
          align="center"
        ),
        gt::cell_fill(color = "#FFFFFF00", alpha = NULL)
      ),
      locations = gt::cells_body(
        columns = gt::everything(),
        rows = gt::everything()
      )
    ) %>%
    #FOOTER SETUP
    gt::tab_source_note(
      source_note = nota
    )%>%
    
    #title setup
    gt::tab_header(
      title = gt::html("<i>", title, "</i>")
    ) %>%
    gt::opt_align_table_header(align = "left") %>%
    gt::opt_css(
      css = paste0("#",ID," .gt_table {display : inline ;}")
    ) %>%
    gt::fmt_markdown(
      columns = gt::everything()
    )
  
}