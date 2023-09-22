`%>%` <- magrittr::`%>%`
`%!in%` <- function(x,y)!('%in%'(x,y))

# Excel Functions --------------------------------------------------

#' Add Sheet
#' adds Excel sheet to existing workbook
#' @param wb the workbook to write to
#' @param df the relevant dataframe
#' @param sheet_name the name of the sheet
#'
add_sheet <- function(wb, df, sheet_name) {
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, df, startRow = 1, startCol = 1)
}


create_wkbk <- function(df_data, df_blanks, df_dupes) {
  wkbk <- openxlsx::createWorkbook()
  
  # add main data sheet
  add_sheet(wkbk, df_data, 'Data')
  
  # add blank sheet + formatting
  add_sheet(wkbk, df_blanks, 'Blank')
  
  add_formatting_rl(wkbk, 'Blank', df_blanks)
  
  # add duplicate sheet + formatting
  add_sheet(wkbk, df_dupes, 'Duplicate')

  add_formatting_dupe(wkbk, 'Duplicate', df_dupes)
  
  # export data
  fp <- stringr::str_remove(fn,'.xlsx$') # gsub('.*/(.*?)$','\\1')
  openxlsx::saveWorkbook(wkbk, file = glue::glue('{fp}_cleaned.xlsx'), overwrite = TRUE)
}

# Add Formatting ----------------------------------------------------------

#' Flag < RL Data
#' finds data < RL in Excel sheet and highlights it in red
#' @param wb the relevant Excel workbook
#' @param sheet the relevant Excel workbook sheet
#' @param df the data frame containing the sheet's data
#'
add_formatting_rl <- function(wb, sheet, df){
  # to avoid applying the function to the metadata columns, we need to define what the last metadata column is  
  # * create vector of metadata columns
  vec_meta <- colnames(df)[colnames(df) %in% colnames_field]
  # * determine the last column to contain metadata, add 1 so the function starts at the next column
  col_num <- length(vec_meta)+1
  
  # define styles for cells
  # neg_style is for '<' data, norm_style is for 'N/A'
  neg_style <- openxlsx::createStyle(fontColour = '#9C0006', bgFill = '#FFC7CE')  
  norm_style <- openxlsx::createStyle(fontColour = '#000000',bgFill = '#FFFFFF', border = 'TopBottomLeftRight', borderColour = '#d4d4d4',
                                      borderStyle = 'thin')
  
  # apply conditional formatting to format all cells with the '<' (&lt;) character
  openxlsx::conditionalFormatting(wb = wb, sheet = sheet, cols = col_num:length(df), 
                                  rows = 1:nrow(df)+1, type = 'notcontains', rule = '&lt;', style = neg_style)
  
  # apply conditional formatting so all cells containing 'N/A' look normal
  # needed because 'N/A' cells are flagged in above code
  openxlsx::conditionalFormatting(wb = wb, sheet = sheet, cols = col_num:length(df), 
                                  rows = 1:nrow(df)+1, type = 'contains', rule = 'N/A', style = norm_style)
  
  # return the notebook variable
  return(wb)
}

#' Flag Mismatched Dupe Data
#' flag duplicate data where one value is < RL and one isn't
#' @param wb the relevant Excel workbook
#' @param sheet the relevant Excel workbook sheet
#' @param df the data frame containing the sheet's data
#'
add_formatting_dupe <- function(wb, sheet, df){
  # to avoid applying the function to the metadata columns, we need to define what the last metadata column is  
    
    # create vector of metadata columns
  func_grep <- function(x){!grepl('[1]',x)}
  vec_meta <- colnames(df_dupes)[unlist(lapply(colnames(df_dupes),func_grep))]
  
    # determine the last column to contain metadata, add 1 so the function starts at the next column
  col_num <- length(vec_meta)+1
  
  # define styles for cells
  # neg_style is for '<' data, norm_style is for 'N/A'
  warn_style <- openxlsx::createStyle(fontColour = '#9c5700', bgFill = '#ffeb9c')  
  neg_style <- openxlsx::createStyle(fontColour = '#9C0006', bgFill = '#FFC7CE')  
  bold_style <- openxlsx::createStyle(textDecoration = 'Bold')

  # apply conditional formatting 
  # TODO: write this better
  openxlsx::conditionalFormatting(wb = wb, sheet = sheet, cols = col_num:length(df), 
                                  rows = 1:nrow(df)+1, type = 'contains', rule = 'mismatch', style = warn_style)
  
  openxlsx::conditionalFormatting(wb = wb, sheet = sheet, cols = col_num:length(df), 
                                  rows = 1:nrow(df)+1, type = 'contains', rule = '!', style = neg_style)
  
  openxlsx::addStyle(wb = wb, sheet = sheet, style = bold_style,
                     rows = 1:nrow(df)+1, cols = which(grepl('RPD',colnames(df_dupes))), gridExpand = TRUE)
  
  # return the notebook variable
  return(wb)
}

# Format Main Dataframes --------------------------------------------------------

# * Helper funcs ----------------------------------------------------------

# *-- Main ----------------------------------------------------------

#' TODO: later
format_dfs <- function(fn){
  df_raw <- readxl::read_excel(fn, skip = 1)
  
  df_raw <- df_raw %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::relocate(id, .before = `Station Name`) 
  
  # determine row where `Station Name` is `<<Field Results>>`
  mid_idrow <- df_raw %>% subset(`Station Name` == '<<<Field Results>>>')
  
  # grab the ID for this row
  vari_id <- mid_idrow$id
  
  # subset field and lab dataframes using this ID
  df_field <- df_raw %>% subset(id > vari_id)
  df_lab <- df_raw %>% subset(id < vari_id)
  
  # remove columns containing '...' for lab
  df_lab <- df_lab %>% dplyr::select(-c(id, contains('...'))) 
  
  # rename columns for easier combining later
  df_lab <- df_lab %>% dplyr::rename(Date = `Sample Date`)
  
  out <- list('df_field' = df_field, 'df_lab' = df_lab)
}

rename_cols <- function(df_field) {
  # import field colnames csv
  df_fieldnames <-
    readr::read_csv('01_WaterQuality/wq-qaqc/admin_files/WDL_Column_Headers.csv', show_col_types = FALSE)
  
  # get rid of ID column because 'df_fieldnames' doesn't have it
  df_field <- df_field %>%
    dplyr::select(-'id')
  
  # "map" df_fieldnames$`Field New` onto the colnames for df_field
  colnames(df_field) <- df_fieldnames$Field_New
  
  # remove first row; always will be old column names
  df_field <- df_field %>% dplyr::slice(-1)
  
  return(df_field) 
}

reorder_cols <- function(df_field) {
  # import field order csv
  df_fieldorder <- readr::read_csv('01_WaterQuality/wq-qaqc/admin_files/WDL_Column_Order.csv', show_col_types = FALSE)
  
  # select 'New Order' column from df_fieldorder
  vec_fieldorder <- df_fieldorder$New_Order
  
  # rebuild df_field with columns in the correct order
  df_field <- df_field %>% dplyr::select(vec_fieldorder)    
  
  return(df_field)
}

# *-- Dupe ---------------------------------------------------

func_rpd <- function(x,y){
  x <- as.numeric(x)
  y <- as.numeric(y)
  out <- abs((y-x)/((y+x)/2)*100)
  out <- round(out, 2)
  return(out)
}

coalesce_by_column <- function(df) {
  return(dplyr::coalesce(!!! as.list(df)))
}
 
format_rpd <- function(df){
  # create df for RPD calcs
  df_rpd <- dplyr::left_join(df, df_data, by = c('Parent Sample Code' = 'Sample Code'))
  
  # remove lab duplicate data
  df_clean <- as.data.frame(apply(df_rpd, c(1, 2), function(x) stringr::str_split(x, ',', simplify = TRUE)[1]))
  
  # pivot df so RPD calc is possible
  # * pivot analyte cols (ie. cols with '*.')
  piv_cols <- colnames(df_clean)[stringr::str_detect(colnames(df_clean),'\\*\\.')]
  
  df_long <- tidyr::pivot_longer(df_clean, cols = all_of(piv_cols), names_pattern = "(.*)\\.(.)$", names_to = c('analyte','type'))
  
  df_wide <- tidyr::pivot_wider(df_long, names_from = type, values_from = value) %>%
    dplyr::rename('parent' = x,
           'dupe' = y)
  
  # create RPD df
  # * NOTE: the first logic string (~ 9999) will be used to flag if one is '<' but the other isn't. 9999 is a dummy value.
  # * (throws 'NA introduced by coercion' warning, but when checked, don't see any)
  df_rpd <- df_wide %>%
    dplyr::mutate(
      RPD =
        dplyr::case_when((stringr::str_detect(parent, '<') | stringr::str_detect(dupe, '<')) & (stringr::str_detect(parent, '<') != stringr::str_detect(dupe, '<')) ~ 9999,
                  stringr::str_detect(parent, 'N/A') | stringr::str_detect(parent, 'N/A') ~ NA_real_,
                  stringr::str_detect(parent, '<') | stringr::str_detect(dupe, '<') ~ NA_real_,
                  TRUE ~ func_rpd(parent,dupe)))
  
  # * add signifier for RPD > 25
  df_rpd <- df_rpd %>%
    dplyr::mutate(
      RPD =
        dplyr::case_when(RPD >= 25 ~ paste0(RPD,'!'),
                         TRUE ~ as.character(RPD))
    )
  
  df_wide <- df_rpd %>% tidyr::pivot_wider(names_from = analyte, names_glue = '{analyte}_RPD', values_from = RPD) %>%
    dplyr::select('Station Name.x','Date.x','Sample Code', contains('_RPD')) # to clean up a lil
  
  df_rpd <- df_wide %>%
    dplyr::group_by(`Station Name.x`, Date.x, `Sample Code`) %>%
    dplyr::summarise_all(coalesce_by_column)
  
  return(df_rpd)
}

add_parent <- function(df){
  # determine metadata cols
  col_overlap <- colnames(df_data)[colnames(df_data) %in% colnames(df)]
  meta_cols <- col_overlap[unlist(lapply(col_overlap, function(x) !grepl('[1]',x)))]
  meta_cols <- stringr::str_subset(meta_cols, 'Sample Type|Parent Sample Code|Sample Code|Chlorophyll Volume', negate = TRUE)
  
  # subset by non-lab data
  df_parent <- df_data %>%
    dplyr::select(!all_of(vec_field))
  
  
  df_parent <- df_parent %>%
    dplyr::filter(`Sample Code` %in% df$`Parent Sample Code`)
  
  colnames(df_parent) <- unlist(lapply(colnames(df_parent), function(x) ifelse(x %in% meta_cols, x, paste0(x,'_Parent'))))
  
  df_out <- dplyr::full_join(df_parent, df, by = meta_cols)
  
  meta_cols <<- meta_cols
  
  return(df_out)
}

#' Order Duplicate Dataframe
#' 

order_dupes <- function(df){
  # add '_' to every column for str split
  colnames(df) <- lapply(colnames(df), function(x) ifelse(stringr::str_detect(x,'_'), x, paste0(x,'_Dupe')))
  
  m <- names(df) # extract column names
  
  r <- strsplit(m,'_') # split the column names based on '_'
  
  # create a data.frame with two columns X1 being x or y
  df_names <- data.frame(matrix(unlist(r),nrow=length(m),byrow=T)) 
  
  # order q according to analyte
  df_names <- df_names[order(df_names$X1, decreasing = FALSE),]
  
  # reformat names
  m <- paste0(df_names$X1,'_',df_names$X2)
  
  # rearrange columns
  df <- df[,m]
  
  # move metadata cols first
  df_meta <- df_names %>%
    dplyr::group_by(X1) %>%
    dplyr::summarize(n = dplyr::n())
  
  meta_final <- paste0(df_meta$X1[df_meta$n == 1])
  meta_final <- c(meta_final,'Sample Code_Parent','Sample Code_Dupe')
  meta_dupe <- paste0(meta_final,'_Dupe')
  
  #* remove dummy '_Dupe' from metacols
  colnames(df)[colnames(df) %in% meta_dupe] <- unlist(lapply(meta_dupe, function(x) stringr::str_remove(x,'_Dupe')))
  
  df_out <- df %>%
    dplyr::relocate(all_of(meta_final), .before = everything()) %>%
    dplyr::select(-c(dplyr::contains('Sample Type'), dplyr::contains('Parent Sample Code')))
  
  return(df_out)
}

# * Main funcs ------------------------------------------------------------

format_raw <- function(fn){
  # format data
  df_list <- format_dfs(fn)
  
  # define varis
  df_field <- df_list$df_field
  df_lab <- df_list$df_lab
  
  # remove extra rows
  field_idrow <- df_field %>% subset(is.na(df_field$`Station Name`))
  
  vari_id <- field_idrow$id
  
  df_field <- df_field %>% subset(id < vari_id)
  
  # rename columns
  df_field <- rename_cols(df_field)
  
  # reorder columns
  df_field <- reorder_cols(df_field)
  
  # recombine data
  vec_names <-
    colnames(df_field)[colnames(df_field) %in% colnames(df_lab)]
  
  # recombine by vec names
  df_combined <- dplyr::full_join(df_lab, df_field, by = vec_names)
  
  # scope colname overlap for use by later functions
  colnames_field <<- colnames(df_field)
  colnames_lab <<- colnames(df_lab)
  
  #* remove unnecessary analytes (field + chla vol)
  vec_field <<- colnames(df_field)[(!(colnames(df_field) == 'Chlorophyll Volume')) & (!(colnames(df_field) %in% colnames(df_lab)))]
  
  return(df_combined)
}

format_data <- function(df){
  # subset out non-main data
  df <- df %>% dplyr::filter(`Sample Type` %!in% c('Blank; Equipment', 'Duplicate, Specific Analyte(s)'))
}

format_blanks <- function(df){
  # subset blank data
  df_blank <- df %>%
    dplyr::filter(`Sample Type` == 'Blank; Equipment')  
  
  df_blank <- df_blank %>%
    dplyr::select(!c(all_of(vec_field),`Chlorophyll Volume`))
}

format_dupes <- function(df){
  # subset dupe data and remove extra cols
  df_dupes <- df %>%
    dplyr::filter(`Sample Type` == 'Duplicate, Specific Analyte(s)')
  
  df_dupes <- df_dupes %>%
    dplyr::select(!all_of(vec_field))
  
  # add RPD calcs
  df_rpd <- format_rpd(df_dupes)
  
  df_out <- dplyr::left_join(df_dupes, df_rpd, by = 'Sample Code') %>%
    dplyr::select(-contains('.x'))
  
  # add parent values
  df_out <- add_parent(df_out)
  
  df_out <- order_dupes(df_out)
  
  # change dummy '9999' variable to 'mismatch
  df_out[df_out=='9999!'] <- 'mismatch'
  
  return(df_out)
}
