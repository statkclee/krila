---
title: "EDA I"
editor_options: 
  chunk_output_type: console
---


# 데이터


::: {.cell}

```{.r .cell-code}
library(tidyverse)
library(readxl)

law_raw <- read_excel("data/(230410)최종결과표(통합본).xlsx", skip = 0)

law_tbl <- law_raw |>
  janitor::clean_names(ascii = FALSE) |> 
  mutate(사무_판단 = ifelse(is.na(사무_판단), 0, 사무_판단)) |> 
  mutate(사무유형 = str_remove(사무유형, "\r\n")) |>
  # 사무유형: 국가, 시도, 공동, ...
  mutate(사무유형 = case_when(사무유형 == "국가시도" ~ "국가-시도",
                              사무유형 == "정부" ~ "국가",
                              사무유형 == "공동" ~ "국가-시도",
                              is.na(사무유형) ~ "없음",
                              TRUE ~ 사무유형))  
```
:::



# EDA

## 법령명 행수


::: {.cell}

```{.r .cell-code}
library(gt)

law_tbl |> 
  count(법령명, sort = TRUE, name = "행수") |> 
  slice_max(행수, n = 10) |> 
  gt()
```

::: {.cell-output-display}
```{=html}
<div id="tttknlxeyj" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#tttknlxeyj table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#tttknlxeyj thead, #tttknlxeyj tbody, #tttknlxeyj tfoot, #tttknlxeyj tr, #tttknlxeyj td, #tttknlxeyj th {
  border-style: none;
}

#tttknlxeyj p {
  margin: 0;
  padding: 0;
}

#tttknlxeyj .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#tttknlxeyj .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#tttknlxeyj .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#tttknlxeyj .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#tttknlxeyj .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#tttknlxeyj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tttknlxeyj .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#tttknlxeyj .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#tttknlxeyj .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#tttknlxeyj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#tttknlxeyj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#tttknlxeyj .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#tttknlxeyj .gt_spanner_row {
  border-bottom-style: hidden;
}

#tttknlxeyj .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#tttknlxeyj .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#tttknlxeyj .gt_from_md > :first-child {
  margin-top: 0;
}

#tttknlxeyj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#tttknlxeyj .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#tttknlxeyj .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#tttknlxeyj .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#tttknlxeyj .gt_row_group_first td {
  border-top-width: 2px;
}

#tttknlxeyj .gt_row_group_first th {
  border-top-width: 2px;
}

#tttknlxeyj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tttknlxeyj .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#tttknlxeyj .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#tttknlxeyj .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tttknlxeyj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#tttknlxeyj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#tttknlxeyj .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#tttknlxeyj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#tttknlxeyj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#tttknlxeyj .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#tttknlxeyj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#tttknlxeyj .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#tttknlxeyj .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#tttknlxeyj .gt_left {
  text-align: left;
}

#tttknlxeyj .gt_center {
  text-align: center;
}

#tttknlxeyj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#tttknlxeyj .gt_font_normal {
  font-weight: normal;
}

#tttknlxeyj .gt_font_bold {
  font-weight: bold;
}

#tttknlxeyj .gt_font_italic {
  font-style: italic;
}

#tttknlxeyj .gt_super {
  font-size: 65%;
}

#tttknlxeyj .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#tttknlxeyj .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#tttknlxeyj .gt_indent_1 {
  text-indent: 5px;
}

#tttknlxeyj .gt_indent_2 {
  text-indent: 10px;
}

#tttknlxeyj .gt_indent_3 {
  text-indent: 15px;
}

#tttknlxeyj .gt_indent_4 {
  text-indent: 20px;
}

#tttknlxeyj .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="법령명">법령명</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="행수">행수</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="법령명" class="gt_row gt_left">자본시장과 금융투자업에 관한 법률 시행령</td>
<td headers="행수" class="gt_row gt_right">5590</td></tr>
    <tr><td headers="법령명" class="gt_row gt_left">조세특례제한법 시행령</td>
<td headers="행수" class="gt_row gt_right">4878</td></tr>
    <tr><td headers="법령명" class="gt_row gt_left">자본시장과 금융투자업에 관한 법률</td>
<td headers="행수" class="gt_row gt_right">4693</td></tr>
    <tr><td headers="법령명" class="gt_row gt_left">소득세법 시행령</td>
<td headers="행수" class="gt_row gt_right">3427</td></tr>
    <tr><td headers="법령명" class="gt_row gt_left">자동차관리법 시행규칙</td>
<td headers="행수" class="gt_row gt_right">3372</td></tr>
    <tr><td headers="법령명" class="gt_row gt_left">조세특례제한법</td>
<td headers="행수" class="gt_row gt_right">3254</td></tr>
    <tr><td headers="법령명" class="gt_row gt_left">관세법 시행령</td>
<td headers="행수" class="gt_row gt_right">3141</td></tr>
    <tr><td headers="법령명" class="gt_row gt_left">산업안전보건기준에 관한 규칙</td>
<td headers="행수" class="gt_row gt_right">2717</td></tr>
    <tr><td headers="법령명" class="gt_row gt_left">법인세법 시행령</td>
<td headers="행수" class="gt_row gt_right">2406</td></tr>
    <tr><td headers="법령명" class="gt_row gt_left">관세법</td>
<td headers="행수" class="gt_row gt_right">2365</td></tr>
  </tbody>
  
  
</table>
</div>
```
:::
:::


## 사무판단 건수


::: {.cell}

```{.r .cell-code}
law_tbl |> 
  mutate(사무_판단 = ifelse(is.na(사무_판단), 0, 사무_판단)) |>
  # filter(is.na(사무_판단)) |> 
  count(사무_판단, sort = TRUE, name = "건수") |> 
  mutate(비율 = 건수 / sum(건수)) |> 
  janitor::adorn_totals(name = "합계") |> 
  gt() |> 
  cols_align("center") |> 
  fmt_integer(건수) |> 
  fmt_percent(비율, decimals = 1)
```

::: {.cell-output-display}
```{=html}
<div id="ywivcfsykk" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ywivcfsykk table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ywivcfsykk thead, #ywivcfsykk tbody, #ywivcfsykk tfoot, #ywivcfsykk tr, #ywivcfsykk td, #ywivcfsykk th {
  border-style: none;
}

#ywivcfsykk p {
  margin: 0;
  padding: 0;
}

#ywivcfsykk .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ywivcfsykk .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ywivcfsykk .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ywivcfsykk .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ywivcfsykk .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ywivcfsykk .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ywivcfsykk .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ywivcfsykk .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ywivcfsykk .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ywivcfsykk .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ywivcfsykk .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ywivcfsykk .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ywivcfsykk .gt_spanner_row {
  border-bottom-style: hidden;
}

#ywivcfsykk .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#ywivcfsykk .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ywivcfsykk .gt_from_md > :first-child {
  margin-top: 0;
}

#ywivcfsykk .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ywivcfsykk .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ywivcfsykk .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#ywivcfsykk .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#ywivcfsykk .gt_row_group_first td {
  border-top-width: 2px;
}

#ywivcfsykk .gt_row_group_first th {
  border-top-width: 2px;
}

#ywivcfsykk .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ywivcfsykk .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ywivcfsykk .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ywivcfsykk .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ywivcfsykk .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ywivcfsykk .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ywivcfsykk .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ywivcfsykk .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ywivcfsykk .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ywivcfsykk .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ywivcfsykk .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ywivcfsykk .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ywivcfsykk .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ywivcfsykk .gt_left {
  text-align: left;
}

#ywivcfsykk .gt_center {
  text-align: center;
}

#ywivcfsykk .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ywivcfsykk .gt_font_normal {
  font-weight: normal;
}

#ywivcfsykk .gt_font_bold {
  font-weight: bold;
}

#ywivcfsykk .gt_font_italic {
  font-style: italic;
}

#ywivcfsykk .gt_super {
  font-size: 65%;
}

#ywivcfsykk .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ywivcfsykk .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ywivcfsykk .gt_indent_1 {
  text-indent: 5px;
}

#ywivcfsykk .gt_indent_2 {
  text-indent: 10px;
}

#ywivcfsykk .gt_indent_3 {
  text-indent: 15px;
}

#ywivcfsykk .gt_indent_4 {
  text-indent: 20px;
}

#ywivcfsykk .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="사무_판단">사무_판단</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="건수">건수</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="비율">비율</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="사무_판단" class="gt_row gt_center">0</td>
<td headers="건수" class="gt_row gt_center">801,689</td>
<td headers="비율" class="gt_row gt_center">93.0%</td></tr>
    <tr><td headers="사무_판단" class="gt_row gt_center">1</td>
<td headers="건수" class="gt_row gt_center">60,030</td>
<td headers="비율" class="gt_row gt_center">7.0%</td></tr>
    <tr><td headers="사무_판단" class="gt_row gt_center">합계</td>
<td headers="건수" class="gt_row gt_center">861,719</td>
<td headers="비율" class="gt_row gt_center">100.0%</td></tr>
  </tbody>
  
  
</table>
</div>
```
:::
:::



## 사무판단 건수와 행수

:::{.panel-tabset}

### 표 {.unnumbered}



::: {.cell}

```{.r .cell-code}
library(gt)
law_tbl |> 
  group_by(법령명) |> 
  summarise(사무건수 = sum(사무_판단),
            행수 = n()) |> 
  slice_max(사무건수, n = 10) |>
  gt::gt() |> 
  cols_align("center") 
```

::: {.cell-output-display}
```{=html}
<div id="acezwcmcum" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#acezwcmcum table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#acezwcmcum thead, #acezwcmcum tbody, #acezwcmcum tfoot, #acezwcmcum tr, #acezwcmcum td, #acezwcmcum th {
  border-style: none;
}

#acezwcmcum p {
  margin: 0;
  padding: 0;
}

#acezwcmcum .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#acezwcmcum .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#acezwcmcum .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#acezwcmcum .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#acezwcmcum .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#acezwcmcum .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#acezwcmcum .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#acezwcmcum .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#acezwcmcum .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#acezwcmcum .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#acezwcmcum .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#acezwcmcum .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#acezwcmcum .gt_spanner_row {
  border-bottom-style: hidden;
}

#acezwcmcum .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#acezwcmcum .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#acezwcmcum .gt_from_md > :first-child {
  margin-top: 0;
}

#acezwcmcum .gt_from_md > :last-child {
  margin-bottom: 0;
}

#acezwcmcum .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#acezwcmcum .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#acezwcmcum .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#acezwcmcum .gt_row_group_first td {
  border-top-width: 2px;
}

#acezwcmcum .gt_row_group_first th {
  border-top-width: 2px;
}

#acezwcmcum .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#acezwcmcum .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#acezwcmcum .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#acezwcmcum .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#acezwcmcum .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#acezwcmcum .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#acezwcmcum .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#acezwcmcum .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#acezwcmcum .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#acezwcmcum .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#acezwcmcum .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#acezwcmcum .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#acezwcmcum .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#acezwcmcum .gt_left {
  text-align: left;
}

#acezwcmcum .gt_center {
  text-align: center;
}

#acezwcmcum .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#acezwcmcum .gt_font_normal {
  font-weight: normal;
}

#acezwcmcum .gt_font_bold {
  font-weight: bold;
}

#acezwcmcum .gt_font_italic {
  font-style: italic;
}

#acezwcmcum .gt_super {
  font-size: 65%;
}

#acezwcmcum .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#acezwcmcum .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#acezwcmcum .gt_indent_1 {
  text-indent: 5px;
}

#acezwcmcum .gt_indent_2 {
  text-indent: 10px;
}

#acezwcmcum .gt_indent_3 {
  text-indent: 15px;
}

#acezwcmcum .gt_indent_4 {
  text-indent: 20px;
}

#acezwcmcum .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="법령명">법령명</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="사무건수">사무건수</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="행수">행수</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="법령명" class="gt_row gt_center">제주특별자치도 설치 및 국제자유도시 조성을 위한 특별법</td>
<td headers="사무건수" class="gt_row gt_center">332</td>
<td headers="행수" class="gt_row gt_center">2358</td></tr>
    <tr><td headers="법령명" class="gt_row gt_center">자본시장과 금융투자업에 관한 법률</td>
<td headers="사무건수" class="gt_row gt_center">281</td>
<td headers="행수" class="gt_row gt_center">4693</td></tr>
    <tr><td headers="법령명" class="gt_row gt_center">지방세특례제한법</td>
<td headers="사무건수" class="gt_row gt_center">270</td>
<td headers="행수" class="gt_row gt_center">1788</td></tr>
    <tr><td headers="법령명" class="gt_row gt_center">재난 및 안전관리 기본법</td>
<td headers="사무건수" class="gt_row gt_center">258</td>
<td headers="행수" class="gt_row gt_center">902</td></tr>
    <tr><td headers="법령명" class="gt_row gt_center">대기환경보전법</td>
<td headers="사무건수" class="gt_row gt_center">257</td>
<td headers="행수" class="gt_row gt_center">1155</td></tr>
    <tr><td headers="법령명" class="gt_row gt_center">물환경보전법</td>
<td headers="사무건수" class="gt_row gt_center">252</td>
<td headers="행수" class="gt_row gt_center">954</td></tr>
    <tr><td headers="법령명" class="gt_row gt_center">관세법</td>
<td headers="사무건수" class="gt_row gt_center">244</td>
<td headers="행수" class="gt_row gt_center">2365</td></tr>
    <tr><td headers="법령명" class="gt_row gt_center">산림보호법</td>
<td headers="사무건수" class="gt_row gt_center">218</td>
<td headers="행수" class="gt_row gt_center">806</td></tr>
    <tr><td headers="법령명" class="gt_row gt_center">폐기물관리법</td>
<td headers="사무건수" class="gt_row gt_center">204</td>
<td headers="행수" class="gt_row gt_center">964</td></tr>
    <tr><td headers="법령명" class="gt_row gt_center">수도법</td>
<td headers="사무건수" class="gt_row gt_center">182</td>
<td headers="행수" class="gt_row gt_center">778</td></tr>
  </tbody>
  
  
</table>
</div>
```
:::
:::


### 그래프 {.unnumbered}


::: {.cell}

```{.r .cell-code}

law_tbl |> 
  group_by(법령명) |> 
  summarise(사무건수 = sum(사무_판단),
            행수 = n()) |> 
  arrange(사무건수) |> 
  ggplot(aes(x = 행수, y = 사무건수)) +
    geom_point() 
```

::: {.cell-output-display}
![](EDA_files/figure-html/unnamed-chunk-5-1.png){width=672}
:::
:::


:::


## 행수분포와 사무판단


::: {.cell}

```{.r .cell-code}
law_tbl |> 
  group_by(법령명) |> 
  summarise(사무건수 = sum(사무_판단),
            행수 = n()) |> 
  mutate(사무판단 = ifelse(사무건수 == 0, "사무없음", "사무있음")) |> 
  ggplot(aes(x = 행수, fill = 사무판단)) +
    geom_density(alpha = 0.7) +
    theme(legend.position = "bottom") +
    scale_x_sqrt() 
```

::: {.cell-output-display}
![](EDA_files/figure-html/unnamed-chunk-6-1.png){width=672}
:::
:::


## 수행주체

### 조문 기준


::: {.cell}

```{.r .cell-code}
library(gtExtras)

law_tbl |>
  mutate(중앙지자체 = case_when(사무유형 %in% c("국가") ~ "중앙정부",
                                사무유형 %in% c("시도", "시도-시군구", "시군구") ~ "지자체",
                                TRUE ~ "공동")) |>
  filter(사무유형 != "없음") |> 
  count(중앙지자체, 사무유형, name = "건수") |>
  mutate(비율 = 건수 / sum(건수)) |> 
  gt(groupname_col = "중앙지자체",
     rowname_col   = "사무유형"  ) |> 
  fmt_percent(비율, decimals = 1) |>
  fmt_integer(건수) |>
  grand_summary_rows(
    fns = list(
      "합계" = ~sum(.)),
    columns = c("건수"),
    formatter = fmt_integer
  ) |> 
  grand_summary_rows(
    fns = list(
      "합계" = ~sum(.)),
    columns = c("비율"),
    formatter = fmt_percent,
    decimals = 0
  ) |> 
  gt_theme_538()
```

::: {.cell-output-display}
```{=html}
<div id="fhvlmxkujn" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>@import url("https://fonts.googleapis.com/css2?family=Chivo:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
@import url("https://fonts.googleapis.com/css2?family=Chivo:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
@import url("https://fonts.googleapis.com/css2?family=Chivo:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
@import url("https://fonts.googleapis.com/css2?family=Cairo:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
#fhvlmxkujn table {
  font-family: Cairo, system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#fhvlmxkujn thead, #fhvlmxkujn tbody, #fhvlmxkujn tfoot, #fhvlmxkujn tr, #fhvlmxkujn td, #fhvlmxkujn th {
  border-style: none;
}

#fhvlmxkujn p {
  margin: 0;
  padding: 0;
}

#fhvlmxkujn .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: 400;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: none;
  border-top-width: 3px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#fhvlmxkujn .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#fhvlmxkujn .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#fhvlmxkujn .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#fhvlmxkujn .gt_heading {
  background-color: #FFFFFF;
  text-align: left;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fhvlmxkujn .gt_bottom_border {
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fhvlmxkujn .gt_col_headings {
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #000000;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fhvlmxkujn .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#fhvlmxkujn .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#fhvlmxkujn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#fhvlmxkujn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#fhvlmxkujn .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #000000;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#fhvlmxkujn .gt_spanner_row {
  border-bottom-style: hidden;
}

#fhvlmxkujn .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #000000;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#fhvlmxkujn .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #000000;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #FFFFFF;
  vertical-align: middle;
}

#fhvlmxkujn .gt_from_md > :first-child {
  margin-top: 0;
}

#fhvlmxkujn .gt_from_md > :last-child {
  margin-bottom: 0;
}

#fhvlmxkujn .gt_row {
  padding-top: 3px;
  padding-bottom: 3px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#fhvlmxkujn .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 0px;
  border-right-color: #FFFFFF;
  padding-left: 5px;
  padding-right: 5px;
}

#fhvlmxkujn .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#fhvlmxkujn .gt_row_group_first td {
  border-top-width: 2px;
}

#fhvlmxkujn .gt_row_group_first th {
  border-top-width: 2px;
}

#fhvlmxkujn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fhvlmxkujn .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#fhvlmxkujn .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#fhvlmxkujn .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fhvlmxkujn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fhvlmxkujn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#fhvlmxkujn .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#fhvlmxkujn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#fhvlmxkujn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fhvlmxkujn .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fhvlmxkujn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#fhvlmxkujn .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fhvlmxkujn .gt_sourcenote {
  font-size: 12px;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#fhvlmxkujn .gt_left {
  text-align: left;
}

#fhvlmxkujn .gt_center {
  text-align: center;
}

#fhvlmxkujn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#fhvlmxkujn .gt_font_normal {
  font-weight: normal;
}

#fhvlmxkujn .gt_font_bold {
  font-weight: bold;
}

#fhvlmxkujn .gt_font_italic {
  font-style: italic;
}

#fhvlmxkujn .gt_super {
  font-size: 65%;
}

#fhvlmxkujn .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#fhvlmxkujn .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#fhvlmxkujn .gt_indent_1 {
  text-indent: 5px;
}

#fhvlmxkujn .gt_indent_2 {
  text-indent: 10px;
}

#fhvlmxkujn .gt_indent_3 {
  text-indent: 15px;
}

#fhvlmxkujn .gt_indent_4 {
  text-indent: 20px;
}

#fhvlmxkujn .gt_indent_5 {
  text-indent: 25px;
}

tbody tr:last-child {
  border-bottom: 2px solid #ffffff00;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=""></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="border-top-width: 0px; border-top-style: solid; border-top-color: black; font-family: Chivo; font-size: 14px; vertical-align: bottom; font-weight: 200; text-transform: uppercase;" scope="col" id="건수">건수</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="border-top-width: 0px; border-top-style: solid; border-top-color: black; font-family: Chivo; font-size: 14px; vertical-align: bottom; font-weight: 200; text-transform: uppercase;" scope="col" id="비율">비율</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: black;" scope="colgroup" id="공동">공동</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_1" scope="row" class="gt_row gt_left gt_stub">국가-시군구</th>
<td headers="공동 stub_1_1 건수" class="gt_row gt_right">195</td>
<td headers="공동 stub_1_1 비율" class="gt_row gt_right">0.3%</td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_left gt_stub">국가-시도</th>
<td headers="공동 stub_1_2 건수" class="gt_row gt_right">1,427</td>
<td headers="공동 stub_1_2 비율" class="gt_row gt_right">2.4%</td></tr>
    <tr><th id="stub_1_3" scope="row" class="gt_row gt_left gt_stub">국가-시도-시군구</th>
<td headers="공동 stub_1_3 건수" class="gt_row gt_right">5,539</td>
<td headers="공동 stub_1_3 비율" class="gt_row gt_right">9.2%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: black;" scope="colgroup" id="중앙정부">중앙정부</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_4" scope="row" class="gt_row gt_left gt_stub">국가</th>
<td headers="중앙정부 stub_1_4 건수" class="gt_row gt_right">41,834</td>
<td headers="중앙정부 stub_1_4 비율" class="gt_row gt_right">69.6%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: black;" scope="colgroup" id="지자체">지자체</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_5" scope="row" class="gt_row gt_left gt_stub">시군구</th>
<td headers="지자체 stub_1_5 건수" class="gt_row gt_right">3,239</td>
<td headers="지자체 stub_1_5 비율" class="gt_row gt_right">5.4%</td></tr>
    <tr><th id="stub_1_6" scope="row" class="gt_row gt_left gt_stub">시도</th>
<td headers="지자체 stub_1_6 건수" class="gt_row gt_right">3,358</td>
<td headers="지자체 stub_1_6 비율" class="gt_row gt_right">5.6%</td></tr>
    <tr><th id="stub_1_7" scope="row" class="gt_row gt_left gt_stub">시도-시군구</th>
<td headers="지자체 stub_1_7 건수" class="gt_row gt_right">4,479</td>
<td headers="지자체 stub_1_7 비율" class="gt_row gt_right">7.5%</td></tr>
    <tr><th id="grand_summary_stub_1" scope="row" class="gt_row gt_left gt_stub gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row">합계</th>
<td headers="grand_summary_stub_1 건수" class="gt_row gt_right gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row">60,071</td>
<td headers="grand_summary_stub_1 비율" class="gt_row gt_right gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row">100%</td></tr>
  </tbody>
  
  
</table>
</div>
```
:::
:::


### 법령 기준

:::{.panel-tabset}

#### 표 {.unnumbered}



::: {.cell}

```{.r .cell-code}
law_tbl |>
  mutate(중앙지자체 = case_when(사무유형 %in% c("국가") ~ "중앙정부",
                                사무유형 %in% c("시도", "시도-시군구", "시군구") ~ "지자체",
                                사무유형 == "없음" ~ "없음",
                                TRUE ~ "공동")) |>  
  # filter(사무유형 != "없음") |>
  group_by(법령명) |>
  count(중앙지자체, 사무유형) |> 
  ungroup() |> 
  count(중앙지자체, 사무유형, name = "법령건수", sort = TRUE) |> 
mutate(비율 = 법령건수 / sum(법령건수)) |> 
  gt(groupname_col = "중앙지자체",
     rowname_col   = "사무유형"  ) |> 
  fmt_percent(비율, decimals = 1) |>
  fmt_integer(법령건수) |>
  grand_summary_rows(
    fns = list(
      "합계" = ~sum(.)),
    columns = c("법령건수"),
    formatter = fmt_integer
  ) |> 
  grand_summary_rows(
    fns = list(
      "합계" = ~sum(.)),
    columns = c("비율"),
    formatter = fmt_percent,
    decimals = 0
  ) |> 
  gt_theme_538()  
```

::: {.cell-output-display}
```{=html}
<div id="gbibtcyvns" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>@import url("https://fonts.googleapis.com/css2?family=Chivo:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
@import url("https://fonts.googleapis.com/css2?family=Chivo:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
@import url("https://fonts.googleapis.com/css2?family=Chivo:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
@import url("https://fonts.googleapis.com/css2?family=Cairo:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
#gbibtcyvns table {
  font-family: Cairo, system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#gbibtcyvns thead, #gbibtcyvns tbody, #gbibtcyvns tfoot, #gbibtcyvns tr, #gbibtcyvns td, #gbibtcyvns th {
  border-style: none;
}

#gbibtcyvns p {
  margin: 0;
  padding: 0;
}

#gbibtcyvns .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: 400;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: none;
  border-top-width: 3px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#gbibtcyvns .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#gbibtcyvns .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#gbibtcyvns .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#gbibtcyvns .gt_heading {
  background-color: #FFFFFF;
  text-align: left;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gbibtcyvns .gt_bottom_border {
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gbibtcyvns .gt_col_headings {
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #000000;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gbibtcyvns .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#gbibtcyvns .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#gbibtcyvns .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#gbibtcyvns .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#gbibtcyvns .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #000000;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#gbibtcyvns .gt_spanner_row {
  border-bottom-style: hidden;
}

#gbibtcyvns .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #000000;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#gbibtcyvns .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #000000;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #FFFFFF;
  vertical-align: middle;
}

#gbibtcyvns .gt_from_md > :first-child {
  margin-top: 0;
}

#gbibtcyvns .gt_from_md > :last-child {
  margin-bottom: 0;
}

#gbibtcyvns .gt_row {
  padding-top: 3px;
  padding-bottom: 3px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#gbibtcyvns .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 0px;
  border-right-color: #FFFFFF;
  padding-left: 5px;
  padding-right: 5px;
}

#gbibtcyvns .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#gbibtcyvns .gt_row_group_first td {
  border-top-width: 2px;
}

#gbibtcyvns .gt_row_group_first th {
  border-top-width: 2px;
}

#gbibtcyvns .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gbibtcyvns .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#gbibtcyvns .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#gbibtcyvns .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gbibtcyvns .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gbibtcyvns .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#gbibtcyvns .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#gbibtcyvns .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#gbibtcyvns .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gbibtcyvns .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gbibtcyvns .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#gbibtcyvns .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gbibtcyvns .gt_sourcenote {
  font-size: 12px;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#gbibtcyvns .gt_left {
  text-align: left;
}

#gbibtcyvns .gt_center {
  text-align: center;
}

#gbibtcyvns .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#gbibtcyvns .gt_font_normal {
  font-weight: normal;
}

#gbibtcyvns .gt_font_bold {
  font-weight: bold;
}

#gbibtcyvns .gt_font_italic {
  font-style: italic;
}

#gbibtcyvns .gt_super {
  font-size: 65%;
}

#gbibtcyvns .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#gbibtcyvns .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#gbibtcyvns .gt_indent_1 {
  text-indent: 5px;
}

#gbibtcyvns .gt_indent_2 {
  text-indent: 10px;
}

#gbibtcyvns .gt_indent_3 {
  text-indent: 15px;
}

#gbibtcyvns .gt_indent_4 {
  text-indent: 20px;
}

#gbibtcyvns .gt_indent_5 {
  text-indent: 25px;
}

tbody tr:last-child {
  border-bottom: 2px solid #ffffff00;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=""></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="border-top-width: 0px; border-top-style: solid; border-top-color: black; font-family: Chivo; font-size: 14px; vertical-align: bottom; font-weight: 200; text-transform: uppercase;" scope="col" id="법령건수">법령건수</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="border-top-width: 0px; border-top-style: solid; border-top-color: black; font-family: Chivo; font-size: 14px; vertical-align: bottom; font-weight: 200; text-transform: uppercase;" scope="col" id="비율">비율</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: black;" scope="colgroup" id="없음">없음</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_1" scope="row" class="gt_row gt_left gt_stub">없음</th>
<td headers="없음 stub_1_1 법령건수" class="gt_row gt_right">4,323</td>
<td headers="없음 stub_1_1 비율" class="gt_row gt_right">40.2%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: black;" scope="colgroup" id="중앙정부">중앙정부</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_2" scope="row" class="gt_row gt_left gt_stub">국가</th>
<td headers="중앙정부 stub_1_2 법령건수" class="gt_row gt_right">3,112</td>
<td headers="중앙정부 stub_1_2 비율" class="gt_row gt_right">28.9%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: black;" scope="colgroup" id="공동">공동</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_3" scope="row" class="gt_row gt_left gt_stub">국가-시도-시군구</th>
<td headers="공동 stub_1_3 법령건수" class="gt_row gt_right">974</td>
<td headers="공동 stub_1_3 비율" class="gt_row gt_right">9.0%</td></tr>
    <tr><th id="stub_1_4" scope="row" class="gt_row gt_left gt_stub">국가-시도</th>
<td headers="공동 stub_1_4 법령건수" class="gt_row gt_right">424</td>
<td headers="공동 stub_1_4 비율" class="gt_row gt_right">3.9%</td></tr>
    <tr><th id="stub_1_5" scope="row" class="gt_row gt_left gt_stub">국가-시군구</th>
<td headers="공동 stub_1_5 법령건수" class="gt_row gt_right">99</td>
<td headers="공동 stub_1_5 비율" class="gt_row gt_right">0.9%</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" style="border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: black;" scope="colgroup" id="지자체">지자체</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_6" scope="row" class="gt_row gt_left gt_stub">시도</th>
<td headers="지자체 stub_1_6 법령건수" class="gt_row gt_right">694</td>
<td headers="지자체 stub_1_6 비율" class="gt_row gt_right">6.4%</td></tr>
    <tr><th id="stub_1_7" scope="row" class="gt_row gt_left gt_stub">시도-시군구</th>
<td headers="지자체 stub_1_7 법령건수" class="gt_row gt_right">617</td>
<td headers="지자체 stub_1_7 비율" class="gt_row gt_right">5.7%</td></tr>
    <tr><th id="stub_1_8" scope="row" class="gt_row gt_left gt_stub">시군구</th>
<td headers="지자체 stub_1_8 법령건수" class="gt_row gt_right">524</td>
<td headers="지자체 stub_1_8 비율" class="gt_row gt_right">4.9%</td></tr>
    <tr><th id="grand_summary_stub_1" scope="row" class="gt_row gt_left gt_stub gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row">합계</th>
<td headers="grand_summary_stub_1 법령건수" class="gt_row gt_right gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row">10,767</td>
<td headers="grand_summary_stub_1 비율" class="gt_row gt_right gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row">100%</td></tr>
  </tbody>
  
  
</table>
</div>
```
:::
:::


#### 그래프 {.unnumbered}

[Venn diagram with ggVennDiagram](https://r-charts.com/part-whole/ggvenndiagram/)


::: {.cell}

```{.r .cell-code}
library(ggVennDiagram)


affairs_type <- law_tbl |>
  filter(사무유형 != "없음") |>
  group_by(법령명) |>
  count(사무유형) |>
  arrange(법령명, 사무유형) |>
  ungroup() |>
  separate(사무유형, into = c("사무1", "사무2", "사무3"), sep = "-") |>
  mutate(번호 = row_number()) |> 
  mutate(사무1 = str_glue("{사무1}_{번호}"),
         사무2 = str_glue("{사무2}_{번호}"),
         사무3 = str_glue("{사무3}_{번호}")) |> 
  select(-법령명, -n, -번호) |> 
  pivot_longer(사무1:사무3, names_to = "사무구분", values_to = "사무") |> 
  filter(!str_detect(사무, "^NA")) |> 
  mutate(집합 = case_when(str_detect(사무, "국가") ~ "국가",
                          str_detect(사무, "시도") ~ "시도",
                          str_detect(사무, "시군구") ~ "시군구",
                          TRUE ~ "기타")) |> 
  mutate(원소 = str_extract(사무, "\\d{1,5}"))

    
korea_type <- affairs_type |> 
  filter(집합 == "국가") |> 
  pull(원소) |> 
  unlist()

sido_type <- affairs_type |> 
  filter(집합 == "시도") |> 
  pull(원소) |> 
  unlist()

sgg_type <- affairs_type |> 
  filter(집합 == "시군구") |> 
  pull(원소) |> 
  unlist()

ggVennDiagram(list(국가 = korea_type, 시도 = sido_type, 시군구 = sgg_type),
              category.names = c("국가",
                                 "시도",
                                 "구시군"),
               label_alpha = 0,
               color = 1, lwd = 0.7) +
  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF") +
  theme(legend.position = "none")
```

::: {.cell-output-display}
![](EDA_files/figure-html/unnamed-chunk-9-1.png){width=672}
:::
:::


:::

### 국가사무 유형



::: {.cell}

```{.r .cell-code}
law_tbl |>
  filter(사무유형 != "없음") |>
  group_by(법령명) |>
  count(사무유형) |>
  arrange(법령명, 사무유형) |> 
  summarise(사무패턴 = str_c(사무유형, collapse = "; ")) |> 
  count(사무패턴, name = "법령건수", sort = TRUE) |> 
  gt() |> 
  fmt_integer(법령건수) |> 
  grand_summary_rows(
    fns = list(
      "합계" = ~sum(.)),
    columns = c("법령건수"),
    formatter = fmt_integer
  ) |> 
  gt_theme_538()
```

::: {.cell-output-display}
```{=html}
<div id="fvhofeibej" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>@import url("https://fonts.googleapis.com/css2?family=Chivo:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
@import url("https://fonts.googleapis.com/css2?family=Chivo:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
@import url("https://fonts.googleapis.com/css2?family=Chivo:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
@import url("https://fonts.googleapis.com/css2?family=Cairo:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
#fvhofeibej table {
  font-family: Cairo, system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#fvhofeibej thead, #fvhofeibej tbody, #fvhofeibej tfoot, #fvhofeibej tr, #fvhofeibej td, #fvhofeibej th {
  border-style: none;
}

#fvhofeibej p {
  margin: 0;
  padding: 0;
}

#fvhofeibej .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: 400;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: none;
  border-top-width: 3px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#fvhofeibej .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#fvhofeibej .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#fvhofeibej .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#fvhofeibej .gt_heading {
  background-color: #FFFFFF;
  text-align: left;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fvhofeibej .gt_bottom_border {
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fvhofeibej .gt_col_headings {
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #000000;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fvhofeibej .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#fvhofeibej .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#fvhofeibej .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#fvhofeibej .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#fvhofeibej .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #000000;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#fvhofeibej .gt_spanner_row {
  border-bottom-style: hidden;
}

#fvhofeibej .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #000000;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#fvhofeibej .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #000000;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #FFFFFF;
  vertical-align: middle;
}

#fvhofeibej .gt_from_md > :first-child {
  margin-top: 0;
}

#fvhofeibej .gt_from_md > :last-child {
  margin-bottom: 0;
}

#fvhofeibej .gt_row {
  padding-top: 3px;
  padding-bottom: 3px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#fvhofeibej .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 0px;
  border-right-color: #FFFFFF;
  padding-left: 5px;
  padding-right: 5px;
}

#fvhofeibej .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#fvhofeibej .gt_row_group_first td {
  border-top-width: 2px;
}

#fvhofeibej .gt_row_group_first th {
  border-top-width: 2px;
}

#fvhofeibej .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fvhofeibej .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#fvhofeibej .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#fvhofeibej .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fvhofeibej .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fvhofeibej .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#fvhofeibej .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#fvhofeibej .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#fvhofeibej .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fvhofeibej .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fvhofeibej .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#fvhofeibej .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fvhofeibej .gt_sourcenote {
  font-size: 12px;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#fvhofeibej .gt_left {
  text-align: left;
}

#fvhofeibej .gt_center {
  text-align: center;
}

#fvhofeibej .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#fvhofeibej .gt_font_normal {
  font-weight: normal;
}

#fvhofeibej .gt_font_bold {
  font-weight: bold;
}

#fvhofeibej .gt_font_italic {
  font-style: italic;
}

#fvhofeibej .gt_super {
  font-size: 65%;
}

#fvhofeibej .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#fvhofeibej .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#fvhofeibej .gt_indent_1 {
  text-indent: 5px;
}

#fvhofeibej .gt_indent_2 {
  text-indent: 10px;
}

#fvhofeibej .gt_indent_3 {
  text-indent: 15px;
}

#fvhofeibej .gt_indent_4 {
  text-indent: 20px;
}

#fvhofeibej .gt_indent_5 {
  text-indent: 25px;
}

tbody tr:last-child {
  border-bottom: 2px solid #ffffff00;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id=""></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="border-top-width: 0px; border-top-style: solid; border-top-color: black; font-family: Chivo; font-size: 14px; vertical-align: bottom; font-weight: 200; text-transform: uppercase;" scope="col" id="사무패턴">사무패턴</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="border-top-width: 0px; border-top-style: solid; border-top-color: black; font-family: Chivo; font-size: 14px; vertical-align: bottom; font-weight: 200; text-transform: uppercase;" scope="col" id="법령건수">법령건수</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><th id="stub_1_1" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_1 사무패턴" class="gt_row gt_left">국가</td>
<td headers="stub_1_1 법령건수" class="gt_row gt_right">1,742</td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_2 사무패턴" class="gt_row gt_left">국가; 국가-시도-시군구</td>
<td headers="stub_1_2 법령건수" class="gt_row gt_right">296</td></tr>
    <tr><th id="stub_1_3" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_3 사무패턴" class="gt_row gt_left">국가; 시도</td>
<td headers="stub_1_3 법령건수" class="gt_row gt_right">98</td></tr>
    <tr><th id="stub_1_4" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_4 사무패턴" class="gt_row gt_left">국가; 국가-시도-시군구; 시도-시군구</td>
<td headers="stub_1_4 법령건수" class="gt_row gt_right">85</td></tr>
    <tr><th id="stub_1_5" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_5 사무패턴" class="gt_row gt_left">국가; 국가-시도; 국가-시도-시군구; 시군구; 시도; 시도-시군구</td>
<td headers="stub_1_5 법령건수" class="gt_row gt_right">83</td></tr>
    <tr><th id="stub_1_6" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_6 사무패턴" class="gt_row gt_left">국가; 시도-시군구</td>
<td headers="stub_1_6 법령건수" class="gt_row gt_right">80</td></tr>
    <tr><th id="stub_1_7" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_7 사무패턴" class="gt_row gt_left">국가; 국가-시도-시군구; 시군구; 시도; 시도-시군구</td>
<td headers="stub_1_7 법령건수" class="gt_row gt_right">65</td></tr>
    <tr><th id="stub_1_8" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_8 사무패턴" class="gt_row gt_left">국가; 시군구</td>
<td headers="stub_1_8 법령건수" class="gt_row gt_right">56</td></tr>
    <tr><th id="stub_1_9" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_9 사무패턴" class="gt_row gt_left">국가; 국가-시도; 국가-시도-시군구; 시도; 시도-시군구</td>
<td headers="stub_1_9 법령건수" class="gt_row gt_right">47</td></tr>
    <tr><th id="stub_1_10" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_10 사무패턴" class="gt_row gt_left">국가; 국가-시도</td>
<td headers="stub_1_10 법령건수" class="gt_row gt_right">44</td></tr>
    <tr><th id="stub_1_11" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_11 사무패턴" class="gt_row gt_left">국가; 국가-시도-시군구; 시도; 시도-시군구</td>
<td headers="stub_1_11 법령건수" class="gt_row gt_right">40</td></tr>
    <tr><th id="stub_1_12" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_12 사무패턴" class="gt_row gt_left">국가; 국가-시도-시군구; 시도</td>
<td headers="stub_1_12 법령건수" class="gt_row gt_right">38</td></tr>
    <tr><th id="stub_1_13" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_13 사무패턴" class="gt_row gt_left">국가; 국가-시도; 시도</td>
<td headers="stub_1_13 법령건수" class="gt_row gt_right">38</td></tr>
    <tr><th id="stub_1_14" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_14 사무패턴" class="gt_row gt_left">국가; 국가-시도; 국가-시도-시군구; 시도</td>
<td headers="stub_1_14 법령건수" class="gt_row gt_right">36</td></tr>
    <tr><th id="stub_1_15" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_15 사무패턴" class="gt_row gt_left">시도</td>
<td headers="stub_1_15 법령건수" class="gt_row gt_right">36</td></tr>
    <tr><th id="stub_1_16" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_16 사무패턴" class="gt_row gt_left">국가-시도-시군구</td>
<td headers="stub_1_16 법령건수" class="gt_row gt_right">34</td></tr>
    <tr><th id="stub_1_17" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_17 사무패턴" class="gt_row gt_left">국가; 국가-시도; 국가-시도-시군구</td>
<td headers="stub_1_17 법령건수" class="gt_row gt_right">32</td></tr>
    <tr><th id="stub_1_18" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_18 사무패턴" class="gt_row gt_left">국가; 시군구; 시도</td>
<td headers="stub_1_18 법령건수" class="gt_row gt_right">32</td></tr>
    <tr><th id="stub_1_19" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_19 사무패턴" class="gt_row gt_left">시도-시군구</td>
<td headers="stub_1_19 법령건수" class="gt_row gt_right">29</td></tr>
    <tr><th id="stub_1_20" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_20 사무패턴" class="gt_row gt_left">시군구</td>
<td headers="stub_1_20 법령건수" class="gt_row gt_right">28</td></tr>
    <tr><th id="stub_1_21" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_21 사무패턴" class="gt_row gt_left">국가; 국가-시도-시군구; 시군구</td>
<td headers="stub_1_21 법령건수" class="gt_row gt_right">27</td></tr>
    <tr><th id="stub_1_22" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_22 사무패턴" class="gt_row gt_left">국가; 국가-시도-시군구; 시군구; 시도-시군구</td>
<td headers="stub_1_22 법령건수" class="gt_row gt_right">26</td></tr>
    <tr><th id="stub_1_23" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_23 사무패턴" class="gt_row gt_left">국가; 국가-시도-시군구; 시군구; 시도</td>
<td headers="stub_1_23 법령건수" class="gt_row gt_right">24</td></tr>
    <tr><th id="stub_1_24" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_24 사무패턴" class="gt_row gt_left">국가; 시도; 시도-시군구</td>
<td headers="stub_1_24 법령건수" class="gt_row gt_right">20</td></tr>
    <tr><th id="stub_1_25" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_25 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 국가-시도; 국가-시도-시군구; 시군구; 시도; 시도-시군구</td>
<td headers="stub_1_25 법령건수" class="gt_row gt_right">17</td></tr>
    <tr><th id="stub_1_26" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_26 사무패턴" class="gt_row gt_left">국가-시도</td>
<td headers="stub_1_26 법령건수" class="gt_row gt_right">16</td></tr>
    <tr><th id="stub_1_27" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_27 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 국가-시도-시군구; 시군구; 시도; 시도-시군구</td>
<td headers="stub_1_27 법령건수" class="gt_row gt_right">16</td></tr>
    <tr><th id="stub_1_28" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_28 사무패턴" class="gt_row gt_left">국가; 국가-시도; 국가-시도-시군구; 시군구; 시도</td>
<td headers="stub_1_28 법령건수" class="gt_row gt_right">16</td></tr>
    <tr><th id="stub_1_29" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_29 사무패턴" class="gt_row gt_left">국가; 국가-시도; 국가-시도-시군구; 시도-시군구</td>
<td headers="stub_1_29 법령건수" class="gt_row gt_right">16</td></tr>
    <tr><th id="stub_1_30" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_30 사무패턴" class="gt_row gt_left">국가; 시군구; 시도-시군구</td>
<td headers="stub_1_30 법령건수" class="gt_row gt_right">16</td></tr>
    <tr><th id="stub_1_31" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_31 사무패턴" class="gt_row gt_left">국가; 시군구; 시도; 시도-시군구</td>
<td headers="stub_1_31 법령건수" class="gt_row gt_right">12</td></tr>
    <tr><th id="stub_1_32" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_32 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 국가-시도-시군구; 시군구</td>
<td headers="stub_1_32 법령건수" class="gt_row gt_right">9</td></tr>
    <tr><th id="stub_1_33" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_33 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 시군구</td>
<td headers="stub_1_33 법령건수" class="gt_row gt_right">9</td></tr>
    <tr><th id="stub_1_34" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_34 사무패턴" class="gt_row gt_left">국가; 국가-시도; 국가-시도-시군구; 시군구</td>
<td headers="stub_1_34 법령건수" class="gt_row gt_right">9</td></tr>
    <tr><th id="stub_1_35" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_35 사무패턴" class="gt_row gt_left">국가; 국가-시도; 국가-시도-시군구; 시군구; 시도-시군구</td>
<td headers="stub_1_35 법령건수" class="gt_row gt_right">9</td></tr>
    <tr><th id="stub_1_36" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_36 사무패턴" class="gt_row gt_left">국가; 국가-시도; 시군구; 시도</td>
<td headers="stub_1_36 법령건수" class="gt_row gt_right">9</td></tr>
    <tr><th id="stub_1_37" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_37 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 국가-시도-시군구; 시군구; 시도</td>
<td headers="stub_1_37 법령건수" class="gt_row gt_right">7</td></tr>
    <tr><th id="stub_1_38" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_38 사무패턴" class="gt_row gt_left">국가; 국가-시도; 시군구; 시도; 시도-시군구</td>
<td headers="stub_1_38 법령건수" class="gt_row gt_right">7</td></tr>
    <tr><th id="stub_1_39" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_39 사무패턴" class="gt_row gt_left">시군구; 시도</td>
<td headers="stub_1_39 법령건수" class="gt_row gt_right">7</td></tr>
    <tr><th id="stub_1_40" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_40 사무패턴" class="gt_row gt_left">국가-시도; 시도</td>
<td headers="stub_1_40 법령건수" class="gt_row gt_right">6</td></tr>
    <tr><th id="stub_1_41" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_41 사무패턴" class="gt_row gt_left">국가-시도; 국가-시도-시군구; 시도</td>
<td headers="stub_1_41 법령건수" class="gt_row gt_right">5</td></tr>
    <tr><th id="stub_1_42" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_42 사무패턴" class="gt_row gt_left">국가; 국가-시군구</td>
<td headers="stub_1_42 법령건수" class="gt_row gt_right">5</td></tr>
    <tr><th id="stub_1_43" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_43 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 국가-시도-시군구; 시군구; 시도-시군구</td>
<td headers="stub_1_43 법령건수" class="gt_row gt_right">5</td></tr>
    <tr><th id="stub_1_44" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_44 사무패턴" class="gt_row gt_left">국가; 국가-시도; 시도; 시도-시군구</td>
<td headers="stub_1_44 법령건수" class="gt_row gt_right">5</td></tr>
    <tr><th id="stub_1_45" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_45 사무패턴" class="gt_row gt_left">국가-시도-시군구; 시군구</td>
<td headers="stub_1_45 법령건수" class="gt_row gt_right">4</td></tr>
    <tr><th id="stub_1_46" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_46 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 국가-시도; 국가-시도-시군구; 시군구; 시도</td>
<td headers="stub_1_46 법령건수" class="gt_row gt_right">4</td></tr>
    <tr><th id="stub_1_47" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_47 사무패턴" class="gt_row gt_left">시군구; 시도; 시도-시군구</td>
<td headers="stub_1_47 법령건수" class="gt_row gt_right">4</td></tr>
    <tr><th id="stub_1_48" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_48 사무패턴" class="gt_row gt_left">국가-시도-시군구; 시도</td>
<td headers="stub_1_48 법령건수" class="gt_row gt_right">3</td></tr>
    <tr><th id="stub_1_49" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_49 사무패턴" class="gt_row gt_left">국가-시도-시군구; 시도-시군구</td>
<td headers="stub_1_49 법령건수" class="gt_row gt_right">3</td></tr>
    <tr><th id="stub_1_50" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_50 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 시군구; 시도-시군구</td>
<td headers="stub_1_50 법령건수" class="gt_row gt_right">3</td></tr>
    <tr><th id="stub_1_51" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_51 사무패턴" class="gt_row gt_left">국가; 국가-시도; 시도-시군구</td>
<td headers="stub_1_51 법령건수" class="gt_row gt_right">3</td></tr>
    <tr><th id="stub_1_52" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_52 사무패턴" class="gt_row gt_left">시군구; 시도-시군구</td>
<td headers="stub_1_52 법령건수" class="gt_row gt_right">3</td></tr>
    <tr><th id="stub_1_53" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_53 사무패턴" class="gt_row gt_left">시도; 시도-시군구</td>
<td headers="stub_1_53 법령건수" class="gt_row gt_right">3</td></tr>
    <tr><th id="stub_1_54" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_54 사무패턴" class="gt_row gt_left">국가-시군구</td>
<td headers="stub_1_54 법령건수" class="gt_row gt_right">2</td></tr>
    <tr><th id="stub_1_55" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_55 사무패턴" class="gt_row gt_left">국가-시도-시군구; 시군구; 시도; 시도-시군구</td>
<td headers="stub_1_55 법령건수" class="gt_row gt_right">2</td></tr>
    <tr><th id="stub_1_56" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_56 사무패턴" class="gt_row gt_left">국가-시도; 시도-시군구</td>
<td headers="stub_1_56 법령건수" class="gt_row gt_right">2</td></tr>
    <tr><th id="stub_1_57" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_57 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 국가-시도-시군구</td>
<td headers="stub_1_57 법령건수" class="gt_row gt_right">2</td></tr>
    <tr><th id="stub_1_58" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_58 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 국가-시도-시군구; 시도-시군구</td>
<td headers="stub_1_58 법령건수" class="gt_row gt_right">2</td></tr>
    <tr><th id="stub_1_59" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_59 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 국가-시도; 국가-시도-시군구; 시도-시군구</td>
<td headers="stub_1_59 법령건수" class="gt_row gt_right">2</td></tr>
    <tr><th id="stub_1_60" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_60 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 국가-시도; 국가-시도-시군구; 시도; 시도-시군구</td>
<td headers="stub_1_60 법령건수" class="gt_row gt_right">2</td></tr>
    <tr><th id="stub_1_61" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_61 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 국가-시도; 시군구; 시도; 시도-시군구</td>
<td headers="stub_1_61 법령건수" class="gt_row gt_right">2</td></tr>
    <tr><th id="stub_1_62" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_62 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 시군구; 시도</td>
<td headers="stub_1_62 법령건수" class="gt_row gt_right">2</td></tr>
    <tr><th id="stub_1_63" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_63 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 시도</td>
<td headers="stub_1_63 법령건수" class="gt_row gt_right">2</td></tr>
    <tr><th id="stub_1_64" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_64 사무패턴" class="gt_row gt_left">국가; 국가-시도; 시군구</td>
<td headers="stub_1_64 법령건수" class="gt_row gt_right">2</td></tr>
    <tr><th id="stub_1_65" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_65 사무패턴" class="gt_row gt_left">국가; 국가-시도; 시군구; 시도-시군구</td>
<td headers="stub_1_65 법령건수" class="gt_row gt_right">2</td></tr>
    <tr><th id="stub_1_66" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_66 사무패턴" class="gt_row gt_left">국가-시도-시군구; 시군구; 시도-시군구</td>
<td headers="stub_1_66 법령건수" class="gt_row gt_right">1</td></tr>
    <tr><th id="stub_1_67" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_67 사무패턴" class="gt_row gt_left">국가-시도-시군구; 시도; 시도-시군구</td>
<td headers="stub_1_67 법령건수" class="gt_row gt_right">1</td></tr>
    <tr><th id="stub_1_68" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_68 사무패턴" class="gt_row gt_left">국가-시도; 국가-시도-시군구</td>
<td headers="stub_1_68 법령건수" class="gt_row gt_right">1</td></tr>
    <tr><th id="stub_1_69" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_69 사무패턴" class="gt_row gt_left">국가-시도; 국가-시도-시군구; 시군구</td>
<td headers="stub_1_69 법령건수" class="gt_row gt_right">1</td></tr>
    <tr><th id="stub_1_70" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_70 사무패턴" class="gt_row gt_left">국가-시도; 국가-시도-시군구; 시도-시군구</td>
<td headers="stub_1_70 법령건수" class="gt_row gt_right">1</td></tr>
    <tr><th id="stub_1_71" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_71 사무패턴" class="gt_row gt_left">국가-시도; 시군구</td>
<td headers="stub_1_71 법령건수" class="gt_row gt_right">1</td></tr>
    <tr><th id="stub_1_72" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_72 사무패턴" class="gt_row gt_left">국가-시도; 시군구; 시도-시군구</td>
<td headers="stub_1_72 법령건수" class="gt_row gt_right">1</td></tr>
    <tr><th id="stub_1_73" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_73 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 국가-시도-시군구; 시도</td>
<td headers="stub_1_73 법령건수" class="gt_row gt_right">1</td></tr>
    <tr><th id="stub_1_74" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_74 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 국가-시도; 국가-시도-시군구</td>
<td headers="stub_1_74 법령건수" class="gt_row gt_right">1</td></tr>
    <tr><th id="stub_1_75" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_75 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 국가-시도; 국가-시도-시군구; 시도</td>
<td headers="stub_1_75 법령건수" class="gt_row gt_right">1</td></tr>
    <tr><th id="stub_1_76" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_76 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 국가-시도; 시군구</td>
<td headers="stub_1_76 법령건수" class="gt_row gt_right">1</td></tr>
    <tr><th id="stub_1_77" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_77 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 국가-시도; 시군구; 시도</td>
<td headers="stub_1_77 법령건수" class="gt_row gt_right">1</td></tr>
    <tr><th id="stub_1_78" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_78 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 국가-시도; 시도</td>
<td headers="stub_1_78 법령건수" class="gt_row gt_right">1</td></tr>
    <tr><th id="stub_1_79" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_79 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 시군구; 시도; 시도-시군구</td>
<td headers="stub_1_79 법령건수" class="gt_row gt_right">1</td></tr>
    <tr><th id="stub_1_80" scope="row" class="gt_row gt_left gt_stub"></th>
<td headers="stub_1_80 사무패턴" class="gt_row gt_left">국가; 국가-시군구; 시도-시군구</td>
<td headers="stub_1_80 법령건수" class="gt_row gt_right">1</td></tr>
    <tr><th id="grand_summary_stub_1" scope="row" class="gt_row gt_left gt_stub gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row">합계</th>
<td headers="grand_summary_stub_1 사무패턴" class="gt_row gt_left gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row">—</td>
<td headers="grand_summary_stub_1 법령건수" class="gt_row gt_right gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row">3,306</td></tr>
  </tbody>
  
  
</table>
</div>
```
:::
:::



# 법령

법령은 법률 + 시행령 + 시행규칙 + ~~행정규칙~~ 으로 구성된다.


::: {.cell}

```{.r .cell-code}
law_type <- law_tbl |>
  group_by(법령명) |>
  summarise(조문수 = n()) |> 
  mutate(법령구분 = case_when(
    str_detect(법령명, "법률$") ~ "법률",
    str_detect(법령명, "시행령$") ~ "시행령",
    str_detect(법령명, "시행규칙$") ~ "시행규칙",
    TRUE ~ "기타"
  )) 

law_type |> 
  count(법령구분)
#> # A tibble: 4 × 2
#>   법령구분     n
#>   <chr>    <int>
#> 1 기타      1518
#> 2 법률       528
#> 3 시행규칙   935
#> 4 시행령    1343
```
:::

