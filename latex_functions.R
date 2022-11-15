#' Write header.tex file
#'
#' Overwrite header.tex in the rmd folder. This is needed because inline R code
#' is not allowed in .tex files. This work-around allows for dynamic
#' headers and footers.'
#'
#' @param time_compiled A datetime object
#'
#' @return header.tex in the rmd folder
#'
#' @export

write_header <- function(time_compiled, rmd_path) {

  book_name <- case_when(params$book == "prelim" ~ "Preliminary Budget",
                         params$book == "exec" ~ "Executive Summary",
                         params$book == "sota" ~ "Summary of the Adopted Budget")

  book_name <- paste0("Fiscal 20", params$fy, " ", book_name)

  header <- paste0(
    "\\usepackage{xcolor}
    \\usepackage{sectsty}
    \\usepackage{multicol}
    \\usepackage{booktabs}
    \\usepackage{longtable}
    \\usepackage{array}
    \\usepackage{fancyhdr}
    \\usepackage{makecell}
    \\usepackage{multirow}
    \\usepackage{placeins}
    \\usepackage{afterpage}
    \\usepackage{wrapfig}
    \\usepackage{float}
    \\usepackage{colortbl}
    \\usepackage{tabu}
    \\usepackage{threeparttable}
    \\usepackage{threeparttablex}
    \\usepackage[normalem]{ulem}
    \\usepackage{makecell}
    \\usepackage{titlesec}
    \\usepackage{fontspec}
    \\usepackage{microtype} % improves line breaks, justification
    \\usepackage{wrapfig} % allows for wrapping text around tables and figures
    \\usepackage{lscape}
    \\usepackage{pdfpages} % allows for insertion of multi-page pdfs
    \\usepackage{graphicx}
    \\pagestyle{fancy}
    \\setlength{\\headheight}{15pt}

    % hyperref is already loaded in default LaTex template
    % adding option to turn off guessable names for links, forcing it to instead
    % rely on consecutive identifers; this is more accurate for creating PDF bookmarks
    \\PassOptionsToPackage{hypertexnames=false}{hyperref}

    % change header/footer font size
    \\newcommand{\\changefont}{%
        \\fontsize{10}{12}\\selectfont
    }

  % No headers on empty pages before new chapter
  % http://www.markschenk.com/tensegrity/latexexplanation.html
    \\makeatletter
    \\def\\cleardoublepage{\\clearpage\\if@twoside \\ifodd\\c@page\\else
    \\thispagestyle{empty}
    \\hspace{0pt}\\vfill
    \\begin{center} Intentionally left blank.\\end{center}
    \\vfill\\hspace{0pt}
    \\newpage
    \\if@twocolumn\\hbox{}\\newpage\\fi\\fi\\fi}
    \\makeatother \\clearpage{\\pagestyle{empty}\\cleardoublepage}

    % https://stackoverflow.com/questions/40982836/latex-multicolumn-block-in-pandoc-markdown
    \\newcommand{\\hideFromPandoc}[1]{#1}
    \\hideFromPandoc{
      \\let\\Begin\\begin
      \\let\\End\\end
      }
    \\setlength{\\columnsep}{2cm}

    \\fancyhead[LE]{\\changefont\\leftmark}
    \\fancyhead[LO]{\\changefont{", book_name ,"}}
    \\fancyhead[RE]{\\changefont{", book_name ,"}}
    \\fancyhead[RO]{\\changefont\\leftmark}

    % change page number size
    \\fancyfoot[C]{\\changefont\\thepage}")

    if (identical(params$draft, TRUE)) {
      header <- paste0(
        header,
        "
        \\fancyfoot[L]{Draft generated ", time_compiled ,"}")
    }

    header <- paste0(header,

    "
    \\setlength{\\columnseprule}{1pt}

    \\definecolor{color_header}{RGB}{", params$color_header, "}
    \\definecolor{color_table_header}{RGB}{", params$color_table_header, "}
    \\definecolor{color_table_header}{RGB}{", params$color_table_header_font, "}

    % \\definecolor{color_table_total}{RGB}{", params$color_table_total, "}
    % \\definecolor{color_table_total_font}{RGB}{", params$color_table_total_font, "}

    \\definecolor{color_table_subtotal}{RGB}{", params$color_table_subtotal, "}
    \\definecolor{color_divider}{RGB}{", params$color_divider, "}

    % equivalent of #

    \\assignpagestyle{\\chapter}{empty}

    % set new HUGE command for divider pages %
    \\newcommand\\HUGE{\\fontsize{60}{10}\\selectfont}

    \\titleformat{\\chapter}
      {\\normalfont\\fontsize{5}{3}\\bfseries\\color{white}}{\\thechapter}{1em}{}

    % equivalent of ##

    \\titleclass{\\section}{top} % start each section on a new page
    \\newcommand{\\sectionbreak}{\\clearpage}

    \\titlespacing*{\\section}{0pt}{-2\\baselineskip}{1em}

    \\titleformat{name=\\section,page=odd}{\\LARGE\\bfseries\\raggedleft\\color{color_header}}{\\thesection}{1em}{}[{\\titlerule[1.5pt]}]
    \\titleformat{name=\\section,page=even}{\\LARGE\\bfseries\\raggedright\\color{color_header}}{\\thesection}{1em}{}[{\\titlerule[1.5pt]}]

    % equivalent of ###
    \\titleformat{\\subsection}
      {\\normalfont\\Large\\bfseries\\color{color_header}}{\\thesubsection}{1em}{}

    % equivalent of ###
    \\titleformat{\\subsubsection}
      {\\normalfont\\Large\\color{color_header}}{\\thesubsubsection}{1em}{}

    % remove title pages before Table of Contents
    \\AtBeginDocument{\\let\\maketitle\\relax}

    % rename TOC from Contents to Table of Contents
    \\renewcommand{\\contentsname}{Table of Contents}

    % prevent page/column stretching by allowing odd and even bottoms of pages/columns to not match
    \\raggedbottom
    \\raggedcolumns

    % allow for roman numerals before Intro section
    \\frontmatter")

    writeLines(header,  paste0(rmd_path, "header.tex"))
}


#' Write before_body.tex file
#'
#' Overwrite before_body.tex in the folder specified by rmd_path. This is needed because inline R code
#' is not allowed in .tex files. This work-around allows for a dynamic cover page, which has information for
#' analysts and editors, that stands in for the illustrated cover page.
#'
#' @param files
#' @param rmd_path
#' @param time_compiled A datetime object
#' @param data_dates A list
#'
#' @return before_body.tex in the rmd folder
#'
#' @importFrom lubridate today floor_date
#' @export

write_before_body <- function(files, rmd_path, time_compiled, data_dates) {

  if (identical(params$draft, TRUE)) {
    edited <- paste0(rmd_path, files) %>%
      file.mtime()

    edited <- ifelse(edited < today() - 3,
                     "\\textcolor{gray}{Over 3 days ago}", as.character(edited))

    files <- gsub(rmd_path, "", files) %>%
      gsub("_", "\\_", ., fixed = TRUE) # need to escape underscores for LaTeX

    edited <- paste0("\\textbf{", files, "}: ", edited)

    # only use two columns if there are more than 20 files included in this run
    if (length(files) > 20) {
      file_cols <- "\\Begin{multicols}{2}"
    } else {
      file_cols <- ""
    }

    # tally the number of files added by each chapter so that the column break can be placed after
    # 20 files has been listed
    num_of_files <- 0

    for (i in c("00\\_before\\_body/", "01\\_intro/", "02\\_budget\\_plan/",
                "03\\_fiscal\\_env/", "04\\_budgetary\\_process\\_policies/", "05\\_revenue/", "06\\_operating/",
                "07\\_capital/", "08\\_appendix/")) {
      # identify if there are any files in chapter i
      chapter_files <- edited[grep(i, edited, fixed = TRUE)]

      if (length(chapter_files > 0)) {

        # remove chapter folder from file path for cleaner presentation
        chapter_files <- gsub(i, "", chapter_files, fixed = TRUE)

        chapter_name <- switch(
          i, "00\\_before\\_body/" = "00: Before Body", "01\\_intro/" = "01: Introduction",
          "02\\_budget\\_plan/" = "02: Budget Plan",
          "03\\_fiscal\\_env/" = "03: Fiscal Environment", "04\\_budgetary\\_process\\_policies/" = "04: Budgetary Process and Policies",
          "05\\_revenue/" = "05: Revenue", "06\\_operating/" = "06: Operating",
          "07\\_capital/" = "07: Capital", "08\\_appendix/" = "08: Appendix")

        file_cols <- paste0(
          file_cols,
          "{\\small ", chapter_name, "}",

          "\\begin{itemize}
                  \\item \n", paste0(chapter_files, collapse = "\n \\item "),
          "\n \\end{itemize}")

        # add a column break if this chapter passes 25 files
        if (num_of_files < 20 & (num_of_files + length(chapter_files)) >= 20 & length(files) > 20) {

          file_cols <- paste0(file_cols, "\\vfill\\null
              \\columnbreak")
        }

        num_of_files <- num_of_files + length(chapter_files)
      }
    }

    if (length(files) > 20) {
      file_cols <- paste0(file_cols, "\\End{multicols}")
    }

    writeLines(
      paste0(
        "\\thispagestyle{empty}
          \\begin{center}

          {\\Large Fiscal 20", params$fy,
            case_when(
              identical(params$book, "prelim") ~ " Preliminary Draft",
              identical(params$book, "exec") ~ " Executive Summary Draft",
              identical(params$book, "sota") ~ " Summary of the Adopted Draft"), "}

          {\\large Draft generated ", time_compiled, "}

          \\end{center}

          {\\large Base Data Updated}

          \\begin{scriptsize}
          \\Begin{multicols}{2}

            \\begin{itemize}
              \\item \\textbf{Expenditure line items}: ", data_dates$expend, "
              \\item \\textbf{Expenditure positions}: ", data_dates$position, "
            \\end{itemize}

          \\vfill\\null
          \\columnbreak

            \\begin{itemize}
              \\item \\textbf{Revenue}: ", data_dates$revenue, "
              \\item \\textbf{Capital}: ", data_dates$capital, "
            \\end{itemize}

          \\End{multicols}
          \\end{scriptsize}

          {\\large Sections Last Edited}

          \\begin{scriptsize}",
            file_cols,
          "\\end{scriptsize}"),
      paste0(rmd_path, "before_body.tex"))
  } else {
    # if final version, don't include cover page
    writeLines("", paste0(rmd_path, "before_body.tex"))
  }

}

#' Format divider
#'
#' @param title A string, the chapter title
#'
#' @importFrom bookHelpers insert_blank_page
#' @export

format_divider <- function(title) {

  cat("\\clearpage")

  cat("\n\n#", title, "\n")

  cat("\\vfill")

  cat("\\null\\hfill{\\textcolor{color_divider}{\\textbf{{\\HUGE FISCAL}}}}\\par")
  cat("\\null\\hfill{\\textcolor{color_divider}{\\textbf{{\\HUGE 20",
      params$fy, "}}}}", sep = "")

  cat("\\vfill")

  cat("\\begin{center}")

  cat("\\textbf{{\\Huge",
      case_when(
        params$book == "prelim" ~ "PRELIMINARY BUDGET}}",
        params$book == "exec" ~ "EXECUTIVE SUMMARY\\newline\\null\\newline}}",
        params$book == "sota" ~ "SUMMARY OF THE \\newline ADOPTED BUDGET}}"))

  if (params$book == "exec") {
    cat("{\\Huge Board of Estimates Recommendations}")
  }

  cat("\\end{center}")

  cat("\\vfill")

  cat("\\null\\hfill{\\Huge", title, "}")

  cat("\\vfill\\clearpage")

  insert_blank_page()
}
