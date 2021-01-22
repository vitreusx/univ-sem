\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{psv}

\LoadClass[a4paper,12pt]{article}

\RequirePackage[margin=1in]{geometry}
\RequirePackage[utf8]{inputenc}
\frenchspacing

\RequirePackage{titling}
\setlength{\droptitle}{-5em}
\posttitle{\par\end{center}}

\author{\begin{tabular}{ccc}
  \textbf{Author:}
  \emph{Jakub Bednarz}
  \end{tabular}}

\def\TaskNo{1}
\title{%
  Solution for Homework \textnumero{\TaskNo}\\
  \large Homework\\ PSV 2020/21}
\date{}

\AtBeginDocument{%
  \maketitle}
