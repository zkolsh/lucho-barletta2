\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{corsage}[2025/08/03 we're just paper crowns in paper gowns]

\def\corsageclass{book}
\DeclareOption{tiny}{\def\corsageclass{article}}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{\corsageclass}}
\ProcessOptions\relax
\LoadClass[twoside, 10pt]{\corsageclass}

\RequirePackage[symbol, perpage]{footmisc}
\RequirePackage[defaultlines=3, all]{nowidow}
\RequirePackage{enumitem}
\RequirePackage{etoolbox}
\RequirePackage{fancyhdr}
\RequirePackage{fourier-orns}
\RequirePackage{geometry}
\RequirePackage{iftex}
\RequirePackage{lmodern}
\RequirePackage{titlesec}
\RequirePackage{microtype}

\newcommand{\periodafter}[1]{#1.}
\ifstrequal{\corsageclass}{book}{
	\titleformat{\chapter}{\normalfont\Huge\bfseries}{\periodafter\thechapter}{1em}{\periodafter}
	\titleformat{\section}{\normalfont\Large\bfseries}{\periodafter\thesection}{1em}{\periodafter}
}{
	\titleformat{\section}{\normalfont\Large\bfseries}{\periodafter\thesection}{1em}{\periodafter}
}

\geometry{inner=4cm, outer=3cm, top=4.5cm, bottom=3cm}
\setlength{\parindent}{1.27cm}
\setlength{\parskip}{4pt plus 1pt minus 4pt}
\setlist[itemize, 1]{label=\starredbullet}
\setlist[itemize, 2]{label=$\cdot$}
\setlist{topsep=\parskip}
\raggedbottom

\ifluatex
	\pagewidth=\paperwidth
	\pageheight=\paperheight
\else
	\pdfpagewidth=\paperwidth
	\pdfpageheight=\paperheight
\fi

\pagestyle{fancy}
\fancyhf{}

\fancyhead[C]{\decoone}
\fancyhead[LE, RO]{\thepage}
\fancyhead[LO]{\leafSE{} \nouppercase{\it\leftmark}}
\fancyhead[RE]{\nouppercase{\it\rightmark} \leafSW}

\let\oldcleardoublepage\cleardoublepage
\renewcommand{\cleardoublepage}{%
	\clearpage%
	\ifodd\value{page}\else%
		\thispagestyle{empty}%
	\fi%
	\oldcleardoublepage%
}

\ifx\corsageclass\article
	\renewcommand{\sectionmark}[1]{\markboth{#1}{}}
	\newcommand{\sectionbreak}{\cleardoublepage}
\else
\fi
