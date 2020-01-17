{smcl}
{* 26aug2019}{...}
{cmd:help cbtable}
{hline}

{title:Title}

{phang}
{cmd:cbtable} {hline 2} Create codebook tables from SurveyCTO survey data.


{title:Syntax}

	{cmd:cbtable} {cmd:{help varlist}} {cmd:{help using}} [{cmd:,} {opt append} {opt replace}]


{title:Description}

{pstd}
{cmd:cbtable} creates a new LaTeX table using {help esttab} for each variable
in {help varlist}. The command uses metadata generated by {help ctometa} to
determine which style of table is used. 

{pstd}
{it:select_one} variables are given
a table equivalent to Stata's {cmd:{help tab}} command (using {help estpost} 
{cmd:tabstat}). {it:integer} and {it:decimal} variables are given
a table equivalent to Stata's {cmd:sum, detail} command (using {help estpost} 
{cmd:sum}), with missing variables in a tabulated list below the summary stats. Finally,
{it:select_multiple} variables are given
a table equivalent to Stata's {cmd:{help sum}} command (using {help estpost} 
{cmd:sum}) for each option that could have been selected (i.e. a tabulated list 
of options with the % of respondents selecting each option).

{title:New Variables}

{pstd}
New variables (created outside SurveyCTO) can be added to tables as well by 
specifying a new {it:type} {help char:characteristic} for each variable. {cmd:cbtable}
will recognize 3 new variable types:

		{phang} {cmd:char newvarname[type] binary} will cause {cmd:cbtable} to export
a {help tab}-style table, intended for "Yes/No" variables.

		{phang} {cmd:char newvarname[type] calculate} will cause {cmd:cbtable} to export
a {help tab}-style table, intended for new categorical variables.

		{phang} {cmd:char newvarname[type] integer} will cause {cmd:cbtable} to export
a {help sum}-style table, intended for continuous variables.


{title:Options}

	{phang}{opt append} will append the resulting LaTeX table to the existing
LaTeX document specified in {cmd:using}.
	
	{phang}{opt replace} will replace the existing LaTeX document specified in {cmd:using}.


{title:Notes}

{pstd}
{cmd:append} or {cmd:replace} must be specified. Specifying both will cause an error.

{pstd}
{cmd:cbtable} will also export each table with a LaTeX label in the caption to
enable cross-referencing within a LaTeX document. The reference label will be "tab:"
and the variable name, with no underscores. E.g. variable {cmd:my_var} will have the label 
{cmd:\label{tab:myvar}}.

{pstd}
{cmd:cbtable} requires {cmd:{search esttab}} in order to run. If {cmd:{search esttab}}
is not installed, {cmd:cbtable} will return an error.

{pstd}

{title:Authors}

{pstd}Aaron Wolf, Yale University{p_end}
{pstd}aaron.wolf@yale.edu{p_end}
