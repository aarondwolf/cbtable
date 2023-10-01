*! version 1.0.1  16sep2019 Aaron Wolf, aaron.wolf@u.northwestern.edu
cap program drop cbtable
program define cbtable

	syntax varname using [, append replace addnote(string)]
	cap assert ("`append'" == "append" | "`replace'" == "replace")
		if _rc != 0 {
			di as error "either {bf:append} or {bf:replace} must be specified."
			exit 198
		}
	cap assert ("`append'" == "" | "`replace'" == "")
		if _rc != 0 {
			di as error "{bf:append} and {bf:replace} cannot both be specified."
			exit 198
		}
	
	eststo clear 
	
	* Common formatting
	local sumcells	count(label(N) fmt(0)) mean(label(Mean) fmt(2)) sd(label(SD) fmt(2)) ///
					p1(fmt(0)) p5(fmt(0)) p10(fmt(0)) p25(fmt(0)) 			///
					p50(fmt(0)) p75(fmt(0)) p90(fmt(0)) p95(fmt(0)) p99(fmt(0))
	local tabcells b(label(Freq.)) pct(label(\%) fmt(1))
	local smcells  sum(label(Selected) fmt(0))	count(label(N) fmt(0)) pct(label(\%) fmt(1)) 
	eststo clear
	
		
	* Pull metadata
	local var `varlist'
	local ref = subinstr("`var'","_","",.)
	local varlabel: variable label `var'
	local label = substr("``var'[CTO_label]'",1,70) + cond(strlen("``var'[CTO_label]'")>70,"...","")
	local label = subinstr("`label'","*","",.)
	local hint = substr(`"``var'[CTO_hint]'"',1,70) + cond(strlen(`"``var'[CTO_hint]'"')>70,"...","")
	local hint = subinstr(subinstr(`"`hint'"',`"""',"",.),"*","",.)
	local relevance=cond("``var'[CTO_relevance]'"!="",`"``var'[CTO_relevance]'"',"Always Asked")
	local relevance = subinstr(`"`relevance'"',`"""',"",.)
	local relevance = subinstr("`relevance'","&","\&",.)
	
	* Notes
	local notes `""Question Type: \texttt{``var'[CTO_type]'}"	"Original Name: \texttt{``var'[CTO_name]'}"	"Question Asked: \texttt{`label'}" "Hint: \texttt{`hint'}" "Asked If: \texttt{`relevance'}""'
	local title "\texttt{`var'} | `varlabel'\label{tab:`ref'}"
		
	* Integer/Decimal
	if inlist("``var'[CTO_type]'","integer","decimal") & "``var'[topcode_original]'" == "" & "``var'[topcode_indicator]'" == "" {
		estpost sum `var', detail
		qui count if `var' == .a
			if `r(N)' > 0 {
				estadd scalar rf = `r(N)'
				local rf `""rf Refused""'
			}
		qui count if `var' == .b
			if `r(N)' > 0 {
				estadd scalar dn = `r(N)'
				local dn `""dn Don't Know""'
			}
		qui count if `var' == .c
			if `r(N)' > 0 {
				estadd scalar na = `r(N)'
				local na `""na N/A (Not Relevant)""'
			}
		qui count if `var' == .d
			if `r(N)' > 0 {
				estadd scalar er = `r(N)'
				local er `""er SurveyCTO Error""'
			}
		esttab `using', append booktabs cells("`sumcells'") noobs ///
			nomtitles scalars(`rf' `dn' `na' `er') 	nonumbers title(`title') ///
			coeflabels(`var' "Value") addnotes(`notes' "Topcoded @ ``var'[topcode_max]'" "`addnote'")
	}
	
	* select_one
	if "``var'[CTO_type]'"=="select_one" {
		estpost tab `var', miss
		esttab `using', append booktabs cells("`tabcells'") noobs ///
			label varlabels(`e(labels)') nomtitles nonumbers title(`title') addnotes(`notes' "`addnote'")
	}	
	
	* select_multiple
	if "``var'[CTO_type]'"=="select_multiple" {
	preserve
		foreach sm of varlist `var'_* {
			if "``sm'[CTO_sm_name]'" == "``var'[CTO_name]'" {
				local vlist `vlist' `sm'
				local smvlab: variable label `sm'
				local smvlab = subinstr("`smvlab'","==","=",.)
				local smvlab = subinstr("`smvlab'","`var'=","",.)
				la var `sm' "`smvlab'"
			}
		}
		estpost sum `vlist'
			estadd matrix pct = e(mean)*100
		esttab `using', append booktabs cells("`smcells'") noobs ///
			label nomtitles	nonumbers title(`title') addnotes(`notes' "`addnote'")
	restore
	}
	
	
	* calculate
	if "``var'[CTO_type]'"=="calculate" {
		estpost tab `var', miss
		esttab `using', append booktabs cells("`tabcells'") noobs ///
			label varlabels(`e(labels)') nomtitles nonumbers title(`title') addnotes(`notes' "`addnote'")
	
	}
	
	* Undefined type, but with binary type or post-calculated type
	if "``var'[CTO_type]'"==""  & ("``var'[type]'" == "binary" | "``var'[type]'" == "calculate") {
		estpost tab `var', miss
		esttab `using', append booktabs cells("`tabcells'") noobs ///
			label varlabels(`e(labels)') nomtitles nonumbers title(`title') 	///
			addnotes("Variable calculated ex post." "`addnote'")
		}
		
	* Undefined type, but with integer characteristic
	if "``var'[CTO_type]'"==""  & "``var'[type]'" == "integer" {
		estpost sum `var', detail
		qui count if `var' == .a
			if `r(N)' > 0 {
				estadd scalar rf = `r(N)'
				local rf `""rf Refused""'
			}
		qui count if `var' == .b
			if `r(N)' > 0 {
				estadd scalar dn = `r(N)'
				local dn `""dn Don't Know""'
			}
		qui count if `var' == .c
			if `r(N)' > 0 {
				estadd scalar na = `r(N)'
				local na `""na N/A (Not Relevant)""'
			}
		qui count if `var' == .d
			if `r(N)' > 0 {
				estadd scalar er = `r(N)'
				local er `""er SurveyCTO Error""'
			}
		esttab `using', append booktabs cells("`sumcells'") noobs ///
			nomtitles scalars(`rf' `dn' `na' `er') 	nonumbers title(`title') ///
			coeflabels(`var' "Value") addnotes("Variable calculated ex post." "`addnote'")
	}

	eststo clear

end

