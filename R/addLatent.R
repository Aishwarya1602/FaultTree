# addLatent.R
# copyright 2015-2016, openreliability.org
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

addLatent<-function(DF, at, mttf, mttr=NULL, pzero=NULL, inspect=NULL, display_under=NULL,
		 tag="", label="", name="",name2="", description="")  {

	at <- tagconnect(DF, at)

	if(label!="")  {
		if(any(DF$Name!="") || any(DF$Name2!="")) {
			stop("Cannot use label once name convention has been established.")
		}
	}
	if(any(DF$Label!="")) {
		if(name!="" || name2!="") {
			stop("Cannot use name convention once label has been established.")
		}
	}

	tp<-2 
	etp<-0
	mt<-DF$P2[which(DF$ID==min(DF$ID))]
	if(mt>0)  {
		etp<-4
	}

	info<-test.basic(DF, at,  display_under, tag)
	thisID<-info[1]
	parent<-info[2]
	gp<-info[3]
	condition<-info[4]


	if(is.null(mttf))  {stop("latent component must have mttf")}
## can't do this yet still need to reference the argument at pf calculation below.
##	if(is.null(mttr)) { mttr<- (-1)}
	if(!is.null(pzero)) {} # silently ignore any input. Argument to be depreciated.

	if(is.null(inspect))  {stop("latent component must have inspection entry")}



	if(is.character(inspect))  {
		if(exists("inspect")) {
			Tao<-eval((parse(text=inspect)))
		}else{
			stop("inspection interval object does not exist")
		}
	}else{
		Tao=inspect
	}

## pzero argument has been depreciated, pzero will be calculated based on mttr, if it is provided

## default Pzero=0, it is only a calculated value if mttr is provided
	pzero<-0
	if(length(mttr)>0) {
		pzero=mttr/(mttf+mttr)
	}
		## fractional downtime method
	pf<-1-1/((1/mttf)*Tao)*(1-exp(-(1/mttf)*Tao))
	pf<- 1-(1-pf)*(1-pzero)
## Now it is okay to set mttr to -1 for ftree entry
	if(is.null(mttr)) { mttr<- (-1)}

## apply default tag names if not specified
	if(tag=="")  {
		tag<-paste0("E_", thisID)
	}

	Dfrow<-data.frame(
		ID=	thisID	,
		GParent=	gp	,
		Tag=	tag	,
		Type=	tp	,
		CFR=	1/mttf	,
		PBF=	pf	,
		CRT=	mttr	,
		MOE=	0	,
		Condition=	condition,
		Cond_Code=	0,
		EType=	etp,
		P1=	pzero	,
		P2=	Tao	,
		Collapse=	0	,
		Label=	label	,
		Name=	name	,
		Name2=	name2	,
		CParent=	at	,
		Level=	DF$Level[parent]+1	,
		Description=	description	,
		UType=	0	,
		UP1=	0	,
		UP2=	0
	)


	DF<-rbind(DF, Dfrow)
	DF
}
