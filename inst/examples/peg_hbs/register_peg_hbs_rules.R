# =============================================================================
# register_peg_hbs_rules.R
# The PEG + HBS study-specific recode rules, registered onto cohortHarmonize via
# register_recode(). This is the "bring your own rules" pattern: the package ships
# generic rules; here we add the ~40 study-specific ones the PEG/HBS source maps
# reference (checkbox roll-ups, scale thresholds, smoking logic, etc.).
# Source this file after loading the package, before harmonize_source().
# =============================================================================
suppressPackageStartupMessages({library(cohortHarmonize); library(dplyr)})

# ---- shared helpers ---------------------------------------------------------
PEG_MISS <- c(-4, -6, -7, -8, -9)
.n  <- function(df, c) if (c %in% names(df)) suppressWarnings(as.numeric(df[[c]])) else rep(NA_real_, nrow(df))
.nc <- function(x) { x[x %in% PEG_MISS] <- NA; x }
.pd <- function(x) { x <- trimws(as.character(x)); x[x == "" | tolower(x) %in% c("na","nan")] <- NA
  fmts <- c("%Y-%m-%d","%m-%d-%Y","%m/%d/%Y","%Y/%m/%d"); best <- as.Date(rep(NA_character_, length(x))); bn <- -1L
  for (f in fmts) { d <- suppressWarnings(as.Date(x, format = f)); k <- sum(!is.na(d)); if (k > bn) { bn <- k; best <- d } }; best }
.coal <- function(df, cols) { cols <- cols[cols %in% names(df)]; if (!length(cols)) return(rep(NA_real_, nrow(df)))
  Reduce(function(a,b) dplyr::coalesce(a,b), lapply(cols, function(c) .nc(.n(df,c)))) }
.anyflag <- function(df, cols) { cols <- cols[cols %in% names(df)]; if (!length(cols)) return(rep(NA_integer_, nrow(df)))
  m <- sapply(cols, function(c) .nc(.n(df,c))); if (is.null(dim(m))) m <- matrix(m, ncol=1)
  apply(m, 1, function(r) if (all(is.na(r))) NA_integer_ else if (any(r==1, na.rm=TRUE)) 1L else 0L) }

# ============================ PEG rules ======================================
register_recode("binary",  function(df, cols, param=NULL){ v <- .nc(.n(df,cols[1])); v[!v %in% c(0,1)] <- NA; as.integer(v) })
register_recode("numeric", function(df, cols, param=NULL){ v <- .n(df,cols[1]); v[v %in% PEG_MISS | v < 0] <- NA; v })
register_recode("numeric_todo", function(df, cols, param=NULL){ v <- .n(df,cols[1]); v[v %in% PEG_MISS | v < 0] <- NA; v })
register_recode("passthrough_int", function(df, cols, param=NULL) as.integer(.nc(.n(df,cols[1]))))
register_recode("gt0_any", function(df, cols, param=NULL){ cols <- cols[cols %in% names(df)]; if(!length(cols)) return(rep(NA_integer_,nrow(df)))
  m <- sapply(cols, function(c) .nc(.n(df,c))); if(is.null(dim(m))) m <- matrix(m,ncol=1)
  apply(m,1,function(r) if(all(is.na(r))) NA_integer_ else if(any(r>0,na.rm=TRUE)) 1L else 0L) })
register_recode("any_of", function(df, cols, param=NULL) .anyflag(df, cols))
register_recode("sex_text", function(df, cols, param=NULL){ s <- tolower(trimws(as.character(df[[cols[1]]])))
  dplyr::case_when(is.na(s)|s %in% c("","na","nan") ~ NA_integer_, s %in% c("f","female","1") ~ 1L,
                   s %in% c("m","male","2") ~ 2L, TRUE ~ 3L) })
register_recode("education_map", function(df, cols, param=NULL){ v <- .nc(.n(df,cols[1]))
  lut <- c(`1`=1L,`2`=1L,`3`=2L,`4`=3L,`5`=4L,`6`=5L,`7`=NA_integer_); out <- lut[as.character(v)]; unname(out) })
register_recode("cig_current", function(df, cols, param=NULL){ v <- .nc(.n(df,cols[1])); dplyr::case_when(v==1~1L, v==2~0L, TRUE~NA_integer_) })
register_recode("arthritis_type", function(df, cols, param=NULL){
  o <- if("D12_Osteoarthritis" %in% names(df)){v<-.nc(.n(df,"D12_Osteoarthritis"));v[!v%in%c(0,1)]<-NA;v} else NA
  r <- if("D8_RheumatoidArthritis" %in% names(df)){v<-.nc(.n(df,"D8_RheumatoidArthritis"));v[!v%in%c(0,1)]<-NA;v} else NA
  dplyr::case_when(r==1~2L, o==1~1L, TRUE~NA_integer_) })
register_recode("derive_latino", function(df, cols, param=NULL){
  cs <- names(df)[startsWith(names(df),"A3C_") & !grepl("Specify", names(df))]
  if(!length(cs)) return(rep(NA_integer_,nrow(df))); m <- sapply(cs,function(c){v<-.nc(.n(df,c));v[!v%in%c(0,1)]<-NA;v})
  if(is.null(dim(m))) m<-matrix(m,ncol=1); apply(m,1,function(r) if(all(is.na(r))) NA_integer_ else if(any(r==1,na.rm=TRUE)) 1L else 0L) })
register_recode("derive_race", function(df, cols, param=NULL){
  grp <- list(a="A3A_",b="A3B_",n="A3D_",w="A3E_",o="A3F_"); code <- c(a=1L,b=2L,n=3L,w=4L,o=5L)
  flag <- function(p){ cs <- names(df)[startsWith(names(df),p) & !grepl("Specify", names(df))]
    if(!length(cs)) return(rep(FALSE,nrow(df))); m <- sapply(cs,function(c){v<-.nc(.n(df,c));!is.na(v)&v==1})
    if(is.null(dim(m))) m<-matrix(m,ncol=1); apply(m,1,any) }
  fl <- lapply(grp,flag); vapply(seq_len(nrow(df)), function(i){ hits <- names(code)[vapply(names(code),function(g) fl[[g]][i],logical(1))]
    if(!length(hits)) NA_integer_ else if(length(hits)>1) 5L else unname(code[hits]) }, integer(1)) })
register_recode("unavailable", function(df, cols, param=NULL) rep(NA_real_, nrow(df)))

# ============================ HBS rules ======================================
register_recode("hbs_direct",       function(df, cols, param=NULL) .coal(df, cols))
register_recode("hbs_binary",       function(df, cols, param=NULL){ v <- .coal(df,cols); v[!v %in% c(0,1)] <- NA; as.integer(v) })
register_recode("hbs_coalesce_bin", function(df, cols, param=NULL){ v <- .coal(df,cols); v[!v %in% c(0,1)] <- NA; as.integer(v) })
register_recode("hbs_coalesce_num", function(df, cols, param=NULL){ v <- .coal(df,cols); v[v<0] <- NA; v })
register_recode("hbs_any_of",       function(df, cols, param=NULL) .anyflag(df, cols))
register_recode("hbs_sex",   function(df, cols, param=NULL){ v <- .coal(df,cols); v[!v %in% c(1,2,3)] <- NA; as.integer(v) })
register_recode("hbs_race_checkbox", function(df, cols, param=NULL){
  cm <- c(`1`=1L,`4`=1L,`2`=2L,`3`=3L,`5`=4L,`98`=5L); present <- cols[cols %in% names(df)]; n <- nrow(df)
  if(!length(present)) return(rep(NA_integer_,n)); grp <- matrix(FALSE,n,5)
  for(c in present){ suf <- sub("^.*___","",c); ck <- {v<-suppressWarnings(as.numeric(df[[c]]));!is.na(v)&v==1}
    if(suf %in% names(cm)){ g<-cm[[suf]]; grp[,g] <- grp[,g] | ck } }
  nc <- rowSums(grp); vapply(seq_len(n),function(i) if(nc[i]==0) NA_integer_ else if(nc[i]>1) 5L else which(grp[i,])[1], integer(1)) })
register_recode("hbs_education_bins", function(df, cols, param=NULL){ v <- .coal(df,cols)
  dplyr::case_when(v>=0&v<=13~1L, v %in% c(14,15)~2L, v>=16&v<=18~3L, v==19~4L, v>=20&v<=22~5L, TRUE~NA_integer_) })
register_recode("hbs_farm", function(df, cols, param=NULL){ v <- .coal(df,cols); dplyr::case_when(v==1~0L,v==2~1L,v==3~2L,TRUE~NA_integer_) })
register_recode("hbs_pesticide", function(df, cols, param=NULL){ v <- .coal(df,cols); dplyr::case_when(v==0~0L,v==1~1L,TRUE~NA_integer_) })
register_recode("hbs_alcohol", function(df, cols, param=NULL){ v <- .coal(df,cols); dplyr::case_when(v==0~0L,v %in% c(1,2)~1L,TRUE~NA_integer_) })
register_recode("hbs_smoke_type", function(df, cols, param=NULL){
  sm<-.nc(.n(df,cols[1])); ck<-if(length(cols)>=2).nc(.n(df,cols[2])) else rep(NA,nrow(df))
  smf<-if(length(cols)>=3).nc(.n(df,cols[3])) else rep(NA,nrow(df)); ckf<-if(length(cols)>=4).nc(.n(df,cols[4])) else rep(NA,nrow(df))
  base<-ifelse(is.na(sm),NA,ifelse(sm==1 & !is.na(ck) & ck==1,1L,0L)); fu<-ifelse(is.na(smf),NA,ifelse(smf==1 & !is.na(ckf) & ckf==1,1L,0L))
  dplyr::case_when((base==1)|(fu==1)~1L, is.na(base)&is.na(fu)~NA_integer_, TRUE~0L) })
register_recode("hbs_current_smoker", function(df, cols, param=NULL){
  cur<-.coal(df, cols[grepl("currently_not_smoke",cols)]); amt<-.coal(df, cols[grepl("current_average",cols)])
  dplyr::case_when(is.na(cur)&is.na(amt)~NA_integer_, (cur==1&(is.na(amt)|amt>0))|(is.na(cur)&amt>0)~1L, TRUE~0L) })
register_recode("hbs_type_cancer", function(df, cols, param=NULL){
  br<-.anyflag(df,cols[grepl("breast",cols)]); co<-.anyflag(df,cols[grepl("colon",cols)]); pr<-.anyflag(df,cols[grepl("prostate",cols)])
  dplyr::case_when(br==1~0L,co==1~1L,pr==1~2L,TRUE~NA_integer_) })
register_recode("hbs_head_injury", function(df, cols, param=NULL){
  tr<-.anyflag(df,cols[grepl("traumatic",cols)]); p1<-.anyflag(df,cols[grepl("pdrf_1",cols)]); p2<-.anyflag(df,cols[grepl("pdrf_2",cols)])
  pos<-(!is.na(tr)&tr==1)|(!is.na(p1)&p1==1&!is.na(p2)&p2==1); ans<-!is.na(tr)|!is.na(p1)
  dplyr::case_when(pos~1L, ans~0L, TRUE~NA_integer_) })
register_recode("hbs_arthritis_type", function(df, cols, param=NULL){
  o<-.anyflag(df,cols[grepl("osteo",cols)]); r<-.anyflag(df,cols[grepl("rheumatoid",cols)]); dplyr::case_when(r==1~2L,o==1~1L,TRUE~NA_integer_) })
register_recode("hbs_med_flag", function(df, cols, param=NULL){ present<-cols[cols %in% names(df)]; if(!length(present)) return(rep(NA_integer_,nrow(df)))
  m<-sapply(present,function(c){v<-suppressWarnings(as.numeric(df[[c]]));!is.na(v)&v==1}); if(is.null(dim(m))) m<-matrix(m,ncol=1); as.integer(apply(m,1,any)) })
register_recode("hbs_gds_sum", function(df, cols, param=NULL){ present<-cols[cols %in% names(df)]; if(!length(present)) return(rep(NA_real_,nrow(df)))
  m<-sapply(present,function(c) .nc(.n(df,c))); if(is.null(dim(m))) m<-matrix(m,ncol=1); apply(m,1,function(r) if(all(is.na(r))) NA_real_ else sum(r,na.rm=TRUE)) })
register_recode("hbs_gds_cat", function(df, cols, param=NULL){ present<-cols[cols %in% names(df)]
  s<-if(length(present)){m<-sapply(present,function(c).nc(.n(df,c)));if(is.null(dim(m)))m<-matrix(m,ncol=1);apply(m,1,function(r) if(all(is.na(r))) NA_real_ else sum(r,na.rm=TRUE))} else rep(NA_real_,nrow(df))
  dplyr::case_when(s>=0&s<=4~1L, s>=5&s<=9~2L, s>=10&s<=15~3L, TRUE~NA_integer_) })
register_recode("hbs_mmse_complete", function(df, cols, param=NULL){ v<-.coal(df,cols); dplyr::case_when(v==0~0L,v==2~1L,TRUE~NA_integer_) })
register_recode("hbs_mmse_score", function(df, cols, param=NULL){ v<-.coal(df,cols); if(any(!is.na(v))&&max(v,na.rm=TRUE)<=1) v<-v*30; v[v<0]<-NA; v })
register_recode("hbs_hy", function(df, cols, param=NULL){ v<-.coal(df,cols); dplyr::case_when(v==1.5~2L,v==2.5~3L,v>=0&v<=5~as.integer(round(v)),TRUE~NA_integer_) })
register_recode("hbs_scopa_thresh", function(df, cols, param=NULL){ present<-cols[cols %in% names(df)]; if(!length(present)) return(rep(NA_integer_,nrow(df)))
  m<-sapply(present,function(c).nc(.n(df,c))); if(is.null(dim(m))) m<-matrix(m,ncol=1); apply(m,1,function(r) if(all(is.na(r))) NA_integer_ else if(any(r>=2,na.rm=TRUE)) 1L else 0L) })
register_recode("hbs_ndg", function(df, cols, param=NULL){ ec<-.nc(.n(df,cols[1])); nc<-.nc(.n(df,cols[2])); dplyr::case_when(ec %in% c(0,1) & nc==1~1L, ec==2 | nc==0~0L, TRUE~NA_integer_) })
register_recode("hbs_year_of", function(df, cols, param=NULL){ if(!cols[1] %in% names(df)) return(rep(NA_integer_,nrow(df))); as.integer(format(.pd(df[[cols[1]]]),"%Y")) })
register_recode("hbs_age_from_dob", function(df, cols, param=NULL){
  visitc <- getOption("ch.hbs_visit_date_col","enrollment_date"); dobc <- cols[1]
  if(!dobc %in% names(df) || !visitc %in% names(df)) return(rep(NA_integer_,nrow(df)))
  d1<-.pd(df[[dobc]]); d2<-.pd(df[[visitc]]); y<-as.integer(format(d2,"%Y"))-as.integer(format(d1,"%Y"))
  before<-as.integer(format(d2,"%m%d"))<as.integer(format(d1,"%m%d")); a<-y-as.integer(before); a[is.na(d1)|is.na(d2)|a<0|a>120]<-NA; as.integer(a) })

message("Registered ", length(list_recodes()), " recode rules (built-ins + PEG/HBS).")
