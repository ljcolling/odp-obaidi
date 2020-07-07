# Install ODP helper
remotes::install_github("ljcolling/ODPHelper", ref = "dev")
xfun::pkg_attach2("osfr")

obaidi_download_info <- ODPHelper::download_obaidi()
dir.create("made")
saveRDS(object = obaidi_download_info, file = "made/obaidi_download_info.Rdata")
