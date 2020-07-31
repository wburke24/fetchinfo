#' fetchinfo
#'
#' Outputs info on R session, host machine, and pretty picture
#' @author William Burke
#' @import crayon
#' @export

fetchinfo = function(vars = NULL) {

  # --- Styles ---
  rblue = make_style(grDevices::rgb(22,92,170, maxColorValue = 255))
  rgrey = make_style(grDevices::rgb(203,206,208, maxColorValue = 255))

  # --- Variables & Formatting ---
  # header stuff
  header = c(rblue(Sys.info()[8]) %+% rgrey("@") %+% rblue(Sys.info()[4]))
  uline = rgrey(strrep("-", nchar( paste0(Sys.info()[8], "@", Sys.info()[4]) )))

  # system and setup info
  os = c(rblue("OS") %+% rgrey(": " %+% paste0(Sys.info()[c(1,2)],collapse = "") ))
  pkg_libs = c(rblue("Library") %+% rgrey(": " %+% path.expand(Sys.getenv("R_LIBS")) ))

  # hardware info (WINDOWS)
  if (Sys.info()[1] == "Windows") {
    cpuread = system("wmic cpu get maxclockspeed, name, ThreadCount, numberofcores",intern = T)
    cpuinfo = unlist(strsplit(trimws(substr(cpuread[2], 0, nchar(cpuread[2]) - 2)), "  +"))
    cpuname = gsub(pattern = "\\(R\\)", replacement = "", cpuinfo[2], )
    cpuname = gsub(pattern = "Core\\(TM\\) ", replacement = "", cpuname, )
    cpuname = gsub(pattern = "CPU ", replacement = paste0(cpuinfo[3],"(",cpuinfo[4],") "), cpuname, )
    cpu = c(rblue("CPU") %+% rgrey(": " %+% cpuname))

    gpuread = system("wmic path win32_VideoController get name", intern = T)
    gpu = c(rblue("GPU") %+% rgrey(": " %+% trimws(substr(gpuread[2], 0, nchar(gpuread[2]) - 2))))

    ramtotread = system("wmic computersystem get TotalPhysicalMemory" , intern = T)[2]
    ramtot = ceiling(as.numeric(trimws(substr(ramtotread, 0, nchar(ramtotread) - 2)))/1024^3)
    ramavailread = system("wmic OS get FreePhysicalMemory" , intern = T)[2]
    ramavail = as.numeric(trimws(substr(ramavailread, 0, nchar(ramavailread) - 2)))/1024^2
    ram = c(rblue("Memory") %+% rgrey(": " %+% as.character(round(ramtot - ramavail,1)) %+% " / " %+% as.character(ramtot)))

  }



  # --- Console Output ---

  catout =
    c("\n",
      rgrey("         **::::::::**         ")                                                         %+% "\t" %+% header %+% "\n",
      rgrey("    *:.........::::::::::     ")                                                         %+% "\t" %+% uline %+% "\n",
      rgrey("  :......:*  ") %+% rblue("**********") %+% rgrey(" *:*  ")                              %+% "\t" %+% os %+% "\n",
      rgrey(" .....:      ") %+% rblue("***********") %+% rgrey("   *: ")                             %+% "\t" %+% pkg_libs %+% "\n",
      rgrey(":.....       ") %+% rblue("****") %+% "     " %+% rblue("*****") %+% rgrey(" **")        %+% "\t" %+% cpu %+% "\n",
      rgrey("*....:       ") %+% rblue("****") %+% "     " %+% rblue("*****") %+% rgrey(" **")        %+% "\t" %+% gpu %+% "\n",
      rgrey(" :.:::*      ") %+% rblue("************") %+% rgrey("   * ")                             %+% "\t" %+% ram %+% "\n",
      rgrey("   *::::*    ") %+% rblue("****") %+% "  " %+% rblue("****") %+% rgrey("   .*   ")       %+% "\n",
      rgrey("      *:::::*") %+% rblue("****") %+% rgrey(":**") %+% rblue("*****") %+% rgrey("'    ") %+% "\n",
      rgrey("             ") %+% rblue("****") %+% "     " %+% rblue("****") %+% "    "               %+% "\n",
      rgrey("             ") %+% rblue("****") %+% "      " %+% rblue("****") %+% "   "               %+% "\n"
      )

  cat(catout)

    # cat( "\n",
    # "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@" ,"\n",
    # "@@@@@@@@@,,,,,,,,,,,,,,,,******@@@@@@@@", "\n",
    # "@@@@@,,,,,,,,,,,,,,,,**************@@@@", "\n",
    # "@@@,,,,,,,,,,,@@@@@@@@@@@@@@@@@@@****@@", "\n",
    # "@,,,,,,,,,@@@@@@@(((((((((((((((((@@***", "\n",
    # "@,,,,,,,,@@@@@@@@((((((((((((((((((#@*/", "\n",
    # "@,,,,,,*@@@@@@@@@((((((@@@@@@(((((#(@//", "\n",
    # "@,,******@@@@@@@@(((((((((((((((###@#//", "\n",
    # "@@@********@@@@@@(((((((((((((((#@@//@@", "\n",
    # "@@@@@.**********@((((((@@((######/@@@@@", "\n",
    # "@@@@@@@@@@*******((((((////#######@@@@@", "\n",
    # "@@@@@@@@@@@@@@@@@(((((#@@@@@########@@@", "\n",
    # "@@@@@@@@@@@@@@@@@(((((#@@@@@@@#######@@")

    #   $$$$$$MFV**::::::::**VFM$$$$$$
    #   $$$F*:.........::::::::::VM$$$
    #   $F:......:*VFMN$$$$$NMMFV*:*F$
    #   V.....:VM$$$V***********VFF*:F
    #   :.....F$$$$$V****VVVVV*****M**
    #   *....:M$$$$$V****F$$$M*****M**
    #   M:.:::*F$$$$V************VFV*M
    #   $NV*::::*VFMV*****V****VFV*FN$
    #   $$$$MV*:::::******V******VN$$$
    #   $$$$$$$$MMFVV****VFFMF****VM$$
    #   $$$$$$$$$$$$V****F$$$$FVVVVVM$


}
