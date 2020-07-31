#' fetchinfo
#'
#' Outputs info on R session, host machine, and pretty picture
#' @author William Burke
#' @export

fetchinfo = function() {

  cpuinfo = system("wmic cpu get maxclockspeed, name, ThreadCount, numberofcores",intern = T)
  cpuinfo = trimws(substr(cpuinfo[2], 0, nchar(cpuinfo[2]) - 2))
  cpuinfo = unlist(strsplit(cpuinfo, "  +"))
  cpucore = paste0(cpuinfo[3],"(",cpuinfo[4],")")

  # gpu = system("wmic path win32_VideoController get name", intern = T)
  # gpu = trimws(substr(gpu[2], 0, nchar(gpu[2]) - 2))
  #
  # ram = system('wmic MEMORYCHIP get BankLabel, DeviceLocator, MemoryType, TypeDetail, Capacity, Speed', intern = T)
  #
  # ram = system('wmic MEMORYCHIP', intern = T)

  catout = c(
    "\n",
    "\t", "ooooooooo.   ", "      OS:", Sys.info()[c(1,2)], "\n",
    "\t", "888   `Y88.  ", "Pkg libs:", path.expand(Sys.getenv("R_LIBS")), "\n",
    "\t", "888   .d88'  ", "Computer:", Sys.info()[4], "\n",
    "\t", "888ooo88P'   ", "    User:", Sys.info()[8], "\n",
    "\t", "888`88b.     ", "     CPU:", cpuinfo[2], "\n",
    "\t", "888  `88b.   ", "   Cores:", cpucore, "\n",
    "\t", "o888o  o888o ", "\n"
  )

  cat(catout)

# ooooooooo.
# 888   `Y88.
# 888   .d88'
# 888ooo88P'
# 888`88b.
# 888  `88b.
# o888o  o888o

#   8888888b.
#   888   Y88b
#   888    888
#   888   d88P
#   8888888P"
#   888 T88b
#   888  T88b
#   888   T88b



}
