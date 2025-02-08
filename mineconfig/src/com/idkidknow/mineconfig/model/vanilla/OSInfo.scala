package com.idkidknow.mineconfig.model.vanilla

/** @param name
 *    one of "windows", "osx" or "linux"
 *  @param version
 *    `sys.props("os.version")`
 *  @param arch
 *    `sys.props("os.arch")`
 */
final case class OSInfo(
    name: String,
    version: String,
    arch: String,
)

object OSInfo {
  def fromSystemProperties: OSInfo = {
    val name = sys.props("os.name") match {
      case name if name.startsWith("Windows") => "windows"
      case name if name.startsWith("Mac") => "osx"
      case name if name.startsWith("Linux") => "linux"
      case name => name.toLowerCase
    }
    val version = sys.props("os.version")
    val arch = sys.props("os.arch")
    OSInfo(name, version, arch)
  }
}
