package gastro

package object utils {

  def sanitizeActorName(name: String): String =
    name
      .trim
      .replaceAll("\n+", "")
      .replaceAll("\\s+", "")
      .replaceAll("\\(+", "")
      .replaceAll("\\)+", "")
      .replace("é", "")
      .replace("è", "")
      .replace("ê", "")
      .replace("à", "")
      .replace("$", "")
}
