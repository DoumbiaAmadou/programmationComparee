package server

/** Correspond a une erreur survenue lors d'une requete */
case class RequestError(error_code: Int, error_msg: String) {
  override def toString: String = "Error n°" + error_code + ": " + error_msg
}
