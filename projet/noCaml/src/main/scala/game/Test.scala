package game

/**Ce fichier réunis les divers implémentations pour tester le code
 * du fichier Game.
 * 
 */

/**CreateGame2players
 * (test de CreateGame et JoinGame)
 * Permet de tester si on peux créer une partie à 2 joueur.
 * Note : ce code est à faire fonctionner avec JoinGame1Player
 */
object CreateGame2players{
  def main(args: Array[String]):Unit={
    println("Creation de la partie")
    val nb_turn = 1000
    val nb_ants=42
    val nb_players = 2
    val minimal_players = 2
    val g1 = new CreateGame(  "koko","atat",List("all"),
                              "test_create_Game!",100,nb_turn,
                              nb_ants,nb_players,minimal_players,1,100)
    g1.main_loop()
    
  }
}

/**JoinGame1player
 * (test de JoinGame)
 * Ce code permet de tester si on peux rejoindre une partie.
 * Note : Il faut faire le test avec une autre personne.
 * Pour avoir l'id de la partie.
 */
object JoinGame1player{
  def main(args: Array[String]):Unit={
    println("Creation de la partie")
    val nb_turn = 1000
    val nb_ants=42
    val nb_players = 2
    val minimal_players = 2
    val id ="444681616313643089568182443820652330"//chiffre à rentrer à la main
    val g2 = new JoinGame(  "ikki","atat",List("all"),
                            "test_create_Game!",100,
                            nb_turn,nb_ants,nb_players,
                            minimal_players,1,100,id)    
    g2.main_loop()
    
  }
}

/**CreateGame1player
 * 1er test de CreateGame.
 * Crée une partie à 1 joueur
 */
object CreateGame1player{
  def main(args: Array[String]):Unit={
    println("Creation de la partie")
    val nb_turn = 25
    val nb_ants=25
    val nb_players = 1
    val minimal_players = 1
    val g1 = new CreateGame(  "koko","atat",List("all"),
                              "test_create_Game!",10,nb_turn,
                              nb_ants,nb_players,minimal_players,1,100)
    g1.main_loop()
    
  }
}