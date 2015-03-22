package comm

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write => swrite}
import net.liftweb.json.FieldSerializer._


/** Ce fichier est destiner aux tests en local pour parser les string json */
object DataJson {
    val register = """{ "status": "completed", "response": {} }"""
    val auth_err ="""{
                        "status": "error",
                        "response": {
                          "error_code": 202165063,
                          "error_msg": "I do not know you, aoaoao."
                        }
                     }"""
    
   val auth_ok = """{ "status": "completed", "response": {} }"""
    
    val status = """{
                      "status": "error",
                      "response": {
                        "error_code": 796193025,
                        "error_msg": "Invalid game identifier 11"
                      }
                    }"""
    
    val whoami ="""{ "status": "completed", "response": { "status": "logged as zzz" } }"""
    

    val play =
      """
{
  "status": "completed",
  "response": {
    "turn": 57,
    "observations": [
      [
        {
          "x": 43,
          "y": 90,
          "dx": 1,
          "dy": 0,
          "brain": "controlled",
          "id": 4,
          "energy": 1,
          "acid": 100
        },
        [
          { "x": 42, "y": 89, "content": { "kind": "grass" } },
          { "x": 43, "y": 89, "content": { "kind": "grass" } },
          { "x": 44, "y": 89, "content": { "kind": "grass" } },
          { "x": 42, "y": 90, "content": { "kind": "grass" } },
          { "x": 43, "y": 90, "content": { "kind": "grass" } },
          { "x": 44, "y": 90, "content": { "kind": "grass" } },
          { "x": 42, "y": 91, "content": { "kind": "grass" } },
          { "x": 43, "y": 91, "content": { "kind": "grass" } },
          { "x": 44, "y": 91, "content": { "kind": "grass" } }
        ],
        [ { "x": 43, "y": 90, "dx": 1, "dy": 0, "brain": "controlled" } ]
      ],
      [
        {
          "x": 23,
          "y": 68,
          "dx": 1,
          "dy": 0,
          "brain": "controlled",
          "id": 3,
          "energy": 1,
          "acid": 100
        },
        [
          { "x": 22, "y": 67, "content": { "kind": "grass" } },
          { "x": 23, "y": 67, "content": { "kind": "grass" } },
          { "x": 24, "y": 67, "content": { "kind": "grass" } },
          { "x": 22, "y": 68, "content": { "kind": "grass" } },
          { "x": 23, "y": 68, "content": { "kind": "grass" } },
          { "x": 24, "y": 68, "content": { "kind": "grass" } },
          { "x": 22, "y": 69, "content": { "kind": "grass" } },
          { "x": 23, "y": 69, "content": { "kind": "grass" } },
          { "x": 24, "y": 69, "content": { "kind": "grass" } }
        ],
        [ { "x": 23, "y": 68, "dx": 1, "dy": 0, "brain": "controlled" } ]
      ],
      [
        {
          "x": 3,
          "y": 269,
          "dx": 0,
          "dy": 1,
          "brain": "controlled",
          "id": 2,
          "energy": 1,
          "acid": 100
        },
        [
          { "x": 2, "y": 268, "content": { "kind": "grass" } },
          { "x": 3, "y": 268, "content": { "kind": "grass" } },
          { "x": 4, "y": 268, "content": { "kind": "grass" } },
          { "x": 2, "y": 269, "content": { "kind": "grass" } },
          { "x": 3, "y": 269, "content": { "kind": "grass" } },
          { "x": 4, "y": 269, "content": { "kind": "grass" } },
          { "x": 2, "y": 270, "content": { "kind": "grass" } },
          { "x": 3, "y": 270, "content": { "kind": "grass" } },
          { "x": 4, "y": 270, "content": { "kind": "grass" } }
        ],
        [ { "x": 3, "y": 269, "dx": 0, "dy": 1, "brain": "controlled" } ]
      ],
      [
        {
          "x": 82,
          "y": 258,
          "dx": -1,
          "dy": 0,
          "brain": "controlled",
          "id": 1,
          "energy": 1,
          "acid": 100
        },
        [
          { "x": 81, "y": 257, "content": { "kind": "grass" } },
          { "x": 82, "y": 257, "content": { "kind": "grass" } },
          { "x": 83, "y": 257, "content": { "kind": "grass" } },
          { "x": 81, "y": 258, "content": { "kind": "grass" } },
          { "x": 82, "y": 258, "content": { "kind": "grass" } },
          { "x": 83, "y": 258, "content": { "kind": "grass" } },
          { "x": 81, "y": 259, "content": { "kind": "grass" } },
          { "x": 82, "y": 259, "content": { "kind": "grass" } },
          { "x": 83, "y": 259, "content": { "kind": "grass" } }
        ],
        [ { "x": 82, "y": 258, "dx": -1, "dy": 0, "brain": "controlled" } ]
      ],
      [
        {
          "x": 55,
          "y": 71,
          "dx": -1,
          "dy": 0,
          "brain": "controlled",
          "id": 0,
          "energy": 1,
          "acid": 100
        },
        [
          { "x": 54, "y": 70, "content": { "kind": "grass" } },
          { "x": 55, "y": 70, "content": { "kind": "grass" } },
          { "x": 56, "y": 70, "content": { "kind": "grass" } },
          { "x": 54, "y": 71, "content": { "kind": "grass" } },
          { "x": 55, "y": 71, "content": { "kind": "grass" } },
          { "x": 56, "y": 71, "content": { "kind": "grass" } },
          { "x": 54, "y": 72, "content": { "kind": "grass" } },
          { "x": 55, "y": 72, "content": { "kind": "grass" } },
          { "x": 56, "y": 72, "content": { "kind": "grass" } }
        ],
        [ { "x": 55, "y": 71, "dx": -1, "dy": 0, "brain": "controlled" } ]
      ]
    ]
  }
}
      """
    val play_lighter =
      """
{
  "status": "completed",
  "response": {
    "turn": 57,
    "observations": [
      [
        {
          "x": 43,
          "y": 90,
          "dx": 1,
          "dy": 0,
          "brain": "controlled",
          "id": 4,
          "energy": 1,
          "acid": 100
        },
        [
          { "x": 42, "y": 89, "content": { "kind": "grass" } },
          { "x": 43, "y": 89, "content": { "kind": "grass" } }
        ],
        [ { "x": 43, "y": 90, "dx": 1, "dy": 0, "brain": "controlled" } ]
      ],
      [
        {
          "x": 23,
          "y": 68,
          "dx": 1,
          "dy": 0,
          "brain": "controlled",
          "id": 3,
          "energy": 1,
          "acid": 100
        },
        [
          { "x": 22, "y": 67, "content": { "kind": "grass" } },
          { "x": 23, "y": 67, "content": { "kind": "grass" } }
        ],
        [ { "x": 23, "y": 68, "dx": 1, "dy": 0, "brain": "controlled" } ]
      ]
    ]
  }
}
      """
    

}
