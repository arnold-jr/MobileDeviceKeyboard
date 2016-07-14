import Suggester._

val sentence = "The third thing that I need to tell you is that this thing does not think thoroughly."

val farm = createTree("Cat cow 1234;_cattle dog dart.")
farm parse


val q = expand("darts")
q map (farm contains _)

val q2 = expand("dogs")
q2 map (farm contains _)


farm getSuggestions "do"

