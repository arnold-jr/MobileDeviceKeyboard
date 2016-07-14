import Suggester._

val sentence = "The third thing that I need to tell you is that this thing does not think thoroughly."

val farm = createTree("Cat9834cat cat dog cow 1234;_cattle dog . dart adart")
farm parse


val q = expand("darts")
q map (farm contains _)

val q2 = expand("dogs")
q2 map (farm contains _)


farm getSuggestions "ca"
farm getSuggestions "d"
farm getSuggestions "a"



