-include("structures.hrl").

-define(Concept, #class{name = golf, categories = [true, false]}).
-define(Attributes, [#attribute{name = aura, type = nominal, values = [sloneczna, pochmurna, deszczowa]}, #attribute{name = temperatura, type = nominal, values = [ciepla, umiarkowana, zimna]}, #attribute{name = wilgotnosc, type = nominal, values = [normalna, duza]}, #attribute{name = wiatr, type = nominal, values = [slaby, silny]} ]).

