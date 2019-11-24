library(DiagrammeR)
#~~~~LOOKING AT DECISION TREE MODELLING~~~####
first_step <- grViz("
digraph dot {

graph [layout = dot,
       rankdir = LR]

node [shape = circle,
      style = filled,
      color = grey,
      label = '']

node [fillcolor = red,
      label = 'Before Bloom']
a

node [fillcolor = red,
      label = 'Apply Fungicide']
b c

node [fillcolor = green,
      label = 'Apply Insecticide']
c 

node [fillcolor = green,
      label = 'Apply Both']
d

node [fillcolor = green,
      label = 'Apply Nothing']
e

node [fillcolor = orange]

edge [color = grey]
a -> {b c d e}
}")
first_step





tree_2_2_2 <- grViz("
digraph dot {

graph [layout = dot,
       rankdir = LR]

node [shape = circle,
      style = filled,
      color = grey,
      label = '0']

node [fillcolor = red,
      label = '19']
a

node [fillcolor = green,
label = '15']
b 

node [fillcolor = green,
label = '4']
c

node [fillcolor = orange,
label = '3']
d 

node [fillcolor = orange,
label = '12']
e

node [fillcolor = orange,
label = '0']
f 

node [fillcolor = orange,
label = '4']
g

node [fillcolor = grey,
label = '1']
h 

node [fillcolor = grey,
label = '2']
i 

node [fillcolor = grey,
label = '9']
j 

node [fillcolor = grey,
label = '3']
k

node [fillcolor = grey,
label = '4']
o 

node [fillcolor = grey,
label = '0']
n

node [fillcolor = grey,
label = '0']
m 


node [fillcolor = grey,
label = '0']
i l

edge [color = grey]
a -> {b c}
b -> {d e}
c -> {f g}
d -> {h i}
e -> {j k}
f -> {l m}
g -> {n o}
}")
tree_2_2_2


tree_2_2_2 <- grViz("
digraph dot {

graph [layout = dot,
       rankdir = LR]

node [shape = circle,
      style = filled,
      color = grey,
      label = '0']

node [fillcolor = red,
      label = '16']
a

node [fillcolor = green,
label = '10']
b 

node [fillcolor = green,
label = '6']
c

node [fillcolor = orange,
label = '1']
d 

node [fillcolor = orange,
label = '9']
e

node [fillcolor = orange,
label = '1']
f 

node [fillcolor = orange,
label = '5']
g

node [fillcolor = grey,
label = '1']
h 

node [fillcolor = grey,
label = '9']
j 

node [fillcolor = grey,
label = '2']
o 

node [fillcolor = grey,
label = '3']
n

node [fillcolor = grey,
label = '1']
m 


node [fillcolor = grey,
label = '0']
i k l

edge [color = grey]
a -> {b c}
b -> {d e}
c -> {f g}
d -> {h i}
e -> {j k}
f -> {l m}
g -> {n o}
}")
tree_2_2_2



























whole_decision_tree










whole_decision_tree <- grViz("
digraph dot {

graph [layout = dot,
       rankdir = LR]

node [shape = circle,
      style = filled,
      color = grey,
      label = '']

node [fillcolor = red]
a

node [fillcolor = green]
b c d e

node [fillcolor = orange]

edge [color = grey]
a -> {b c d e}
b -> {f g h i j k}
c -> {l m n o p q}
d -> {q s t u v w}
e -> {x y z aa ab ac}
f -> {ad ae af ag}
g -> {ah ai aj ak}
h -> {al am an ao}
i -> {ap aq ar as}
j -> {at au av aw}
k -> {ax ay az ba}
l -> {bb bc bd be}
m -> {bf bg bh bi}
n -> {bj bk bl bm}
o -> {bn bo bp bq}
p -> {bs bt bu bv}
q -> {bw bx by bz}
r -> {ca cb cc cd}
s -> {ce cf ch ci}
t -> {cj ck cl cm}
u -> {cn co cp cq}
v -> {cr cs ct cu}
w -> {cv cw cx cy}
x -> {cz da db dc}
y -> {dd de df dg}
z -> {dh di dj dk}
aa -> {dl dm dn do}
ab -> {dp dq dr ds}
ac -> {ds dt du dv}
}")
whole_decision_tree