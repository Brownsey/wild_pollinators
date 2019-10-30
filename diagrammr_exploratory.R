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

node [fillcolor = green,
      label = 'Apply Fungicide']
b 

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