library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
flowchart<-"digraph study_FC {
  # a 'graph' statement
  graph [fontname = Helvetica]
  #labeljust = 'l';
  # several 'node' statements
  node [shape = rect, width = 3]
  A [label='ECMO insertion\\lFrom Jan 2014 to June 2020\\l(N=852)\\l']
  B [label='Age >=18\\l(N=765)\\l']
  C [label='First ECMO insertion\\l(N=705)\\l']
  D [label='Study population\\l(N=639)\\l']
  none1 [label='',width=0,height=0]
  none2 [label='',width=0,height=0]
  ex1 [label='Repeated ECMO insertion\\l(N=56)\\lHopeless transfer\\l(N=4)\\l']
  ex2 [label='Sepsis before ECMO\\l(N=66)\\l']
  
  A->B
  

  B->none1 [dir=none]
  none1->C; none1->ex1 [minlen=3]
  {rank=same;none1 ex1}
  
  C->none2 [dir=none]
  none2->D; none2->ex2 [minlen=3]
  {rank=same;none2 ex2}
  
  
}"
#[1]: 'ECMO insertion\\nFrom Jan 2014 to June 2020\\n(N=852)'"

