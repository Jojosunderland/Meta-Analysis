# Create PRISMA flow chart
install.packages('DiagrammeR')
install.packages('PRISMA2020')
library(PRISMA2020)
library(DiagrammeR)


PRISMA_flowdiagram(found = 165, 
                   found_other = 0, 
                   no_dupes = 165, 
                   screened = 100, 
                   excluded_screening = 74, 
                   full_text = 26, 
                   excluded_eligibility = 9, 
                   qualitative = 8, 
                   quantitative = 8)

DiagrammeR::grViz("
digraph prisma {
  graph [layout = dot, rankdir = TB]

  node [shape = rectangle, style = filled, fillcolor = lightblue, fontname = Helvetica]

  A [label = 'Records identified\\nfrom Web of Science (n = 165)']
  B [label = 'Records screened\\n(n=100)']
  C [label = 'Records excluded\\n(n = 74)']
  D [label = 'Full-text articles assessed\\n(n = 26)']
  E [label = 'Full-text articles excluded\\n(n = 16)']
  F [label = 'Reasons for exclusion:\\nNo access (n = 2)\\nNo effect size data (n = 14)']
  G [label = 'Studies included\\nin meta-analysis\\n(n = 10)']

  A -> B
  B -> C [label = 'Excluded']
  B -> D [label = 'Accepted']
  D -> E [label = 'Excluded']
  E -> F
  D -> G [label = 'Data extracted']
}
")
