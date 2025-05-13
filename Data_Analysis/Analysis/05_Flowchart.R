library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)


graph<- grViz("
digraph flowchart {

  graph [layout = dot, rankdir = TB]
  node [shape = box, style = filled, fillcolor = white, fontname = Helvetica, fontsize = 12]

  # Main vertical flow
  Longitudinal [label = 'Longitudinal Food & CGM\\nrecordings\\nN = 109']
  Merged [label = 'Merged\\n(n = 109)']
  Postprandial [label = 'Postprandial data\\n(n = 108)']
  PPG [label = 'PPG data ≥ 20 Observations\\n(n = 72)']
  Final [label = 'Final data set\\n(n = 67)']

  # Side branches
  NoPost [label = 'No postprandial measures\\n(n = 1)']
  TooFewObs [label = '< 20 Observations per Individual\\n(n = 36)']
  NoF1 [label = 'No F1 performance metric\\nof model\\n(n = 5)']

  # Vertical flow
  Longitudinal -> Merged
  Merged -> Postprandial
  Postprandial -> PPG
  PPG -> Final

  # Side exclusions (same rank = horizontal position)
  { rank = same; Merged; NoPost }
  { rank = same; Postprandial; TooFewObs }
  { rank = same; PPG; NoF1 }

  # Arrows to side exclusions
  Merged -> NoPost
  Postprandial -> TooFewObs
  PPG -> NoF1
}
")

graph_svg <- export_svg(graph)

# Save as PNG
rsvg_png(charToRaw(graph_svg), file = "Plots/flowchart.png", width = 800)

# Optional: Save as PDF
rsvg_pdf(charToRaw(graph_svg), file = "flowchart.pdf")